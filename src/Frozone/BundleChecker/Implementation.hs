{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Frozone.BundleChecker.Implementation where

import Frozone.Types
--import Frozone.Model
--import Frozone.User
import Frozone.VCS

import Frozone.Util.General
import Frozone.Util.Logging
import Frozone.Util.Random
import Frozone.Util.Email
import Frozone.Util.Db
--import Frozone.Util.Rest


import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Error
import qualified Cook.Build as D
import qualified Cook.Types as D
import Data.List hiding (group)
import Data.Maybe
import Data.Time
import Database.Persist.Sqlite hiding (get)
import System.FilePath
import Web.Spock hiding (patch)
import Web.Spock.Worker
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as YML
import qualified Database.Persist as DB

data NewBundleArrived
   = NewBundleArrived
   { _nba_proj :: (ProjectId, Project)
   , _nba_user :: (UserId, User)
   , _nba_patchBundle :: BS.ByteString
   , _nba_patchQueue :: WorkQueue BuildRepositoryId
   }

type WorkerErrM a = ErrorT String (WebStateM Connection FrozoneSession FrozoneState) a

emailFrom :: T.Text
emailFrom = "Frozone <thiemann@cp-med.com>" 

{- |* split patch bundle, 
    * create a build repository for each patch
    * add them to the patchQueue
-}
newBundleArrived :: FrozoneQueueWorker NewBundleArrived
newBundleArrived (NewBundleArrived (projId,proj) (userId,user) patchBundleBS patchQueue) =
    do st <- getState
       let
           storageDir = fc_storageDir $ fs_config st
           vcs = fs_vcs st
           bundleHash = T.decodeUtf8 $ B16.encode $ SHA1.hash $ (patchBundleBS `BS.append` T.encodeUtf8 (projectShortName proj))
           doLogWithBundleHash :: forall m. MonadIO m => String -> m ()
           doLogWithBundleHash m = doLog LogInfo $ "[Bundle:" ++ T.unpack bundleHash ++ "] " ++ m

           repoPath = storageDir </> (T.unpack bundleHash)
           bundlePath = repoPath ++ ".dpatch"
       doLogWithBundleHash $ "Bundle arrived. Sent by " ++ T.unpack (userName user) ++ " for " ++ T.unpack (projectName proj)
       bundleId <- getPatchBundle (projId,proj) doLogWithBundleHash repoPath bundlePath patchBundleBS bundleHash
       patchBundles <- runSQL $ DB.selectList [ PatchBundle ==. bundleId ] []
       mapM_ (prepareForBuild vcs patchBundleBS (userId,user) (projId,proj) patchQueue) patchBundles
       return WorkComplete

{- |use dockercook to generate docker-images from the BuildRepository -}
buildPatchBundle :: FrozoneQueueWorker BuildRepositoryId
buildPatchBundle buildRepoId =
    do st <- getState
       runSQL $ updateBuildState buildRepoId BuildPreparing ""
       (repo, repoCfg) <- loadRepoAndConfig
       heart <- getSpockHeart
       buildOutputV <- liftIO $ newTVarIO BS.empty
       buildLogId <- runSQL $ updateBuildState buildRepoId BuildStarted ""
       let cookConfig =
               D.CookConfig
               { D.cc_stateDir = (fc_storageDir . fs_config) st
               , D.cc_dataDir = (buildRepositoryPath repo)
               , D.cc_buildFileDir = (buildRepositoryPath repo) </> rc_cookDir repoCfg
               , D.cc_boringFile = fmap (\b -> (buildRepositoryPath repo) </> b) $ rc_boringFile repoCfg
               , D.cc_buildEntryPoints = [ rc_entryPoint repoCfg ]
               }
           cookCb bs =
               do newBS <-
                      atomically $
                        do modifyTVar' buildOutputV (\orig -> BS.concat [orig, bs])
                           readTVar buildOutputV
                  let logToDb =
                          runSpockIO heart $ runSQL $ DB.update buildLogId [ BuildLogMessage =. (T.decodeUtf8 newBS) ]
                  logToDb `catch` \(e :: SomeException) ->
                      doLog LogError ("Failed to persist current log to database: " ++ (show e))
       res <- liftIO $ (D.cookBuild cookConfig (Just $ D.StreamHook cookCb) :: IO [D.DockerImage])
       case res of
         ((D.DockerImage imageName) : _) ->
             do let msg = "Dockerbuild of " ++ show buildRepoId ++ " complete! Image: " ++ T.unpack imageName
                doLog LogNote msg
                runSQL $
                       do updateBuildState buildRepoId BuildSuccess (T.pack msg)
                          DB.update buildRepoId [ BuildRepositoryDockerImage =. (Just imageName) ]
                          sendNotifications (fc_mailConfig $ fs_config st) buildRepoId
                return WorkComplete
         _ ->
             throwError "Cook build failed!"
    where
        loadRepoAndConfig :: WorkerErrM (BuildRepository, RepoConfig)
        loadRepoAndConfig =
            do buildRepo <- lift (runSQL $ DB.get buildRepoId)
                 >>= maybeToErrorT "Repository vanished! Something is inconsistent here."
               lift (runSQL $ DB.get (buildRepositoryPatch buildRepo))
                 >>= maybeToErrorT "Patch vanished! Something is very bad here"
               repoCfg <- liftIO (YML.decodeFileEither (buildRepositoryPath buildRepo </> ".frozone.yml"))
                 >>= eitherToErrorT  (\e -> "Error in .frozone.yml: " ++ show e)
               return (buildRepo, repoCfg)


{- |
    * write new bundle to disk, also to database
    * clone project repo to disk
    * split the bundle using vcs, add patches to database (also add patches to corresponding PatchCollections in the database)
-}
getPatchBundle
    :: (ProjectId, Project)
    -> (String -> WorkerErrM ())
    -> FilePath
    -> [Char]
    -> BSC.ByteString
    -> T.Text
    -> WorkerErrM BundleDataId
getPatchBundle (projId,proj) doLogWithBundleHash projectRepoCopyPath bundlePath patchBundleBS bundleHash =
       -- lookup if patch already exists:
    do st <- getState
       let
           vcs = fs_vcs st
       mBundle <- runSQL $ DB.getBy (UniqueBundleHash bundleHash)
       case mBundle of
         Nothing ->
             do doLogWithBundleHash "Bundle unknown. Inserting into database!"
                now <- liftIO getCurrentTime
                -- write new bundle to disk
                liftIO $ BS.writeFile bundlePath patchBundleBS
                -- patch bundle -> db
                bundleId <- runSQL $ DB.insert (BundleData bundleHash bundlePath now)

                doLogWithBundleHash $ "Pulling from " ++ T.unpack (projectRepoLoc proj)
                rawPatches <- -- :: [VCSPatch]
                    -- clone project repo to 'projectRepoCopyPath'
                    withVCS ((vcs_cloneRepository vcs) (VCSSource $ T.unpack $ projectRepoLoc proj) (VCSRepository projectRepoCopyPath)) $ \_ ->
                        do doLogWithBundleHash "Getting the patches out of the bundle"
                           -- return all patches in the patch bundle, which are not yet part of the project repository
                           withVCS ((vcs_patchesFromBundle vcs) (VCSRepository projectRepoCopyPath) (VCSPatchBundle patchBundleBS)) $ \r ->
                               return $ fromMaybe [] (vcs_data r)
                let mkDbPatch rawPatch  =
                        ( vp_id rawPatch
                        , vp_name rawPatch
                        , \patchCollectionId ->
                            Patch
                            { patchVcsId = T.decodeUtf8 $ unVCSPatchId (vp_id rawPatch)
                            , patchName = vp_name rawPatch
                            , patchAuthor = vp_author rawPatch
                            , patchDate = vp_date rawPatch
                            , patchBundle = bundleId
                            , patchDependents = []
                            , patchPatchCollection = patchCollectionId
                            }
                        )
                    dbPatchesNoDeps = map mkDbPatch rawPatches -- :: [(VCSPatchId, T.Text, PatchCollectionId -> Patch)]
                -- update database from the new patches, return their ids:
                patchMap <- mapM (writePatchIfNotExists doLogWithBundleHash (projId,proj)) dbPatchesNoDeps -- :: m [(VCSPatchId, PatchId)]
                -- correct dependencies into database
                mapM_ (applyDeps patchMap) rawPatches
                return bundleId
         Just bundle ->
             do doLogWithBundleHash "Bundle already known!"
                return $ entityKey bundle
    where
      {- | update dependencies in the database -}
      applyDeps :: [(VCSPatchId, PatchId)] -> VCSPatch -> WorkerErrM ()
      applyDeps patchIdMap rawPatch =
          do me <- toDbId patchIdMap (vp_id rawPatch)
             myDeps <- mapM (toDbId patchIdMap) (vp_dependents rawPatch)
             runSQL $ DB.update me [ PatchDependents =. myDeps ]
      toDbId :: [(VCSPatchId, PatchId)] -> VCSPatchId -> WorkerErrM PatchId
      toDbId patchIdMap rawPatchId =
          maybeToErrorT ("Internal inconsistency! Patch " ++ show rawPatchId ++ " is not in database, but was just a second ago.") $
            lookup rawPatchId patchIdMap
        
{- | lookup if patch exists in database (using the vcs id), otherwise add entry. return the id of the patch -}
writePatchIfNotExists
    :: (String -> WorkerErrM ()) -> (ProjectId,Project)
    -> (VCSPatchId, T.Text, PatchCollectionId -> Patch)
    -> WorkerErrM (VCSPatchId, PatchId)
writePatchIfNotExists doLogWithBundleHash projKV (vcsId, localPatchName, rawPatch) =
      do let dbId = T.decodeUtf8 $ unVCSPatchId vcsId
         mPatch <- runSQL $ DB.getBy (UniquePatchVcsId dbId)
         case mPatch of
           Just p ->
               do _ <- doLogWithBundleHash $ "Patch " ++ (show localPatchName) ++ " already known!"
                  return (vcsId, entityKey p)
           Nothing ->
               do _ <- doLogWithBundleHash $ "Patch " ++ (show localPatchName) ++ " is new."
                  groupId <- createPatchCollectionIfNotExists doLogWithBundleHash projKV localPatchName
                  patchId <- runSQL $ DB.insert (rawPatch groupId)
                  return (vcsId, patchId)

createPatchCollectionIfNotExists doLogWithBundleHash (projId, _) patchCollName =
      do groups <- runSQL $ DB.selectList [ PatchCollectionName ==. patchCollName, PatchCollectionOpen ==. True ] []
         case groups of
           (group : _) ->
               return $ entityKey group
           [] ->
               do _ <- doLogWithBundleHash $ "Creating new PatchCollection " ++ show patchCollName
                  runSQL $ DB.insert (PatchCollection patchCollName projId True)


{- ? -}
loadChangeInfo :: FileChangeMap
               -> FilePath
               -> (FilePath -> FileChangeAction -> IO (Maybe T.Text))
               -> IO (HM.HashMap FilePath (Maybe T.Text))
loadChangeInfo changeMap repoDir loadStrategy =
    HM.fromList <$> mapM getLocalContent (HM.toList changeMap)
    where
      getLocalContent (fp, fa) =
          do mCt <- loadStrategy (repoDir </> fp) fa
             return (fp, mCt)

{- ? -}
preApplyContent :: FilePath -> FileChangeAction -> IO (Maybe T.Text)
preApplyContent _ FileChangeCreated = return Nothing
preApplyContent fp _ =
    Just <$> T.readFile fp

{- ? -}
postApplyContent :: FilePath -> FileChangeAction -> IO (Maybe T.Text)
postApplyContent _ FileChangeDeleted = return Nothing
postApplyContent fp _ =
    Just <$> T.readFile fp

{- |insert new bundle changes into database ? -}
generateBundleChanges :: BuildRepositoryId
                      -> HM.HashMap FilePath (Maybe T.Text)
                      -> HM.HashMap FilePath (Maybe T.Text)
                      -> [BundleChange]
generateBundleChanges repoId preApply postApply =
    map (\(fp, mCt) ->
             case HM.lookup fp postApply of
               Nothing ->
                   error $ "generateBundleChanges failed! Missing file " ++ fp ++ " in post-apply repo!"
               Just mNewCt ->
                   BundleChange fp mCt mNewCt repoId
        ) (HM.toList preApply)



{- |* create a new path rndPath = <storage>/<random>
    * use vcs to:
        create a repo at rndPath by cloning the project repo and applying the patch

-}
prepareForBuild :: VCSApi
                -> BS.ByteString
                -> (UserId,User) -> (ProjectId,Project)
                -> WorkQueue BuildRepositoryId
                -> Entity Patch
                -> FrozoneQueueWorkerM ()
prepareForBuild vcs patchBundleBS (_,user) (projId,proj) patchQueue patch =
    let
      branch = projectRepoLoc proj
    in
    do st <- getState
       targetIdent <- liftIO $ randomB16Ident 10
       let targetDir = (fc_storageDir $ fs_config st) </> targetIdent
       doLogWithBundleHash $ "Will clone " ++ T.unpack branch ++ " into " ++ targetDir
       -- clone branch to <storage>/<random>
       withVCS ((vcs_cloneRepository vcs) (VCSSource $ T.unpack branch) (VCSRepository targetDir)) $ \_ ->
           withVCS ((vcs_changedFiles vcs) vcsId bundle) $ \r1 ->
               do let Just changeMap = vcs_data r1
                  preApply <- liftIO $ loadChangeInfo changeMap targetDir preApplyContent
                  -- apply patch to <storage>/<random>
                  withVCS ((vcs_applyPatch vcs) vcsId bundle (VCSRepository targetDir)) $ \_ ->
                      do doLogWithBundleHash "Patches applied!"
                         withVCS ((vcs_changeLog $ fs_vcs st) (VCSRepository targetDir)) $ \r2 ->
                             enqueuePatch (vcs_stdOut r2) targetDir changeMap preApply
    where
      doLogWithBundleHash :: forall m. MonadIO m => String -> m ()
      doLogWithBundleHash m = doLog LogInfo $ "[Patch:" ++ (T.unpack $ patchName $ entityVal patch) ++ "] " ++ m
      vcsId = VCSPatchId $ T.encodeUtf8 $ patchVcsId (entityVal patch)
      bundle = VCSPatchBundle patchBundleBS


      {- |* add a BuildRepository to database, if not yet existing
          * if shouldRebuild
            then: add it to the patchQueue
            else: sendNotifications
      -}
      enqueuePatch changes targetDir changeMap preApply =
          do st <- getState
             now <- liftIO getCurrentTime
             let changesHash =
                     T.decodeUtf8 $ B16.encode $ SHA1.hash changes
                 rp =
                     BuildRepository
                     { buildRepositoryProject = projId
                     , buildRepositoryBranch = projectRepoLoc proj
                     , buildRepositoryPath = targetDir
                     , buildRepositoryCreatedOn = now
                     , buildRepositoryNotifyEmail = [userEmail user]
                     , buildRepositoryChangesHash = changesHash
                     , buildRepositoryPatch = entityKey patch
                     , buildRepositoryDockerImage = Nothing
                     , buildRepositoryState = BuildEnqueued
                     }
             mRepo <- runSQL $ DB.getBy (UniqueChangesHash changesHash)
             case mRepo of
               Nothing ->
                    do postApply <- liftIO $ loadChangeInfo changeMap targetDir postApplyContent
                       buildId <- runSQL $ DB.insert rp
                       _ <- runSQL $ DB.insertMany (generateBundleChanges buildId preApply postApply)
                       doLogWithBundleHash "Ready to build!"
                       addWork WorkNow buildId patchQueue
               Just repo ->
                    if shouldRebuild (buildRepositoryState $ entityVal repo)
                    then do runSQL $ DB.update (entityKey repo) [ BuildRepositoryNotifyEmail =. nub ((userEmail user) : buildRepositoryNotifyEmail (entityVal repo))
                                                                , BuildRepositoryState =. BuildEnqueued
                                                                , BuildRepositoryDockerImage =. Nothing
                                                                ]
                            doLogWithBundleHash "Ready to rebuild!"
                            addWork WorkNow (entityKey repo) patchQueue
                    else runSQL $
                         do DB.update (entityKey repo) [ BuildRepositoryNotifyEmail =. nub (userEmail user : buildRepositoryNotifyEmail (entityVal repo)) ]
                            liftIO $ doLogWithBundleHash "Nothing to do"
                            sendNotifications (fc_mailConfig $ fs_config st) (entityKey repo)
      shouldRebuild buildState =
          case buildState of
            BuildCanceled -> True
            BuildNeedsRecheck -> True
            _ -> False


{- |send an email:
    from:
    to:
    subject: repo and patch name
    message: build log of the patch
-}
sendNotifications :: Maybe FrozoneSmtp -> BuildRepositoryId -> SqlPersistM ()
sendNotifications mSmtp buildRepoId =
    do eitherRepoAndPatch <- runErrorT $
          do repo <-
                 lift (DB.get buildRepoId)
                 >>= maybeToErrorT ("Something very bad happened. Can not find " ++ show buildRepoId ++ " anymore!")
             patch <-
                 lift (DB.get $ buildRepositoryPatch repo)
                 >>= maybeToErrorT ("Something shitty happened. A build " ++ show buildRepoId ++ " doesn't have a patch?!")
             return (repo, patch)
       handleEither eitherRepoAndPatch (\err -> doLog LogError err) $ \(repo,patch) ->
          do msg <- mkMsg repo patch
             let subj = subject repo patch
                 recv = buildRepositoryNotifyEmail repo
             doLog LogInfo ("Sending mail " ++ show subj ++ " to " ++ (T.unpack $ T.intercalate "," recv))
             liftIO $ sendEmail mSmtp emailFrom recv subj msg
    where
      subject repo patch =
          T.concat [ "[", prettyBuildState (buildRepositoryState repo), "] "
                   , "Updates for "
                   , patchName patch
                   ]
      mkLogBlock changeLog =
          T.concat [ "===========================================\n"
                   , "State:\n "
                   , prettyBuildState (buildLogState changeLog)
                   , " (", T.pack $ show (buildLogTime changeLog), ") \n"
                   , "===========================================\n"
                   , buildLogMessage changeLog
                   ]
      mkMsg _ _ =
          do changeLog <- DB.selectList [ BuildLogRepo ==. buildRepoId ] []
             return $ T.intercalate "\n\n" (map (mkLogBlock . entityVal) changeLog)

{- |cancel all build repositories, currently enqueued or started -}
closeDangelingActions :: Maybe FrozoneSmtp -> SqlPersistM ()
closeDangelingActions mSmtp =
    do liftIO (doLog LogInfo "Fixing dangeling actions...")
       dangelingBuilds <- DB.selectList ( [BuildRepositoryState ==. BuildEnqueued]
                                          ||. [BuildRepositoryState ==. BuildStarted]
                                        ) []
       mapM_ closeDangeling dangelingBuilds
    where
      closeDangeling build =
          do updateBuildState (entityKey build) BuildFailed "Dangeling after frozone restart"
             sendNotifications mSmtp (entityKey build)

withVCS :: (MonadIO m, MonadError String m) =>
     IO (VCSResponse a) -> (VCSResponse a -> m b) -> m b
withVCS vcsAction okAction =
    do resp <- liftIO vcsAction
       if vcs_success resp
       then okAction resp
       else throwError (BSC.unpack $ vcs_stdErr resp)
