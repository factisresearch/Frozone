{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Frozone.BundleChecker ( bundleApi ) where

import Frozone.Types
--import Frozone.Model
--import Frozone.User
import Frozone.VCS

import Frozone.Util.Logging
import Frozone.Util.Random
import Frozone.Util.Email
import Frozone.Util.Db
import Frozone.Util.Rest


import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Error.Class
import Control.Monad.Trans
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
   --{ _nba_repo :: String
   , _nba_user :: (UserId, User)
   --, nba_email :: T.Text
   , _nba_patchBundle :: BS.ByteString
   , _nba_patchQueue :: WorkQueue BuildRepositoryId
   }

emailFrom = "Frozone <thiemann@cp-med.com>" 

bundleApi :: String -> FrozoneApp (WorkQueue BuildRepositoryId)
bundleApi currentRoute =
    do runSQL closeDangelingActions
       st <- getState
       let concurrentBuilds = fc_concurrentBuilds $ fs_config st
           bundleQueueLength = concurrentBuilds * 3
           patchQueueLenght = bundleQueueLength * 5
       bundleQueue <- -- :: FrozoneApp (WorkQueue NewBundleArrived)
           newWorker
             (WorkerConfig bundleQueueLength WorkerNoConcurrency)
             newBundleArrived {- :: NewBundleArrived -> m? WorkResult-} $
             ErrorHandlerIO $ \errorMsg newBundle ->
               do doLog LogError $ "BundleWorker error: " ++ errorMsg
                  sendEmail emailFrom [userEmail $ snd $ _nba_user newBundle] "Bundle Error" (T.concat ["Build failed! \n\n ", T.pack errorMsg])
                  return WorkError
       patchQueue <- -- FrozoneApp (WorkQueue BuildRepositoryId)
           newWorker
             (WorkerConfig patchQueueLenght (WorkerConcurrentBounded concurrentBuilds))
             buildPatchBundle {- :: BuildRepositoryId -> m? WorkResult -} $
             ErrorHandlerSpock $ \errorMsg buildRepoId ->
               do doLog LogError $ "Build in " ++ show buildRepoId ++ " failed: " ++ errorMsg
                  runSQL $
                         do updateBuildState buildRepoId BuildFailed (T.pack errorMsg)
                            sendNotifications buildRepoId
                  return WorkError
       userRoute POST [] currentRoute "/check" $ \route ->
         withProjectFromShortName "projShortName" "project not found" $ \(projId,proj) (userId,user) ->
           do bundleCheckAction route ((userId,user), (projId,proj)) bundleQueue patchQueue
              answerAndLog (Just $ userName user)
                ("checked in patch bundle for project \"" ++ T.unpack (projectName proj)++ "\"")
                FrozoneCmdCheck
       return patchQueue

{- |* read patch bundle from http message
    * pack patch bundle into a "NewBundleArrived"
    * add this "NewBundleArrived" to bundleQueue
-}
bundleCheckAction :: String -> ((UserId,User),(ProjectId,Project)) -> WorkQueue NewBundleArrived -> WorkQueue BuildRepositoryId -> FrozoneAction ()
bundleCheckAction route ((userId,user),(projId,proj)) bundleQueue patchQueue =
    do let usersInProject = projectUsers proj :: [UserId]
       if not (userId `elem` usersInProject)
       then errorInRoute LogNote (Just $ userName user) route "check action not executed. reason: user not part of the project" "user not in project"
       else
         do allFiles <- files
            case HM.lookup "patch-bundle" allFiles of
              Just patchBundleBS ->
                  do bs <- liftIO $ BS.readFile (uf_tempLocation patchBundleBS)
                     addWork WorkNow (NewBundleArrived (projId,proj) (userId,user) bs patchQueue) bundleQueue
                     json (FrozoneInfo "Patch bundle will now be processed!")
              Nothing ->
                  errorInRoute LogError (Just $ userName user) route
                    "no patch-bundle sent" "no patch-bundle sent"

{- |
    * write new bundle to <storage>/<hash>.dpatch, bundle to database
    * clone project repo to <storage>/<hash>
    * split the bundle using vcs, add patches to database (also add patches to corresponding PatchCollections in the database)
    * find out dependencies between patches using vcs, add them to database
-}
newBundleArrived :: FrozoneQueueWorker NewBundleArrived
newBundleArrived (NewBundleArrived (projId,proj) (userId,user) patchBundleBS patchQueue) =
    do st <- getState
       -- calc bundle hash:
       -- TODO: include project into hash
       let bundleHash = T.decodeUtf8 $ B16.encode $ SHA1.hash patchBundleBS
           vcs = fs_vcs st
           localLog :: forall m. MonadIO m => String -> m ()
           localLog m = doLog LogInfo $ "[Bundle:" ++ T.unpack bundleHash ++ "] " ++ m
       localLog $ "Bundle arrived. Sent by " ++ T.unpack (userName user) ++ " for " ++ T.unpack (projectName proj)
       -- lookup if patch already exists:
       mBundle <- runSQL $ DB.getBy (UniqueBundleHash bundleHash)
       bundleId <-
           case mBundle of
             Nothing ->
                 do localLog "Bundle unknown. Inserting into database!"
                    let fp = (fc_storageDir $ fs_config st) </> (T.unpack bundleHash) -- <storage>/<hash>
                        bundleFp = fp ++ ".dpatch"                                    -- <storage>/<hash>.dpatch
                    now <- liftIO getCurrentTime
                    -- write new bundle to <storage>/<hash>.dpatch
                    liftIO $ BS.writeFile bundleFp patchBundleBS
                    -- patch bundle -> db
                    bundleId <- runSQL $ DB.insert (BundleData bundleHash bundleFp now)
                    localLog $ "Pulling from " ++ T.unpack (projectRepoLoc proj)
                    rawPatches <- -- :: [VCSPatch]
                        -- create a new empty repository at "<storage>/hash" and add patches from the project repo
                        -- ("darcs get --lazy projectRepoLoc <storage>/hash") : 
                        withVCS ((vcs_cloneRepository vcs) (VCSSource $ T.unpack $ projectRepoLoc proj) (VCSRepository fp)) $ \_ ->
                            do localLog "Getting the patches out of the bundle"
                               -- return all patches from "<storage>/hash.dpatch", that are not yet part of "<storage>/<hash>"
                               withVCS ((vcs_patchesFromBundle vcs) (VCSRepository fp) (VCSPatchBundle patchBundleBS)) $ \r ->
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
                    patchMap <- mapM (writePatchIfNotExists localLog (projId,proj)) dbPatchesNoDeps -- :: m [(VCSPatchId, PatchId)]
                    -- correct dependencies into database
                    mapM_ (applyDeps patchMap) rawPatches
                    return bundleId
             Just bundle ->
                 do localLog "Bundle already known!"
                    return $ entityKey bundle
       patches <- runSQL $ DB.selectList [ PatchBundle ==. bundleId ] []
       mapM_ (prepareForBuild vcs patchBundleBS (userId,user) (projId,proj) patchQueue) patches
       return WorkComplete
    where

      --toDbId :: [(VCSPatchId, PatchId)] -> VCSPatchId -> FrozoneQueueWorkerM PatchId
      toDbId patchIdMap rawPatchId =
          case lookup rawPatchId patchIdMap of
            Nothing ->
                throwError $ "Internal inconsistency! Patch " ++ show rawPatchId
                               ++ " is not in database, but was just a second ago."
            Just dbId ->
                return dbId
      {- | update dependencies in the database -}
      --applyDeps :: [(VCSPatchId, PatchId)] -> VCSPatch -> FrozoneQueueWorkerM ()
      applyDeps patchIdMap rawPatch =
          do me <- toDbId patchIdMap (vp_id rawPatch)
             myDeps <- mapM (toDbId patchIdMap) (vp_dependents rawPatch)
             runSQL $ DB.update me [ PatchDependents =. myDeps ]

        
  {- | lookup if patch exists in database (using the vcs id), otherwise add entry. return the id of the patch -}
  {-
  writePatchIfNotExists :: MonadIO m =>
      (String -> m ())
      -> (ProjectId, Project)
      -> (VCSPatchId, T.Text, PatchCollectionId -> Patch)
      -> m (VCSPatchId, PatchId)
  -}
writePatchIfNotExists localLog projKV (vcsId, localPatchName, rawPatch) =
      do let dbId = T.decodeUtf8 $ unVCSPatchId vcsId
         mPatch <- runSQL $ DB.getBy (UniquePatchVcsId dbId)
         case mPatch of
           Just p ->
               do _ <- localLog $ "Patch " ++ (show localPatchName) ++ " already known!"
                  return (vcsId, entityKey p)
           Nothing ->
               do _ <- localLog $ "Patch " ++ (show localPatchName) ++ " is new."
                  groupId <- createPatchCollectionIfNotExists localLog projKV localPatchName
                  patchId <- runSQL $ DB.insert (rawPatch groupId)
                  return (vcsId, patchId)

createPatchCollectionIfNotExists localLog (projId, _) patchCollName =
      do groups <- runSQL $ DB.selectList [ PatchCollectionName ==. patchCollName, PatchCollectionOpen ==. True ] []
         case groups of
           (group : _) ->
               return $ entityKey group
           [] ->
               do _ <- localLog $ "Creating new PatchCollection " ++ show patchCollName
                  runSQL $ DB.insert (PatchCollection patchCollName projId True)

{- |use dockercook to generate docker-images from the BuildRepository -}
buildPatchBundle :: FrozoneQueueWorker BuildRepositoryId
buildPatchBundle buildRepoId =
    do st <- getState
       runSQL $ updateBuildState buildRepoId BuildPreparing ""
       mBuildRepo <- runSQL $ DB.get buildRepoId
       case mBuildRepo of
         Nothing ->
             throwError "Repository vanished! Something is inconsistent here."
         Just repo ->
             do mPatch <- runSQL $ DB.get (buildRepositoryPatch repo)
                case mPatch of
                  Nothing -> throwError "Patch vanished! Something is very bad here"
                  Just _ ->
                      do yml <- liftIO $ YML.decodeFileEither (buildRepositoryPath repo </> ".frozone.yml")
                         case yml of
                           Left parseException ->
                               throwError ("Error in .frozone.yml: " ++ show parseException)
                           Right repoCfg ->
                               do heart <- getSpockHeart
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
                                                     sendNotifications buildRepoId
                                           return WorkComplete
                                    _ ->
                                        throwError "Cook build failed!"



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
       localLog $ "Will clone " ++ T.unpack branch ++ " into " ++ targetDir
       -- clone branch to <storage>/<random>
       withVCS ((vcs_cloneRepository vcs) (VCSSource $ T.unpack branch) (VCSRepository targetDir)) $ \_ ->
           withVCS ((vcs_changedFiles vcs) vcsId bundle) $ \r1 ->
                    do let Just changeMap = vcs_data r1
                       preApply <- liftIO $ loadChangeInfo changeMap targetDir preApplyContent
                       -- apply patch to <storage>/<random>
                       withVCS ((vcs_applyPatch vcs) vcsId bundle (VCSRepository targetDir)) $ \_ ->
                           do localLog "Patches applied!"
                              withVCS ((vcs_changeLog $ fs_vcs st) (VCSRepository targetDir)) $ \r2 ->
                                  enqueuePatch (vcs_stdOut r2) targetDir changeMap preApply
    where
      localLog :: forall m. MonadIO m => String -> m ()
      localLog m = doLog LogInfo $ "[Patch:" ++ (T.unpack $ patchName $ entityVal patch) ++ "] " ++ m
      vcsId = VCSPatchId $ T.encodeUtf8 $ patchVcsId (entityVal patch)
      bundle = VCSPatchBundle patchBundleBS

      shouldRebuild buildState =
          case buildState of
            BuildCanceled -> True
            BuildNeedsRecheck -> True
            _ -> False

      {- |* add a BuildRepository to database, if not yet existing
          * if shouldRebuild
            then: add it to the patchQueue
            else: sendNotifications
      -}
      enqueuePatch changes targetDir changeMap preApply =
          do now <- liftIO getCurrentTime
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
                       localLog "Ready to build!"
                       addWork WorkNow buildId patchQueue
               Just repo ->
                    if shouldRebuild (buildRepositoryState $ entityVal repo)
                    then do runSQL $ DB.update (entityKey repo) [ BuildRepositoryNotifyEmail =. nub ((userEmail user) : buildRepositoryNotifyEmail (entityVal repo))
                                                                , BuildRepositoryState =. BuildEnqueued
                                                                , BuildRepositoryDockerImage =. Nothing
                                                                ]
                            localLog "Ready to rebuild!"
                            addWork WorkNow (entityKey repo) patchQueue
                    else runSQL $
                         do DB.update (entityKey repo) [ BuildRepositoryNotifyEmail =. nub (userEmail user : buildRepositoryNotifyEmail (entityVal repo)) ]
                            liftIO $ localLog "Nothing to do"
                            sendNotifications (entityKey repo)


{- |send an email:
    from:
    to:
    subject: repo and patch name
    message: build log of the patch
-}
sendNotifications buildRepoId =
    do mRepo <- DB.get buildRepoId
       case mRepo of
         Nothing ->
             doLog LogError ("Something very bad happened. Can not find " ++ show buildRepoId ++ " anymore!")
         Just repo ->
             do mPatch <- DB.get (buildRepositoryPatch repo)
                case mPatch of
                  Nothing ->
                      doLog LogError ("Something shitty happened. A build " ++ show buildRepoId ++ " doesn't have a patch?!")
                  Just patch ->
                      do msg <- mkMsg repo patch
                         let subj = subject repo patch
                             recv = buildRepositoryNotifyEmail repo
                         doLog LogInfo ("Sending mail " ++ show subj ++ " to " ++ (T.unpack $ T.intercalate "," recv))
                         liftIO $ sendEmail "Frozone <thiemann@cp-med.com>" recv subj msg
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
closeDangelingActions :: (PersistMonadBackend m ~ SqlBackend, MonadIO m, PersistQuery m, MonadSqlPersist m) => m ()
closeDangelingActions =
    do liftIO (doLog LogInfo "Fixing dangeling actions...")
       dangelingBuilds <- DB.selectList ( [BuildRepositoryState ==. BuildEnqueued]
                                          ||. [BuildRepositoryState ==. BuildStarted]
                                        ) []
       mapM_ closeDangeling dangelingBuilds
    where
      closeDangeling build =
          do updateBuildState (entityKey build) BuildFailed "Dangeling after frozone restart"
             sendNotifications (entityKey build)

withVCS :: (MonadIO m, MonadError String m) =>
     IO (VCSResponse a) -> (VCSResponse a -> m b) -> m b
withVCS vcsAction okAction =
    do resp <- liftIO vcsAction
       if vcs_success resp
       then okAction resp
       else throwError (BSC.unpack $ vcs_stdErr resp)
