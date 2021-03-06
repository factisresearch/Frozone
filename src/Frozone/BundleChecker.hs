{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Frozone.BundleChecker ( bundleApi ) where

import Frozone.Types
import Frozone.Model
import Frozone.VCS
import Frozone.Util.Logging
import Frozone.Util.Random
import Frozone.Util.Email
import Frozone.Util.Db

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Error.Class
import Control.Monad.Trans
import Cook.Build
import Cook.Types
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
   { _nba_repo :: String
   , nba_email :: T.Text
   , _nba_patchBundle :: BS.ByteString
   , _nba_patchQueue :: WorkQueue BuildRepositoryId
   }

bundleApi :: FrozoneApp (WorkQueue BuildRepositoryId)
bundleApi =
    do st <- getState
       runSQL (closeDangelingActions (fc_mailConfig $ fs_config st))
       let concurrentBuilds = fc_concurrentBuilds $ fs_config st
           bundleQueue = concurrentBuilds * 3
           patchQueue = bundleQueue * 5
       bundleWorker <-
           newWorker (WorkerConfig bundleQueue WorkerNoConcurrency) newBundleArrived $ ErrorHandlerIO $ \errorMsg newBundle ->
               do doLog LogError $ "BundleWorker error: " ++ errorMsg
                  sendEmail (fc_mailConfig $ fs_config st) "thiemann@cp-med.com" [nba_email newBundle] "Bundle Error" (T.concat ["Build failed! \n\n ", T.pack errorMsg])
                  return WorkError
       patchWorker <-
           newWorker (WorkerConfig patchQueue (WorkerConcurrentBounded concurrentBuilds)) buildPatchBundle $ ErrorHandlerSpock $ \errorMsg buildRepoId ->
               do doLog LogError $ "Build in " ++ show buildRepoId ++ " failed: " ++ errorMsg
                  runSQL $
                         do updateBuildState buildRepoId BuildFailed (T.pack errorMsg)
                            sendNotifications (fc_mailConfig $ fs_config st) buildRepoId
                  return WorkError
       post "/check" $ bundleCheckAction bundleWorker patchWorker
       return patchWorker

bundleCheckAction :: WorkQueue NewBundleArrived -> WorkQueue BuildRepositoryId -> FrozoneAction ()
bundleCheckAction wq rwq =
    do Just repo <- param "target-repo"
       Just email <- param "email"
       allFiles <- files
       case HM.lookup "patch-bundle" allFiles of
         Just patchBundleBS ->
             do bs <- liftIO $ BS.readFile (uf_tempLocation patchBundleBS)
                addWork WorkNow (NewBundleArrived repo email bs rwq) wq
                json (FrozoneMessage "Patch bundle will now be processed!")
         Nothing ->
             json (FrozoneError "No patch-bundle sent!")

closeDangelingActions :: (PersistMonadBackend m ~ SqlBackend, MonadIO m, PersistQuery m, MonadSqlPersist m) => Maybe FrozoneSmtp -> m ()
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

sendNotifications mSmtp buildRepoId =
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
                         liftIO $ sendEmail mSmtp "Frozone <thiemann@cp-med.com>" recv subj msg
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
                                          CookConfig
                                          { cc_stateDir = (fc_storageDir . fs_config) st
                                          , cc_dataDir = (buildRepositoryPath repo)
                                          , cc_buildFileDir = (buildRepositoryPath repo) </> rc_cookDir repoCfg
                                          , cc_boringFile = fmap (\b -> (buildRepositoryPath repo) </> b) $ rc_boringFile repoCfg
                                          , cc_buildEntryPoints = [ rc_entryPoint repoCfg ]
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
                                  res <- liftIO $ cookBuild cookConfig (Just $ StreamHook cookCb)
                                  case res of
                                    ((DockerImage imageName) : _) ->
                                        do let msg = "Dockerbuild of " ++ show buildRepoId ++ " complete! Image: " ++ T.unpack imageName
                                           doLog LogNote msg
                                           runSQL $
                                                  do updateBuildState buildRepoId BuildSuccess (T.pack msg)
                                                     DB.update buildRepoId [ BuildRepositoryDockerImage =. (Just imageName) ]
                                                     sendNotifications (fc_mailConfig $ fs_config st) buildRepoId
                                           return WorkComplete
                                    _ ->
                                        throwError "Cook build failed!"

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

preApplyContent :: FilePath -> FileChangeAction -> IO (Maybe T.Text)
preApplyContent _ FileChangeCreated = return Nothing
preApplyContent fp _ =
    Just <$> T.readFile fp

postApplyContent :: FilePath -> FileChangeAction -> IO (Maybe T.Text)
postApplyContent _ FileChangeDeleted = return Nothing
postApplyContent fp _ =
    Just <$> T.readFile fp

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

withVCS vcsAction okAction =
    do resp <- liftIO vcsAction
       if vcs_success resp
       then okAction resp
       else throwError (BSC.unpack $ vcs_stdErr resp)

newBundleArrived :: FrozoneQueueWorker NewBundleArrived
newBundleArrived (NewBundleArrived repo email patchBundleBS wq) =
    do st <- getState
       let bundleHash = T.decodeUtf8 $ B16.encode $ SHA1.hash patchBundleBS
           vcs = fs_vcs st
           localLog :: forall m. MonadIO m => String -> m ()
           localLog m = doLog LogInfo $ "[Bundle:" ++ T.unpack bundleHash ++ "] " ++ m
       localLog $ "Bundle arrived. Sent by " ++ T.unpack email ++ " for " ++ repo
       mBundle <- runSQL $ DB.getBy (UniqueBundleHash bundleHash)
       bundleId <-
           case mBundle of
             Nothing ->
                 do localLog "Bundle unknown. Inserting into database!"
                    let fp = (fc_storageDir $ fs_config st) </> (T.unpack bundleHash)
                        bundleFp = fp ++ ".dpatch"
                    now <- liftIO getCurrentTime
                    liftIO $ BS.writeFile bundleFp patchBundleBS
                    bundleId <- runSQL $ DB.insert (BundleData bundleHash bundleFp now)
                    localLog $ "Pulling from " ++ repo
                    rawPatches <-
                        withVCS ((vcs_cloneRepository vcs) (VCSSource repo) (VCSRepository fp)) $ \_ ->
                            do localLog "Getting the patches out of the bundle"
                               withVCS ((vcs_patchesFromBundle vcs) (VCSRepository fp) (VCSPatchBundle patchBundleBS)) $ \r ->
                                   return $ fromMaybe [] (vcs_data r)
                    let mkDbPatch rawPatch  =
                            ( vp_id rawPatch
                            , vp_name rawPatch
                            , \groupId ->
                                Patch
                                { patchVcsId = T.decodeUtf8 $ unVCSPatchId (vp_id rawPatch)
                                , patchName = vp_name rawPatch
                                , patchAuthor = vp_author rawPatch
                                , patchDate = vp_date rawPatch
                                , patchBundle = bundleId
                                , patchDependents = []
                                , patchGroup = groupId
                                }
                            )
                        dbPatchesNoDeps = map mkDbPatch rawPatches
                    patchMap <- mapM (writePatchIfNotExists localLog) dbPatchesNoDeps
                    mapM_ (applyDeps patchMap) rawPatches
                    return bundleId
             Just bundle ->
                 do localLog "Bundle already known!"
                    return $ entityKey bundle
       patches <- runSQL $ DB.selectList [ PatchBundle ==. bundleId ] []
       mapM_ (prepareForBuild vcs patchBundleBS repo email wq) patches
       return WorkComplete
    where
      createGroupIfNotExists localLog patchCollName =
          do groups <- runSQL $ DB.selectList [ PatchCollectionName ==. patchCollName, PatchCollectionOpen ==. True ] []
             case groups of
               (group : _) ->
                   return $ entityKey group
               [] ->
                   do _ <- localLog $ "Creating new PatchCollection " ++ show patchCollName
                      runSQL $ DB.insert (PatchCollection patchCollName True)

      toDbId :: [(VCSPatchId, PatchId)] -> VCSPatchId -> FrozoneQueueWorkerM PatchId
      toDbId patchIdMap rawPatchId =
          case lookup rawPatchId patchIdMap of
            Nothing ->
                throwError $ "Internal inconsistency! Patch " ++ show rawPatchId
                               ++ " is not in database, but was just a second ago."
            Just dbId ->
                return dbId

      applyDeps patchIdMap rawPatch =
          do me <- toDbId patchIdMap (vp_id rawPatch)
             myDeps <- mapM (toDbId patchIdMap) (vp_dependents rawPatch)
             runSQL $ DB.update me [ PatchDependents =. myDeps ]

      writePatchIfNotExists localLog (vcsId, localPatchName, rawPatch) =
          do let dbId = T.decodeUtf8 $ unVCSPatchId vcsId
             mPatch <- runSQL $ DB.getBy (UniquePatchVcsId dbId)
             case mPatch of
               Just p ->
                   do _ <- localLog $ "Patch " ++ (show localPatchName) ++ " already known!"
                      return (vcsId, entityKey p)
               Nothing ->
                   do _ <- localLog $ "Patch " ++ (show localPatchName) ++ " is new."
                      groupId <- createGroupIfNotExists localLog localPatchName
                      patchId <- runSQL $ DB.insert (rawPatch groupId)
                      return (vcsId, patchId)

prepareForBuild :: VCSApi
                -> BS.ByteString
                -> String
                -> T.Text
                -> WorkQueue BuildRepositoryId
                -> Entity Patch
                -> FrozoneQueueWorkerM ()
prepareForBuild vcs patchBundleBS branch email wq patch =
    do st <- getState
       targetIdent <- liftIO $ randomB16Ident 10
       let targetDir = (fc_storageDir $ fs_config st) </> targetIdent
       localLog $ "Will clone " ++ branch ++ " into " ++ targetDir
       withVCS ((vcs_cloneRepository vcs) (VCSSource branch) (VCSRepository targetDir)) $ \_ ->
           withVCS ((vcs_changedFiles vcs) vcsId bundle) $ \r1 ->
                    do let Just changeMap = vcs_data r1
                       preApply <- liftIO $ loadChangeInfo changeMap targetDir preApplyContent
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

      enqueuePatch changes targetDir changeMap preApply =
          do now <- liftIO getCurrentTime
             st <- getState
             let changesHash =
                     T.decodeUtf8 $ B16.encode $ SHA1.hash changes
                 rp =
                     BuildRepository
                     { buildRepositoryBranch = T.pack branch
                     , buildRepositoryPath = targetDir
                     , buildRepositoryCreatedOn = now
                     , buildRepositoryNotifyEmail = [email]
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
                       addWork WorkNow buildId wq
               Just repo ->
                    if shouldRebuild (buildRepositoryState $ entityVal repo)
                    then do runSQL $ DB.update (entityKey repo) [ BuildRepositoryNotifyEmail =. nub (email : buildRepositoryNotifyEmail (entityVal repo))
                                                                , BuildRepositoryState =. BuildEnqueued
                                                                , BuildRepositoryDockerImage =. Nothing
                                                                ]
                            localLog "Ready to rebuild!"
                            addWork WorkNow (entityKey repo) wq
                    else runSQL $
                         do DB.update (entityKey repo) [ BuildRepositoryNotifyEmail =. nub (email : buildRepositoryNotifyEmail (entityVal repo)) ]
                            liftIO $ localLog "Nothing to do"
                            sendNotifications (fc_mailConfig $ fs_config st) (entityKey repo)
