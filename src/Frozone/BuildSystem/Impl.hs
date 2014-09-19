{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
module Frozone.BuildSystem.Impl(
    BuildSystemConfig(..), BuildSystemRef(),
    buildSysImpl,
    startBuildSystem, stopBuildSystem,
    --just for testing: 
    awaitBuildRepoMaxTime, buildSysRef_refModel
) where


import Frozone.BuildSystem.Intern.Types
import Frozone.BuildSystem.API
import Frozone.BuildSystem.Persistence
import qualified Frozone.Util.Concurrency.Scheduling as Sched
import Frozone.BuildSystem.ThreadMonad

import Frozone.BuildTypes
import Frozone.Util.ErrorHandling
import Frozone.Util.Process
import Frozone.Util.Logging
import Frozone.Util.Concurrency


import qualified Data.ByteString as BS
import Control.Concurrent.STM
import System.Exit
import System.FilePath
import System.Directory
import Data.Maybe


type SchedRef = Sched.SchedRef BuildParams


data BuildSystemRef
    = BuildSystemRef
    { buildSysRef_sched :: SchedRef
    , buildSysRef_refModel :: TVar (BuildSystemState, PersistSched)
    , buildSysRef_config :: BuildSystemConfig
    }

buildSysImpl buildSystemRef = 
    BuildSystem
    { bs_addBuild = addBuild buildSystemRef
    , bs_getBuildRepositoryState = getBuildRepositoryState buildSystemRef

    , bs_getBuildQueue = getBuildQueue buildSystemRef
    , bs_stopBuild = stopBuild buildSystemRef
    , bs_archiveBuild = archiveBuild buildSystemRef
    , bs_restartBuild = restartBuild buildSystemRef
    }

startBuildSystem :: BuildSystemConfig -> ErrorT ErrMsg IO BuildSystemRef
startBuildSystem config =
    do doLog LogInfo $ "startBuildSystem called"
       when (isNothing $ bsc_storage config) $
           doLog LogWarn $ "no storage dir -> frozone will not safe builds!"
       let 
           mStoragePath = bsc_storage config
       model <-
           case mStoragePath of
             Just path ->
                 lift (doesFileExist path) >>= \fileExists ->
                 if fileExists
                   then loadModel path
                   else return emptyBuildSystemState
             Nothing -> 
                 do doLog LogWarn $ "no storage dir -> frozone will not safe builds!"
                    return $ emptyBuildSystemState
       --let model = emptyBuildSystemState
       doLog LogInfo $ "starting build scheduler..."
       (schedRef, _, ref) <- lift $ runThreadMonadTUnsafe (Sched.runScheduler 6 buildThread) config model id
       doLog LogInfo $ "end of startBuildSystem"
       return $
           BuildSystemRef
           { buildSysRef_sched = schedRef
           , buildSysRef_refModel = ref
           , buildSysRef_config = config
           }

stopBuildSystem :: BuildSystemRef -> IO ()
stopBuildSystem ref =
    do doLog LogInfo $ "stopBuildSystem called: killing all running builds"
       Sched.killAllJobs $ buildSysRef_sched ref
       doLog LogInfo $ "stopping scheduler..."
       Sched.stopScheduler $ buildSysRef_sched ref
       doLog LogInfo $ "finished ."


------------------------------------------------------------------------------
-- just API:
------------------------------------------------------------------------------

addBuild :: BuildSystemRef -> BuildId -> Tar -> ErrorT ErrMsg IO ()
addBuild BuildSystemRef{ buildSysRef_refModel = refModel, buildSysRef_sched = refSched, buildSysRef_config = config} buildRepoId tarFile = 
    do logBuild buildRepoId LogInfo $ "addBuild called"
       runInThreadMonadAndReturnErrors config refModel (addBuildAction refSched buildRepoId tarFile)
       `logErrors` (logBuild buildRepoId LogError)

getBuildRepositoryState :: BuildSystemRef -> BuildId -> ErrT IO BuildState
getBuildRepositoryState buildSysRef buildRepoId =
    do model <- liftM fst $ lift $ atomically $ readTVar (buildSysRef_refModel buildSysRef)
       liftM br_buildState $ getBuildRepository buildRepoId model

getBuildQueue :: BuildSystemRef -> BuildState -> IO [BuildId]
getBuildQueue BuildSystemRef{ buildSysRef_refModel = refModel, buildSysRef_config = config } buildState = 
    do doLog LogInfo $ "getBuildQueue called"
       runInThreadMonadAndReturnErrors config refModel (getBuildQueueAction buildState)
       `handleError` (\err -> doLog LogError err >> return [])

stopBuild :: BuildSystemRef -> BuildId -> ErrorT ErrMsg IO ()
stopBuild BuildSystemRef{ buildSysRef_refModel = refModel, buildSysRef_sched = refSched, buildSysRef_config = config } buildRepoId =
    do logBuild buildRepoId LogInfo $ "stopBuild called"
       runInThreadMonadAndReturnErrors config refModel (stopAction refSched buildRepoId)
       `logErrors` (logBuild buildRepoId LogError)

archiveBuild = undefined

restartBuild :: BuildSystemRef -> BuildId -> ErrorT ErrMsg IO ()
restartBuild BuildSystemRef{ buildSysRef_refModel = refModel, buildSysRef_sched = refSched, buildSysRef_config = config} buildRepoId = 
    do logBuild buildRepoId LogInfo $ "restartBuild called"
       runInThreadMonadAndReturnErrors config refModel (restartBuildAction refSched buildRepoId)
       `logErrors` (logBuild buildRepoId LogError)


------------------------------------------------------------------------------
-- implementation using the ThreadMonadT:
------------------------------------------------------------------------------

type BuildParams = (BuildId, TarFilePath)


addBuildAction :: MonadIO m => SchedRef -> BuildId -> Tar -> ErrT (ThreadMonadT m) ()
addBuildAction refSched buildRepoId tarFile =
    do
       -- throw error if repository already exists:
       errorOrBuildRep <- lift $ runErrorT $ getRepo buildRepoId
       case errorOrBuildRep of
         Left _ -> return ()
         Right _ -> throwError "repository already exists!"

       -- backup tar file
       tarFilePath <-
           lift getConfig >>= \config ->
               return $ bsc_incoming config </> show (fromBuildId buildRepoId) <.> "tar"
       liftIO $ BS.writeFile tarFilePath (fromTar tarFile)
       jobId <- liftIO $ Sched.addTask refSched $ Sched.Task (buildRepoId, tarFilePath)
       modifyModelErr $ addBuildRepository buildRepoId $ newRep jobId tarFilePath
    where
        newRep threadId tarFilePath =
            BuildRepository
            { br_path = Nothing
            , br_buildState = BuildScheduled
            , br_incoming = tarFilePath
            , br_thread = Just threadId
            }

getBuildQueueAction :: MonadIO m => BuildState -> ErrT (ThreadMonadT m) [BuildId]
getBuildQueueAction buildState =
    do model <- lift $ getModel
       return $ getBuildsInState buildState model

stopAction :: MonadIO m => SchedRef -> BuildId -> ErrT (ThreadMonadT m) ()
stopAction schedRef buildRepoId =
    do repo <- getRepo buildRepoId 
       case (br_thread repo) of
         Just jobId ->
             do logBuild buildRepoId LogInfo $ "removing job from build scheduler"
                Sched.removeJob schedRef jobId
         Nothing ->
             do logBuild buildRepoId LogInfo $ "build not scheduled or running, nothing to do"
                return ()
       modifyRepoAndLog buildRepoId $ mapToBuildState $ const BuildStopped
       modifyRepoAndLog buildRepoId $ mapToThread $ const Nothing


restartBuildAction :: MonadIO m => SchedRef -> BuildId -> ErrT (ThreadMonadT m) ()
restartBuildAction refSched buildRepoId =
    do repo <- getRepo buildRepoId
       let
         buildState = br_buildState repo
         tarFile = br_incoming repo
       when (not $ buildState `elem` [BuildStopped, BuildFailed, BuildSuccess]) $
           throwError $ "error restarting build: build in state \"" ++ show buildState ++ "\""
       jobId <- liftIO $ Sched.addTask refSched $ Sched.Task (buildRepoId, tarFile)
       modifyRepoAndLog buildRepoId $ mapToThread $ const $ Just jobId


buildThread :: BuildParams -> ThreadMonadT IO ()
buildThread params =
    uncurry buildThread' params

buildThread' :: BuildId -> TarFilePath -> ThreadMonadT IO ()
buildThread' buildRepoId tarFilePath = 
    do config <- lift $ getConfig
       modifyRepoAndLog buildRepoId $ mapToBuildState $ const BuildPreparing

       -- untar
       path <- buildDirFromFile config buildRepoId tarFilePath
       modifyRepoAndLog buildRepoId $ mapToPath $ const $ Just path

       modifyRepoAndLog buildRepoId $ mapToBuildState $ const Building
       logBuild buildRepoId LogInfo $ "starting build"
       -- build
       (newBuildState, stdOut, stdErr) <- build config path buildRepoId
       case newBuildState of
         BuildSuccess -> logBuild buildRepoId LogInfo $ "exit build SUCCESS"
         BuildFailed ->
             do logBuild buildRepoId LogNote $ "build FAILED"
                logBuild buildRepoId LogNote $ "stdout was: " ++ stdOut
                logBuild buildRepoId LogNote $ "stderr was: " ++ stdErr
         _ -> throwError $ "strange build state after build: " ++ show newBuildState
       modifyRepoAndLog buildRepoId $ mapToBuildState $ const newBuildState
    `handleError`
    do
       logBuild buildRepoId LogError
       --modifyRepoAndLog buildRepoId $ mapToBuildState $ const BuildFailed
    --updateBuildRepositoryAndLog buildRepoId $ mapToBuildState $ const Building

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------

runInThreadMonadAndReturnErrors ::
    BuildSystemConfig -> TVar (BuildSystemState, PersistSched) ->
    (forall m . MonadIO m => ErrT (ThreadMonadT m) a) ->
    ErrT IO a
runInThreadMonadAndReturnErrors config refModel action = 
    do 
       let action' = runErrorT action -- :: ThreadMonadT (ErrT IO) (Either ErrMsg a)
       (errOrVal, _) <- runThreadMonadTWithTVar action' config refModel toIO -- :: ErrT IO (Either ErrMsg a)
       case errOrVal of
         Left err ->
             do --logBuild buildRepoId LogError $ "error: " ++ err
                throwError err
         Right val ->
            do --logBuild buildRepoId LogInfo $ "stopped build"
               return val
    where
        toIO :: ErrT IO a -> IO a
        toIO _ =
            do doLog LogError "ERROR: a thread has used fork which should not have done so!"
               fail "ERROR: a thread has used fork which should not have done so!"

logErrors :: MonadIO m => ErrorT ErrMsg m a -> (ErrMsg -> m ()) -> ErrorT ErrMsg m a
logErrors mx loggingFunc =
    do errOrVal <- lift $ runErrorT mx
       case errOrVal of
         Left err -> lift (loggingFunc err) >> throwError err
         Right val -> return val

-- determines the location where to save a BuildRepository
destDir :: BuildSystemConfig -> BuildId -> TarFilePath -> FilePath
destDir config buildId _ = bsc_baseDir config </> show (fromBuildId buildId)

build :: MonadIO m => BuildSystemConfig -> FilePath -> BuildId -> ErrorT ErrMsg m (BuildState, String, String)
build _ path buildRepoId =
    do (exitCode, stdOut, stdErr) <- liftIO $ runProc (logBuild buildRepoId LogNote) (path </> "build.sh") ["run"]
       return $
           case exitCode of
             ExitSuccess -> (BuildSuccess, stdOut, stdErr)
             _ -> (BuildFailed, stdOut, stdErr)

buildDirFromFile :: MonadIO m => BuildSystemConfig -> BuildId -> TarFilePath -> ErrT m FilePath
buildDirFromFile config buildId tarFilePath =
    do runProcErr (logBuild buildId LogNote) "mkdir" ["--parent", destForBuild]
       runProcErr (logBuild buildId LogNote) "tar" ["-x", "-f", tarFilePath, "-C", destForBuild]
       runProcErr (logBuild buildId LogNote) "chmod" ["u+x", destForBuild </> "build.sh"]
       -- would be nicer to use the tar library, but it doesn't preserve filemode bits :-(
       --lift $ Tar.extract destForBuild (bsc_incoming config </> fromTarFile tarFile)
       return $ destForBuild
    where
        destForBuild = destDir config buildId tarFilePath


getRepo :: MonadIO m => BuildId -> ErrT (ThreadMonadT m) BuildRepository
getRepo buildRepoId =
    do model <- lift $ getModel
       getBuildRepository buildRepoId model

modifyRepoAndLog :: MonadIO m => BuildId -> (BuildRepository -> BuildRepository) -> ErrT (ThreadMonadT m) ()
modifyRepoAndLog buildRepoId f =
    do modifyRepo buildRepoId f
       model <- lift $ getModel
       logBuild buildRepoId LogInfo . show =<< getBuildRepository buildRepoId model

logBuild buildRepoId logLevel msg = 
    doLog logLevel $ "Build " ++ show (fromBuildId buildRepoId) ++ ": " ++ msg

awaitBuildRepoMaxTime :: TimeMs -> (BuildRepository -> Bool) -> BuildId -> TVar (BuildSystemState, PersistSched) -> ErrorT ErrMsg IO AwaitRes
awaitBuildRepoMaxTime maxTime condBr buildRepoId ref =
    awaitMaxTimeOrErr maxTime (\(x, _) -> cond x) ref 
    where
        cond buildSystemState = return . condBr =<< getBuildRepository buildRepoId buildSystemState
