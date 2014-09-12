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
import qualified Frozone.Util.Concurrency.Scheduling as Sched
import Frozone.BuildSystem.ThreadMonad

import Frozone.BuildTypes
import Frozone.Util.ErrorHandling
import Frozone.Util.Process
import Frozone.Util.Logging
import Frozone.Util.Concurrency



import Control.Concurrent.STM
--import Control.Concurrent.STM.TVar
import Control.Monad.Trans
import Control.Monad.Error
import System.Exit
import System.FilePath

{-
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
--import qualified Data.List as L
--import qualified Data.Map.Strict as M
--import System.Directory
--import Control.Exception

import qualified Data.ByteString.Lazy as BS
-}


type SchedRef = Sched.SchedRef BuildParams

--type RefToBuildSystemState = TVar BuildSystemState


data BuildSystemRef
    = BuildSystemRef
    { buildSysRef_sched :: SchedRef
    , buildSysRef_refModel :: TVar BuildSystemState
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

startBuildSystem :: BuildSystemConfig -> IO BuildSystemRef
startBuildSystem config =
    do doLog LogInfo $ "startBuildSystem called"
       doLog LogInfo $ "starting scheduler..."
       (schedRef, ref) <- evalThreadMonadTUnsafe (Sched.runScheduler 6 buildThread) config emptyBuildSystemState id
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

addBuild :: BuildSystemRef -> BuildId -> TarFile -> ErrorT ErrMsg IO ()
addBuild BuildSystemRef{ buildSysRef_refModel = refModel, buildSysRef_sched = refSched, buildSysRef_config = config} buildRepoId tarFile = 
    do logBuild buildRepoId LogInfo $ "addBuild called"
       runInThreadMonadAndReturnErrors config refModel (addBuildAction refSched buildRepoId tarFile)
       `logErrors` (logBuild buildRepoId LogError)

getBuildRepositoryState :: BuildSystemRef -> BuildId -> ErrT IO BuildState
getBuildRepositoryState buildSysRef buildRepoId =
    do model <- lift $ atomically $ readTVar (buildSysRef_refModel buildSysRef)
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

addBuildAction :: MonadIO m => SchedRef -> BuildId -> TarFile -> ErrT (ThreadMonadT m) ()
addBuildAction refSched buildRepoId tarFile =
    do
       -- throw error if repository already exists:
       errorOrBuildRep <- lift $ runErrorT $ getRepo buildRepoId
       case errorOrBuildRep of
         Left _ -> return ()
         Right _ -> throwError "repository already exists!"

       jobId <- liftIO $ Sched.addTask refSched $ Sched.Task (buildRepoId, tarFile)
       modifyModelErr $ addBuildRepository buildRepoId $ newRep jobId
    where
        newRep threadId =
            BuildRepository
            { br_path = Nothing
            , br_buildState = BuildScheduled
            , br_incoming = tarFile
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


type BuildParams = (BuildId, TarFile)

buildThread :: BuildParams -> ThreadMonadT IO ()
buildThread params =
    uncurry buildThread' params

buildThread' :: BuildId -> TarFile -> ThreadMonadT IO ()
buildThread' buildRepoId tarFile = 
    do config <- lift $ getConfig
       modifyRepoAndLog buildRepoId $ mapToBuildState $ const BuildPreparing

       -- untar
       path <- buildDirFromFile config buildRepoId tarFile 
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
    `handleError` (logBuild buildRepoId LogError)
    --updateBuildRepositoryAndLog buildRepoId $ mapToBuildState $ const Building

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------

runInThreadMonadAndReturnErrors ::
    BuildSystemConfig -> TVar BuildSystemState ->
    (forall m . MonadIO m => ErrT (ThreadMonadT m) a) ->
    ErrT IO a
runInThreadMonadAndReturnErrors config refModel action = 
    do 
       let action' = runErrorT action -- :: ThreadMonadT (ErrT IO) (Either ErrMsg a)
       errOrVal <- evalThreadMonadTWithTVar action' config refModel toIO -- :: ErrT IO (Either ErrMsg a)
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
destDir :: BuildSystemConfig -> BuildId -> TarFile -> FilePath
destDir config buildId _ = bsc_baseDir config </> show (fromBuildId buildId)

build :: MonadIO m => BuildSystemConfig -> FilePath -> BuildId -> ErrorT ErrMsg m (BuildState, String, String)
build _ path buildRepoId =
    do (exitCode, stdOut, stdErr) <- liftIO $ runProc (logBuild buildRepoId LogNote) (path </> "build.sh") ["run"]
       return $
           case exitCode of
             ExitSuccess -> (BuildSuccess, stdOut, stdErr)
             _ -> (BuildFailed, stdOut, stdErr)

buildDirFromFile :: MonadIO m => BuildSystemConfig -> BuildId -> TarFile -> ErrT m FilePath
buildDirFromFile config buildId tarFile =
    do liftIO $ runProc (logBuild buildId LogNote) "mkdir" ["--parent", destForBuild]
       liftIO $ runProc (logBuild buildId LogNote) "tar" ["-x", "-f", (bsc_incoming config </> fromTarFile tarFile), "-C", destForBuild]
       -- would be nicer to use the tar library, but it doesn't preserve filemode bits :-(
       --lift $ Tar.extract destForBuild (bsc_incoming config </> fromTarFile tarFile)
       return $ destForBuild
    where
        destForBuild = destDir config buildId tarFile


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

awaitBuildRepoMaxTime :: TimeMs -> (BuildRepository -> Bool) -> BuildId -> TVar BuildSystemState -> ErrorT ErrMsg IO AwaitRes
awaitBuildRepoMaxTime maxTime condBr buildRepoId ref =
    awaitMaxTimeOrErr maxTime cond ref 
    where
        cond buildSystemState = return . condBr =<< getBuildRepository buildRepoId buildSystemState

{-
loadBuildSystem :: IO RefToBuildSystemState

saveBuildSystem :: RefToBuildSystemState -> IO ()
-}
