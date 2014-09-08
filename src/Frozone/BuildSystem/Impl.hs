{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Frozone.BuildSystem.Impl(
    BuildSystemConfig(..),
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


type SchedRef = Sched.SchedData

--type RefToBuildSystemState = TVar BuildSystemState


data BuildSystemRef
    = BuildSystemRef
    { buildSysRef_sched :: SchedRef BuildParams
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
    do (schedRef, ref) <- evalThreadMonadTUnsafe (Sched.runScheduler 6 buildThread) config emptyBuildSystemState id
       return $
           BuildSystemRef
           { buildSysRef_sched = schedRef
           , buildSysRef_refModel = ref
           , buildSysRef_config = config
           }

stopBuildSystem :: BuildSystemRef -> ErrorT ErrMsg IO ()
stopBuildSystem ref =
    do Sched.stopScheduler $ buildSysRef_sched ref

addBuild :: BuildSystemRef -> BuildId -> TarFile -> ErrorT ErrMsg IO ()
addBuild BuildSystemRef{ buildSysRef_refModel = refModel, buildSysRef_sched = refSched } buildRepoId tarFile = 
    do addRepoOrError <- liftIO $ atomically $ 
           do model <- readTVar refModel
              eitherNewModel <- (runErrorT $ addBuildRepository buildRepoId newRep model)
              case eitherNewModel of
                Left err -> return $ throwError err -- :: STM (ErrorT ErrMsg m ())
                Right newModel ->
                    do writeTVar refModel newModel -- :: STM (ErrorT ErrMsg m ())
                       return $ return ()
       addRepoOrError
       _ <- lift $ Sched.addTask refSched $ Sched.Task (buildRepoId, tarFile)
       return ()
    where
        newRep =
            BuildRepository
            { br_path = Nothing
            , br_buildState = BuildScheduled
            , br_thread = Nothing
            }

getBuildRepositoryState :: BuildSystemRef -> BuildId -> ErrT IO BuildState
getBuildRepositoryState buildSysRef buildRepoId =
    do model <- lift $ atomically $ readTVar (buildSysRef_refModel buildSysRef)
       liftM br_buildState $ getBuildRepository buildRepoId model

getBuildQueue = undefined

stopBuild :: BuildSystemRef -> BuildId -> ErrorT ErrMsg IO ()
stopBuild BuildSystemRef{ buildSysRef_refModel = refModel, buildSysRef_sched = refSched, buildSysRef_config = config } buildRepoId =
    do let remAction' = runErrorT (remAction refSched buildRepoId) :: ThreadMonadT (ErrT IO) (Either ErrMsg ())
       errOrUnity <- evalThreadMonadTWithTVar (remAction') config refModel toIO
       case errOrUnity of
         Left err -> throwError err
         _ -> return ()
    where
        toIO :: ErrT IO a -> IO a
        toIO = undefined -- (`handleError` (logBuild buildRepoId LogError))

remAction :: MonadIO m => SchedRef BuildParams -> BuildId -> ErrT (ThreadMonadT m) ()
remAction schedRef buildRepoId =
    do repo <- getRepo buildRepoId 
       case (br_thread repo) of
         Just jobId -> Sched.removeJob schedRef jobId
         Nothing -> return ()
       modifyRepoAndLog buildRepoId $ mapToBuildState $ const BuildStopped

archiveBuild = undefined
restartBuild = undefined

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
