module Frozone.BuildSystem.Impl where


import Frozone.BuildSystem.Intern.Model
import Frozone.BuildSystem.API

import Frozone.BuildTypes
import Frozone.Util.ErrorHandling
import Frozone.Util.Process
import Frozone.Util.Logging
import Frozone.Util.Concurrency


import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans
import Control.Monad.Error
import qualified Data.List as L
import qualified Data.Map.Strict as M
import System.Directory
import System.Exit
import Control.Concurrent
import Control.Exception

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import System.FilePath
import qualified Data.ByteString.Lazy as BS


data BuildSystemConfig
    = BuildSystemConfig
    { bsc_baseDir :: FilePath
    , bsc_incoming :: FilePath
    }

type RefToBuildSystemState = TVar BuildSystemState


startBuildSystem :: IO RefToBuildSystemState
startBuildSystem =
    do state <- atomically $ newTVar emptyBuildSystemState
       return state

{-
scheduler :: Int -> RefToBuildSystemState -> IO ()
scheduler maxBuilds refToBuildSystemState =
-}

-- |clears all information about builds, in whatever state they are
clearBuildSystem :: RefToBuildSystemState -> IO ()
clearBuildSystem state = 
    atomically $ writeTVar state emptyBuildSystemState

buildSysImpl config refToBuildSystemState = 
    BuildSystem
    { bs_addBuild = addBuild config refToBuildSystemState
    , bs_getBuildRepositoryState = getBuildRepositoryState config refToBuildSystemState

    , bs_getBuildQueue = getBuildQueue config refToBuildSystemState
    , bs_stopBuild = stopBuild config refToBuildSystemState
    , bs_archiveBuild = archiveBuild config refToBuildSystemState
    , bs_restartBuild = restartBuild config refToBuildSystemState

    }
        
-- > BuildPreparing ... BuildReady
addBuild :: BuildSystemConfig -> TVar BuildSystemState -> BuildId -> TarFile -> ErrT IO ()
addBuild config stateRef buildRepoId tarFile = 
    do logBuild buildRepoId LogNote $ "addBuild called"
       updateState stateRef $ addBuildRepository buildRepoId (buildRep Nothing BuildScheduled)
       stopIfRequested stateRef buildRepoId
       _ <- lift $ forkIO $ buildThread config stateRef buildRepoId tarFile
       return ()
    where
        buildRep path buildState =
            BuildRepository
            { br_path = path
            , br_buildState = buildState
            , br_thread = Nothing
            }

readState ref = atomically $ readTVar ref

-- for any known Build: return its state
getBuildRepositoryState :: BuildSystemConfig -> TVar BuildSystemState -> BuildId -> ErrT IO BuildState
getBuildRepositoryState config stateVar buildId = 
    do allBuilds <- lift $ liftM buildSysSt_allBuilds $ atomically $ readTVar stateVar
       (return $ M.lookup buildId allBuilds) >>= (\x -> handleMaybe x (throwError "build not found!") return)
           >>= return . br_buildState

getBuildQueue = undefined

-- BuildScheduled | Building -> BuildStopped
stopBuild :: BuildSystemConfig -> RefToBuildSystemState -> BuildId -> ErrT IO ()
stopBuild config stateRef buildId =
    do logBuild buildId LogNote $ "stopBuild called"
       --throwError "Test!"
       withState stateRef $ \buildSystemState ->
           do buildRep <- getBuildRepository buildId buildSystemState
              when (not $ (br_buildState buildRep) `elem` validStopStates) $
                  throwError $ "Build in state " ++ show (br_buildState buildRep)
              logBuild buildId LogInfo $ "adding to stop queue: " ++ show buildId
              updateState stateRef $ return . mapToExpectedToStop (`L.union` [buildId])
              -- wait for build to stop:
              logBuild buildId LogInfo $ "waiting for build to stop..."
              awaitRes <- awaitStateCond 10000 (`elem` allStates L.\\ validStopStates) buildId stateRef
              case awaitRes of
                TimeOut -> throwError $ "could not stop build! (time out while waiting to stop)"
                _ -> return ()
              logBuild buildId LogInfo $ "stopBuild returns"

archiveBuild config refToBuildSystemState buildId = undefined
restartBuild config refToBuildSystemState buildId = undefined


-----------------------------------------------------------------------------------
-- internals:
-----------------------------------------------------------------------------------

buildScriptPath buildRep = br_path buildRep >>= \brDir -> return $ brDir </> "build.sh"


buildThread :: BuildSystemConfig -> RefToBuildSystemState -> BuildId -> TarFile -> IO ()
buildThread config stateRef buildRepoId tarFile =
    handleBuildErrors stateRef buildRepoId $ 
    do 
       threadId <- lift myThreadId
       updateBuild stateRef buildRepoId $
           (mapToBuildState (const BuildPreparing) . mapToThread (const $ Just threadId))
       stopIfRequested stateRef buildRepoId
       -- 1. untar
       prepareBuild config stateRef buildRepoId tarFile 
       stopIfRequested stateRef buildRepoId
       -- 2. run build script
       build config stateRef buildRepoId

stopIfRequested :: RefToBuildSystemState -> BuildId -> ErrorT ErrMsg IO ()
stopIfRequested stateRef buildRepoId =
    withState stateRef $ \buildSysState ->
        do currentBuildState <- (liftM br_buildState) $ getBuildRepository buildRepoId $ buildSysState
           when (buildRepoId `elem` buildSysSt_expectedToStop buildSysState) $
               do updateBuild stateRef buildRepoId $ mapToBuildState $ const BuildStopped
                  throwError $ "build " ++ show buildRepoId ++ " stopped in state "
                      ++ show currentBuildState

prepareBuild :: BuildSystemConfig -> RefToBuildSystemState -> BuildId -> TarFile -> ErrorT ErrMsg IO ()
prepareBuild config stateRef buildRepoId tarFile =
    do
       path <- buildDirFromFile config buildRepoId tarFile
       updateBuild stateRef buildRepoId $
           mapToPath $ const $ Just path

build :: BuildSystemConfig -> RefToBuildSystemState -> BuildId -> ErrorT ErrMsg IO ()
build config stateRef buildRepoId =
    do
       updateBuild stateRef buildRepoId $
           mapToBuildState $ const Building
       withState stateRef $ \buildSystemState -> 
           do buildRepository <- getBuildRepository buildRepoId $ buildSystemState
              path <- handleMaybe (buildScriptPath buildRepository)
                  (throwError "inconsistency: unable to determine path of build repository, although it is in state \"READY\"") return
              logBuild buildRepoId LogInfo $ "starting build"
              -- run script
              (exitCode, stdOut, stdErr) <- lift $ runProc (logBuild buildRepoId LogNote) path ["run"]
              let newBuildRepositoryState =
                      case exitCode of
                        ExitSuccess -> BuildSuccess
                        _ -> BuildFailed
              case exitCode of
                ExitSuccess -> logBuild buildRepoId LogInfo $ "exit build SUCCESS"
                ExitFailure code ->
                    do logBuild buildRepoId LogNote $ "build FAILED with exit code " ++ show code
                       logBuild buildRepoId LogNote $ "stdout was: " ++ stdOut
                       logBuild buildRepoId LogNote $ "stderr was: " ++ stdErr
              updateBuild stateRef buildRepoId $
                  (mapToBuildState (const newBuildRepositoryState))

handleBuildErrors :: RefToBuildSystemState -> BuildId -> ErrT IO () -> IO ()
handleBuildErrors stateRef buildRepId errVal =
    runErrorT errVal >>= either errHandler return
    where
        setFailed :: IO (Either ErrMsg ())
        setFailed = runErrorT $
            updateBuild stateRef buildRepId $ mapToBuildState $ \currentBuildState ->
                case currentBuildState of
                  Building -> BuildFailed
                  BuildStopped -> BuildStopped
                  _ -> BuildFailed
        errHandler :: ErrMsg -> IO ()
        errHandler msg = 
            do 
               logBuild buildRepId LogError $ "internal build error: " ++ msg
               setFailed >>= either (\e -> logBuild buildRepId LogError $ "error setting build state to FAILED: " ++ e) return

stopBuildThread buildThreadId =
    lift $ killThread buildThreadId

buildDirFromFile :: BuildSystemConfig -> BuildId -> TarFile -> ErrT IO FilePath
buildDirFromFile config buildId tarFile =
    do lift $ runProc (logBuild buildId LogNote) "mkdir" ["--parent", destForBuild]
       lift $ runProc (logBuild buildId LogNote) "tar" ["-x", "-f", (bsc_incoming config </> fromTarFile tarFile), "-C", destForBuild]
       -- would be nicer to use the tar library, but it doesn't preserve filemode bits :-(
       --lift $ Tar.extract destForBuild (bsc_incoming config </> fromTarFile tarFile)
       return $ destForBuild
    where
        destForBuild = destDir config buildId tarFile
        setExecutable entry =
            case takeFileName (Tar.entryPath entry) == "build.sh" of
              True -> entry{ Tar.entryPermissions = Tar.executableFilePermissions }
              _ -> entry

-- determines the location where to save a BuildRepository
destDir :: BuildSystemConfig -> BuildId -> TarFile -> FilePath
destDir config buildId tarFile = bsc_baseDir config </> show (fromBuildId buildId)

withState :: MonadIO m => TVar BuildSystemState -> (BuildSystemState -> m a) -> m a
withState stateRef f =
    do state <- liftIO $ atomically $ readTVar stateRef
       f state 

updateBuild :: TVar BuildSystemState -> BuildId -> (BuildRepository -> BuildRepository) -> ErrT IO ()
updateBuild stateRef buildRepoId updateF =
    do updateState stateRef $ updateBuildRepository buildRepoId $ updateF
       logBuildState stateRef buildRepoId LogInfo

logBuildState stateRef buildRepoId logLevel = 
    withState stateRef $ \buildSystem ->
        do buildRepo <- getBuildRepository buildRepoId buildSystem
           logBuild buildRepoId logLevel $ show buildRepo

logBuild buildRepoId logLevel msg = 
    doLog logLevel $ "Build " ++ show (fromBuildId buildRepoId) ++ ": " ++ msg

updateState :: TVar BuildSystemState -> (BuildSystemState -> ErrT IO BuildSystemState) -> ErrT IO ()
updateState stateRef f = 
    do
       oldState <- lift $ atomically $ readTVar stateRef
       newState <- f oldState 
       --lift $ doLog LogInfo $ "build state: " ++ show newState
       (lift . atomically . writeTVar stateRef) newState

waitForState :: TimeMs -> BuildState -> BuildId -> RefToBuildSystemState -> ErrorT ErrMsg IO AwaitRes
waitForState maxTime state = awaitStateCond maxTime (==state)

awaitStateCond :: TimeMs -> (BuildState -> Bool) -> BuildId -> RefToBuildSystemState -> ErrorT ErrMsg IO AwaitRes
awaitStateCond maxTime cond = awaitBuildRepo maxTime (cond . br_buildState)

awaitBuildRepo :: TimeMs -> (BuildRepository -> Bool) -> BuildId -> RefToBuildSystemState -> ErrorT ErrMsg IO AwaitRes
awaitBuildRepo maxTime condBr buildRepoId ref =
    awaitOrErr maxTime cond ref 
    where
        cond buildSystemState = return . condBr =<< getBuildRepository buildRepoId buildSystemState

{-
loadBuildSystem :: IO RefToBuildSystemState

saveBuildSystem :: RefToBuildSystemState -> IO ()
-}
