module Frozone.Controller where


import Frozone.BuildTypes
import Frozone.PackageManager.API as PackMan
import Frozone.BuildSystem.API as BuildSys

--import Frozone.Util.ErrorHandling
import Frozone.Util.Logging hiding (doLog)
import qualified Frozone.Util.Logging as Log

import Control.Monad.Reader
import Control.Monad.Error
import Control.Concurrent

--import Data.Either


data ControllerData
    = ControllerData
    { froz_packageMan :: PackMan.PackageManager
    , froz_buildSystem :: BuildSys.BuildSystem
    --, froz_userManagement :: UserMan.UserManagement
    }

{-
runController :: PackageManager -> BuildSystem -> IO ()
runController _ _ =
    do doLog LogInfo "runController called"
       doLog LogInfo "end of runController"
-}
runController packMan buildSys =
    do doLog LogInfo "runController called"
       forever $
       --id $ 
           do
              putStrLn $ "-------------------------------------------------------------------"
              runReaderT runController' (ControllerData packMan buildSys)
              threadDelay 10000000
       doLog LogInfo "end of runController"

runController' :: ContrM IO ()
runController' =
    do packMan <- asks froz_packageMan
       microBranches <- lift $ pkgMan_listMicroBranches packMan
       --doLog LogInfo $ show microBranches
       mapM_ (lift . handleBuildResp <=< runErrorT . possiblyBuildMicroBranch) $
           microBranches
    where
        handleBuildResp :: Either ErrMsg BuildResp -> IO ()
        handleBuildResp (Left err) =
            doLog LogError $ "error while treating with microbranch: " ++ err
        handleBuildResp (Right _) =
            return ()

possiblyBuildMicroBranch :: MicroBranchInfo -> ErrorT ErrMsg (ContrM IO) BuildResp
possiblyBuildMicroBranch microBranchInfo =
    do errOrBuildState <- lift $ runErrorT $ lookupBuildRep microBranchInfo
       reallyStartBuild <-
           case errOrBuildState of
             Left _ ->
                 do logMicroBranch microBranchInfo LogInfo $ "not found"
                    return BuildStarted -- error means repository not found
             Right buildState ->
                 case buildState of
                   BuildStopped ->
                       do logMicroBranch microBranchInfo LogInfo $ "in state STOPPED"
                          return BuildStarted
                   BuildFailed ->
                       do logMicroBranch microBranchInfo LogInfo $ "in state FAILED"
                          return $ BuildStarted
                   _ -> 
                       do logMicroBranch microBranchInfo LogInfo $ "in state \"" ++ show buildState ++ "\" - nothing to do"
                          return $ BuildAlreadyDone
       when (reallyStartBuild == BuildStarted) $
        do logMicroBranch microBranchInfo LogInfo $ "retrieving build repository..."
           getAndAdd microBranchInfo
       return reallyStartBuild

logMicroBranch microBranchInfo logLevel msg =
    doLog logLevel $
        "microBranch \"" ++ microBranchHashToString (microBranch_id microBranchInfo) ++ "\": " ++ msg

data BuildResp = BuildAlreadyDone | BuildStarted
    deriving (Show, Eq)


lookupBuildRep :: MicroBranchInfo -> ErrorT ErrMsg (ContrM IO) BuildState
lookupBuildRep microBranchInfo = 
    do buildSys <- lift $ asks froz_buildSystem 
       ErrorT $ lift $ runErrorT $
           bs_getBuildRepositoryState buildSys $
           buildIdFromMicroBranch microBranchInfo

getAndAdd :: MicroBranchInfo -> ErrorT ErrMsg (ContrM IO) ()
getAndAdd microBranchInfo =
    do pkgMan <- lift $ asks froz_packageMan
       buildSys <- asks froz_buildSystem
       tarFile <- liftIO $ pkgMan_getBuildRepository pkgMan microBranchInfo
       ErrorT $ lift $ runErrorT $
           bs_addBuild buildSys
           (buildIdFromMicroBranch microBranchInfo)
           tarFile

buildIdFromMicroBranch :: MicroBranchInfo -> BuildId
buildIdFromMicroBranch microBranchInfo =
    BuildId $ microBranchHashToString $ microBranch_id microBranchInfo


--type ControllerAPI =

type ContrM m = ReaderT ControllerData m

{-
data ControllerState
    = ControllerState
    { controller_
-}

doLog logLevel msg = Log.doLog logLevel $ "CONTROLLER: " ++ msg

handleError' :: ContrM (ErrorT ErrMsg IO) a -> ContrM IO (Either ErrMsg a)
handleError' mx = 
    ReaderT $ \frozData -> 
        let temp = runReaderT mx frozData -- :: ErrorT IO a
        in
            runErrorT temp
