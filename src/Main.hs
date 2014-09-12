{-# LANGUAGE TemplateHaskell #-}
module Main where

import Frozone.Util.ErrorHandling
--import Frozone.Model
import qualified Frozone.PackageManager.Connection as PkgManConn
import qualified Frozone.BuildSystem.Impl as BuildSys
--import qualified Frozone.Persist as Persist
import qualified Frozone.Controller as Controller

import System.Environment
import qualified Data.Yaml as YML
import Frozone.Util.Json

main :: IO ()
main =
    do putStrLn "Welcome to Frozone"
       eConfig <- getArgs >>= parseArgs
       case eConfig of
         Left err -> putStrLn err
         Right cfg ->
             do fs <- initFrozone cfg
                --threadDelay 2000000
                runFrozone fs
                exitFrozone fs


initFrozone :: FrozoneConfig -> IO FrozoneState
initFrozone fc =
    let pkgManConnInfo = fc_pkgManConn fc in
    do connection <- PkgManConn.connect pkgManConnInfo
       buildSysRef <- BuildSys.startBuildSystem $ fc_buildSysConfig fc
       return $
           FrozoneState
           { fs_pkgManConn = connection
           , fs_buildSysRef = buildSysRef
           }

runFrozone :: FrozoneState -> IO ()
runFrozone st = 
    Controller.runController
        (PkgManConn.pkgManByConnection $ fs_pkgManConn st)
        (BuildSys.buildSysImpl $ fs_buildSysRef st)
--controllerStateFromFrozoneState st

exitFrozone :: FrozoneState -> IO ()
exitFrozone st =
    do BuildSys.stopBuildSystem $ fs_buildSysRef st
       PkgManConn.disconnect $ fs_pkgManConn st

data FrozoneState
    = FrozoneState
    { fs_pkgManConn :: PkgManConn.Connection
    , fs_buildSysRef :: BuildSys.BuildSystemRef
    }
    

parseArgs :: [String] -> IO (Either String FrozoneConfig)
parseArgs args = 
    case args of
      [cfgFilePath] ->
          YML.decodeFileEither cfgFilePath >>= return . mapLeft show
      _ ->
          return $ Left $ usageStr

usageStr = "Usage: ./Frozone [cfgFile]"



--controllerStateFromFrozoneState _ = Controller.ControllerState

data FrozoneConfig
    = FrozoneConfig
    { fc_pkgManConn :: PkgManConn.ConnectionInfo
    , fc_buildSysConfig :: BuildSys.BuildSystemConfig
    }

$(deriveJSON (jDrop 8) ''PkgManConn.ConnectionInfo)
$(deriveJSON (jDrop 4) ''BuildSys.BuildSystemConfig)
$(deriveJSON (jDrop 3) ''FrozoneConfig)
