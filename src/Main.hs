{-# LANGUAGE TemplateHaskell #-}
module Main where

import Frozone.Util.ErrorHandling
import Frozone.Model
import qualified Frozone.PkgManConnection as PkgManConn
import qualified Frozone.Persist as Persist
import qualified Frozone.Controller as Controller

import System.Environment
import qualified Data.Yaml as YML
import Control.Monad

import Frozone.Util.Json

main :: IO ()
main =
    do putStrLn "Welcome to Frozone"
       eConfig <- getArgs >>= parseArgs
       case eConfig of
         Left err -> putStrLn err
         Right cfg ->
             do fs <- initFrozone cfg
                runFrozone fs
                exitFrozone fs


initFrozone :: FrozoneConfig -> IO FrozoneState
initFrozone fc =
    let pkgManConnInfo = fc_pkgManConnInfo fc in
    do Persist.startPersistence
       connection <- PkgManConn.connect pkgManConnInfo
       return $
           FrozoneState
           { fs_pkgManConn = connection
           }

runFrozone :: FrozoneState -> IO ()
runFrozone st = Controller.runController $ controllerStateFromFrozoneState st

exitFrozone :: FrozoneState -> IO ()
exitFrozone st =
    do PkgManConn.disconnect $ fs_pkgManConn st
       

data FrozoneState
    = FrozoneState
    { fs_pkgManConn :: PkgManConn.Connection
    }
    

parseArgs :: [String] -> IO (Either String FrozoneConfig)
parseArgs args = 
    case args of
      [cfgFilePath] ->
          YML.decodeFileEither cfgFilePath >>= return . mapLeft show
      _ ->
          return $ Left $ usageStr

usageStr = "Usage: ./Frozone [cfgFile]"



controllerStateFromFrozoneState st = Controller.ControllerState

data FrozoneConfig
    = FrozoneConfig
    { fc_pkgManConnInfo :: PkgManConn.ConnectionInfo
    }

$(deriveJSON (jDrop 3) ''PkgManConn.ConnectionInfo)
$(deriveJSON (jDrop 3) ''FrozoneConfig)
