module Frozone.Controller where


import qualified Frozone.PackageManager as PackMan
import qualified Frozone.BuildSystem.Impl as BuildSys
import qualified Frozone.Model as M
import qualified Frozone.Persist as Persist


runController :: ControllerState -> IO ()
runController st = putStrLn "controller not yet implemented!"

data ControllerState = ControllerState
