{-# LANGUAGE TemplateHaskell #-}
module Frozone.Config where

import qualified Frozone.PackageManager.Connection as PkgManConn
import qualified Frozone.BuildSystem.Impl as BuildSys

import Frozone.Util.Json


data FrozoneConfig
    = FrozoneConfig
    { fc_pkgManConn :: PkgManConn.ConnectionInfo
    , fc_buildSysConfig :: BuildSys.BuildSystemConfig
    }

$(deriveJSON (jDrop 8) ''PkgManConn.ConnectionInfo)
$(deriveJSON (jDrop 4) ''BuildSys.BuildSystemConfig)
$(deriveJSON (jDrop 3) ''FrozoneConfig)
