module Frozone.PkgManConnection where

import Frozone.PackageManager

pkgManByConnection :: Connection -> PackageManager
pkgManByConnection conn = 
    PackageManager
    { pkgMan_addPatchBundle = addPatchBundle conn
    , pkgMan_listMicroBranches = listMicroBranches conn
    , pkgMan_getBuildRepository = getBuildRepository conn
    }

data ConnectionInfo
    = ConnectionInfo
    { host :: String
    , port :: Int
    }
data Connection = Connection

connect :: ConnectionInfo -> IO Connection
connect connInfo = undefined

disconnect :: Connection -> IO ()
disconnect conn = undefined

addPatchBundle conn = undefined

listMicroBranches conn = undefined

getBuildRepository conn = undefined
