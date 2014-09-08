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
connect _ = undefined

disconnect :: Connection -> IO ()
disconnect _ = undefined

addPatchBundle _ = undefined

listMicroBranches _ = undefined

getBuildRepository _ = undefined
