module Frozone.PackageManager.Connection(
    Connection(), ConnectionInfo(..),
    connect, disconnect,
    pkgManByConnection,
) where

import Frozone.PackageManager.API

pkgManByConnection :: Connection -> PackageManager
pkgManByConnection conn = 
    PackageManager
    { pkgMan_addPatchBundle = addPatchBundle conn
    , pkgMan_listMicroBranches = listMicroBranches conn
    , pkgMan_getBuildRepository = getBuildRepository conn
    }

data ConnectionInfo
    = ConnectionInfo
    { connection_host :: String
    , connection_port :: Int
    }
data Connection = Connection

connect :: ConnectionInfo -> IO Connection
connect _ = undefined

disconnect :: Connection -> IO ()
disconnect _ = undefined

addPatchBundle _ = undefined

listMicroBranches _ = undefined

getBuildRepository _ = undefined
