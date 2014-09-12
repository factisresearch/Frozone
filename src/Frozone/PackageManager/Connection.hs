module Frozone.PackageManager.Connection(
    Connection(), ConnectionInfo(..),
    connect, disconnect,
    pkgManByConnection,
) where

import Frozone.BuildTypes
import Frozone.PackageManager.API

import Control.Monad.Reader

pkgManByConnection :: Connection -> PackageManager
pkgManByConnection conn = 
    PackageManager
    { pkgMan_addPatchBundle = \patchBundle -> runReaderT (addPatchBundle patchBundle) conn
    , pkgMan_listMicroBranches = runReaderT listMicroBranches conn
    , pkgMan_getBuildRepository = \microBranchInfo -> runReaderT (getBuildRepository microBranchInfo) conn
    }

data ConnectionInfo
    = ConnectionInfo
    { connInf_host :: String
    , connInf_port :: Int
    }
data Connection = Connection

connect :: ConnectionInfo -> IO Connection
connect _ = return Connection

disconnect :: Connection -> IO ()
disconnect _ = return ()

addPatchBundle :: PatchBundle -> ReaderT Connection IO ()
addPatchBundle _ = undefined

listMicroBranches :: ReaderT Connection IO [MicroBranchInfo]
listMicroBranches = undefined

getBuildRepository :: MicroBranchInfo -> ReaderT Connection IO TarFile
getBuildRepository _ = undefined
