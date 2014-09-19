module Frozone.PackageManager.API where

import Frozone.BuildTypes


data PackageManager
    = PackageManager
    { pkgMan_listMicroBranches :: IO [MicroBranchInfo]
    , pkgMan_getBuildRepository :: MicroBranchInfo -> IO Tar
    --, pkgMan_addPatchBundle :: PatchBundle -> IO ()
    }
