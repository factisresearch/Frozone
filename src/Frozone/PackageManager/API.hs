module Frozone.PackageManager.API where

import Frozone.BuildTypes


data PackageManager
    = PackageManager
    { pkgMan_listMicroBranches :: IO [MicroBranchInfo]
    , pkgMan_getBuildRepository :: IO TarFile
    , pkgMan_addPatchBundle :: PatchBundle -> IO ()
    }
