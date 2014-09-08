module Frozone.BuildSystem.Intern.Types(
    module Frozone.BuildSystem.Intern.Types,
    module Frozone.BuildSystem.Intern.Model
) where

import Frozone.BuildSystem.Intern.Model

data BuildSystemConfig
    = BuildSystemConfig
    { bsc_baseDir :: FilePath
    , bsc_incoming :: FilePath
    }
