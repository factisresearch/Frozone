module Frozone.BuildSystem.Persistence where

import Frozone.BuildSystem.Intern.Model
import Frozone.Util.ErrorHandling

loadModel :: FilePath -> ErrorT String IO BuildSystemState
loadModel filePath =
    return . read =<< catchAsErr (readFile filePath)


safeModel :: FilePath -> BuildSystemState -> IO ()
safeModel path model = writeFile path (show model)
