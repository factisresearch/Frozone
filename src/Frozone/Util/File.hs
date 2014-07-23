module Frozone.Util.File where

import System.Directory
import Control.Exception
import System.IO.Error

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
