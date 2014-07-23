module Frozone.VCS.Types where

import System.Exit
import System.Process
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM

data FileChangeAction
   = FileChangeCreated
   | FileChangeModified
   | FileChangeDeleted
   deriving (Show, Eq)

type FileChangeMap = HM.HashMap FilePath FileChangeAction

newtype VCSSource
    = VCSSource { unVCSSource :: String }

newtype VCSRepository
    = VCSRepository { unVCSRepository :: String }

newtype VCSPatch
    = VCSPatch { unVCSPatch :: BS.ByteString }

data VCSResponse a
   = VCSResponse
   { vcs_stdOut :: BSC.ByteString
   , vcs_stdErr :: BSC.ByteString
   , vcs_success :: Bool
   , vcs_data :: Maybe a
   }

data VCSApi
   = VCSApi
   { vcs_cloneRepository :: VCSSource -> VCSRepository -> IO (VCSResponse ())
   , vcs_applyPatch :: VCSPatch -> VCSRepository -> IO (VCSResponse ())
   , vcs_changeLog :: VCSRepository -> IO (VCSResponse ())
   , vcs_changedFiles :: VCSPatch -> IO (VCSResponse FileChangeMap)
   }

runVCS :: FilePath -> [String] -> String -> (BS.ByteString -> a) -> IO (VCSResponse a)
runVCS command args stdIn successHandler =
    do (ec, stdOut, stdErr) <- readProcessWithExitCode command args stdIn
       let stdOut' = BSC.pack stdOut
       return $ VCSResponse
                { vcs_stdOut = stdOut'
                , vcs_stdErr = BSC.pack stdErr
                , vcs_success = (ec == ExitSuccess)
                , vcs_data =
                    if ec == ExitSuccess
                    then Just $ successHandler stdOut'
                    else Nothing
                }
