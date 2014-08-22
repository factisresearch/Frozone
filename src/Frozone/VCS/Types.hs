module Frozone.VCS.Types where

import Data.Time
import System.Exit
import System.Process
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data FileChangeAction
   = FileChangeCreated
   | FileChangeModified
   | FileChangeDeleted
   deriving (Show, Eq)

type FileChangeMap = HM.HashMap FilePath FileChangeAction

newtype VCSSource
    = VCSSource { unVCSSource :: String }
      deriving (Show, Read, Eq)

newtype VCSRepository
    = VCSRepository { unVCSRepository :: FilePath }
      deriving (Show, Read, Eq)

newtype VCSPatchBundle
    = VCSPatchBundle { unVCSPatch :: BS.ByteString }
      deriving (Show, Read, Eq)

newtype VCSPatchId
    = VCSPatchId { unVCSPatchId :: BS.ByteString }
      deriving (Show, Read, Eq)

data VCSPatch
   = VCSPatch
   { vp_id :: VCSPatchId
   , vp_name :: T.Text
   , vp_author :: T.Text
   , vp_date :: UTCTime
   , vp_dependents :: [VCSPatchId] -- dependencies
   } deriving (Show, Read, Eq)

data VCSResponse a
   = VCSResponse
   { vcs_stdOut :: BSC.ByteString
   , vcs_stdErr :: BSC.ByteString
   , vcs_success :: Bool
   , vcs_data :: Maybe a
   } deriving (Show, Read, Eq)

data VCSApi
   = VCSApi
   { vcs_patchesFromBundle :: VCSRepository -> VCSPatchBundle -> IO (VCSResponse [VCSPatch]) -- ^
   , vcs_changedFiles :: VCSPatchId -> VCSPatchBundle -> IO (VCSResponse FileChangeMap)
   , vcs_cloneRepository :: VCSSource -> VCSRepository -> IO (VCSResponse ()) -- ^makes a local copy of a repository
   -- , vcs_pushRepository :: VCSRepository -> VCSSource -> IO (VCSResponse ())
   , vcs_applyPatch :: VCSPatchId -> VCSPatchBundle -> VCSRepository -> IO (VCSResponse ())
   , vcs_changeLog :: VCSRepository -> IO (VCSResponse ()) -- read vcs log
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
