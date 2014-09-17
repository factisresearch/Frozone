{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Frozone.BuildTypes where

--import System.FilePath
--import qualified Data.Text as T

{-
data MicroBranchInfo
    = MicroBranchInfo
    { microBranch_id :: T.Text -- could be a hash
    , microBranch_user :: T.Text
    , microBranch_describtion :: T.Text
    , microBranch_dependencyInfo :: DependencyInfo
    }
    deriving (Show, Eq)


data PatchBundle = PatchBundle
    deriving (Show, Eq)

data DependencyInfo = DependencyInfo
    deriving (Show, Eq)
-}


import Frozone.Util.Json
import Data.Aeson.TH

import qualified Data.ByteString as BS


newtype PatchID = PatchID { unPatchID :: String }
                deriving (Eq,Ord,Show,Read)

data MicroBranchInfo
    = MicroBranchInfo
    { microBranch_id :: MicroBranchHash
    , microBranch_name :: String -- name/describtion of the corresponding patch
    , microBranch_user :: String -- author of the patch
    , microBranch_describtion :: String -- some additional info (dependency information, ...) in text form
    }
    deriving (Show, Eq)

data MicroBranchHash
    = MicroBranchHash
    { microBranchHash_dpm :: PatchID
    , microBranchHash_extension :: String
    }
    deriving (Show, Read, Eq, Ord)

data Tar = Tar { fromTar :: BS.ByteString }
    deriving (Show, Eq)

microBranchHashToString :: MicroBranchHash -> String
microBranchHashToString hash = unPatchID (microBranchHash_dpm hash) ++ "_" ++ microBranchHash_extension hash

microBranchHashFromString :: String -> Maybe MicroBranchHash
microBranchHashFromString str =
    case split str of
      ([],_) -> Nothing
      (_,[]) -> Nothing
      (dpmHash, extension) -> Just $ MicroBranchHash (PatchID dpmHash) extension
      where
          split str' =
              let (x,y) = span (/='_') str'
              in (x, drop 1 y)

$(deriveJSON (defaultOptions{ fieldLabelModifier = dropPrefix }) ''PatchID)
$(deriveJSON (defaultOptions{ fieldLabelModifier = dropPrefix }) ''MicroBranchHash)

$(deriveJSON (defaultOptions{ fieldLabelModifier = dropPrefix }) ''MicroBranchInfo)

{-
import Frozone.Model

import Frozone.VCS

import Database.Persist.Sql
import Control.Monad.Trans.Error
import Web.Spock
import Web.Spock.Auth
import Web.Spock.Worker
import qualified Data.Text as T

type FrozoneApp a = SpockM Connection FrozoneSession FrozoneState a
type FrozoneAction a = SpockAction Connection FrozoneSession FrozoneState a
type FrozoneWorker a = WebStateM Connection FrozoneSession FrozoneState a

type FrozoneSession = VisitorSession () SessionId

type FrozoneQueueWorker a = WorkHandler Connection FrozoneSession FrozoneState a
type FrozoneQueueWorkerM a = ErrorT String (WebStateM Connection FrozoneSession FrozoneState) a

data FrozoneConfig
   = FrozoneConfig
   { fc_sqliteFile :: FilePath
   , fc_storageDir :: FilePath -- where to unpack patch bundles, ?, ...
   , fc_httpPort :: Int
   , fc_vcs :: String
   , fc_concurrentBuilds :: Int
   , fc_mailConfig :: Maybe FrozoneSmtp
   }

data FrozoneSmtp
   = FrozoneSmtp
   { fs_host :: String
   , fs_port :: Int
   , fs_user :: Maybe String
   , fs_password :: Maybe String
   }

data FrozoneState
   = FrozoneState
   { fs_config :: FrozoneConfig
   , fs_vcs :: VCSApi
   }

data FrozoneResponse
    = FrozoneCmdLogin
    | FrozoneCmdLogout
    -- user management
    | FrozoneGetUsers [UserInfo] -- [(UserName, EMail, isAdmin)]
    | FrozoneCmdCreateUser
    | FrozoneCmdDeleteUser
    | FrozoneCmdUpdatePassword
    | FrozoneCmdUpdateEmail
    | FrozoneCmdUpdateIsAdmin
    -- patch
    | FrozoneGetPatch Patch
    | FrozoneGetPatchBuilds [BuildRepository]
    -- general information
    | FrozoneGetBuilds [BuildRepository]
    -- build repository
    | FrozoneGetBuild BuildRepository
    | FrozoneGetBuildLogs [BuildLog]
    | FrozoneGetBuildFileChanges [BundleChange] -- hash
    | FrozoneCmdBuildCancel
    | FrozoneCmdBuildRetry
    -- patch collection
    | FrozoneGetCollection PatchCollection
    | FrozoneGetCollectionPatches [Patch]
    | FrozoneCmdCollectionClose
    | FrozoneInfo T.Text
    -- projects:
    | FrozoneGetProjects [ProjectInfo]
    | FrozoneGetProjectInfo ProjectInfo
    | FrozoneCmdCreateProject
    | FrozoneCmdDeleteProject
    | FrozoneCmdUpdateProjectName
    | FrozoneCmdUpdateProjectShortName
    | FrozoneCmdUpdateProjectUsers
    | FrozoneCmdUpdateProjectRepoLoc
    | FrozoneCmdUpdateProjectSSHKey
    | FrozoneCmdCheck


data FrozoneError
   = FrozoneError
   { fe_error :: T.Text }

data FrozoneRepoCreated
   = FrozoneRepoCreated
   { frc_pushTarget :: T.Text }

data RepoConfig
   = RepoConfig
   { rc_cookDir :: FilePath
   , rc_entryPoint :: String
   , rc_boringFile :: Maybe FilePath
   } deriving (Show, Eq)

data UserInfo = UserInfo
   { sui_name :: T.Text
   , sui_email :: T.Text
   , sui_isAdmin :: Bool
   }

data ProjectInfo = ProjectInfo
   { pi_name :: T.Text
   , pi_shortName :: T.Text
   , pi_repoLoc :: T.Text
   , pi_sshKey :: T.Text
   , pi_users :: [T.Text]
   }


$(deriveJSON (jDrop 3) ''FrozoneConfig)
$(deriveJSON (jDrop 0) ''FrozoneResponse)
$(deriveJSON (jDrop 3) ''FrozoneSmtp)
$(deriveJSON (jDrop 3) ''FrozoneError)
$(deriveJSON (jDrop 4) ''FrozoneRepoCreated)
$(deriveJSON (jDrop 3) ''RepoConfig)
$(deriveJSON (jDrop 4) ''UserInfo)
$(deriveJSON (jDrop 3) ''ProjectInfo)
-}
