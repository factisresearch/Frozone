{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Frozone.Types
  ( module Frozone.Types
  , module Frozone.Model) where

import Frozone.Model

import Frozone.VCS
import Frozone.Util.Json

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
   }

data FrozoneState
   = FrozoneState
   { fs_config :: FrozoneConfig
   , fs_vcs :: VCSApi
   }

type FrozoneMessage = Either FrozoneError FrozoneResponse

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
$(deriveJSON (jDrop 3) ''FrozoneError)
$(deriveJSON (jDrop 4) ''FrozoneRepoCreated)
$(deriveJSON (jDrop 3) ''RepoConfig)
$(deriveJSON (jDrop 4) ''UserInfo)
$(deriveJSON (jDrop 3) ''ProjectInfo)
