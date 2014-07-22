{-# LANGUAGE TemplateHaskell #-}
module Frozone.Types where

import Frozone.Util.Json

import Control.Concurrent.STM
import Database.Persist.Sql
import Web.Spock
import qualified Data.HashSet as HS
import qualified Data.Text as T

type FrozoneApp = SpockM Connection () FrozoneState ()
type FrozoneAction a = SpockAction Connection () FrozoneState a
type FrozoneWorker a = WebStateM Connection () FrozoneState a

data FrozoneConfig
   = FrozoneConfig
   { fc_sqliteFile :: FilePath
   , fc_storageDir :: FilePath
   , fc_httpPort :: Int
   }

data FrozoneState
   = FrozoneState
   { fs_config :: FrozoneConfig
   , fs_baseImageBuildsVar :: TVar (HS.HashSet T.Text)
   }

data FrozoneMessage
   = FrozoneMessage
   { fm_message :: T.Text }

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

$(deriveJSON (jDrop 3) ''FrozoneConfig)
$(deriveJSON (jDrop 3) ''FrozoneMessage)
$(deriveJSON (jDrop 3) ''FrozoneError)
$(deriveJSON (jDrop 4) ''FrozoneRepoCreated)
$(deriveJSON (jDrop 3) ''RepoConfig)
