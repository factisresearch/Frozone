{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Frozone.Types where

import Frozone.VCS
import Frozone.Util.Json

import Database.Persist.Sql
import Control.Monad.Trans.Error
import Web.Spock
import Web.Spock.Worker
import qualified Data.Text as T

type FrozoneApp a = SpockM Connection () FrozoneState a
type FrozoneAction a = SpockAction Connection () FrozoneState a
type FrozoneWorker a = WebStateM Connection () FrozoneState a

type FrozoneQueueWorker a = WorkHandler Connection () FrozoneState a
type FrozoneQueueWorkerM a = ErrorT String (WebStateM Connection () FrozoneState) a

data FrozoneConfig
   = FrozoneConfig
   { fc_sqliteFile :: FilePath
   , fc_storageDir :: FilePath
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


data BuildState
   = BuildEnqueued
   | BuildPreparing
   | BuildStarted
   | BuildFailed
   | BuildSuccess
   | BuildCanceled
   | BuildNeedsRecheck
   | BuildReviewStarted
   | BuildReviewRejected
   | BuildReviewOkay
   | BuildApplied
   deriving (Read, Show, Eq, Ord, Enum)

prettyBuildState :: BuildState -> T.Text
prettyBuildState st =
    case st of
      BuildEnqueued -> "enqueued"
      BuildPreparing -> "preparing"
      BuildStarted -> "started"
      BuildFailed -> "failed"
      BuildSuccess -> "success"
      BuildCanceled -> "canceled"
      BuildNeedsRecheck -> "recheck"
      BuildReviewStarted -> "in-review"
      BuildReviewRejected -> "review-rejected"
      BuildReviewOkay -> "review-okay"
      BuildApplied -> "applied"

parseBuildState :: T.Text -> Maybe BuildState
parseBuildState t =
    case t of
      "enqueued" -> Just BuildEnqueued
      "preparing" -> Just BuildPreparing
      "started" -> Just BuildStarted
      "failed" -> Just BuildFailed
      "success" -> Just BuildSuccess
      "canceled" -> Just BuildCanceled
      "recheck" -> Just BuildNeedsRecheck
      "in-review" -> Just BuildReviewStarted
      "review-rejected" -> Just BuildReviewRejected
      "review-okay" -> Just BuildReviewOkay
      "applied" -> Just BuildApplied
      _ -> Nothing

instance PersistFieldSql BuildState where
    sqlType _ = SqlString

instance PersistField BuildState where
    toPersistValue = toPersistValue . prettyBuildState
    fromPersistValue pv =
        case pv of
          PersistText t ->
              case parseBuildState t of
                Nothing -> Left $ (T.pack $ "Failed to parse " ++ show t ++ " as BuildState!")
                Just st -> Right st
          _ ->
              Left $ T.pack $ "The field " ++ show pv ++ " should be PersistText to be parsed as BuildState!"

instance ToJSON BuildState where
    toJSON = toJSON . prettyBuildState

instance FromJSON BuildState where
    parseJSON (String t) =
        case parseBuildState t of
          Just st -> return st
          Nothing -> fail $ show t ++ " is an invalid BuildState string!"
    parseJSON _ = fail "Expecting string to parse as BuildState!"

$(deriveJSON (jDrop 3) ''FrozoneConfig)
$(deriveJSON (jDrop 3) ''FrozoneSmtp)
$(deriveJSON (jDrop 3) ''FrozoneMessage)
$(deriveJSON (jDrop 3) ''FrozoneError)
$(deriveJSON (jDrop 4) ''FrozoneRepoCreated)
$(deriveJSON (jDrop 3) ''RepoConfig)
