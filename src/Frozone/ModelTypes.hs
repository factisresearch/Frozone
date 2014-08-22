{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Frozone.ModelTypes where

import Frozone.VCS
import Frozone.Util.Json

import Database.Persist.Sql
import Control.Monad.Trans.Error
import Web.Spock
import Web.Spock.Auth
import Web.Spock.Worker
import qualified Data.Text as T


data BuildState
   = BuildEnqueued 
   | BuildPreparing -- the BuildRepository is about to get built
   | BuildStarted   -- the BuildRepository ist currently being built into an image
   | BuildFailed
   | BuildSuccess   -- a docker image has successfully been built from the BuildRepository
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

