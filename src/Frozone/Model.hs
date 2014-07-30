{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
module Frozone.Model where

import Frozone.Types

import Control.Monad.Trans
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Data.Time

import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateCore"] [persistLowerCase|
BundleData json
     bundleHash T.Text
     filePath FilePath
     date UTCTime
     UniqueBundleHash bundleHash

PatchCollection json
     name T.Text
     open Bool

Patch json
     vcsId T.Text
     name T.Text
     author T.Text
     date UTCTime
     dependents [PatchId]
     bundle BundleDataId
     group PatchCollectionId
     UniquePatchVcsId vcsId

BuildLog json
     state BuildState
     time UTCTime
     message T.Text
     repo BuildRepositoryId

BuildRepository json
     branch T.Text
     path FilePath
     createdOn UTCTime
     notifyEmail [T.Text]
     changesHash T.Text
     patch PatchId
     state BuildState
     dockerImage T.Text Maybe
     UniqueRepoPath path
     UniqueChangesHash changesHash

BundleChange json
     filename FilePath
     oldContents T.Text Maybe
     newContents T.Text Maybe
     repoId BuildRepositoryId
|]

updateBuildState :: (PersistMonadBackend m ~ SqlBackend, MonadIO m, PersistQuery m, MonadSqlPersist m)
                 => BuildRepositoryId -> BuildState -> T.Text -> m ()
updateBuildState buildRepoId newState msg =
    do now <- liftIO getCurrentTime
       update buildRepoId [ BuildRepositoryState =. newState ]
       _ <- insert (BuildLog BuildFailed now msg buildRepoId)
       return ()
