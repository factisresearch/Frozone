{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
module Frozone.Model(
    module Frozone.ModelTypes
    , module Frozone.Model
) where

import Frozone.ModelTypes

import Control.Monad.Trans
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Data.Time

import qualified Data.Text as T

{-
    PatchBundle
    ->
    [Patch] 
-}

share [mkPersist sqlSettings, mkMigrate "migrateCore"] [persistLowerCase|

User json
     name T.Text
     password T.Text
     email T.Text
     isAdmin Bool
     UniqueUserName name
     --UniqueEmail email

Project json
     name T.Text
     shortName T.Text
     repoLoc T.Text
     sshKey T.Text
     users [UserId] --- > [User]
     UniqueShortName shortName

Session json
     user UserId --- > User
     validUntil UTCTime
     --UniqueUserId user

BundleData json -- collection of patches sent
     bundleHash T.Text
     filePath FilePath
     date UTCTime
     UniqueBundleHash bundleHash

PatchCollection json -- group of patches having the same name
     name T.Text
     project ProjectId --- > Project
     open Bool

Patch json
     vcsId T.Text
     name T.Text
     author T.Text
     date UTCTime
     dependents [PatchId] --- > [Patch]
     bundle BundleDataId --- > [BundleData]
     group PatchCollectionId --- > PatchCollection
     UniquePatchVcsId vcsId

BuildLog json
     state BuildState
     time UTCTime
     message T.Text
     repo BuildRepositoryId --- > BuildRepository

BuildRepository json
     project ProjectId --- > Project
     branch T.Text
     path FilePath
     createdOn UTCTime
     notifyEmail [T.Text]
     changesHash T.Text
     patch PatchId --- > Patch
     state BuildState
     dockerImage T.Text Maybe
     UniqueRepoPath path
     UniqueChangesHash changesHash

BundleChange json
     filename FilePath
     oldContents T.Text Maybe
     newContents T.Text Maybe
     repoId BuildRepositoryId --- > BuildRepository
|]

updateBuildState :: (PersistMonadBackend m ~ SqlBackend, MonadIO m, PersistQuery m, MonadSqlPersist m)
                 => BuildRepositoryId -> BuildState -> T.Text -> m BuildLogId
updateBuildState buildRepoId newState msg =
    do now <- liftIO getCurrentTime
       update buildRepoId [ BuildRepositoryState =. newState ]
       insert (BuildLog newState now msg buildRepoId)
