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

import Database.Persist.TH
import Data.Time

import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateCore"] [persistLowerCase|
TempRepository json
     branch T.Text
     path FilePath
     createdOn UTCTime
     notifyEmail [T.Text]
     changesHash T.Text
     patchBundle T.Text
     buildEnqueuedOn UTCTime Maybe
     buildStartedOn UTCTime Maybe
     buildFailedOn UTCTime Maybe
     buildSuccessOn UTCTime Maybe
     buildMessage T.Text
     patchCanceledOn UTCTime Maybe
     patchCancelReason T.Text Maybe
     dockerImage T.Text Maybe
     UniqueRepoPath path
     UniqueChangesHash changesHash
BundleChange json
     filename FilePath
     oldContents T.Text Maybe
     newContents T.Text Maybe
     repoId TempRepositoryId
|]
