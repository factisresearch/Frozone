{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
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
     buildSuccess Bool Maybe
     buildMessage T.Text Maybe
     dockerBaseImage T.Text Maybe
     dockerImage T.Text Maybe
     UniqueRepoPath path
     UniqueChangesHash changesHash
|]
