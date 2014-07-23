{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.RestApi where

import Frozone.Types
import Frozone.Model
import Frozone.Util.Db

import Control.Monad.Trans
import Data.Time
import Web.Spock
import Database.Persist ((=.), (==.))
import qualified Database.Persist as DB

restApi :: FrozoneApp
restApi =
    do get "/list-builds" $
         do allBuilds <- runSQL $ DB.selectList [] [DB.Desc TempRepositoryId, DB.LimitTo 50]
            json allBuilds

       get "/build/:buildId/cancel" $
         do Just (buildId :: TempRepositoryId) <- param "buildId"
            now <- liftIO getCurrentTime
            runSQL $ DB.update buildId [ TempRepositoryPatchCanceledOn =. (Just now)
                                       , TempRepositoryPatchCancelReason =. (Just "Canceled by user")
                                       ]
            -- todo: kill docker build if running
            json (FrozoneMessage "Canceled!")

       get "/build/:buildId" $
         do Just (buildId :: TempRepositoryId) <- param "buildId"
            mBuild <- runSQL $ DB.get buildId
            case mBuild of
              Nothing ->
                  json (FrozoneError "Build not found!")
              Just build ->
                  json build

       get "/build/:buildId/file-changes" $
         do Just (buildId :: TempRepositoryId) <- param "buildId"
            allChanges <- runSQL $ DB.selectList [BundleChangeRepoId ==. buildId] []
            json allChanges
