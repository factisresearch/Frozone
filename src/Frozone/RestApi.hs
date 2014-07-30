{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.RestApi where

import Frozone.Types
import Frozone.Model
import Frozone.Util.Db

import Web.Spock
import Database.Persist ((==.))
import qualified Database.Persist as DB

restApi :: FrozoneApp
restApi =
    do get "/list-builds" $
         do allBuilds <- runSQL $ DB.selectList [] [DB.Desc BuildRepositoryId, DB.LimitTo 50]
            json allBuilds

       get "/build/:buildId/cancel" $
         do Just (buildId :: BuildRepositoryId) <- param "buildId"
            runSQL $ updateBuildState buildId BuildCanceled  "Aborted by user"
            -- todo: kill docker build if running
            json (FrozoneMessage "Canceled!")

       get "/build/:buildId" $
         do Just (buildId :: BuildRepositoryId) <- param "buildId"
            mBuild <- runSQL $ DB.get buildId
            case mBuild of
              Nothing ->
                  json (FrozoneError "Build not found!")
              Just build ->
                  json build

       get "/build/:buildId/file-changes" $
         do Just (buildId :: BuildRepositoryId) <- param "buildId"
            allChanges <- runSQL $ DB.selectList [BundleChangeRepoId ==. buildId] []
            json allChanges
