{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.RestApi where

import Frozone.Types
import Frozone.Model
import Frozone.Util.Db

import Web.Spock hiding (patch)
import Web.Spock.Worker
import Database.Persist ((==.), (=.))
import qualified Database.Persist as DB

restApi :: WorkQueue BuildRepositoryId -> FrozoneApp ()
restApi buildQueue =
    do get "/list-builds" $
         do allBuilds <- runSQL $ DB.selectList [] [DB.Desc BuildRepositoryId, DB.LimitTo 50]
            json allBuilds
       get "/patch/:patchId" $
         do Just (patchId :: PatchId) <- param "patchId"
            mPatch <- runSQL $ DB.get patchId
            case mPatch of
              Nothing ->
                  json (FrozoneError "Patch not found!")
              Just patchVal ->
                  json patchVal
       get "/build/patch/:patchId" $
           do Just (patchId :: PatchId) <- param "patchId"
              buildList <- runSQL $ DB.selectList [BuildRepositoryPatch ==. patchId] [DB.Desc BuildRepositoryId]
              case buildList of
                [] -> json (FrozoneError "No builds found belonging to this patch!")
                xs -> json xs
       get "/build/:buildId" $
         do Just (buildId :: BuildRepositoryId) <- param "buildId"
            mBuild <- runSQL $ DB.get buildId
            case mBuild of
              Nothing ->
                  json (FrozoneError "Build not found!")
              Just build ->
                  json build
       get "/build/:buildId/logs" $
         do Just buildId <- param "buildId"
            fullLogs <- runSQL $ DB.selectList [BuildLogRepo ==. buildId] [DB.Desc BuildLogTime, DB.LimitTo 50]
            json fullLogs
       get "/build/:buildId/file-changes" $
         do Just (buildId :: BuildRepositoryId) <- param "buildId"
            allChanges <- runSQL $ DB.selectList [BundleChangeRepoId ==. buildId] []
            json allChanges
       get "/build/:buildId/cancel" $
         do Just (buildId :: BuildRepositoryId) <- param "buildId"
            runSQL $ updateBuildState buildId BuildCanceled  "Aborted by user"
            -- todo: kill docker build if running
            json (FrozoneMessage "Canceled!")
       get "/build/:buildId/rebuild" $
         do Just (buildId :: BuildRepositoryId) <- param "buildId"
            mBuild <- runSQL $ DB.get buildId
            case mBuild of
              Nothing ->
                  json (FrozoneError "Build not found!")
              Just build ->
                  if buildRepositoryState build > BuildStarted
                  then do runSQL $ updateBuildState buildId BuildCanceled "Rebuilt scheduled"
                          addWork WorkNow buildId buildQueue
                          json (FrozoneMessage "Rebuild enqueued")
                  else json (FrozoneError "Already building!")
       get "/collection/:collectionId" $
         do Just (collectionId :: PatchCollectionId) <- param "collectionId"
            mCollection <- runSQL $ DB.get collectionId
            case mCollection of
              Nothing ->
                  json (FrozoneError "Collection not found!")
              Just collection ->
                  json collection
       get "/collection/:collectionId/patches" $
         do Just (collectionId :: PatchCollectionId) <- param "collectionId"
            patchList <- runSQL $ DB.selectList [PatchGroup ==. collectionId] [DB.Desc PatchId, DB.LimitTo 50]
            json patchList
       get "/collection/:collectionId/close" $
         do Just (collectionId :: PatchCollectionId) <- param "collectionId"
            runSQL $ DB.update collectionId [ PatchCollectionOpen =. False ]
            json (FrozoneMessage "Collection closed")
