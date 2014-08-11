{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.RestApi where

import Frozone.User
import Frozone.Types
import Frozone.Util.Rest

import Frozone.Util.Db
import Frozone.Util.Logging
import Frozone.Util.Rest

import Web.Spock hiding (patch)
import Web.Spock.Worker
import Web.Spock.Auth hiding (userRoute)
import qualified Web.Spock.Auth as Spock
import qualified Data.Text as T

import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Database.Persist ((==.), (=.))

import Control.Monad


restApi :: WorkQueue BuildRepositoryId -> FrozoneApp ()
restApi buildQueue =
    do post "/login" $
         do mName <- param "name"
            mPassword <- param "password" :: FrozoneAction (Maybe T.Text)
            let mNameAndPassword = (uncurry $ liftM2 (,)) (mName,mPassword) :: Maybe (T.Text,T.Text)
            maybeError LogNote "expecting fields name, password" "expecting fields name, password" mNameAndPassword $ \(name,password) ->
              do mUserId <- runSQL $ checkUser name password
                 case mUserId of
                   Just userId ->
                     do sessionId <- runSQL $ sessionIntoDB userId
                        markAsLoggedIn sessionId
                        json $ FrozoneCmdLogin
                   Nothing ->
                     do doLog LogNote "login failed"
                        json $ FrozoneError "login failed"
       userRoute GET [] "/logout" $ \(userId, _) ->
         do 
            runSQL $ sessionDelFromDB userId
            markAsGuest
            json $ FrozoneCmdLogout
       userRoute GET [] "/list-builds" $ \_ ->
         do allBuilds <- runSQL $ DB.selectList [] [DB.Desc BuildRepositoryId, DB.LimitTo 50]
            json $ FrozoneGetBuilds $ map DB.entityVal allBuilds
       userRoute GET [] "/patch/:patchId" $ -- return patch info
         withPatch "patchId" "Patch not found!" $ \((_,_),(_,patch)) -> 
            json $ FrozoneGetPatch patch
       userRoute GET [] "/build/patch/:patchId" $ -- return patch builds
         withPatch "patchId" "Patch not found" $ \((_,_),(patchId,_)) -> 
           do buildList <- runSQL $ DB.selectList [BuildRepositoryPatch ==. patchId] [DB.Desc BuildRepositoryId]
              case buildList of
                [] -> json (FrozoneError "No builds found belonging to this patch!")
                xs -> json $ FrozoneGetBuilds $ map DB.entityVal xs
       userRoute GET [] "/build/patch/:buildId" $ -- return build info
         withBuild "buildId" "Build not found!" $ \((_,_),(_,build)) ->
           json $ FrozoneGetBuild build
       userRoute GET [] "/build/:buildId/logs" $ -- return build logs
         withBuild "buildId" "Build not found!" $ \((_,_),(buildId,_)) ->
            do fullLogs <- runSQL $ DB.selectList [BuildLogRepo ==. buildId] [DB.Desc BuildLogTime, DB.LimitTo 50]
               json $ FrozoneGetBuildLogs $ map DB.entityVal fullLogs
       userRoute GET [] "/build/:buildId/file-changes" $ -- return build file-changes
         withBuild "buildId" "Build not found!" $ \((_,_),(buildId,_)) ->
            do allChanges <- runSQL $ DB.selectList [BundleChangeRepoId ==. buildId] []
               json $ FrozoneGetBuildFileChanges $ map DB.entityVal allChanges
       userRoute GET [] "/build/:buildId/cancel" $ -- command: cancel build
         withBuild "buildId" "Build not found!" $ \((_,_),(buildId,_)) ->
           do runSQL $ updateBuildState buildId BuildCanceled  "Aborted by user"
              -- todo: kill docker build if running
              json $ FrozoneCmdBuildCancel
       userRoute GET [] "/build/:buildId/rebuild" $ -- command: retry build
         withBuild "buildId" "Build not found!" $ \((_,_),(buildId,_)) ->
            do mBuild <- runSQL $ DB.get buildId
               case mBuild of
                 Nothing ->
                   json (FrozoneError "Build not found!")
                 Just build ->
                   if buildRepositoryState build > BuildStarted
                   then do runSQL $ updateBuildState buildId BuildCanceled "Rebuilt scheduled"
                           addWork WorkNow buildId buildQueue
                           json FrozoneCmdBuildRetry
                   else json (FrozoneError "Already building!")
       userRoute GET [] "/collection/:collectionId" $ -- return collection
         withPatchCollection "collectionId" "Collection not found!" $ \((_,_),(_,collection)) ->
           json $ FrozoneGetCollection collection
       userRoute GET [] "/collection/:collectionId/patches" $ -- return collection patches
         withPatchCollection "collectionId" "Collection not found!" $ \((_,_),(collectionId,_)) ->
            do patchList <- runSQL $ DB.selectList [PatchGroup ==. collectionId] [DB.Desc PatchId, DB.LimitTo 50]
               json $ FrozoneGetCollectionPatches $ map DB.entityVal patchList
       userRoute GET [] "/collection/:collectionId/close" $ -- command: collection close
         withPatchCollection "collectionId" "Collection not found!" $ \((_,_),(collectionId,_)) -> 
            do runSQL $ DB.update collectionId [ PatchCollectionOpen =. False ]
               json FrozoneCmdCollectionClose

