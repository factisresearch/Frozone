{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.RestApi where

import Frozone.User
import Frozone.Types

import Frozone.Util.Db
import Frozone.Util.Logging

import Web.Spock hiding (patch)
import Web.Spock.Worker
import Web.Spock.Auth hiding (userRoute)
import qualified Web.Spock.Auth as Spock
import qualified Data.Text as T

import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Database.Persist ((==.), (=.))


restApi :: WorkQueue BuildRepositoryId -> FrozoneApp ()
restApi buildQueue =
    do post "/login" $
         do Just name <- param "name"
            Just password <- param "password" :: FrozoneAction (Maybe T.Text)
            mUserId <- runSQL $ checkUser name password
            case mUserId of
              Just userId ->
                do sessionId <- runSQL $ sessionIntoDB userId
                   markAsLoggedIn sessionId
                   json $ FrozoneCmdLogin
              Nothing -> doLog LogNote "login failed"
       userRoute GET [] "/logout" $ \(userId, _) ->
         do 
            runSQL $ sessionDelFromDB userId
            markAsGuest
            json $ FrozoneCmdLogout
       --userRoute GET [] "/list-builds" $ \(userId, user) ->
       get "/list-builds" $
         do allBuilds <- runSQL $ DB.selectList [] [DB.Desc BuildRepositoryId, DB.LimitTo 50]
            json $ FrozoneGetBuilds $ map DB.entityVal allBuilds
       get "/patch/:patchId" $ -- return patch info
         do Just (patchId :: PatchId) <- param "patchId"
            mPatch <- runSQL $ DB.get patchId
            case mPatch of
              Nothing ->
                  json (FrozoneError "Patch not found!")
              Just patchVal ->
                  json $ FrozoneGetPatch patchVal
       get "/build/patch/:patchId" $ -- return patch builds
           do Just (patchId :: PatchId) <- param "patchId"
              buildList <- runSQL $ DB.selectList [BuildRepositoryPatch ==. patchId] [DB.Desc BuildRepositoryId]
              case buildList of
                [] -> json (FrozoneError "No builds found belonging to this patch!")
                xs -> json $ FrozoneGetBuilds $ map DB.entityVal xs
       get "/build/:buildId" $ -- return build info
         do Just (buildId :: BuildRepositoryId) <- param "buildId"
            mBuild <- runSQL $ DB.get buildId
            case mBuild of
              Nothing ->
                  json (FrozoneError "Build not found!")
              Just build ->
                  json $ FrozoneGetBuild build
       get "/build/:buildId/logs" $ -- return build logs
         do Just buildId <- param "buildId"
            fullLogs <- runSQL $ DB.selectList [BuildLogRepo ==. buildId] [DB.Desc BuildLogTime, DB.LimitTo 50]
            json $ FrozoneGetBuildLogs $ map DB.entityVal fullLogs
       get "/build/:buildId/file-changes" $ -- return build file-changes
         do Just (buildId :: BuildRepositoryId) <- param "buildId"
            allChanges <- runSQL $ DB.selectList [BundleChangeRepoId ==. buildId] []
            json $ FrozoneGetBuildFileChanges $ map DB.entityVal allChanges
       get "/build/:buildId/cancel" $ -- command: cancel build
         do Just (buildId :: BuildRepositoryId) <- param "buildId"
            runSQL $ updateBuildState buildId BuildCanceled  "Aborted by user"
            -- todo: kill docker build if running
            json $ FrozoneCmdBuildCancel
       get "/build/:buildId/rebuild" $ -- command: retry build
         do Just (buildId :: BuildRepositoryId) <- param "buildId"
            mBuild <- runSQL $ DB.get buildId
            case mBuild of
              Nothing ->
                  json (FrozoneError "Build not found!")
              Just build ->
                  if buildRepositoryState build > BuildStarted
                  then do runSQL $ updateBuildState buildId BuildCanceled "Rebuilt scheduled"
                          addWork WorkNow buildId buildQueue
                          json FrozoneCmdBuildRetry
                  else json (FrozoneError "Already building!")
       get "/collection/:collectionId" $ -- return collection
         do Just (collectionId :: PatchCollectionId) <- param "collectionId"
            mCollection <- runSQL $ DB.get collectionId
            case mCollection of
              Nothing ->
                  json (FrozoneError "Collection not found!")
              Just collection ->
                  json $ FrozoneGetCollection collection
       get "/collection/:collectionId/patches" $ -- return collection patches
         do Just (collectionId :: PatchCollectionId) <- param "collectionId"
            patchList <- runSQL $ DB.selectList [PatchGroup ==. collectionId] [DB.Desc PatchId, DB.LimitTo 50]
            json $ FrozoneGetCollectionPatches $ map DB.entityVal patchList
       get "/collection/:collectionId/close" $ -- command: collection close
         do Just (collectionId :: PatchCollectionId) <- param "collectionId"
            runSQL $ DB.update collectionId [ PatchCollectionOpen =. False ]
            json FrozoneCmdCollectionClose

{-
userInProject :: ((UserId,User) -> FrozoneAction a) -> ((UserId,User) -> FrozoneAction a)
userInProject f (userId,user) = 
    do guard
-}

userRoute
    :: StdMethod -> [UserRights] -> T.Text
    -> ((UserId, User) -> FrozoneAction ()) -> SpockM DB.Connection FrozoneSession FrozoneState ()
userRoute = Spock.userRoute noAccesHandler (runSQL . userFromSession) checkRights
    where
      noAccesHandler reason = 
        do return () -- <- to do: set state
           json $ case reason of
             NotEnoughRights -> FrozoneError "not enough rights"
             NotLoggedIn -> FrozoneError "not logged in"
             NotValidUser -> FrozoneError "not valid user"

      checkRights (_, user) necessaryRights =
        if "admin" `elem` necessaryRights
        then return $ userIsAdmin user
        else return $ True
