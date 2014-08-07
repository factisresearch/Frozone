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
       userRoute GET [] "/list-builds" $ \_ ->
         do allBuilds <- runSQL $ DB.selectList [] [DB.Desc BuildRepositoryId, DB.LimitTo 50]
            json $ FrozoneGetBuilds $ map DB.entityVal allBuilds
       userRoute GET [] "/patch:patchId" $ -- return patch info
         withPatch "patchId" "Patch not found!" $ \((userId,_),(patchId,patch)) -> 
            json $ FrozoneGetPatch patch
       userRoute GET [] "/patch:patchId" $ -- return patch builds
         withPatch "patchId" "Patch not found" $ \((userId,_),(patchId,patch)) -> 
           do buildList <- runSQL $ DB.selectList [BuildRepositoryPatch ==. patchId] [DB.Desc BuildRepositoryId]
              case buildList of
                [] -> json (FrozoneError "No builds found belonging to this patch!")
                xs -> json $ FrozoneGetBuilds $ map DB.entityVal xs
       userRoute GET [] "/patch:buildId" $ -- return build info
         withBuild "buildId" "Build not found!" $ \((userId,_),(buildId,build)) ->
           json $ FrozoneGetBuild build
       userRoute GET [] "/build/:buildId/logs" $ -- return build logs
         withBuild "buildId" "Build not found!" $ \((userId,_),(buildId,build)) ->
            do fullLogs <- runSQL $ DB.selectList [BuildLogRepo ==. buildId] [DB.Desc BuildLogTime, DB.LimitTo 50]
               json $ FrozoneGetBuildLogs $ map DB.entityVal fullLogs
       userRoute GET [] "/build/:buildId/file-changes" $ -- return build file-changes
         withBuild "buildId" "Build not found!" $ \((userId,_),(buildId,build)) ->
            do allChanges <- runSQL $ DB.selectList [BundleChangeRepoId ==. buildId] []
               json $ FrozoneGetBuildFileChanges $ map DB.entityVal allChanges
       userRoute GET [] "/build/:buildId/cancel" $ -- command: cancel build
         withBuild "buildId" "Build not found!" $ \((userId,_),(buildId,build)) ->
           do runSQL $ updateBuildState buildId BuildCanceled  "Aborted by user"
              -- todo: kill docker build if running
              json $ FrozoneCmdBuildCancel
       userRoute GET [] "/build/:buildId/rebuild" $ -- command: retry build
         withBuild "buildId" "Build not found!" $ \((userId,_),(buildId,build)) ->
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
         withPatchCollection "collectionId" "Collection not found!" $ \((userId,_),(collectionId,collection)) ->
           json $ FrozoneGetCollection collection
       userRoute GET [] "/collection/:collectionId/patches" $ -- return collection patches
         withPatchCollection "collectionId" "Collection not found!" $ \((userId,_),(collectionId,collection)) ->
            do patchList <- runSQL $ DB.selectList [PatchGroup ==. collectionId] [DB.Desc PatchId, DB.LimitTo 50]
               json $ FrozoneGetCollectionPatches $ map DB.entityVal patchList
       userRoute GET [] "/collection/:collectionId/close" $ -- command: collection close
         withPatchCollection "collectionId" "Collection not found!" $ \((userId,_),(collectionId,collection)) -> 
            do runSQL $ DB.update collectionId [ PatchCollectionOpen =. False ]
               json FrozoneCmdCollectionClose

withPatch :: T.Text -> T.Text -> (((UserId,User), (PatchId,Patch)) -> FrozoneAction ()) -> ((UserId,User) -> FrozoneAction ())
withPatch name error f (userId,user) = 
    do Just id <- param name
       mVal <- runSQL $ DB.get id
       maybe (json $ FrozoneError error ) (\val -> f ((userId,user),(id,val))) mVal

withBuild :: T.Text -> T.Text -> (((UserId,User), (BuildRepositoryId,BuildRepository)) -> FrozoneAction ()) -> ((UserId,User) -> FrozoneAction ())
withBuild name error f (userId,user) = 
    do Just id <- param name
       mVal <- runSQL $ DB.get id
       maybe (json $ FrozoneError error ) (\val -> f ((userId,user),(id,val))) mVal

withPatchCollection :: T.Text -> T.Text -> (((UserId,User), (PatchCollectionId,PatchCollection)) -> FrozoneAction ()) -> ((UserId,User) -> FrozoneAction ())
withPatchCollection name error f (userId,user) = 
    do Just id <- param name
       mVal <- runSQL $ DB.get id
       maybe (json $ FrozoneError error ) (\val -> f ((userId,user),(id,val))) mVal

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
