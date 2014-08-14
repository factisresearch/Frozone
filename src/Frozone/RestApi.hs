{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.RestApi where

import Frozone.User.API
import Frozone.User.DB
import Frozone.Project.API
import Frozone.Types
--import Frozone.Util.Rest

import Frozone.Util.Db
import Frozone.Util.Logging
import Frozone.Util.Rest

import Web.Spock hiding (patch, subcomponent)
import Web.Spock.Worker
import Web.Spock.Auth hiding (userRoute)
--import qualified Web.Spock.Auth as Spock
import qualified Data.Text as T

import qualified Database.Persist as DB
--import qualified Database.Persist.Sql as DB
import Database.Persist ((==.), (=.))

import Control.Monad


restApi :: String -> WorkQueue BuildRepositoryId -> FrozoneApp ()
restApi currentRoute buildQueue = 
    do post "/login" $
         let route = currentRoute ++ "/login" in
         do mName <- param "name"
            mPassword <- param "password" :: FrozoneAction (Maybe T.Text)
            let mNameAndPassword = (uncurry $ liftM2 (,)) (mName,mPassword) :: Maybe (T.Text,T.Text)
            maybeRestAPIError mNameAndPassword Nothing route ["param","password"] $ \(name,password) ->
              do mUserKV <- runSQL $ checkUser name password
                 case mUserKV of
                   Just userKV -> login userKV
                   Nothing ->
                     do firstUser <- runSQL isFirstUser
                        if firstUser
                           then
                              do mNewUserKV <- runSQL $ createUser name password True
                                 maybeErrorInRoute mNewUserKV LogError Nothing route "failed to create user" "failed to create user" $ \userKV ->
                                   login userKV
                            else
                              errorInRoute LogWarn (Just name) route "login failed" "login failed"
       userRoute GET [] currentRoute "/logout" $ \_ (userId, user) ->
         do 
            runSQL $ sessionDelFromDB userId
            markAsGuest
            answerAndLog (Just $ userName user) "logged out" $ FrozoneCmdLogout

-- user management:
       subcomponent currentRoute "/user" $ userAPI

-- project management:
       subcomponent currentRoute "/project" $ projectApi 

-- patches:
       userRoute GET [] currentRoute "/patch/:patchId" $ \route -> -- return patch info
         --withProjectFromShortNameSafe "projShortName" route $ \(_, proj) ->
           withPatchSafe "patchId" route $ \(_,patch) (_,user) -> 
            answerAndLog (Just $ userName user) "looking up patch info" $
              FrozoneGetPatch patch

       userRoute GET [] currentRoute "/build/patch/:patchId" $ \route -> -- return patch builds
         withPatchSafe "patchId" route $ \(patchId,_) (_,user) -> 
           do buildList <- runSQL $ DB.selectList [BuildRepositoryPatch ==. patchId] [DB.Desc BuildRepositoryId]
              case buildList of
                [] -> errorInRoute LogNote (Just $ userName user) route "No builds found belonging to this patch!" "no builds found belonging to this patch"
                xs ->
                  answerAndLog (Just $ userName user) "looking up patch builds" $
                    FrozoneGetBuilds $ map DB.entityVal xs

-- general information:
       userRoute GET [] currentRoute "/build/list-builds" $ \_ (_, user) ->
         do allBuilds <- runSQL $ DB.selectList [] [DB.Desc BuildRepositoryId, DB.LimitTo 50]
            answerAndLog (Just $ userName user) "listing builds" $
              FrozoneGetBuilds $ map DB.entityVal allBuilds

-- build repositories:
       userRoute GET [] currentRoute "/build/patch/:buildId" $ \route -> -- return build info
         withBuildRepoSafe "buildId" route $ \(_,build) (_,user) ->
           answerAndLog (Just $ userName user) "looking up build info" $
             FrozoneGetBuild build

       userRoute GET [] currentRoute "/build/:buildId/logs" $ \route -> -- return build logs
         withBuildRepoSafe "buildId" route $ \(buildId,_) (_,user) ->
            do fullLogs <- runSQL $ DB.selectList [BuildLogRepo ==. buildId] [DB.Desc BuildLogTime, DB.LimitTo 50]
               answerAndLog (Just $ userName user) "looking up build info" $
                 FrozoneGetBuildLogs $ map DB.entityVal fullLogs

       userRoute GET [] currentRoute "/build/:buildId/file-changes" $ \route -> -- return build file-changes
         withBuildRepoSafe "buildId" route $ \(buildId,_) (_,user) ->
            do allChanges <- runSQL $ DB.selectList [BundleChangeRepoId ==. buildId] []
               answerAndLog (Just $ userName user) "looking up build file changes" $
                 FrozoneGetBuildFileChanges $ map DB.entityVal allChanges

       userRoute GET [] currentRoute "/build/:buildId/cancel" $ \route -> -- command: cancel build
         withBuildRepoSafe "buildId" route $ \(buildId,_) (_,user) ->
           do runSQL $ updateBuildState buildId BuildCanceled  "Aborted by user"
              -- todo: kill docker build if running
              answerAndLog (Just $ userName user) "canceling build" $
                FrozoneCmdBuildCancel

       userRoute GET [] currentRoute "/build/:buildId/rebuild" $ \route -> -- command: retry build
         withBuildRepoSafe "buildId" route $ \(buildId,_) (_,user) ->
            do mBuild <- runSQL $ DB.get buildId
               case mBuild of
                 Nothing ->
                   errorInRoute LogNote (Just $ userName user) route "Build not found" "Build not found"
                 Just build ->
                   if buildRepositoryState build > BuildStarted
                   then do runSQL $ updateBuildState buildId BuildCanceled "Rebuilt scheduled"
                           addWork WorkNow buildId buildQueue
                           answerAndLog (Just $ userName user) "retrying build" $
                             FrozoneCmdBuildRetry
                   else errorInRoute LogNote (Just $ userName user) route "Already building" "Already building"

-- patch collection
       userRoute GET [] currentRoute "/collection/:collectionId" $ \route -> -- return collection
         withPatchCollectionSafe "collectionId" route $ \(_,collection) (_,user) ->
           answerAndLog (Just $ userName user) "looking up collection" $
             FrozoneGetCollection collection

       userRoute GET [] currentRoute "/collection/:collectionId/patches" $ \route -> -- return collection patches
         withPatchCollectionSafe "collectionId" route $ \(collectionId,_) (_,user) ->
            do patchList <- runSQL $ DB.selectList [PatchGroup ==. collectionId] [DB.Desc PatchId, DB.LimitTo 50]
               answerAndLog (Just $ userName user) "looking up collection patches" $
                 FrozoneGetCollectionPatches $ map DB.entityVal patchList

       userRoute GET [] currentRoute "/collection/:collectionId/close" $ \route -> -- command: collection close
         withPatchCollectionSafe "collectionId" route $ \(collectionId,_) (_,user) -> 
            do runSQL $ DB.update collectionId [ PatchCollectionOpen =. False ]
               answerAndLog (Just $ userName user) "closing collection" $
                 FrozoneCmdCollectionClose


safeUserInfoFromUser :: User -> UserInfo
safeUserInfoFromUser user = UserInfo
    { sui_name = userName user
    , sui_email = userEmail user
    , sui_isAdmin = userIsAdmin user
    }

login :: (UserId, User) -> FrozoneAction ()
login (userId,user) =
    do sessionId <- runSQL $ sessionIntoDB userId
       markAsLoggedIn sessionId
       answerAndLog (Just $ userName user) "logged in" $
         FrozoneCmdLogin
