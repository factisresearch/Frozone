{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.RestApi where

import Frozone.User
import Frozone.Types
--import Frozone.Util.Rest

import Frozone.Util.Db
import Frozone.Util.Logging
import Frozone.Util.Rest

import Web.Spock hiding (patch)
import Web.Spock.Worker
import Web.Spock.Auth hiding (userRoute)
--import qualified Web.Spock.Auth as Spock
import qualified Data.Text as T

import qualified Database.Persist as DB
--import qualified Database.Persist.Sql as DB
import Database.Persist ((==.), (=.))

import Control.Monad




restApi :: WorkQueue BuildRepositoryId -> FrozoneApp ()
restApi buildQueue =
    do post "/login" $
         do mName <- param "name"
            mPassword <- param "password" :: FrozoneAction (Maybe T.Text)
            let mNameAndPassword = (uncurry $ liftM2 (,)) (mName,mPassword) :: Maybe (T.Text,T.Text)
            maybeError LogNote "expecting fields name, password" "expecting fields name, password" mNameAndPassword $ \(name,password) ->
              do mUserKV <- runSQL $ checkUser name password
                 case mUserKV of
                   Just userKV -> login userKV
                   Nothing ->
                     do firstUser <- runSQL isFirstUser
                        if firstUser
                            then
                              do mNewUserKV <- runSQL $ createUser name password True
                                 maybeError LogError "failed to create user" "failed to create user" mNewUserKV $ \userKV ->
                                   login userKV
                            else
                              restError LogNote "login failed" "login failed"
       userRoute GET [] "/logout" $ \(userId, user) ->
         do 
            runSQL $ sessionDelFromDB userId
            markAsGuest
            answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": logged out") $ FrozoneCmdLogout
-- user management:
       userRoute GET ["admin"] "/users/list-users" $ \(_, user) ->
         do allUsers <- runSQL $ DB.selectList [] [DB.Desc UserId, DB.LimitTo 50]
            answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": listing users") $ FrozoneGetUsers $ map DB.entityVal allUsers
       userRoute POST ["admin"] "/users/create" $ \(_, user) ->
         do mNewUser <- param "name"
            mNewPassword <- param "password"
            mNewIsAdmin' <- param "isAdmin" :: FrozoneAction (Maybe Int)
            let mNewIsAdmin = mNewIsAdmin' >>= (\p -> return $ if p==0 then False else True)
            let mUserAndPasswordAndIsAdmin = (uncurry3 $ liftM3 (,,)) $ (mNewUser, mNewPassword, mNewIsAdmin) :: Maybe (T.Text, T.Text, Bool)
            maybeError LogNote "expected fields: name, password, isAdmin"
              "expected fields: name, password, isAdmin" mUserAndPasswordAndIsAdmin $ \(newUser,newPassword,newIsAdmin) ->
              do runSQL $ createUser newUser newPassword newIsAdmin
                 answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": created user \"" ++ T.unpack newUser ++ "\"") FrozoneCmdCreateUser
       userRoute GET ["admin"] "/users/delete" $ \(_, user) ->
         do mUserToDelete <- param "name"
            maybeError LogNote "expected fields: name"
              "expected fields: name" mUserToDelete $ \(userToDelete) ->
              do runSQL $ deleteUser userToDelete
                 answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": deleted user \"" ++ T.unpack userToDelete ++ "\"") FrozoneCmdDeleteUser
            --restError LogNote "not yet implemented!" "not yet implemented!"
       userRoute GET [] "/users/update/password" $ \(userId, user) ->
         do mNewPassword <- param "password"
            maybeError LogNote "expected fields: password" "expected fields: password" mNewPassword $ \newPassword ->
              do runSQL $ DB.update userId [ UserPassword =. newPassword ]
                 answerAndLog (T.unpack (userName user) ++ ": updated password")
                   FrozoneCmdUpdatePassword
       userRoute GET [] "/users/update/email" $ \(userId, user) ->
         do mNewEmail <- param "email"
            maybeError LogNote "expected fields: email" "expected fields: email" mNewEmail $ \newEmail ->
              do runSQL $ DB.update userId [ UserEmail =. newEmail ]
                 answerAndLog (T.unpack (userName user) ++ ": updated email")
                   FrozoneCmdUpdateEmail
       userRoute GET ["admin"] "/users/update/isAdmin" $ \(_, user) ->
         do mUserToChangeName <- param "user"
            mIsAdmin <- param "isAdmin" >>= return . (liftM boolFromInt) :: FrozoneAction (Maybe Bool)
            let mUserAndIsAdmin =
                  (uncurry $ liftM2 (,)) $ (mUserToChangeName,mIsAdmin) :: Maybe (T.Text,Bool)
            maybeError LogNote "expected fields: user, isAdmin" "expected fields: user, isAdmin" mUserAndIsAdmin $ \(userToChangeName,newIsAdmin) ->
              do mUserToChange <- (runSQL $ DB.getBy $ UniqueUserName userToChangeName) >>= return . (liftM tupelFromEntity)
                 maybeError LogNote "user not found" "user not found" mUserToChange $ \(userToChangeId,_) ->
                   do runSQL $ DB.update userToChangeId [ UserIsAdmin =. newIsAdmin ]
                      answerAndLog (T.unpack (userName user) ++ ": " ++ T.unpack userToChangeName ++ "is " ++ if newIsAdmin then "now" else "no longer" ++ " admin")
                        FrozoneCmdUpdateIsAdmin
-- patches:
       userRoute GET [] "/patch/:patchId" $ -- return patch info
         withPatch "patchId" "Patch not found!" $ \((_,user),(_,patch)) -> 
            answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": looking up patch info") $ FrozoneGetPatch patch
       userRoute GET [] "/build/patch/:patchId" $ -- return patch builds
         withPatch "patchId" "Patch not found" $ \((_,user),(patchId,_)) -> 
           do buildList <- runSQL $ DB.selectList [BuildRepositoryPatch ==. patchId] [DB.Desc BuildRepositoryId]
              case buildList of
                [] -> restError LogNote "No builds found belonging to this patch!" "no builds found belonging to this patch"
                xs -> answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": looking up patch builds") $ FrozoneGetBuilds $ map DB.entityVal xs
-- general information:
       userRoute GET [] "/build/list-builds" $ \(_, user) ->
         do allBuilds <- runSQL $ DB.selectList [] [DB.Desc BuildRepositoryId, DB.LimitTo 50]
            answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": listing builds") $ FrozoneGetBuilds $ map DB.entityVal allBuilds
-- build repositories:
       userRoute GET [] "/build/patch/:buildId" $ -- return build info
         withBuild "buildId" "Build not found!" $ \((_,user),(_,build)) ->
           answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": looking up build info") $ FrozoneGetBuild build
       userRoute GET [] "/build/:buildId/logs" $ -- return build logs
         withBuild "buildId" "Build not found!" $ \((_,user),(buildId,_)) ->
            do fullLogs <- runSQL $ DB.selectList [BuildLogRepo ==. buildId] [DB.Desc BuildLogTime, DB.LimitTo 50]
               answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": looking up build info") $ FrozoneGetBuildLogs $ map DB.entityVal fullLogs
       userRoute GET [] "/build/:buildId/file-changes" $ -- return build file-changes
         withBuild "buildId" "Build not found!" $ \((_,user),(buildId,_)) ->
            do allChanges <- runSQL $ DB.selectList [BundleChangeRepoId ==. buildId] []
               answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": looking up build file changes") $ FrozoneGetBuildFileChanges $ map DB.entityVal allChanges
       userRoute GET [] "/build/:buildId/cancel" $ -- command: cancel build
         withBuild "buildId" "Build not found!" $ \((_,user),(buildId,_)) ->
           do runSQL $ updateBuildState buildId BuildCanceled  "Aborted by user"
              -- todo: kill docker build if running
              answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": canceling build") $ FrozoneCmdBuildCancel
       userRoute GET [] "/build/:buildId/rebuild" $ -- command: retry build
         withBuild "buildId" "Build not found!" $ \((_,user),(buildId,_)) ->
            do mBuild <- runSQL $ DB.get buildId
               case mBuild of
                 Nothing ->
                   restError LogNote "Build not found!" "Build not found"
                 Just build ->
                   if buildRepositoryState build > BuildStarted
                   then do runSQL $ updateBuildState buildId BuildCanceled "Rebuilt scheduled"
                           addWork WorkNow buildId buildQueue
                           answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": retrying build") $ FrozoneCmdBuildRetry
                   else restError LogNote "Already building!" "Already building"
-- patch collection
       userRoute GET [] "/collection/:collectionId" $ -- return collection
         withPatchCollection "collectionId" "Collection not found!" $ \((_,user),(_,collection)) ->
           answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": looking up collection") $ FrozoneGetCollection collection
       userRoute GET [] "/collection/:collectionId/patches" $ -- return collection patches
         withPatchCollection "collectionId" "Collection not found!" $ \((_,user),(collectionId,_)) ->
            do patchList <- runSQL $ DB.selectList [PatchGroup ==. collectionId] [DB.Desc PatchId, DB.LimitTo 50]
               answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": looking up collection patches") $ FrozoneGetCollectionPatches $ map DB.entityVal patchList
       userRoute GET [] "/collection/:collectionId/close" $ -- command: collection close
         withPatchCollection "collectionId" "Collection not found!" $ \((_,user),(collectionId,_)) -> 
            do runSQL $ DB.update collectionId [ PatchCollectionOpen =. False ]
               answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": closing collection") $ FrozoneCmdCollectionClose


boolFromInt :: Int -> Bool
boolFromInt i = if i==0 then False else True

tupelFromEntity entity = (DB.entityKey entity, DB.entityVal entity)

login :: (UserId, User) -> FrozoneAction ()
login (userId,user) =
    do sessionId <- runSQL $ sessionIntoDB userId
       markAsLoggedIn sessionId
       answerAndLog ("\"" ++ T.unpack (userName user) ++ "\": logged in")
         FrozoneCmdLogin
