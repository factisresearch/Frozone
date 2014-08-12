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
            maybeRestAPIError mNameAndPassword Nothing "/login" ["param","password"] $ \(name,password) ->
            --maybeError LogNote "rest api violation in rest route \"/login\"" "expecting fields name, password" mNameAndPassword $ \(name,password) ->
              do mUserKV <- runSQL $ checkUser name password
                 case mUserKV of
                   Just userKV -> login userKV
                   Nothing ->
                     do firstUser <- runSQL isFirstUser
                        if firstUser
                            then
                              do mNewUserKV <- runSQL $ createUser name password True
                                 maybeErrorInRoute mNewUserKV LogError Nothing "/login" "failed to create user" "failed to create user" $ \userKV ->
                                   login userKV
                            else
                              errorInRoute LogWarn (Just name) "/login" "login failed" "login failed"
                              --restError LogWarn ("login as user \"" ++ T.unpack name ++ "\" failed!") "login failed"
       userRoute GET [] "/logout" $ \(userId, user) ->
         do 
            runSQL $ sessionDelFromDB userId
            markAsGuest
            answerAndLog (Just $ userName user) "logged out" $ FrozoneCmdLogout
-- user management:
       userRoute GET ["admin"] "/users/list-users" $ \(_, user) ->
         do allUsers <- runSQL $ DB.selectList [] [DB.Desc UserId, DB.LimitTo 50]
            answerAndLog (Just $ userName user) "listing users" $ FrozoneGetUsers $ map DB.entityVal allUsers
       userRoute POST ["admin"] "/users/create" $ \(_, user) ->
         do mNewUser <- param "name"
            mNewPassword <- param "password"
            mNewIsAdmin' <- param "isAdmin" :: FrozoneAction (Maybe Int)
            let mNewIsAdmin = mNewIsAdmin' >>= (\p -> return $ if p==0 then False else True)
            let mUserAndPasswordAndIsAdmin = (uncurry3 $ liftM3 (,,)) $ (mNewUser, mNewPassword, mNewIsAdmin) :: Maybe (T.Text, T.Text, Bool)
            maybeRestAPIError mUserAndPasswordAndIsAdmin (Just $ userName user) "/users/create" ["name","password","isAdmin"] $ \(newUser,newPassword,newIsAdmin) ->
            --maybeError LogNote (T.unpack (userName user) ++ ": rest api violation in rest route \"/users/create\"")
              --"expected fields: name, password, isAdmin" mUserAndPasswordAndIsAdmin $ \(newUser,newPassword,newIsAdmin) ->
              do runSQL $ createUser newUser newPassword newIsAdmin
                 answerAndLog (Just $ userName user) ("created user \"" ++ T.unpack newUser ++ "\"") FrozoneCmdCreateUser
------
       userRoute GET ["admin"] "/users/delete" $ \(_, user) ->
         do mUserToDelete <- param "name"
            maybeRestAPIError mUserToDelete (Just $ userName user) "/users/delete" ["name"] $ \(userToDelete) ->
              do runSQL $ deleteUser userToDelete
                 answerAndLog (Just $ userName user) ("deleted user \"" ++ T.unpack userToDelete ++ "\"") FrozoneCmdDeleteUser
       userRoute GET [] "/users/update/password" $ \(userId, user) ->
         do mNewPassword <- param "password"
            maybeRestAPIError mNewPassword (Just $ userName user) "/users/update/password" ["password"] $ \newPassword ->
              do runSQL $ DB.update userId [ UserPassword =. newPassword ]
                 answerAndLog (Just $ userName user) "updated password" $
                   FrozoneCmdUpdatePassword
       userRoute GET [] "/users/update/email" $ \(userId, user) ->
         do mNewEmail <- param "email"
            maybeRestAPIError mNewEmail (Just $ userName user) "/users/update/email" ["email"] $ \newEmail ->
              do runSQL $ DB.update userId [ UserEmail =. newEmail ]
                 answerAndLog (Just $ userName user) "updated email"
                   FrozoneCmdUpdateEmail
       userRoute GET ["admin"] "/users/update/isAdmin" $ \(_, user) ->
         do mUserToChangeName <- param "user"
            mIsAdmin <- param "isAdmin" >>= return . (liftM boolFromInt) :: FrozoneAction (Maybe Bool)
            let mUserAndIsAdmin =
                  (uncurry $ liftM2 (,)) $ (mUserToChangeName,mIsAdmin) :: Maybe (T.Text,Bool)
            maybeRestAPIError mUserAndIsAdmin (Just $ userName user) "/users/update/isAdmin" ["user", "isAdmin"] $ \(userToChangeName,newIsAdmin) ->
              do mUserToChange <- (runSQL $ DB.getBy $ UniqueUserName userToChangeName) >>= return . (liftM tupelFromEntity)
                 maybeErrorInRoute mUserToChange LogNote (Just $ userName user) "/users/update/isAdmin" "user not found" "user not found" $ \(userToChangeId,_) ->
                   do runSQL $ DB.update userToChangeId [ UserIsAdmin =. newIsAdmin ]
                      answerAndLog (Just $ userName user) (T.unpack userToChangeName ++ "is " ++ if newIsAdmin then "now" else "no longer" ++ " admin")
                        FrozoneCmdUpdateIsAdmin
-- patches:
       userRoute GET [] "/patch/:patchId" $ -- return patch info
         withPatch "patchId" "Patch not found!" $ \((_,user),(_,patch)) -> 
            answerAndLog (Just $ userName user) "looking up patch info" $
              FrozoneGetPatch patch
       userRoute GET [] "/build/patch/:patchId" $ -- return patch builds
         withPatch "patchId" "Patch not found" $ \((_,user),(patchId,_)) -> 
           do buildList <- runSQL $ DB.selectList [BuildRepositoryPatch ==. patchId] [DB.Desc BuildRepositoryId]
              case buildList of
                [] -> errorInRoute LogNote (Just $ userName user) "/build/patch/:patchId" "No builds found belonging to this patch!" "no builds found belonging to this patch"
                xs ->
                  answerAndLog (Just $ userName user) "looking up patch builds" $
                    FrozoneGetBuilds $ map DB.entityVal xs
-- general information:
       userRoute GET [] "/build/list-builds" $ \(_, user) ->
         do allBuilds <- runSQL $ DB.selectList [] [DB.Desc BuildRepositoryId, DB.LimitTo 50]
            answerAndLog (Just $ userName user) "listing builds" $
              FrozoneGetBuilds $ map DB.entityVal allBuilds
-- build repositories:
       userRoute GET [] "/build/patch/:buildId" $ -- return build info
         withBuild "buildId" "Build not found!" $ \((_,user),(_,build)) ->
           answerAndLog (Just $ userName user) "looking up build info" $
             FrozoneGetBuild build
       userRoute GET [] "/build/:buildId/logs" $ -- return build logs
         withBuild "buildId" "Build not found!" $ \((_,user),(buildId,_)) ->
            do fullLogs <- runSQL $ DB.selectList [BuildLogRepo ==. buildId] [DB.Desc BuildLogTime, DB.LimitTo 50]
               answerAndLog (Just $ userName user) "looking up build info" $
                 FrozoneGetBuildLogs $ map DB.entityVal fullLogs
       userRoute GET [] "/build/:buildId/file-changes" $ -- return build file-changes
         withBuild "buildId" "Build not found!" $ \((_,user),(buildId,_)) ->
            do allChanges <- runSQL $ DB.selectList [BundleChangeRepoId ==. buildId] []
               answerAndLog (Just $ userName user) "looking up build file changes" $
                 FrozoneGetBuildFileChanges $ map DB.entityVal allChanges
       userRoute GET [] "/build/:buildId/cancel" $ -- command: cancel build
         withBuild "buildId" "Build not found!" $ \((_,user),(buildId,_)) ->
           do runSQL $ updateBuildState buildId BuildCanceled  "Aborted by user"
              -- todo: kill docker build if running
              answerAndLog (Just $ userName user) "canceling build" $
                FrozoneCmdBuildCancel
       userRoute GET [] "/build/:buildId/rebuild" $ -- command: retry build
         withBuild "buildId" "Build not found!" $ \((_,user),(buildId,_)) ->
            do mBuild <- runSQL $ DB.get buildId
               case mBuild of
                 Nothing ->
                   errorInRoute LogNote (Just $ userName user) "/build/:buildId/rebuild" "Build not found" "Build not found"
                 Just build ->
                   if buildRepositoryState build > BuildStarted
                   then do runSQL $ updateBuildState buildId BuildCanceled "Rebuilt scheduled"
                           addWork WorkNow buildId buildQueue
                           answerAndLog (Just $ userName user) "retrying build" $
                             FrozoneCmdBuildRetry
                   else errorInRoute LogNote (Just $ userName user) "/build/:buildId/rebuild" "Already building" "Already building"
-- patch collection
       userRoute GET [] "/collection/:collectionId" $ -- return collection
         withPatchCollection "collectionId" "Collection not found!" $ \((_,user),(_,collection)) ->
           answerAndLog (Just $ userName user) "looking up collection" $
             FrozoneGetCollection collection
       userRoute GET [] "/collection/:collectionId/patches" $ -- return collection patches
         withPatchCollection "collectionId" "Collection not found!" $ \((_,user),(collectionId,_)) ->
            do patchList <- runSQL $ DB.selectList [PatchGroup ==. collectionId] [DB.Desc PatchId, DB.LimitTo 50]
               answerAndLog (Just $ userName user) "looking up collection patches" $
                 FrozoneGetCollectionPatches $ map DB.entityVal patchList
       userRoute GET [] "/collection/:collectionId/close" $ -- command: collection close
         withPatchCollection "collectionId" "Collection not found!" $ \((_,user),(collectionId,_)) -> 
            do runSQL $ DB.update collectionId [ PatchCollectionOpen =. False ]
               answerAndLog (Just $ userName user) "closing collection" $
                 FrozoneCmdCollectionClose


boolFromInt :: Int -> Bool
boolFromInt i = if i==0 then False else True

tupelFromEntity entity = (DB.entityKey entity, DB.entityVal entity)

login :: (UserId, User) -> FrozoneAction ()
login (userId,user) =
    do sessionId <- runSQL $ sessionIntoDB userId
       markAsLoggedIn sessionId
       answerAndLog (Just $ userName user) "logged in" $
         FrozoneCmdLogin
