{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.RestApi where

import Frozone.User
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
       userRoute GET ["admin"] currentRoute "/users/list-users" $ \_ (_, user) ->
         do allUsers <- runSQL $ DB.selectList [] [DB.Desc UserId, DB.LimitTo 50]
            answerAndLog (Just $ userName user) "listing users" $
              FrozoneGetUsers $ map ( safeUserInfoFromUser . DB.entityVal) allUsers
       userRoute POST ["admin"] currentRoute "/users/create" $ \route (_, user) ->
         do mNewUser <- param "name"
            mNewPassword <- param "password"
            mNewIsAdmin' <- param "isAdmin" :: FrozoneAction (Maybe Int)
            let mNewIsAdmin = mNewIsAdmin' >>= (\p -> return $ if p==0 then False else True)
            let mUserAndPasswordAndIsAdmin = (uncurry3 $ liftM3 (,,)) $ (mNewUser, mNewPassword, mNewIsAdmin) :: Maybe (T.Text, T.Text, Bool)
            maybeRestAPIError mUserAndPasswordAndIsAdmin (Just $ userName user) route ["name","password","isAdmin"] $ \(newUser,newPassword,newIsAdmin) ->
              do runSQL $ createUser newUser newPassword newIsAdmin
                 answerAndLog (Just $ userName user) ("created user \"" ++ T.unpack newUser ++ "\"") FrozoneCmdCreateUser
       userRoute GET ["admin"] currentRoute "/users/delete" $ \route (_, user) ->
         do mUserToDelete <- param "name"
            maybeRestAPIError mUserToDelete (Just $ userName user) route ["name"] $ \(userToDelete) ->
              do runSQL $ deleteUser userToDelete
                 answerAndLog (Just $ userName user) ("deleted user \"" ++ T.unpack userToDelete ++ "\"") FrozoneCmdDeleteUser
       userRoute GET [] currentRoute "/users/update/password" $ \_ (userId, user) ->
         do mNewPassword <- param "password"
            maybeRestAPIError mNewPassword (Just $ userName user) "/users/update/password" ["password"] $ \newPassword ->
              do runSQL $ DB.update userId [ UserPassword =. newPassword ]
                 answerAndLog (Just $ userName user) "updated password" $
                   FrozoneCmdUpdatePassword
       userRoute GET [] currentRoute "/users/update/email" $ \route (userId, user) ->
         do mNewEmail <- param "email"
            maybeRestAPIError mNewEmail (Just $ userName user) route ["email"] $ \newEmail ->
              do runSQL $ DB.update userId [ UserEmail =. newEmail ]
                 answerAndLog (Just $ userName user) "updated email"
                   FrozoneCmdUpdateEmail
       userRoute GET ["admin"] currentRoute "/users/update/isAdmin" $ \route (_, user) ->
         do mUserToChangeName <- param "user"
            mIsAdmin <- param "isAdmin" >>= return . (liftM boolFromInt) :: FrozoneAction (Maybe Bool)
            let mUserAndIsAdmin =
                  (uncurry $ liftM2 (,)) $ (mUserToChangeName,mIsAdmin) :: Maybe (T.Text,Bool)
            maybeRestAPIError mUserAndIsAdmin (Just $ userName user) route ["user", "isAdmin"] $ \(userToChangeName,newIsAdmin) ->
              do mUserToChange <- (runSQL $ DB.getBy $ UniqueUserName userToChangeName) >>= return . (liftM tupelFromEntity)
                 maybeErrorInRoute mUserToChange LogNote (Just $ userName user) route "user not found" "user not found" $ \(userToChangeId,_) ->
                   do runSQL $ DB.update userToChangeId [ UserIsAdmin =. newIsAdmin ]
                      answerAndLog (Just $ userName user) (T.unpack userToChangeName ++ "is " ++ if newIsAdmin then "now" else "no longer" ++ " admin")
                        FrozoneCmdUpdateIsAdmin
       subcomponent currentRoute "/project" $ projectApi 
-- patches:
       userRoute GET [] currentRoute "/patch/:patchId" $ \route -> -- return patch info
         withPatch "patchId" route $ \(_,user) (_,patch) -> 
            answerAndLog (Just $ userName user) "looking up patch info" $
              FrozoneGetPatch patch
       userRoute GET [] currentRoute "/build/patch/:patchId" $ \route -> -- return patch builds
         withPatch "patchId" route $ \(_,user) (patchId,_) -> 
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
         withBuild "buildId" route $ \(_,user) (_,build) ->
           answerAndLog (Just $ userName user) "looking up build info" $
             FrozoneGetBuild build
       userRoute GET [] currentRoute "/build/:buildId/logs" $ \route -> -- return build logs
         withBuild "buildId" route $ \(_,user) (buildId,_) ->
            do fullLogs <- runSQL $ DB.selectList [BuildLogRepo ==. buildId] [DB.Desc BuildLogTime, DB.LimitTo 50]
               answerAndLog (Just $ userName user) "looking up build info" $
                 FrozoneGetBuildLogs $ map DB.entityVal fullLogs
       userRoute GET [] currentRoute "/build/:buildId/file-changes" $ \route -> -- return build file-changes
         withBuild "buildId" route $ \(_,user) (buildId,_) ->
            do allChanges <- runSQL $ DB.selectList [BundleChangeRepoId ==. buildId] []
               answerAndLog (Just $ userName user) "looking up build file changes" $
                 FrozoneGetBuildFileChanges $ map DB.entityVal allChanges
       userRoute GET [] currentRoute "/build/:buildId/cancel" $ \route -> -- command: cancel build
         withBuild "buildId" route $ \(_,user) (buildId,_) ->
           do runSQL $ updateBuildState buildId BuildCanceled  "Aborted by user"
              -- todo: kill docker build if running
              answerAndLog (Just $ userName user) "canceling build" $
                FrozoneCmdBuildCancel
       userRoute GET [] currentRoute "/build/:buildId/rebuild" $ \route -> -- command: retry build
         withBuild "buildId" route $ \(_,user) (buildId,_) ->
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
         withPatchCollection "collectionId" route $ \(_,user) (_,collection) ->
           answerAndLog (Just $ userName user) "looking up collection" $
             FrozoneGetCollection collection
       userRoute GET [] currentRoute "/collection/:collectionId/patches" $ \route -> -- return collection patches
         withPatchCollection "collectionId" route $ \(_,user) (collectionId,_) ->
            do patchList <- runSQL $ DB.selectList [PatchGroup ==. collectionId] [DB.Desc PatchId, DB.LimitTo 50]
               answerAndLog (Just $ userName user) "looking up collection patches" $
                 FrozoneGetCollectionPatches $ map DB.entityVal patchList
       userRoute GET [] currentRoute "/collection/:collectionId/close" $ \route -> -- command: collection close
         withPatchCollection "collectionId" route $ \(_,user) (collectionId,_) -> 
            do runSQL $ DB.update collectionId [ PatchCollectionOpen =. False ]
               answerAndLog (Just $ userName user) "closing collection" $
                 FrozoneCmdCollectionClose

projectApi currentRoute = 
    do userRoute GET ["admin"] currentRoute "/list-projects" $ \route ->
         withProjectFromShortName "projShortName" route $ \(userId,user) (projectId,project) ->
           return ()
       userRoute GET ["admin"] currentRoute "/create" $ \route (userId,user) ->
         do return ()
       userRoute GET ["admin"] currentRoute "/delete" $ \route (userId,user) ->
         do return ()
       subcomponent currentRoute "/update" $ \currentRoute -> 
         do userRoute GET ["admin"] currentRoute "/name" $ \route (userId,user) ->
              do return ()
            userRoute GET ["admin"] currentRoute "/shortName" $ \route (userId,user) ->
              do return ()
            userRoute GET ["admin"] currentRoute "/users" $ \route (userId,user) ->
              do return ()

safeUserInfoFromUser :: User -> SafeUserInfo
safeUserInfoFromUser user = SafeUserInfo
    { suiName = userName user
    , suiEmail = userEmail user
    , suiIsAdmin = userIsAdmin user
    }

boolFromInt :: Int -> Bool
boolFromInt i = if i==0 then False else True

tupelFromEntity entity = (DB.entityKey entity, DB.entityVal entity)

login :: (UserId, User) -> FrozoneAction ()
login (userId,user) =
    do sessionId <- runSQL $ sessionIntoDB userId
       markAsLoggedIn sessionId
       answerAndLog (Just $ userName user) "logged in" $
         FrozoneCmdLogin
