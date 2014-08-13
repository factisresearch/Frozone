{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.RestApi where

import Frozone.User
import Frozone.Project
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
import Data.List
import qualified Data.Traversable as T




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
    do userRoute GET ["admin"] currentRoute "/list-projects" $ \route (_,user) ->
         do mAllProjects <- (runSQL projectList) >>= (mapM $ (projectInfoFromProject . snd)) >>= return . T.sequenceA 
            maybeErrorInRoute mAllProjects LogError (Just $ userName user) route "failed looking up users for project" "failed looking up users for project" $ \allProjects -> 
              answerAndLog (Just $ userName user) "listing projects" $
                FrozoneGetProjects $ allProjects
       userRoute GET ["admin"] currentRoute "/create" $ \route (_,user) ->
         do (mName, mShortName, mRepoLoc, mSshKey) <- (uncurry4 $ liftM4 (,,,)) $
              (param "projName", param "projShortName", param "repoLoc", param "sshKey")
            let mProjParams = (uncurry4 $ liftM4 (,,,)) $ (mName, mShortName, mRepoLoc, mSshKey) :: Maybe (T.Text,T.Text,T.Text,T.Text)
            maybeRestAPIError mProjParams (Just $ userName user) route ["projName","projShortName","repoLoc","sshKey"] $ \projParams ->
              do mProjectKV <- (runSQL . uncurry4 createProject) $ projParams :: FrozoneAction (Maybe (ProjectId, Project))
                 maybeErrorInRoute mProjectKV LogNote (Just $ userName user) route
                   "failed to create project" "failed to create project" $ \(_, proj) ->
                     answerAndLog (Just $ userName user)
                       ("creating project \"" ++ T.unpack (projectName proj) ++ "\", short name: \"" ++ T.unpack (projectShortName proj) ++ "\"") $
                       FrozoneCmdCreateProject
       userRoute GET ["admin"] currentRoute "/delete" $ \route ->
         withProjectFromShortName "projShortName" route $ \(_,user) (projId, proj) ->
           do runSQL $ deleteProject projId
              answerAndLog (Just $ userName user) ("deleted project \"" ++ (T.unpack $ projectName proj) ++ "\"") $
                FrozoneCmdDeleteProject
       subcomponent currentRoute "/update" $ \currentRoute -> 
         do userRoute GET ["admin"] currentRoute "/name" $ \route ->
              withProjectFromShortName "projShortName" route $ \(_,user) (projId, _) ->
                do mName <- (param "name")
                   maybeRestAPIError mName (Just $ userName user) route ["name"] $ \name ->
                     do runSQL $ DB.update projId [ ProjectName =. name ]
                        answerAndLog (Just $ userName user) "updating project name" $
                          FrozoneCmdUpdateProjectName
            userRoute GET ["admin"] currentRoute "/shortName" $ \route ->
              withProjectFromShortName "projShortName" route $ \(_,user) (projId, _) ->
                do mShortName <- (param "shortName")
                   maybeRestAPIError mShortName (Just $ userName user) route ["shortName"] $ \shortName ->
                     do runSQL $ DB.update projId [ ProjectShortName =. shortName ]
                        answerAndLog (Just $ userName user) "updating project shortName" $
                          FrozoneCmdUpdateProjectShortName
            userRoute GET ["admin"] currentRoute "/users/add" $ \route ->
              withProjectFromShortName "projShortName" route $ \(_,user) (projId, _) ->
                do mNewUserName <- (param "name")
                   maybeRestAPIError mNewUserName (Just $ userName user) route ["shortName"] $ \newUserName ->
                     do mUserId <- runSQL $ DB.getBy (UniqueUserName newUserName) >>= return . (liftM DB.entityKey)
                        maybeErrorInRoute mUserId LogNote (Just $ userName user) route
                          "user not found" "user not found" $ \userId ->
                            do updateWorked <- runSQL $ updateField ProjectUsers projId $ \projOld ->
                                 [userId] `union` projectUsers projOld 
                               if not updateWorked 
                                 then errorInRoute LogError (Just $ userName user) route "failed updating project" "failed updating project"
                                 else 
                                   answerAndLog (Just $ userName user) "updating project users" $
                                     FrozoneCmdUpdateProjectUsers
            userRoute GET ["admin"] currentRoute "/users/delete" $ \route ->
              withProjectFromShortName "projShortName" route $ \(_,user) (projId, _) ->
                do mNewUserName <- (param "name")
                   maybeRestAPIError mNewUserName (Just $ userName user) route ["shortName"] $ \newUserName ->
                     do mUserId <- runSQL $ DB.getBy (UniqueUserName newUserName) >>= return . (liftM DB.entityKey)
                        maybeErrorInRoute mUserId LogNote (Just $ userName user) route
                          "user not found" "user not found" $ \userId ->
                            do updateWorked <- runSQL $ updateField ProjectUsers projId $ \projOld ->
                                 projectUsers projOld \\ [userId]
                               if not updateWorked 
                                 then errorInRoute LogError (Just $ userName user) route "failed updating project" "failed updating project"
                                 else 
                                   answerAndLog (Just $ userName user) "updating project users" $
                                     FrozoneCmdUpdateProjectUsers
            {-
            userRoute GET ["admin"] currentRoute "/users" $ \route ->
              withProjectFromShortName "projShortName" route $ \(_,user) (projId, _) ->
                do mUsers <- (param "users")
                   maybeRestAPIError mUsers (Just $ userName user) route ["shortName"] $ \users ->
                     do runSQL $ DB.update projId [ ProjectUsers =. users ]
                        answerAndLog (Just $ userName user) "updating project users" $
                          FrozoneCmdUpdateProjectUsers
            -}

safeUserInfoFromUser :: User -> UserInfo
safeUserInfoFromUser user = UserInfo
    { sui_name = userName user
    , sui_email = userEmail user
    , sui_isAdmin = userIsAdmin user
    }

projectInfoFromProject :: Project -> FrozoneAction (Maybe ProjectInfo)
projectInfoFromProject proj =
    do mUsers <- runSQL $ mapM DB.get $ projectUsers proj :: FrozoneAction [Maybe User]
       let mUserNames = T.sequenceA mUsers >>= return . map userName :: Maybe [T.Text]
       return $ mUserNames >>= \userNames -> 
         Just $ ProjectInfo
           { pi_name = projectName proj
           , pi_shortName = projectShortName proj
           , pi_repoLoc = projectRepoLoc proj
           , pi_sshKey = projectSshKey proj
           , pi_users = userNames
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
