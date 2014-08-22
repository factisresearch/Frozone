{-# LANGUAGE OverloadedStrings #-}
module Frozone.Project.API where

import Frozone.Project.DB
import Frozone.Types

import Frozone.Util.Db
import Frozone.Util.Logging
import Frozone.Util.Rest

import Web.Spock hiding (patch, subcomponent)
--import Web.Spock.Auth hiding (userRoute)
--import qualified Web.Spock.Auth as Spock
import qualified Data.Text as T

import qualified Database.Persist as DB
--import qualified Database.Persist.Sql as DB
import Database.Persist ((=.))

import Control.Monad
import Data.List
import qualified Data.Traversable as T


projectApi currentRoute = 
    do userRoute GET ["admin"] currentRoute "/list" $ \route (_,user) ->
         do mAllProjects <- (runSQL projectList) >>= (mapM $ (projectInfoFromProject . snd)) >>= return . T.sequenceA 
            maybeErrorInRoute mAllProjects LogError (Just $ userName user) route "failed looking up users for project" "failed looking up users for project" $ \allProjects -> 
              answerAndLog (Just $ userName user) "listing projects" $
                FrozoneGetProjects $ allProjects

       userRoute GET [] currentRoute "" $ \route ->
         withProjectFromShortName "projShortName" route $ \(_, proj) (_, user) ->
           do mProjectInfo <- projectInfoFromProject $ proj
              maybeErrorInRoute mProjectInfo LogError (Just $ userName user) route
                "failed to lookup project info" "failed to lookup project info" $ \projInfo ->
                  answerAndLog (Just $ userName user) ("looking up project info for \"" ++ T.unpack (projectName proj) ++ "\"") $
                    FrozoneGetProjectInfo $ projInfo

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
         withProjectFromShortName "projShortName" route $ \(projId, proj) (_,user) ->
           do runSQL $ deleteProject projId
              answerAndLog (Just $ userName user) ("deleted project \"" ++ (T.unpack $ projectName proj) ++ "\"") $
                FrozoneCmdDeleteProject

       subcomponent currentRoute "/update" $ updateAPI


updateAPI currentRoute =
    do userRoute GET ["admin"] currentRoute "/name" $ \route ->
         withProjectFromShortName "projShortName" route $ \(projId, _) (_,user) ->
           do mName <- (param "name")
              maybeRestAPIError mName (Just $ userName user) route ["name"] $ \name ->
                do runSQL $ DB.update projId [ ProjectName =. name ]
                   answerAndLog (Just $ userName user) "updating project name" $
                     FrozoneCmdUpdateProjectName

       userRoute GET ["admin"] currentRoute "/shortName" $ \route ->
         withProjectFromShortName "projShortName" route $ \(projId, _) (_,user) ->
           do mShortName <- (param "shortName")
              maybeRestAPIError mShortName (Just $ userName user) route ["shortName"] $ \shortName ->
                do runSQL $ DB.update projId [ ProjectShortName =. shortName ]
                   answerAndLog (Just $ userName user) "updating project shortName" $
                     FrozoneCmdUpdateProjectShortName

       userRoute GET ["admin"] currentRoute "/repoLoc" $ \route ->
         withProjectFromShortName "projShortName" route $ \(projId, _) (_,user) ->
           do mShortName <- (param "repoLoc")
              maybeRestAPIError mShortName (Just $ userName user) route ["repoLoc"] $ \repoLoc ->
                do runSQL $ DB.update projId [ ProjectShortName =. repoLoc ]
                   answerAndLog (Just $ userName user) "updating project repoLoc" $
                     FrozoneCmdUpdateProjectRepoLoc
       userRoute GET ["admin"] currentRoute "/sshKey" $ \route ->
         withProjectFromShortName "projShortName" route $ \(projId, _) (_,user) ->
           do mShortName <- (param "sshKey")
              maybeRestAPIError mShortName (Just $ userName user) route ["sshKey"] $ \sshKey ->
                do runSQL $ DB.update projId [ ProjectShortName =. sshKey ]
                   answerAndLog (Just $ userName user) "updating project sshKey" $
                     FrozoneCmdUpdateProjectSSHKey

       userRoute GET ["admin"] currentRoute "/users/add" $ \route ->
         withProjectFromShortName "projShortName" route $ \(projId, _) (_,user) ->
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
         withProjectFromShortName "projShortName" route $ \(projId, _) (_,user) ->
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
