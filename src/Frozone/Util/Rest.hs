{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.Util.Rest where

import Frozone.User.DB
import Frozone.Types

import Frozone.Util.Db
import Frozone.Util.Logging

import Web.Spock hiding (patch, subcomponent)
import qualified Web.Spock as S
import Web.Spock.Auth hiding (userRoute)
import qualified Web.Spock.Auth as Spock

import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB

import qualified Data.Text as T
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Traversable as Tr


-- |print logMsg to log, send answer to client
answerAndLog mUser logMsg answer =
    do doLog LogInfo $
         maybe "" (\user -> T.unpack user ++ ": ") mUser
         ++ logMsg
       json $ answer

maybeRestAPIError ma mUser route fields =
    maybeError ma (restAPIError mUser route fields)
maybeErrorInRoute ma logLevel mUser route logMsg jsonMsg =
    maybeError ma $ errorInRoute logLevel mUser route logMsg jsonMsg

restAPIError :: Maybe T.Text -> String -> [String] -> FrozoneAction ()
restAPIError mUser route fields =
  restErrorPriv LogNote
    (maybe "" (\user -> T.unpack user ++ ": ") mUser ++ "rest api violation in rest route \"" ++ route ++ "\"")
    ("expected fields: " `T.append` (T.intercalate ", " $ map T.pack fields))

errorInRoute :: LogLevel -> Maybe T.Text -> String -> String -> T.Text -> FrozoneAction ()
errorInRoute logLevel mUser route logMsg jsonMsg =
  restErrorPriv logLevel
    (maybe "" (\user -> T.unpack user ++ ": ") mUser ++ "error in route \"" ++ route ++ "\": " ++ logMsg)
    jsonMsg

maybeError :: Maybe a -> FrozoneAction () -> (a -> FrozoneAction ()) -> FrozoneAction ()
maybeError ma printMsg =
    flip (maybe printMsg) ma

-- |log error, send error to client
restErrorPriv :: LogLevel -> String -> T.Text -> FrozoneAction ()
restErrorPriv logLevel logMsg jsonMsg =
    do doLog logLevel logMsg
       json $ FrozoneError jsonMsg


withPatchSafe
    :: T.Text -> String
    -> ((PatchId,Patch) -> (UserId,User) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
withPatchSafe name route f = withPatch name route f'
    where
      f' patchKV@(patchId,patch) userKV@(userId,user) =
           -- 1. check if user is in a project that this patch is a part of
        do mIsAllowed <- runSQL $
             do buildRepoEntities <- DB.selectList [ BuildRepositoryPatch DB.==. patchId] []
                let projIds = map DB.entityVal buildRepoEntities >>= return . buildRepositoryProject :: [ProjectId]
                projectsMaybe <- mapM DB.get projIds :: DB.SqlPersistM [Maybe Project]
                let
                  mProjects = Tr.sequenceA projectsMaybe :: Maybe [Project]
                  mUsersInProjects = mProjects >>= return . map projectUsers >>= return . join :: Maybe [UserId]
                return $ liftM (userId `elem`) mUsersInProjects
           -- 2. perform 'f' only, if the condition is true
           maybeErrorInRoute mIsAllowed LogError (Just $ userName user) route
             "database error loading users for patch" "server database error" $ \isAllowed ->
             if not (isAllowed || userIsAdmin user)
               then
                errorInRoute LogNote (Just $ userName user) route
                  ("acces forbidden to patch \"" ++ T.unpack (patchName patch) ++ "\"")
                  "no access to patch"
               else f patchKV userKV
withBuildRepoSafe
    :: T.Text -> String
    -> ((BuildRepositoryId,BuildRepository) -> (UserId,User) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
withBuildRepoSafe name route f = withBuildRepo name route f'
    where
      f' repoKV@(repoId,repo) userKV@(userId,user) =
           -- 1. check if user is in a project that this repo is a part of
        do mIsAllowed <- runSQL $
             do mProject <- DB.get $ buildRepositoryProject repo
                let userIds = liftM projectUsers $ mProject
                return $ liftM (userId `elem`) $ userIds
           -- 2. perform 'f' only, if the condition is true
           maybeErrorInRoute mIsAllowed LogError (Just $ userName user) route
             "database error loading users for build repository" "server database error" $ \isAllowed ->
             if not (isAllowed || userIsAdmin user)
               then
                errorInRoute LogNote (Just $ userName user) route
                  ("acces forbidden to build repository with id \"" ++ show repoId ++ "\"")
                  "no access to build repository"
               else f repoKV userKV
withPatchCollectionSafe
    :: T.Text -> String
    -> ((PatchCollectionId,PatchCollection) -> (UserId,User) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
withPatchCollectionSafe name route f = withPatchCollection name route f'
    where
      f' collectionKV@(collectionId,collection) userKV@(userId,user) =
             -- 1. check if user is in a project that this patch collection is a part of
          do mIsAllowed <-
                 runSQL $
                     do mProject <- DB.get $ patchCollectionProject collection
                        let mUsersInProject = mProject >>= return . projectUsers
                        return $ liftM (userId `elem`) mUsersInProject
             -- 2. perform 'f' only, if the condition is true
             maybeErrorInRoute mIsAllowed LogError (Just $ userName user) route
               "database error loading users for patch collection" "server database error" $ \isAllowed ->
                   if not (isAllowed || userIsAdmin user)
                     then
                      errorInRoute LogNote (Just $ userName user) route
                        ("acces forbidden to patch collection with id \"" ++ show collectionId++ "\"")
                        "no access to patch collection"
                     else f collectionKV userKV
             

withPatch
    :: T.Text -> String
    -> ((PatchId,Patch) -> (UserId,User) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withPath = with
withPatch name route f (userId,user) = 
    do mId <- param name
       maybeRestAPIError mId (Just $ userName user) route [T.unpack name] $ \iD ->
         do mVal <- runSQL $ DB.get iD
            maybeErrorInRoute mVal LogNote (Just $ userName user) route "patch not found" "patch not found" $ \val ->
              f (iD,val) (userId,user)

{-
withBuild
    :: T.Text -> T.Text
    -> (((UserId,User), (BuildRepositoryId,BuildRepository)) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withBuild = with
withBuild name err f (userId,user) =
    do mId <- param name
       mVal <- maybe (return Nothing) (\iD -> runSQL $ DB.get iD) mId 
       let mId_Val = (uncurry $ liftM2 (,)) $ (mId,mVal)
       maybe (json $ FrozoneError err ) (\(iD,val) -> f ((userId,user),(iD,val))) mId_Val
-}

withPatchCollection 
    :: T.Text -> String
    -> ((PatchCollectionId,PatchCollection) -> (UserId,User) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withPatchCollection = with
withPatchCollection name route f (userId,user) =
    do mId <- param name
       maybeRestAPIError mId (Just $ userName user) route [T.unpack name] $ \iD ->
         do mVal <- runSQL $ DB.get iD
            maybeErrorInRoute mVal LogNote (Just $ userName user) route "patch collection not found" "patch collection not found" $ \val ->
              f (iD,val) (userId,user)
    
withBuildRepo 
    :: T.Text -> String
    -> ((BuildRepositoryId,BuildRepository) -> (UserId,User) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withBuildRepoCollection = with
withBuildRepo name route f (userId,user) =
    do mId <- param name
       maybeRestAPIError mId (Just $ userName user) route [T.unpack name] $ \iD ->
         do mVal <- runSQL $ DB.get iD
            maybeErrorInRoute mVal LogNote (Just $ userName user) route "repo not found" "repo not found" $ \val ->
              f (iD,val) (userId,user)
         --let mId_Val = (uncurry $ liftM2 (,)) $ (mId,mVal)
       --maybe (json $ FrozoneError err ) (\(iD,val) -> f ((userId,user),(iD,val))) mId_Val

{- also checks if the user is part of the project -}
withProjectFromShortNameSafe
    :: T.Text -> String
    -> ((ProjectId,Project) -> (UserId,User) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
withProjectFromShortNameSafe shortNameParam route f = withProjectFromShortName shortNameParam route f'
    where
      f' projKV@(_,proj) userKV@(userId,user) =
        if not (userId `elem` projectUsers proj || userIsAdmin user)
        then
          errorInRoute LogNote (Just $ userName user) route
            ("user is not in project \"" ++ T.unpack (projectName proj) ++ "\"")
            "not part of the project"
        else f projKV userKV

withProjectFromShortName
    :: T.Text -> String
    -> ((ProjectId,Project) -> (UserId,User) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withBuild = with
withProjectFromShortName shortNameParam route f (userId,user) =
    do mShortName <- param shortNameParam
       maybeRestAPIError mShortName (Just $ userName user) route [T.unpack shortNameParam] $ \shortName ->
         do mProjEntity <- runSQL $ DB.getBy (UniqueShortName shortName)
            let mIdAndVal = liftM (\ent -> (DB.entityKey ent, DB.entityVal ent)) mProjEntity
            maybeErrorInRoute mIdAndVal LogNote (Just $ userName user) route "project not found" "project not found" $ \projKV ->
              f projKV (userId,user) 

{-
with :: T.Text -> T.Text -> (((UserId,User), (key,val)) -> FrozoneAction ()) -> ((UserId,User) -> FrozoneAction ())
with name err f (userId,user) = 
    do mId <- param name
       mVal <- maybe (return Nothing) (\iD -> runSQL $ DB.get iD) mId 
       let mId_Val = (uncurry $ liftM2 (,)) $ (mId,mVal)
       maybe (json $ FrozoneError err ) (\(iD,val) -> f ((userId,user),(iD,val))) mId_Val
-}

{-
getParams :: Maybe User -> String -> [String] -> []
getParams mUser route paramNames = 
-}

subcomponent :: MonadIO m => String -> T.Text -> (String -> SpockT m a) -> SpockT m a
subcomponent prependToRoute route f = S.subcomponent route (f $ prependToRoute ++ T.unpack route)

userRoute
    :: StdMethod -> [UserRights]
    -> String -> T.Text
    -> (String  -> (UserId, User) -> FrozoneAction ())
    -> SpockM DB.Connection FrozoneSession FrozoneState ()
userRoute stdMeth userRights routeToPrepend route f =
  Spock.userRoute noAccesHandler (runSQL . userFromSession) checkRights stdMeth userRights route (f $ routeToPrepend ++ T.unpack route)
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

uncurry3 f (a,b,c) = f a b c
uncurry4 f (a,b,c,d) = f a b c d
