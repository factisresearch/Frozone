{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.Util.Rest where

import Frozone.User
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



--
withPatch
    :: T.Text -> String
    -> ((UserId,User) -> (PatchId,Patch) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withPath = with
withPatch name route f (userId,user) = 
    do mId <- param name
       maybeRestAPIError mId (Just $ userName user) route [T.unpack name] $ \iD ->
         do mVal <- runSQL $ DB.get iD
            maybeErrorInRoute mVal LogNote (Just $ userName user) route "patch not found" "patch not found" $ \val ->
              f (userId,user) (iD,val)

withBuild
    :: T.Text -> String
    -> ((UserId,User) -> (BuildRepositoryId,BuildRepository) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withPath = with
withBuild name route f (userId,user) = 
    do mId <- param name
       maybeRestAPIError mId (Just $ userName user) route [T.unpack name] $ \iD ->
         do mVal <- runSQL $ DB.get iD
            maybeErrorInRoute mVal LogNote (Just $ userName user) route "build not found" "build not found" $ \val ->
              f (userId,user) (iD,val)
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
    -> ((UserId,User) -> (PatchCollectionId,PatchCollection) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withPatchCollection = with
withPatchCollection name route f (userId,user) =
    do mId <- param name
       maybeRestAPIError mId (Just $ userName user) route [T.unpack name] $ \iD ->
         do mVal <- runSQL $ DB.get iD
            maybeErrorInRoute mVal LogNote (Just $ userName user) route "patch collection not found" "patch collection not found" $ \val ->
              f (userId,user) (iD,val)
    
withRepo 
    :: T.Text -> String
    -> ((UserId,User) -> (BuildRepositoryId,BuildRepository) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withRepoCollection = with
withRepo name route f (userId,user) =
    do mId <- param name
       maybeRestAPIError mId (Just $ userName user) route [T.unpack name] $ \iD ->
         do mVal <- runSQL $ DB.get iD
            maybeErrorInRoute mVal LogNote (Just $ userName user) route "repo not found" "repo not found" $ \val ->
              f (userId,user) (iD,val)
         --let mId_Val = (uncurry $ liftM2 (,)) $ (mId,mVal)
       --maybe (json $ FrozoneError err ) (\(iD,val) -> f ((userId,user),(iD,val))) mId_Val

withProjectFromShortName
    :: T.Text -> String
    -> ((UserId,User) -> (ProjectId,Project) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withBuild = with
withProjectFromShortName shortNameParam route f (userId,user) =
    do mShortName <- param shortNameParam
       maybeRestAPIError mShortName (Just $ userName user) route [T.unpack shortNameParam] $ \shortName ->
         do mProjEntity <- runSQL $ DB.getBy (UniqueShortName shortName)
            let mIdAndVal = liftM (\ent -> (DB.entityKey ent, DB.entityVal ent)) mProjEntity
            maybeErrorInRoute mIdAndVal LogNote (Just $ userName user) route "project not found" "project not found" $ \projKV ->
              f (userId,user) projKV
       {-
       maybe (json $ FrozoneError err) (\projKV -> f ((userId,user),projKV)) mIdAndVal
       -}

{-
with :: T.Text -> T.Text -> (((UserId,User), (key,val)) -> FrozoneAction ()) -> ((UserId,User) -> FrozoneAction ())
with name err f (userId,user) = 
    do mId <- param name
       mVal <- maybe (return Nothing) (\iD -> runSQL $ DB.get iD) mId 
       let mId_Val = (uncurry $ liftM2 (,)) $ (mId,mVal)
       maybe (json $ FrozoneError err ) (\(iD,val) -> f ((userId,user),(iD,val))) mId_Val
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
