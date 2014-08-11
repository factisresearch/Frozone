{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.Util.Rest where

import Frozone.User
import Frozone.Types

import Frozone.Util.Db
import Frozone.Util.Logging

import Web.Spock hiding (patch)
import Web.Spock.Auth hiding (userRoute)
import qualified Web.Spock.Auth as Spock

import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB

import qualified Data.Text as T
import Control.Monad


-- |print logMsg to log, send answer to client
answerAndLog logMsg answer =
    do doLog LogInfo logMsg
       json $ answer

{- pseudocode: maybeError logLevel logMsg jsonMsg ma f
if isJust ma ->
    execute f
else
    write logMsg to log, send jsonMsg to client
-}
maybeError :: LogLevel -> String -> T.Text -> Maybe a -> (a -> FrozoneAction ()) -> FrozoneAction ()
maybeError logLevel logMsg jsonMsg ma =
    flip (maybe (do { doLog logLevel logMsg; json $ FrozoneError jsonMsg })) ma

-- |log error, send error to client
restError :: LogLevel -> String -> T.Text -> FrozoneAction ()
restError logLevel logMsg jsonMsg =
    do doLog logLevel logMsg
       json $ FrozoneError jsonMsg
--
withPatch
    :: T.Text -> T.Text
    -> (((UserId,User), (PatchId,Patch)) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withPath = with
withPatch name err f (userId,user) = 
    do mId <- param name
       mVal <- maybe (return Nothing) (\iD -> runSQL $ DB.get iD) mId 
       let mId_Val = (uncurry $ liftM2 (,)) $ (mId,mVal)
       maybe (json $ FrozoneError err ) (\(iD,val) -> f ((userId,user),(iD,val))) mId_Val

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

withPatchCollection 
    :: T.Text -> T.Text
    -> (((UserId,User), (PatchCollectionId,PatchCollection)) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withPatchCollection = with
withPatchCollection name err f (userId,user) =
    do mId <- param name
       mVal <- maybe (return Nothing) (\iD -> runSQL $ DB.get iD) mId 
       let mId_Val = (uncurry $ liftM2 (,)) $ (mId,mVal)
       maybe (json $ FrozoneError err ) (\(iD,val) -> f ((userId,user),(iD,val))) mId_Val

withRepo 
    :: T.Text -> T.Text
    -> (((UserId,User), (BuildRepositoryId,BuildRepository)) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withRepoCollection = with
withRepo name err f (userId,user) =
    do mId <- param name
       mVal <- maybe (return Nothing) (\iD -> runSQL $ DB.get iD) mId 
       let mId_Val = (uncurry $ liftM2 (,)) $ (mId,mVal)
       maybe (json $ FrozoneError err ) (\(iD,val) -> f ((userId,user),(iD,val))) mId_Val

withProjectFromShortName
    :: T.Text -> T.Text
    -> (((UserId,User), (ProjectId,Project)) -> FrozoneAction ())
    -> (UserId,User) -> FrozoneAction ()
--withBuild = with
withProjectFromShortName shortNameParam err f (userId,user) =
    do mShortName <- param shortNameParam
       mProjEntity <- maybe (return Nothing) (\shortName -> runSQL $ DB.getBy (UniqueShortName shortName)) mShortName 
       let mIdAndVal = liftM (\ent -> (DB.entityKey ent, DB.entityVal ent)) mProjEntity
       maybe (json $ FrozoneError err) (\projKV -> f ((userId,user),projKV)) mIdAndVal

{-
with :: T.Text -> T.Text -> (((UserId,User), (key,val)) -> FrozoneAction ()) -> ((UserId,User) -> FrozoneAction ())
with name err f (userId,user) = 
    do mId <- param name
       mVal <- maybe (return Nothing) (\iD -> runSQL $ DB.get iD) mId 
       let mId_Val = (uncurry $ liftM2 (,)) $ (mId,mVal)
       maybe (json $ FrozoneError err ) (\(iD,val) -> f ((userId,user),(iD,val))) mId_Val
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

uncurry3 f (a,b,c) = f a b c

