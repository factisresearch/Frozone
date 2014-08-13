{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.User.API where

import Frozone.User.DB
import Frozone.Types

import Frozone.Util.Db
import Frozone.Util.Logging
import Frozone.Util.Rest

import Web.Spock hiding (patch, subcomponent)
--import Web.Spock.Auth hiding (userRoute)
import qualified Data.Text as T

import qualified Database.Persist as DB
--import qualified Database.Persist.Sql as DB
import Database.Persist ((=.))

import Control.Monad

-- user management:
userAPI :: String -> FrozoneApp ()
userAPI currentRoute = 
    do userRoute GET ["admin"] currentRoute "/list" $ \_ (_, user) ->
         do allUsers <- runSQL $ DB.selectList [] [DB.Desc UserId, DB.LimitTo 50]
            answerAndLog (Just $ userName user) "listing users" $
              FrozoneGetUsers $ map ( safeUserInfoFromUser . DB.entityVal) allUsers

       userRoute POST ["admin"] currentRoute "/create" $ \route (_, user) ->
         do mNewUser <- param "name"
            mNewPassword <- param "password"
            mNewIsAdmin' <- param "isAdmin" :: FrozoneAction (Maybe Int)
            let mNewIsAdmin = mNewIsAdmin' >>= (\p -> return $ if p==0 then False else True)
            let mUserAndPasswordAndIsAdmin = (uncurry3 $ liftM3 (,,)) $ (mNewUser, mNewPassword, mNewIsAdmin) :: Maybe (T.Text, T.Text, Bool)
            maybeRestAPIError mUserAndPasswordAndIsAdmin (Just $ userName user) route ["name","password","isAdmin"] $ \(newUser,newPassword,newIsAdmin) ->
              do runSQL $ createUser newUser newPassword newIsAdmin
                 answerAndLog (Just $ userName user) ("created user \"" ++ T.unpack newUser ++ "\"") FrozoneCmdCreateUser

       userRoute GET ["admin"] currentRoute "/delete" $ \route (_, user) ->
         do mUserToDelete <- param "name"
            maybeRestAPIError mUserToDelete (Just $ userName user) route ["name"] $ \(userToDelete) ->
              do runSQL $ deleteUser userToDelete
                 answerAndLog (Just $ userName user) ("deleted user \"" ++ T.unpack userToDelete ++ "\"") FrozoneCmdDeleteUser

       subcomponent currentRoute "/update" $ updateAPI


updateAPI currentRoute =
    do userRoute GET [] currentRoute "/password" $ \route (userId, user) ->
         do mNewPassword <- param "password"
            maybeRestAPIError mNewPassword (Just $ userName user) route ["password"] $ \newPassword ->
              do runSQL $ DB.update userId [ UserPassword =. newPassword ]
                 answerAndLog (Just $ userName user) "updated password" $
                   FrozoneCmdUpdatePassword

       userRoute GET [] currentRoute "/email" $ \route (userId, user) ->
         do mNewEmail <- param "email"
            maybeRestAPIError mNewEmail (Just $ userName user) route ["email"] $ \newEmail ->
              do runSQL $ DB.update userId [ UserEmail =. newEmail ]
                 answerAndLog (Just $ userName user) "updated email"
                   FrozoneCmdUpdateEmail

       userRoute GET ["admin"] currentRoute "/isAdmin" $ \route (_, user) ->
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


safeUserInfoFromUser :: User -> UserInfo
safeUserInfoFromUser user = UserInfo
    { sui_name = userName user
    , sui_email = userEmail user
    , sui_isAdmin = userIsAdmin user
    }

boolFromInt :: Int -> Bool
boolFromInt i = if i==0 then False else True

tupelFromEntity entity = (DB.entityKey entity, DB.entityVal entity)
