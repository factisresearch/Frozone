module Frozone.User where

import Frozone.Types


import Database.Persist as DB
import Database.Persist.Sql as DB
import qualified Data.Text as T

import Data.Time
import Control.Monad.Trans
import Control.Applicative

checkUser :: T.Text -> T.Text -> DB.SqlPersistM (Maybe UserId)
checkUser name password =
    do mUserEntity <- DB.getBy (UniqueUserName name)
       case mUserEntity of
         Just userEntity -> 
           let user = DB.entityVal userEntity in
           if userPassword user == password
           then return $ Just (DB.entityKey userEntity)
           else return Nothing
         Nothing -> return Nothing

sessionIntoDB :: UserId -> DB.SqlPersistM SessionId
sessionIntoDB userId = 
    do now <- liftIO getCurrentTime
       DB.insert (Session userId (addUTCTime (5 * 3600) now))

userFromSession :: SessionId -> DB.SqlPersistM (Maybe (UserId,User))
userFromSession sessionId =
    do mSession <- DB.get sessionId :: DB.SqlPersistM (Maybe Session)
       let mUserId = mSession >>= return . sessionUser
       mUser <- maybe (return Nothing) (\userId -> DB.get userId) mUserId
       return ((,) <$> mUserId <*> mUser)

sessionDelFromDB :: UserId -> DB.SqlPersistM ()
sessionDelFromDB userId =
    do DB.deleteWhere [ SessionUser ==. userId ]
