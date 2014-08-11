module Frozone.User where

import Frozone.Types


import Database.Persist as DB
import Database.Persist.Sql as DB
import qualified Data.Text as T

import Data.Time
import Control.Monad
import Control.Monad.Trans
import Control.Applicative


isFirstUser = liftM (==0) $ userCountFromDB

userCountFromDB :: DB.SqlPersistM Int
userCountFromDB = return 0

createUser :: T.Text -> T.Text -> Bool -> DB.SqlPersistM ()
createUser user password isAdmin =
    do DB.insert_ $ User
         { userName = user
         , userPassword = password
         , userEmail = T.pack ""
         , userIsAdmin = isAdmin 
         }
deleteUser :: T.Text -> DB.SqlPersistM ()
deleteUser user =
    do DB.deleteBy (UniqueUserName user)

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

repoFromDB :: BuildRepositoryId -> DB.SqlPersistM (Maybe BuildRepository)
repoFromDB repoId = DB.get repoId

    --do return Nothing --mProjectEntity <- DB.getBy (UniqueUserName

accumParams :: (a -> b) -> (a -> (a,b))
accumParams f x = (x, f x)

accumParamsM :: Monad m => (a -> m b) -> (a -> m (a,b))
accumParamsM f x = do
    y <- f x
    return (x, y)
