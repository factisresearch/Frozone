module Frozone.User.DB where

import Frozone.Types


import Database.Persist as DB
import Database.Persist.Sql as DB
import qualified Data.Text as T

import Data.Time
import Control.Monad
import Control.Monad.Trans
import Control.Applicative


isFirstUser = liftM ((==0) . length) $
    (DB.selectList [] [DB.LimitTo 1] :: DB.SqlPersistM [DB.Entity User])


createUser :: T.Text -> T.Text -> Bool -> DB.SqlPersistM (Maybe (UserId,User))
createUser user password isAdmin =
    do key <- DB.insert $ User
         { userName = user
         , userPassword = password
         , userEmail = T.pack ""
         , userIsAdmin = isAdmin 
         }
       mVal <- DB.get key
       return $ uncurry (liftM2 (,)) $ (Just key, mVal)

deleteUser :: T.Text -> DB.SqlPersistM ()
deleteUser user =
    do DB.deleteBy (UniqueUserName user)

checkUser :: T.Text -> T.Text -> DB.SqlPersistM (Maybe (UserId,User))
checkUser name password =
    do mUserEntity <- DB.getBy (UniqueUserName name)
       case mUserEntity of
         Just userEntity -> 
           let (userId,user) = (DB.entityKey userEntity, DB.entityVal userEntity) in
           if userPassword user == password
           then return $ Just (userId,user)
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
