{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
module Frozone.Model where

import Frozone.PackageManager as PM

import Control.Monad

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

data UserManagementData
    = UserManagementData
    { users :: M.Map UserName User
    , projects :: M.Map ProjectShortName Project
    }
-- invariant: join (map projUsers $ projects d) `L.subset` users d

data User
    = User
    { user_name :: UserName 
    , user_password :: Password
    , user_email :: T.Text
    , user_isAdmin :: Bool
    }
    deriving (Eq, Ord, Show)
-- ok

data Project
    = Project
    { proj_name :: ProjectName
    , proj_shortName :: ProjectShortName
    , proj_users :: S.Set UserName
    , proj_pm :: PM.PackageManager
    }


newtype UserName = UserName { fromUserName :: T.Text }
    deriving (Eq, Ord, Show)
data Password = Password T.Text | HashedPassword T.Text
    deriving (Eq, Ord, Show)

newtype ProjectName = ProjectName { fromProjectName :: T.Text }
    deriving (Eq, Ord, Show)
newtype ProjectShortName = ProjectShortName { fromProjectShortName :: T.Text }
    deriving (Eq, Ord, Show)

emptyFrozone :: UserManagementData
emptyFrozone =
    UserManagementData
    { users = M.empty
    , projects = M.empty
    }

-- user management

addUser :: User -> UserManagementData -> Maybe UserManagementData
addUser user frozone =
    do guard $ not $ user_name user `M.member` users frozone
       return $ withUsers (M.insert (user_name user) user) frozone

deleteUser :: UserName -> UserManagementData -> Maybe UserManagementData
deleteUser userName frozone =
    do guard $ userName `M.member` users frozone
       return $ withUsers (M.delete userName) frozone

updateUserName :: UserName -> UserName -> UserManagementData -> Maybe UserManagementData
updateUserName userName newUserName frozone =
    do guard $ not $ newUserName `M.member` users frozone
       updateUser userName (\user -> user{ user_name = newUserName }) $ frozone

updateUserPassword :: UserName -> Password -> UserManagementData -> Maybe UserManagementData
updateUserPassword userName newPassword frozone =
    updateUser userName (\user -> user{ user_password = newPassword }) $ frozone

updateUserEmail :: UserName -> T.Text -> UserManagementData -> Maybe UserManagementData
updateUserEmail userName newEmail frozone =
    updateUser userName (\user -> user{ user_email = newEmail }) $ frozone

updateUserIsAdmin :: UserName -> Bool -> UserManagementData -> Maybe UserManagementData
updateUserIsAdmin userName newIsAdmin frozone =
    updateUser userName (\user -> user{ user_isAdmin = newIsAdmin }) $ frozone

-- project managent:

addUserToProject :: UserName -> ProjectShortName -> UserManagementData -> Maybe UserManagementData
addUserToProject userName projShortName frozone =
    do guard $ not $ userName `M.member` users frozone
       updateProject projShortName (withProjectUsers (S.insert userName)) frozone

deleteUserFromProject :: UserName -> ProjectShortName -> UserManagementData -> Maybe UserManagementData
deleteUserFromProject userName projShortName frozone =
    do guard $ userName `M.member` users frozone
       updateProject projShortName (withProjectUsers (S.delete userName)) frozone

-- helper functions:
updateUser :: UserName -> (User -> User) -> UserManagementData -> Maybe UserManagementData
updateUser userName f frozone =
    do guard $ userName `M.member` users frozone
       return $ withUsers (M.update (return . f) userName) frozone


updateProject :: ProjectShortName -> (Project -> Project) -> UserManagementData -> Maybe UserManagementData
updateProject projShortName f frozone =
    do guard $ projShortName `M.member` projects frozone
       return $ withProjects (M.update (return . f) projShortName) frozone

{-
updateProjectUsers :: UserName -> (S.Set UserName -> S.Set UserName) -> Project -> Maybe Project
updateProjectUsers userName f project = 
    do guard $ userName `S.member` proj_users project
       return $ withProjectUsers f project
-}

withUsers f frozone = frozone{ users= f (users frozone) }
withProjects f frozone = frozone{ projects= f (projects frozone) }

withProjectUsers :: (S.Set UserName -> S.Set UserName) -> Project -> Project
withProjectUsers f project = project{ proj_users = f (proj_users project) }

{-
share [mkPersist sqlSettings, mkMigrate "migrateCore"] [persistLowerCase|

User json
     name T.Text
     password T.Text
     email T.Text
     isAdmin Bool
     UniqueUserName name
     --UniqueEmail email
-- ok

Project json
     name T.Text
     shortName T.Text
     repoLoc T.Text
     sshKey T.Text
     users [UserId] --- > [User]
     UniqueShortName shortName
-- ok

Session json
     user UserId --- > User
     validUntil UTCTime
     --UniqueUserId user

BundleData json -- collection of patches sent
     bundleHash T.Text
     filePath FilePath
     date UTCTime
     UniqueBundleHash bundleHash
-- build system

PatchCollection json -- group of patches having the same name
     name T.Text
     project ProjectId --- > Project
     open Bool
-- ok

Patch json
     vcsId T.Text
     name T.Text
     author T.Text
     date UTCTime
     dependents [PatchId] --- > [Patch]
     bundle BundleDataId --- > BundleData
     patchCollection PatchCollectionId --- > PatchCollection
     UniquePatchVcsId vcsId
-- ok

BuildLog json
     state BuildState
     time UTCTime
     message T.Text
     repo BuildRepositoryId --- > BuildRepository
-- ok

BuildRepository json
     project ProjectId --- > Project
     branch T.Text
     path FilePath
     createdOn UTCTime
     notifyEmail [T.Text]
     changesHash T.Text
     patch PatchId --- > Patch
     state BuildState
     dockerImage T.Text Maybe
     UniqueRepoPath path
     UniqueChangesHash changesHash
-- build system

BundleChange json
     filename FilePath
     oldContents T.Text Maybe
     newContents T.Text Maybe
     repoId BuildRepositoryId --- > BuildRepository
-- build system 


|]

updateBuildState :: (PersistMonadBackend m ~ SqlBackend, MonadIO m, PersistQuery m, MonadSqlPersist m)
                 => BuildRepositoryId -> BuildState -> T.Text -> m BuildLogId
updateBuildState buildRepoId newState msg =
    do now <- liftIO getCurrentTime
       update buildRepoId [ BuildRepositoryState =. newState ]
       insert (BuildLog newState now msg buildRepoId)
-}
