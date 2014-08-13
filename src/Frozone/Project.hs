module Frozone.Project where

import Frozone.Types

import Database.Persist as DB
import Database.Persist.Sql as DB
import qualified Data.Text as T

import Control.Monad

createProject :: T.Text -> T.Text -> T.Text -> T.Text -> DB.SqlPersistM (Maybe (ProjectId,Project))
createProject name shortName repoLoc sshKey =
    do key <- DB.insert $ Project
         { projectName = name
         , projectShortName = shortName
         , projectRepoLoc = repoLoc
         , projectSshKey = sshKey
         , projectUsers = []
         }
       mVal <- DB.get key
       return $ uncurry (liftM2 (,)) $ (Just key, mVal)

deleteProject :: ProjectId -> DB.SqlPersistM ()
deleteProject iD = DB.delete iD

projectList :: DB.SqlPersistM [(ProjectId, Project)]
projectList =
    (DB.selectList [] [DB.Desc ProjectId, DB.LimitTo 50] ) >>= return . map (\e -> (DB.entityKey e, DB.entityVal e))

--updateField ::(?) EntityField val a -> Key a -> (a -> a) -> DB.SqlPersistM ()
{- !NOTE: This is a horrible hack! -}
updateField field iD f = 
    do mVal <- DB.get iD
       flip (maybe (return False)) mVal $ \val ->
         do DB.update iD [field =. f val]
            return True
