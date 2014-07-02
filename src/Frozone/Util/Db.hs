module Frozone.Util.Db where

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist.Sql
import Web.Spock

runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn
