{-# LANGUAGE OverloadedStrings #-}
module Frozone.RestApi where

import Frozone.Types
import Frozone.Model
import Frozone.Util.Db

import Web.Spock
import qualified Database.Persist as DB

restApi :: FrozoneApp
restApi =
    do get "/api/list-builds" $
         do allBuilds <- runSQL $ DB.selectList [] [DB.Desc TempRepositoryId, DB.LimitTo 50]
            json allBuilds
