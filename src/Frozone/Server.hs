{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
module Frozone.Server where

import Frozone.Types
import Frozone.Model
import Frozone.BundleChecker
import Frozone.RestApi
import Frozone.WebFrontend

import Control.Concurrent.STM
import Control.Monad.Logger
import Database.Persist.Sqlite (createSqlitePool, runSqlPool, runMigration)
import Network.Wai.Middleware.Static
import Web.Spock
import qualified Data.HashSet as HS
import qualified Data.Text as T

runServer :: FrozoneConfig -> IO ()
runServer fc =
    do pool <- createSqlitePool (T.pack $ fc_sqliteFile fc) 5
       runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
       baseImageBuildsVar <- newTVarIO HS.empty
       let fcState =
               FrozoneState
               { fs_config = fc
               , fs_baseImageBuildsVar = baseImageBuildsVar
               }
       spock (fc_httpPort fc) sessCfg (PCConduitPool pool) fcState serverApp
    where
      sessCfg =
          SessionCfg "FrozoneCookie" 3600 40 ()


serverApp :: FrozoneApp
serverApp =
    do middleware (staticPolicy (addBase "static"))
       get "/" indexPage
       post "/check-bundle" bundleCheckAction
       restApi
