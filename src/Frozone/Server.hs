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
import Frozone.VCS

import Control.Monad.Logger
import Database.Persist.Sqlite (createSqlitePool, runSqlPool, runMigration)
import Network.Wai.Middleware.Static
import Web.Spock
import Control.Monad.Trans.Resource
import qualified Data.Text as T

runServer :: FrozoneConfig -> IO ()
runServer fc =
    do pool <- createSqlitePool (T.pack $ fc_sqliteFile fc) 5
       runResourceT $ runNoLoggingT $ (flip runSqlPool) pool $
          runMigration migrateCore
       let fcState =
               FrozoneState
               { fs_config = fc
               , fs_vcs =
                   case fc_vcs fc of
                     "darcs" -> darcsVCS
                     _ -> error "Unkown VCS System! Currently supported: darcs"
               }
       spock (fc_httpPort fc) sessCfg (PCConduitPool pool) fcState serverApp
    where
      sessCfg =
          SessionCfg "FrozoneCookie" 3600 40 ()


serverApp :: FrozoneApp ()
serverApp =
    do middleware (staticPolicy (addBase "static"))
       get "/" indexPage
       buildQueue <- subcomponent "/bundle" $ bundleApi
       subcomponent "/api" $ restApi buildQueue
