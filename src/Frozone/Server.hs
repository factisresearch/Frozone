{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
module Frozone.Server where

import Frozone.Types
import Frozone.Model
import Frozone.Persist
{-
import qualified Frozone.BundleChecker.API as Bundle
import qualified Frozone.RestApi as Rest
import Frozone.WebFrontend
import Frozone.VCS
import Frozone.Util.Logging

import Frozone.Util.Rest

import Cook.Clean

import Control.Monad.Logger
import Database.Persist.Sqlite (createSqlitePool, runSqlPool, runMigration)
import Network.Wai.Middleware.Static
import Web.Spock hiding( subcomponent )
import Web.Spock.Auth
import Control.Monad.Trans.Resource
import qualified Data.Text as T
-}




{-
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
       doLog LogInfo "Cleaning up old docker images"
       cookClean (fc_storageDir fc) 10

       doLog LogInfo "Launching web service..."
       spock (fc_httpPort fc) sessCfg (PCConduitPool pool) fcState serverApp
    where
        sessCfg = authSessCfg $ AuthCfg
            { ac_sessionTTL = 3600
            , ac_emptySession = () }
        {-sessCfg =
          SessionCfg "FrozoneCookie" 3600 40 ()-}
-}


{-
serverApp :: FrozoneApp ()
serverApp =
    do middleware (staticPolicy (addBase "static"))
       get "/" indexPage
       buildQueue <- subcomponent "" "/bundle" $ Bundle.bundleApi -- :: FrozoneApp (WorkingQueue BuildRepositoryId)
       subcomponent "" "/api" $ (flip Rest.restApi) buildQueue
-}
