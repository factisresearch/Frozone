module Frozone.PackageManager.Connection(
    Connection(), ConnectionInfo(..),
    connect, disconnect,
    pkgManByConnection,
) where

import Frozone.BuildTypes
import Frozone.PackageManager.API

import Frozone.Util.Logging hiding (doLog)
import qualified Frozone.Util.Logging as Log
import Control.Monad.Reader
import qualified Network.HTTP.Client as HTTP
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy

pkgManByConnection :: Connection -> PackageManager
pkgManByConnection conn = 
    PackageManager
    --{ pkgMan_addPatchBundle = \patchBundle -> runReaderT (addPatchBundle patchBundle) conn
    { pkgMan_listMicroBranches = runReaderT listMicroBranches conn
    , pkgMan_getBuildRepository = \microBranchInfo -> runReaderT (getBuildRepository microBranchInfo) conn
    }

data ConnectionInfo
    = ConnectionInfo
    { connInf_host :: String
    , connInf_port :: Int
    }

data Connection
    = Connection
    { conn_manager :: HTTP.Manager
    , conn_info :: ConnectionInfo
    }

connect :: ConnectionInfo -> IO Connection
connect connInfo =
    do doLog LogInfo "connecting to dpm..."
       manager <- HTTP.newManager HTTP.defaultManagerSettings
       return $
           Connection
           { conn_manager = manager
           , conn_info = connInfo
           }

disconnect :: Connection -> IO ()
disconnect conn =
    do doLog LogInfo "disconnecting from dpm..."
       HTTP.closeManager $ conn_manager conn
       return ()

listMicroBranches :: ReaderT Connection IO [MicroBranchInfo]
listMicroBranches =
    do
    --doLog LogInfo $ "listMicroBranches called"
    ask >>= \conn ->
        lift $
        withResponse "/listMicroBranches" conn $ \resp ->
           case Aeson.decode $ BSLazy.fromStrict $ resp of
             Nothing -> fail "error decoding json"
             Just resp' -> return resp'

getBuildRepository :: MicroBranchInfo -> ReaderT Connection IO Tar
getBuildRepository microBranchInfo =
    do --doLog LogInfo $ "getBuildRepository called with" ++ show microBranchInfo
       conn <- ask
       lift $
           withResponse ("/getBuildRepository/" ++ microBranchHashToString (microBranch_id microBranchInfo)) conn $
           \resp ->
               do doLog LogInfo $ "answer length: " ++ (show $ length $ BS.unpack resp)
                  return $ Tar $ resp

withResponse :: String -> Connection -> (BS.ByteString -> IO a) -> IO a
withResponse request conn f =
    do createReq conn request >>= \req ->
            HTTP.withResponse req (conn_manager conn) $ \resp ->
                do respBinary <- return . BSLazy.toStrict . BSLazy.fromChunks =<< (HTTP.brConsume $ HTTP.responseBody resp)
                   f $ respBinary

createReq Connection{ conn_info = connInfo } restAppendix = 
    HTTP.parseUrl $
        "http://"
        ++ connInf_host connInfo
        ++ ":"
        ++ show (connInf_port connInfo)
        ++ restAppendix

doLog logLevel msg = Log.doLog logLevel $ "CONNECTION: " ++ msg
