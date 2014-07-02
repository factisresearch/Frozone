{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
module Frozone.Server where

import Frozone.Types
import Frozone.Model
import Frozone.BaseImage
import Frozone.Util.Logging
import Frozone.Util.Random
import Frozone.Util.Process

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.List
import Data.Maybe
import Data.Time
import Database.Persist.Sqlite hiding (get)
import Network.Email.Sendmail
import System.Directory
import System.Exit
import System.FilePath
import Web.Spock
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as YML
import qualified Database.Persist as DB
import qualified Network.Wai.Parse as Wai

type FrozoneApp = SpockM Connection () FrozoneState ()
type FrozoneAction a = SpockAction Connection () FrozoneState a
type FrozoneWorker a = WebStateM Connection () FrozoneState a

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

runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn

sendNotifications :: Maybe T.Text -> TempRepository -> IO ()
sendNotifications mTarget repo =
    do doLog LogInfo ("Sending email '" ++ subject ++ "' to " ++ (intercalate ", " targets))
       sendmail (Just "Frozone <thiemann@cp-med.com>") targets
                 ("Subject: Frozone: " ++ subject ++ "\r\n" ++ msg ++ "\n\n" ++ "Patches in repository:\n\n"
                  ++ (T.unpack $ tempRepositoryChanges repo))
    where
      targets =
          case mTarget of
            Just t ->
                [T.unpack t]
            Nothing ->
                (map T.unpack (tempRepositoryNotifyEmail repo))
      msg =
          T.unpack $ fromMaybe "Nothing happened." (tempRepositoryBuildMessage repo)
      subject =
          if tempRepositoryBuildSuccess repo == Just True
          then "[GOOD] The build of your patches was successful!"
          else "[BAD] Failed to build your patches"

launchBuild :: FrozoneState
            -> (forall a. SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a)
            -> TempRepositoryId
            -> TempRepository
            -> IO ()
launchBuild st ioSQL repoId repo =
    finally (catch runBuild (\(e :: SomeException) -> buildFailed (show e))) notifyEveryone
    where
      notifyEveryone =
          do mRepo <- ioSQL $ DB.get repoId
             case mRepo of
               Just r ->
                   sendNotifications Nothing r
               Nothing ->
                   doLog LogWarn ("Failed to send notifications! Repository vanished.")

      buildFailed reason =
          do doLog LogWarn ("Build " ++ show repoId
                            ++ " failed/crashed! Error: " ++ reason)
             _ <- ioSQL $ DB.update repoId [ TempRepositoryBuildSuccess =. (Just False)
                                           , TempRepositoryBuildMessage =. (Just (T.pack $ "Build failed! \n\n" ++ reason))
                                           ]
             return ()

      runBuild =
          do now <- getCurrentTime
             ioSQL $ DB.update repoId [TempRepositoryBuildEnqueuedOn =. (Just now)]
             yml <- YML.decodeFileEither (tempRepositoryPath repo </> ".frozone.yml")
             case yml of
               Left parseException ->
                   buildFailed ("Error in .frozone.yml: " ++ show parseException)
               Right repoCfg ->
                   do cabalOk <- doesFileExist (tempRepositoryPath repo </> rc_cabalFile repoCfg)
                      if cabalOk
                      then do cabalBS <- BS.readFile (tempRepositoryPath repo </> rc_cabalFile repoCfg)
                              mBaseImage <- ensureBaseImageExists (fs_baseImageBuildsVar st) repoCfg cabalBS
                              case mBaseImage of
                                Left errMsg ->
                                    buildFailed ("Something went wrong while trying to build the base image:\n" ++ errMsg)
                                Right baseImage ->
                                    prepareDockerFile baseImage
                      else buildFailed ("Your cabal file " ++ rc_cabalFile repoCfg ++ "doesn't exist")

      prepareDockerFile baseImage =
          do let dockerPath = (tempRepositoryPath repo </> "Dockerfile")
                 baseImgTag = "$$BASE_IMAGE$$"
             dockerOk <- doesFileExist dockerPath
             if dockerOk
             then do dockerfile <- T.readFile dockerPath
                     if T.isInfixOf baseImgTag dockerfile
                     then do T.writeFile dockerPath (T.replace baseImgTag baseImage dockerfile)
                             ioSQL $ DB.update repoId [TempRepositoryDockerBaseImage =. (Just baseImage)]
                             runDockerBuild
                     else buildFailed ("Missing 'FROM " ++ (T.unpack baseImgTag) ++ "' in your Dockerfile!")
             else buildFailed "Missing a Dockerfile in your Repository root."

      runDockerBuild =
          do doLog LogNote ("Starting to build " ++ show repoId)
             now <- getCurrentTime
             ioSQL $ DB.update repoId [TempRepositoryBuildStartedOn =. (Just now)]
             let dockerImageId = BSC.unpack $ B16.encode $ SHA1.hash $ T.encodeUtf8 (tempRepositoryChanges repo)
                 imageName = "frozone/build-" ++ dockerImageId
             (ec, stdout, stderr) <-
                 runProc "docker" ["build", "-rm", "-t", imageName, tempRepositoryPath repo]
             case ec of
               ExitFailure _ ->
                   buildFailed (stdout ++ "\n \n" ++ stderr)
               ExitSuccess ->
                   do doLog LogNote ("Dockerbuild of " ++ show repoId ++ " complete! Image: " ++ imageName)
                      ioSQL $ DB.update repoId [ TempRepositoryBuildSuccess =. (Just True)
                                               , TempRepositoryBuildMessage =. (Just (T.pack $ "Build successed!\n\n" ++ stdout))
                                               , TempRepositoryDockerImage =. (Just (T.pack imageName))
                                               ]

mkTempRepo :: String -> T.Text -> (FilePath -> FrozoneAction ()) -> FrozoneAction ()
mkTempRepo repo email otherAction =
    do st <- getState
       targetIdent <- liftIO $ randomB16Ident 10
       let targetDir = (fc_storageDir $ fs_config st) </> targetIdent
           withDarcs = withProgResult "darcs" (json . FrozoneError . T.pack)
       withDarcs ["get", "--lazy", repo, targetDir] $ \_ ->
           do now <- liftIO getCurrentTime
              otherAction targetDir
              withDarcs ["changes", "--repodir", targetDir] $ \changes ->
                  do let changesHash =
                             T.decodeUtf8 $ B16.encode $ SHA1.hash $ BSC.pack changes
                         rp =
                             TempRepository
                             { tempRepositoryBranch = T.pack repo
                             , tempRepositoryPath = targetDir
                             , tempRepositoryCreatedOn = now
                             , tempRepositoryNotifyEmail = [email]
                             , tempRepositoryChanges = (T.pack changes)
                             , tempRepositoryChangesHash = changesHash
                             , tempRepositoryBuildEnqueuedOn = Nothing
                             , tempRepositoryBuildStartedOn = Nothing
                             , tempRepositoryBuildSuccess = Nothing
                             , tempRepositoryBuildMessage = Nothing
                             , tempRepositoryDockerBaseImage = Nothing
                             , tempRepositoryDockerImage = Nothing
                             }

                     mRepo <- runSQL $ DB.getBy (UniqueChangesHash changesHash)
                     case mRepo of
                       Just sameRepoEntity ->
                           let sameRepo = entityVal sameRepoEntity
                               sameRepoId = entityKey sameRepoEntity
                           in if isJust $ tempRepositoryBuildSuccess sameRepo
                              then liftIO $
                                   do doLog LogNote ("Build of " ++ show changesHash ++ " already completed. Sending results via email")
                                      sendNotifications (Just email) sameRepo
                              else do liftIO $ doLog LogNote ("Build of " ++ show changesHash ++ " in progress. Adding sender to notification list")
                                      runSQL $ DB.update sameRepoId [ TempRepositoryNotifyEmail =. (email : tempRepositoryNotifyEmail sameRepo) ]
                       Nothing ->
                           do dbId <- runSQL $ DB.insert rp
                              liftIO $ doLog LogNote ("Will schedule build for " ++ show changesHash ++ " (" ++ targetDir ++ ")")
                              spockHeart <- getSpockHeart
                              _ <- liftIO $ forkIO (launchBuild st (runSpockIO spockHeart . runSQL) dbId rp)
                              return ()

                     json (FrozoneMessage "Patchbundle recieved and enqueued for building")

serverApp :: FrozoneApp
serverApp =
    do post "/check-bundle" $
         do repo <- param "target-repo"
            email <- param "email"
            allFiles <- files
            case lookup "patch-bundle" allFiles of
              Just patchBundle ->
                  mkTempRepo repo email $ \repoPath ->
                      do let withDarcs = withProgResult "darcs" (json . FrozoneError . T.pack)
                             bundleBs = BSL.toStrict $ Wai.fileContent patchBundle
                             bundleLoc = repoPath </> "patches.dpatch"
                         liftIO $ BS.writeFile bundleLoc bundleBs
                         withDarcs ["apply", "--repodir", repoPath, bundleLoc] $ \_ ->
                             liftIO $ doLog LogInfo ("Patches applied!")
              Nothing ->
                  json (FrozoneError "No patch-bundle sent!")
