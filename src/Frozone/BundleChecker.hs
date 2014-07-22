{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
module Frozone.BundleChecker where

import Frozone.Types
import Frozone.Model
import Frozone.Util.Logging
import Frozone.Util.Random
import Frozone.Util.Process
import Frozone.Util.Db

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Cook.Build
import Cook.Types
import Data.List
import Data.Maybe
import Data.Time
import Database.Persist.Sqlite hiding (get)
import Network.Email.Sendmail
import System.FilePath
import Web.Spock
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as YML
import qualified Database.Persist as DB

closeDangelingActions :: (MonadIO m, PersistQuery m, MonadSqlPersist m) => m ()
closeDangelingActions =
    do liftIO (doLog LogInfo "Fixing dangeling actions...")
       now <- liftIO getCurrentTime
       DB.updateWhere [ TempRepositoryBuildStartedOn !=. Nothing
                      , TempRepositoryBuildFailedOn ==. Nothing
                      , TempRepositoryBuildSuccessOn ==. Nothing
                      ] [ TempRepositoryPatchCanceledOn =. (Just now)
                        , TempRepositoryPatchCancelReason =. (Just "Was dangeling after Frozone restart")
                        ]

bundleCheckAction :: FrozoneAction ()
bundleCheckAction =
    do Just repo <- param "target-repo"
       Just email <- param "email"
       allFiles <- files
       case HM.lookup "patch-bundle" allFiles of
         Just patchBundle ->
             do bs <- liftIO $ BS.readFile (uf_tempLocation patchBundle)
                mkTempRepo repo email bs
         Nothing ->
             json (FrozoneError "No patch-bundle sent!")

sendNotifications :: Maybe T.Text -> TempRepository -> IO ()
sendNotifications mTarget repo =
    do doLog LogInfo ("Sending email '" ++ subject ++ "' to " ++ (intercalate ", " targets))
       sendmail (Just "Frozone <thiemann@cp-med.com>") targets
                 ("Subject: Frozone: " ++ subject ++ "\r\n" ++ msg ++ "\n\n" ++ "Patchbundle:\n\n"
                  ++ (T.unpack $ tempRepositoryPatchBundle repo))
    where
      targets =
          case mTarget of
            Just t ->
                [T.unpack t]
            Nothing ->
                (map T.unpack (tempRepositoryNotifyEmail repo))
      msg =
          T.unpack (tempRepositoryBuildMessage repo)
      subject =
          if isJust (tempRepositoryBuildSuccessOn repo)
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
          do now <- getCurrentTime
             doLog LogWarn ("Build " ++ show repoId
                            ++ " failed/crashed! Error: " ++ reason)
             _ <- ioSQL $ DB.update repoId [ TempRepositoryBuildFailedOn =. (Just now)
                                           , TempRepositoryBuildMessage =. (T.pack $ "Build failed! \n\n" ++ reason)
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
                   do let cookConfig =
                              CookConfig
                              { cc_stateDir = (fc_storageDir . fs_config) st
                              , cc_dataDir = (tempRepositoryPath repo)
                              , cc_buildFileDir = (tempRepositoryPath repo) </> rc_cookDir repoCfg
                              , cc_boringFile = fmap (\b -> (tempRepositoryPath repo) </> b) $ rc_boringFile repoCfg
                              , cc_buildEntryPoints = [ rc_entryPoint repoCfg ]
                              }
                      res <- cookBuild cookConfig
                      case res of
                        ((DockerImage imageName) : _) ->
                            do doLog LogNote ("Dockerbuild of " ++ show repoId ++ " complete! Image: " ++ T.unpack imageName)
                               now' <- getCurrentTime
                               ioSQL $ DB.update repoId [ TempRepositoryBuildSuccessOn =. (Just now')
                                                        , TempRepositoryBuildMessage =. (T.pack $ "Build okay!")
                                                        , TempRepositoryDockerImage =. (Just imageName)
                                                        ]
                        _ ->
                            buildFailed "Cook build failed!"

data FileChangeAction
   = FileChangeCreated
   | FileChangeModified
   | FileChangeDeleted
   deriving (Show, Eq)

type FileChangeMap = HM.HashMap FilePath FileChangeAction

getFileChangeInfo :: BS.ByteString -> FileChangeMap
getFileChangeInfo bundleBS =
    foldl handleLine HM.empty (BSC.split '\n' bundleBS)
    where
      handleLine hm bs =
          if BS.isPrefixOf "] " bs
          then handleLine' hm (BS.drop 2 bs)
          else handleLine' hm bs
      handleLine' hm bs
          | BS.isPrefixOf "hunk" bs =
              case splitBS of
                ["hunk", filename, _] ->
                    HM.insertWith (\new old -> if old == FileChangeCreated then old else new) (BSC.unpack filename) FileChangeModified hm
                _ -> hm
          | BS.isPrefixOf "rmfile" bs =
              case splitBS of
                ["rmfile", filename] ->
                    HM.insert (BSC.unpack filename) FileChangeDeleted hm
                _ -> hm
          | BS.isPrefixOf "addfile" bs =
              case splitBS of
                ["addfile", filename] ->
                    HM.insert (BSC.unpack filename) FileChangeCreated hm
                _ -> hm
          | otherwise = hm
          where
            splitBS = BSC.split ' ' bs

loadChangeInfo :: FileChangeMap
               -> FilePath
               -> (FilePath -> FileChangeAction -> IO (Maybe T.Text))
               -> IO (HM.HashMap FilePath (Maybe T.Text))
loadChangeInfo changeMap repoDir loadStrategy =
    HM.fromList <$> mapM getLocalContent (HM.toList changeMap)
    where
      getLocalContent (fp, fa) =
          do mCt <- loadStrategy (repoDir </> fp) fa
             return (fp, mCt)

preApplyContent :: FilePath -> FileChangeAction -> IO (Maybe T.Text)
preApplyContent _ FileChangeCreated = return Nothing
preApplyContent fp _ =
    Just <$> T.readFile fp

postApplyContent :: FilePath -> FileChangeAction -> IO (Maybe T.Text)
postApplyContent _ FileChangeDeleted = return Nothing
postApplyContent fp _ =
    Just <$> T.readFile fp

generateBundleChanges :: TempRepositoryId
                      -> HM.HashMap FilePath (Maybe T.Text)
                      -> HM.HashMap FilePath (Maybe T.Text)
                      -> [BundleChange]
generateBundleChanges repoId preApply postApply =
    map (\(fp, mCt) ->
             case HM.lookup fp postApply of
               Nothing ->
                   error $ "generateBundleChanges failed! Missing file " ++ fp ++ " in post-apply repo!"
               Just mNewCt ->
                   BundleChange fp mCt mNewCt repoId
        ) (HM.toList preApply)

mkTempRepo :: String -> T.Text -> BS.ByteString -> FrozoneAction ()
mkTempRepo repo email patchBundle =
    do st <- getState
       targetIdent <- liftIO $ randomB16Ident 10
       let targetDir = (fc_storageDir $ fs_config st) </> targetIdent
           withDarcs = withProgResult "darcs" (json . FrozoneError . T.pack)
       withDarcs ["get", "--lazy", repo, targetDir] $ \_ ->
           do now <- liftIO getCurrentTime
              let bundleLoc = targetDir </> "patches.dpatch"
                  changeMap = getFileChangeInfo patchBundle
              preApply <- liftIO $ loadChangeInfo changeMap targetDir preApplyContent
              liftIO $ BS.writeFile bundleLoc patchBundle
              withDarcs ["apply", "--repodir", targetDir, bundleLoc] $ \_ ->
                  liftIO $ doLog LogInfo ("Patches applied!")
              withDarcs ["changes", "--repodir", targetDir] $ \changes ->
                  do let changesHash =
                             T.decodeUtf8 $ B16.encode $ SHA1.hash $ BSC.pack changes
                         rp =
                             TempRepository
                             { tempRepositoryBranch = T.pack repo
                             , tempRepositoryPath = targetDir
                             , tempRepositoryCreatedOn = now
                             , tempRepositoryNotifyEmail = [email]
                             , tempRepositoryChangesHash = changesHash
                             , tempRepositoryPatchBundle = T.decodeUtf8 patchBundle
                             , tempRepositoryBuildEnqueuedOn = Nothing
                             , tempRepositoryBuildStartedOn = Nothing
                             , tempRepositoryBuildFailedOn = Nothing
                             , tempRepositoryBuildSuccessOn = Nothing
                             , tempRepositoryBuildMessage = ""
                             , tempRepositoryPatchCanceledOn = Nothing
                             , tempRepositoryPatchCancelReason = Nothing
                             , tempRepositoryDockerBaseImage = Nothing
                             , tempRepositoryDockerImage = Nothing
                             }

                     mRepo <- runSQL $ DB.getBy (UniqueChangesHash changesHash)
                     let scheduleBuild dbId =
                          do liftIO $ doLog LogNote ("Will schedule build for " ++ show changesHash ++ " (" ++ targetDir ++ ")")
                             spockHeart <- getSpockHeart
                             _ <- liftIO $ forkIO (launchBuild st (runSpockIO spockHeart . runSQL) dbId rp)
                             return ()
                     case mRepo of
                       Just sameRepoEntity ->
                           let sameRepo = entityVal sameRepoEntity
                               sameRepoId = entityKey sameRepoEntity
                           in if (isJust $ tempRepositoryPatchCanceledOn sameRepo)
                              then do runSQL $ DB.update sameRepoId [ TempRepositoryNotifyEmail =. nub (email : tempRepositoryNotifyEmail sameRepo)
                                                                    , TempRepositoryBuildEnqueuedOn =. Nothing
                                                                    , TempRepositoryBuildStartedOn =. Nothing
                                                                    , TempRepositoryBuildFailedOn =. Nothing
                                                                    , TempRepositoryBuildSuccessOn =. Nothing
                                                                    , TempRepositoryBuildMessage =. ""
                                                                    , TempRepositoryPatchCanceledOn =. Nothing
                                                                    , TempRepositoryPatchCancelReason =. Nothing
                                                                    , TempRepositoryDockerBaseImage =. Nothing
                                                                    , TempRepositoryDockerImage =. Nothing
                                                                    ]
                                      scheduleBuild sameRepoId
                              else if (isJust (tempRepositoryBuildSuccessOn sameRepo) || isJust (tempRepositoryBuildFailedOn sameRepo))
                                   then liftIO $
                                        do doLog LogNote ("Build of " ++ show changesHash ++ " already completed. Sending results via email")
                                           sendNotifications (Just email) sameRepo
                                   else do liftIO $ doLog LogNote ("Build of " ++ show changesHash ++ " in progress. Adding sender to notification list")
                                           runSQL $ DB.update sameRepoId [ TempRepositoryNotifyEmail =. nub (email : tempRepositoryNotifyEmail sameRepo) ]
                       Nothing ->
                           do postApply <- liftIO $ loadChangeInfo changeMap targetDir postApplyContent
                              dbId <-
                                  do repoId <- runSQL $ DB.insert rp
                                     _ <- runSQL $ DB.insertMany (generateBundleChanges repoId preApply postApply)
                                     return repoId
                              scheduleBuild dbId

                     json (FrozoneMessage "Patchbundle recieved and enqueued for building")
