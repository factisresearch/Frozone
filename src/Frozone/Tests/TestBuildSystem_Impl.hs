{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Frozone.Tests.TestBuildSystem_Impl(
    htf_thisModulesTests
) where

import Frozone.BuildSystem.API
import Frozone.BuildSystem.Impl
import Frozone.BuildSystem.Intern.Model -- is this really necessary?
import Frozone.BuildTypes

import Frozone.Util.Testing
import Frozone.Util.Logging
import Frozone.Util.Concurrency

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import Control.Monad.Error
import Control.Exception

import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as BS

import Test.Framework


-- > preparing ... ready
test_add  :: IO ()
test_add = 
    withConfig $ \config ->
        do bs <- startBuildSystem config
           let impl = buildSysImpl bs
           --clearBuildSystem bs

           fakeIncomingTar False (bsc_incoming config) "test.tar"
           _ <- assertError $ bs_getBuildRepositoryState impl $ BuildId 0

           _ <- assertNoError $ bs_addBuild impl (BuildId 0) (TarFile "test.tar")
           state <- (assertNoError $ bs_getBuildRepositoryState impl $ BuildId 0)
           when (not $ state `elem` [BuildScheduled, BuildPreparing, Building, BuildSuccess, BuildFailed]) $ 
               fail "added build, but still in wrong state!"

           waitRes <- assertNoError $
               awaitBuildRepoMaxTime 2000 (\br -> br_buildState br `elem` [BuildSuccess,BuildFailed]) (BuildId 0) (buildSysRef_refModel bs)
           assertEqual StateReached waitRes

           assertNoError $ stopBuildSystem bs
           return ()

test_build :: IO ()
test_build =
    withConfig $ \config ->
        do bs <- startBuildSystem config
           let impl = buildSysImpl bs

           doLog LogInfo $ "starting build 0..."
           -- building this repository should FAIL:
           fakeIncomingTar True (bsc_incoming config) "0.tar"
           _ <- assertNoError $ bs_addBuild impl (BuildId 0) (TarFile "0.tar")
           state_0 <- assertNoError $ bs_getBuildRepositoryState impl (BuildId 0) 
           when (not $ state_0 `elem` [BuildScheduled, BuildPreparing, Building, BuildSuccess, BuildFailed]) $ 
               fail "added build, but still in wrong state!"

           doLog LogInfo $ "wait for build 0..."
           waitRes <- assertNoError $
               awaitBuildRepoMaxTime 1500 ((==BuildFailed) . br_buildState) (BuildId 0) (buildSysRef_refModel bs)
           assertEqual StateReached waitRes

           doLog LogInfo $ "starting build 1..."
           -- building this repository should work:
           fakeIncomingTar False (bsc_incoming config) "1.tar"
           _ <- assertNoError $ bs_addBuild impl (BuildId 1) (TarFile "1.tar")
           state_1 <- assertNoError $ bs_getBuildRepositoryState impl (BuildId 1) 
           when (not $ state_1 `elem` [BuildScheduled, BuildPreparing, Building, BuildSuccess, BuildFailed]) $ 
               fail "added build, but still in wrong state!"

           doLog LogInfo $ "wait for build 1..."
           waitRes2 <- assertNoError $
               awaitBuildRepoMaxTime 1500 ((==BuildSuccess) . br_buildState) (BuildId 1) (buildSysRef_refModel bs)
           assertEqual StateReached waitRes2

           assertNoError $ stopBuildSystem bs
           return ()

test_concurrentBuilds :: IO ()
test_concurrentBuilds =
    withConfig $ \config ->
        do bs <- startBuildSystem config
           let impl = buildSysImpl bs

           fakeIncomingTar False (bsc_incoming config) "0.tar"
           _ <- assertNoError $ bs_addBuild impl (BuildId 0) (TarFile "0.tar")
           fakeIncomingTar False (bsc_incoming config) "1.tar"
           _ <- assertNoError $ bs_addBuild impl (BuildId 1) (TarFile "1.tar")
           fakeIncomingTar False (bsc_incoming config) "2.tar"
           _ <- assertNoError $ bs_addBuild impl (BuildId 2) (TarFile "2.tar")

           waitRes <- assertNoError $ awaitMaxTimeOrErr 2000 cond (buildSysRef_refModel bs)
           assertEqual StateReached $ waitRes
    where
        cond :: Monad m => BuildSystemState -> ErrorT ErrMsg m Bool
        cond buildSystem =
            do buildRepositories <- mapM ((flip getBuildRepository) buildSystem . BuildId) $ [0..2]
               return $ and $ map ((==BuildSuccess) . br_buildState) buildRepositories

withConfig :: (BuildSystemConfig -> IO a) -> IO a
withConfig f =
    bracket_
        (initTest dir)
        (cleanUp dir) -- wait for running builds
        (f buildSysConfig)
    where
      dir = "./tmp"
      buildSysConfig =
          BuildSystemConfig
          { bsc_baseDir = bsBaseDir dir
          , bsc_incoming = incomingDir dir
          }

initTest dir = 
    do createDirectory $ dir
       createDirectory $ bsBaseDir dir
       createDirectory $ incomingDir dir

fakeIncomingTar scriptShouldFail dir fileName = createTar scriptShouldFail $ dir </> fileName

cleanUp dir =
    do removeDirectoryRecursive $ dir


bsBaseDir dir = dir </> "build-repos"
incomingDir dir = dir </> "incoming"

createTar :: Bool -> FilePath -> IO ()
createTar scriptShouldFail destPath =
    do --writeFile "./tmp/build.sh" content
       --setFileMode "./tmp/build.sh" $ ownerReadMode
       --setFileMode "./tmp/build.sh" $ ownerExecuteMode
       --setFileMode "./tmp/build.sh" $ ownerReadMode `intersectFileModes` ownerExecuteMode
       BS.writeFile destPath . Tar.write =<< ((liftM $ liftM $ setExecutable) $ Tar.pack baseDir filesToAdd)
       --BS.writeFile destPath . Tar.write =<< Tar.pack baseDir filesToAdd
       --removeFile "./tmp/build.sh"
    where
      baseDir =
          case scriptShouldFail of
            False -> "./res/scripts/success"
            True -> "./res/scripts/fail"
      filesToAdd = ["build.sh"]

      setExecutable entry =
          case Tar.entryContent entry of
            Tar.NormalFile _ _ -> entry{ Tar.entryPermissions = Tar.executableFilePermissions }
            _ -> entry

{-
test_getBuildQueue :: IO ()
test_getBuildQueue = 
    do clearBuildSystem
       assertEqual ([] :: [BuildId])=<< getBuildQueue

test_setBuildQueue = 
    do setBuildQueue someBuildQueue 
       assertEqual someBuildQueue =<< getBuildQueue
    where
        someBuildQueue = map BuildId [0..50]
-}

--ms = 1000
