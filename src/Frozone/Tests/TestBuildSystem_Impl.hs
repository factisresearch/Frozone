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
--import Frozone.Util.Process

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import Control.Monad.Error
import Control.Exception
--import Control.Concurrent

import System.Directory
import System.FilePath
import System.Random
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy

import Test.Framework


test_add  :: IO ()
test_add = 
    withConfig $ \config ->
    bracket (assertSUCCESS $ startBuildSystem config) stopBuildSystem $ \bs ->
        do let impl = buildSysImpl bs

           tar <- fakeIncomingTar False
           _ <- assertERROR $ bs_getBuildRepositoryState impl $ BuildId "0"

           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "0") tar
           state <- (assertSUCCESS $ bs_getBuildRepositoryState impl $ BuildId "0")
           when (not $ state `elem` [BuildScheduled, BuildPreparing, Building, BuildSuccess, BuildFailed]) $ 
               fail "added build, but still in wrong state!"

           waitRes <- assertSUCCESS $
               awaitBuildRepoMaxTime 2000 (\br -> br_buildState br `elem` [BuildSuccess,BuildFailed]) (BuildId "0") (buildSysRef_refModel bs)
           assertEqual StateReached waitRes

           return ()

test_addTwice  :: IO ()
test_addTwice = 
    withConfig $ \config ->
    bracket (assertSUCCESS $ startBuildSystem config) (stopBuildSystem) $ \bs ->
        do let impl = buildSysImpl bs

           tar <- fakeIncomingTar False

           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "0") tar
           state <- (assertSUCCESS $ bs_getBuildRepositoryState impl $ BuildId "0")
           when (not $ state `elem` [BuildScheduled, BuildPreparing, Building, BuildSuccess, BuildFailed]) $ 
               fail "added build, but still in wrong state!"
           -- adding the same build again should give an error!
           _ <- assertERROR $ bs_addBuild impl (BuildId "0") tar

           waitRes <- assertSUCCESS $
               awaitBuildRepoMaxTime 2000 (\br -> br_buildState br `elem` [BuildSuccess,BuildFailed]) (BuildId "0") (buildSysRef_refModel bs)
           assertEqual StateReached waitRes

           return ()

test_addThenRebuild  :: IO ()
test_addThenRebuild = 
    withConfig $ \config ->
    bracket (assertSUCCESS $ startBuildSystem config) (stopBuildSystem) $ \bs ->
        do let impl = buildSysImpl bs

           tar <- fakeIncomingTar False

           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "0") tar
           state <- (assertSUCCESS $ bs_getBuildRepositoryState impl $ BuildId "0")
           when (not $ state `elem` [BuildScheduled, BuildPreparing, Building, BuildSuccess, BuildFailed]) $ 
               fail "added build, but still in wrong state!"
           waitRes <- assertSUCCESS $
               awaitBuildRepoMaxTime 2000 (\br -> br_buildState br `elem` [BuildSuccess,BuildFailed]) (BuildId "0") (buildSysRef_refModel bs)
           assertEqual StateReached waitRes

           -- trying to start a build that has already been finished should give an error!
           _ <- assertERROR $ bs_addBuild impl (BuildId "0") tar
           -- use bs_restart for this case:
           _ <- assertSUCCESS $ bs_restartBuild impl (BuildId "0")

           return ()

test_stop  :: IO ()
test_stop = 
    withConfig $ \config ->
    bracket (assertSUCCESS $ startBuildSystem config) (stopBuildSystem) $ \bs ->
        do let impl = buildSysImpl bs
           tar <- fakeIncomingTar False

           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "0") tar
           _ <- (assertSUCCESS $ bs_getBuildRepositoryState impl $ BuildId "0")

           -- this is not a good test. could be the build is already finished...
           assertSUCCESS $
               bs_stopBuild impl (BuildId "0")
           buildRepState <- assertSUCCESS $
               bs_getBuildRepositoryState impl (BuildId "0")
           assertEqual BuildStopped buildRepState

           --threadDelay 1000000

           return ()

test_build :: IO ()
test_build =
    withConfig $ \config ->
    bracket (assertSUCCESS $ startBuildSystem config) (stopBuildSystem) $ \bs ->
        do let impl = buildSysImpl bs

           doLog LogInfo $ "starting build 0..."
           -- building this repository should FAIL:
           tar0 <- fakeIncomingTar True
           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "0") tar0 
           state_0 <- assertSUCCESS $ bs_getBuildRepositoryState impl (BuildId "0") 
           when (not $ state_0 `elem` [BuildScheduled, BuildPreparing, Building, BuildSuccess, BuildFailed]) $ 
               fail "added build, but still in wrong state!"

           doLog LogInfo $ "wait for build 0..."
           waitRes <- assertSUCCESS $
               awaitBuildRepoMaxTime 2000 ((==BuildFailed) . br_buildState) (BuildId "0") (buildSysRef_refModel bs)
           assertEqual StateReached waitRes

           doLog LogInfo $ "starting build 1..."
           -- building this repository should work:
           tar1 <- fakeIncomingTar False
           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "1") tar1 
           state_1 <- assertSUCCESS $ bs_getBuildRepositoryState impl (BuildId "1") 
           when (not $ state_1 `elem` [BuildScheduled, BuildPreparing, Building, BuildSuccess, BuildFailed]) $ 
               fail "added build, but still in wrong state!"

           doLog LogInfo $ "wait for build 1..."
           waitRes2 <- assertSUCCESS $
               awaitBuildRepoMaxTime 2000 ((==BuildSuccess) . br_buildState) (BuildId "1") (buildSysRef_refModel bs)
           assertEqual StateReached waitRes2

           return ()

-- what does this test test?
test_concurrentBuilds :: IO ()
test_concurrentBuilds =
    withConfig $ \config ->
    bracket (assertSUCCESS $ startBuildSystem config) (stopBuildSystem) $ \bs ->
        do let impl = buildSysImpl bs

           tar0 <- fakeIncomingTar False
           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "0") tar0
           tar1 <- fakeIncomingTar False
           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "1") tar1
           tar2 <- fakeIncomingTar False
           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "2") tar2

           waitRes <- assertSUCCESS $ awaitMaxTimeOrErr 2000 (\(model,_) -> cond model) (buildSysRef_refModel bs)
           assertEqual StateReached $ waitRes
    where
        cond :: Monad m => BuildSystemState -> ErrorT ErrMsg m Bool
        cond buildSystem =
            do buildRepositories <- mapM ((flip getBuildRepository) buildSystem . BuildId . show ) $ ([0..2] :: [Int])
               return $ and $ map ((==BuildSuccess) . br_buildState) buildRepositories

test_getBuildQueue  :: IO ()
test_getBuildQueue = 
    withConfig $ \config ->
    bracket (assertSUCCESS $ startBuildSystem config) (stopBuildSystem) $ \bs ->
        do let impl = buildSysImpl bs

           tar0 <- fakeIncomingTar False
           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "0") tar0
           tar1 <- fakeIncomingTar False
           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "1") tar1
           tar2 <- fakeIncomingTar False
           _ <- assertSUCCESS $ bs_addBuild impl (BuildId "2") tar2

           -- sometimes it misses one build, why?!
           allBuilds <- mapM (bs_getBuildQueue impl) [BuildScheduled, Building, BuildSuccess, BuildFailed] :: IO [[BuildId]]
           assertEqual (map (BuildId . show) ([0..2] :: [Int])) (join allBuilds)

           waitRes <- assertSUCCESS $ awaitMaxTimeOrErr 2000 (\(model,_) -> allBuildsFinished model) (buildSysRef_refModel bs)
           assertEqual StateReached $ waitRes

           -- now all builds should be successful or failed:
           partitionOfBuilds <- mapM (bs_getBuildQueue impl) [BuildSuccess, BuildFailed] :: IO [[BuildId]]
           assertEqual (map (BuildId . show) ([0..2] :: [Int])) (join partitionOfBuilds)
    where
        allBuildsFinished :: Monad m => BuildSystemState -> ErrorT ErrMsg m Bool
        allBuildsFinished buildSystem =
            do buildRepositories <- mapM ((flip getBuildRepository) buildSystem . BuildId . show) $ ([0..2] :: [Int])
               return $ and $ map ((==BuildSuccess) . br_buildState) buildRepositories

withConfig :: (BuildSystemConfig -> IO a) -> IO a
withConfig f =
    do dir <- liftM2 (</>) (return "/tmp/") (liftM (take 8 . show) (randomIO :: IO Int))
       bracket_
           (initTest dir)
           (cleanUp dir) -- wait for running builds
           (f $ buildSysConfig dir)
    where
      buildSysConfig dir =
          BuildSystemConfig
          { bsc_baseDir = bsBaseDir dir
          , bsc_incoming = incomingDir dir
          , bsc_storage = Nothing
          }

initTest dir = 
    do thereAreRelics <- liftM2 (&&) (doesFileExist dir) (doesDirectoryExist dir)
       when thereAreRelics $ removeDirectoryRecursive dir
       putStrLn $ "creating directory structure at " ++ dir
       createDirectoryIfMissing True $ dir
       createDirectory $ bsBaseDir dir
       createDirectory $ incomingDir dir
       --showDir dir

fakeIncomingTar scriptShouldFail =
    do --putStrLn $ "fakeIncomingTar params: dir=" ++ show dir ++ ", fileName=" ++ show fileName
       createTar scriptShouldFail 
       --showDir dir

cleanUp dir =
    do removeDirectoryRecursive $ dir


bsBaseDir dir = dir </> "build-repos"
incomingDir dir = dir </> "incoming"

createTar :: Bool -> IO Tar
createTar scriptShouldFail =
    do --putStrLn $ "createTar parameters: destPath=" ++ destPath
       return . Tar . BSLazy.toStrict . Tar.write =<< ((liftM $ liftM $ setExecutable) $ Tar.pack baseDir filesToAdd)
       --BS.writeFile destPath . Tar.write =<< ((liftM $ liftM $ setExecutable) $ Tar.pack baseDir filesToAdd)
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
showDir dir = 
    do (_,stdOut,_) <- runProc (doLog LogInfo) "tree" [dir]
       putStrLn stdOut
-}
--ms = 1000
