{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE FlexibleInstances #-}
module Frozone.Tests.TestScheduler_Impl(
    htf_thisModulesTests
) where

import Test.Framework

import Frozone.Util.Concurrency.Scheduling
--import Frozone.Util.ErrorHandling
import Frozone.Util.Testing

import Control.Monad.STM
--import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
{-
import qualified Frozone.Util.Queue as Q
import qualified Data.Map as M
import qualified Data.Foldable as F

import Text.Show.Functions()

import Data.Maybe
import Data.Either
import Data.List

--import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Random
import Control.Applicative
-}


test_scheduler =
    do sched <- runScheduler 4 simpleWorker
       stopScheduler sched
       return ()

test_removeNonExistentJob =
    do sched0 <- runScheduler 4 simpleWorker
       sched1 <- runScheduler 4 simpleWorker
       jobId <- addTask sched0 (Task 42)

       assertERROR $ removeJob sched1 jobId

       stopScheduler sched0
       stopScheduler sched1

test_addTask =
    do refDebugInfo <- atomically $ newTVar $ DebuggingInfo [] 0 0
       sched <- runScheduler maxThreads (workerFunc refDebugInfo)

       let taskList = map Task [0..10]
       mapM_ (addTask sched) taskList

       waitForAll sched
       stopScheduler sched

       debugInfo <- atomically $ readTVar refDebugInfo

       assertEqual (reverse taskList) (map Task $ checkList debugInfo)
       assertCompare (<=) (maxWorkers debugInfo) maxThreads
    where
        maxThreads = 2

assertCompare cmp a b =
    when (not (a `cmp` b)) $ fail "comparison failed"

workerFunc :: TVar DebuggingInfo -> Int -> IO ()
workerFunc debugInfo x =
    let res = foldr (.) id (take 100000 $ repeat (+1)) $ x
    in
        do workerStarts debugInfo x
           putStrLn $ "f " ++ show x ++ " = " ++ show res
           workerFinished debugInfo

simpleWorker :: Int -> IO ()
simpleWorker x =
    let res = foldr (.) id (take 100000 $ repeat (+1)) $ x
    in
        putStrLn $ "f " ++ show x ++ " = " ++ show res

workerStarts refDebugInfo x =
    atomically $ modifyTVar refDebugInfo $ \debugInfo ->
        DebuggingInfo
        { checkList = x : checkList debugInfo
        , workersCount = workersCount debugInfo + 1
        , maxWorkers = max (workersCount debugInfo) (maxWorkers debugInfo)
        }

workerFinished refDebugInfo =
    atomically $ modifyTVar refDebugInfo $ mapToWorkersCount (pred)

data DebuggingInfo
    = DebuggingInfo
    { checkList :: [Int]
    , workersCount :: Int
    , maxWorkers :: Int
    }

mapToWorkersCount f debugInfo = debugInfo { workersCount = f (workersCount debugInfo) }
