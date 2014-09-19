{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Frozone.Tests.TestScheduler_Model(
    htf_thisModulesTests
) where

import Frozone.Util.Concurrency.Scheduling.Model
import Frozone.Util.ErrorHandling
import qualified Frozone.Util.Queue as Q
import qualified Data.Map as M
import qualified Data.Foldable as F

import Test.Framework
import Text.Show.Functions()

import Data.Maybe
import Data.Either
import Data.List

--import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Random
import Control.Applicative


prop_emptyIsValid maxThreads = isValidModel $ emptySchedulerData maxThreads

prop_removeFromEmpty maxThreads jobId =
    let emptyModel = emptySchedulerData maxThreads :: SchedData Int Int
    in
        (isError $ runStateT (removeJob jobId) emptyModel)

prop_nextToRunningOnEmpty maxThreads threadId =
    let emptyModel = emptySchedulerData maxThreads :: SchedData Int Int
        (res, newModel) = runState (nextToRunning threadId) emptyModel 
    in
        isNothing res && newModel == emptyModel
    where
        _ = threadId :: Int
        
prop_getAllJobsFromEmpty maxThreads =
    let emptyModel = emptySchedulerData maxThreads
    in
        ([],[]) == getAllJobs emptyModel

-- check if all operations preserve the validity of the model:

prop_setSchedulerThread_preservesModel schedData threadId=
    preservesValidModel (setSchedulerThread threadId) schedData
    where
        _ = schedData :: SchedData Int Int

prop_addToTasksPreservesModel schedData task =
    preservesValidModel (execState (addToTasks task)) schedData
    where
        _ = schedData :: SchedData Int Int

prop_removeJob_preservesModel schedData jobId =
    preservesValidModelOrErr (execStateT $ removeJob jobId) schedData
    where
        _ = schedData :: SchedData Int Int

prop_nextToRunning_preservesModel schedData threadId =
    preservesValidModel (execState $ nextToRunning threadId) schedData
    where
        _ = schedData :: SchedData Int Int

-- check for the correct behaviour of the operations:

prop_setSchedulerThread schedData threadId =
    sched_threadId (setSchedulerThread threadId schedData) == Just threadId
    where
        _ = schedData :: SchedData Int Int

prop_addToTasks schedData task =
    let (jobId, newModel) = runState (addToTasks task) schedData
    in
        getJob jobId newModel == (Just $ Left $ task)
    where
        _ = schedData :: SchedData Int Int

prop_removeJob schedData jobId =
    let
        mResAndNewModel = runStateT (removeJob jobId) schedData
    in
        case jobId `elem` allJobs schedData of
          False -> isLeft $ runError mResAndNewModel
          True ->
              case runError mResAndNewModel of
                Left _ -> False
                Right (_, newModel) ->
                  allJobs newModel == (allJobs schedData) \\ [jobId]
    where
        allJobs = uncurry (++) . getAllJobs
        _ = schedData :: SchedData Int Int

prop_nextToRunning schedData threadId =
    if M.size (sched_running schedData) < sched_maxThreads schedData
      then
          if sched_tasks schedData /= Q.empty
            then isJust $ evalState (nextToRunning threadId) schedData
            else isNothing $ evalState (nextToRunning threadId) schedData
      else isNothing $ evalState (nextToRunning threadId) schedData
    where
        _ = schedData :: SchedData Int Int

prop_getJob schedData jobId = 
    case (jobId `elem` uncurry (++) (getAllJobs schedData)) of
        False -> (isNothing $ getJob jobId schedData)
        True ->
            case [(jobId `elem`)] <*> ([fst, snd] <*> [getAllJobs schedData]) of
              [True, False] -> (isJustLeft $ getJob jobId schedData)
              [False, True] -> (isJustRight $ getJob jobId schedData)
              _ -> False
    where
        _ = schedData :: SchedData Int Int

prop_testArbitraryModel model =
    isValidModel model
    where
        _ = model :: SchedData Int Int

preservesValidModel f schedData = isValidModel $ f $ schedData
preservesValidModelOrErr f schedData = either (const True) isValidModel $
    (runError $ f $ schedData)

isValidModel schedData = 
    (map fst $ F.toList (sched_tasks schedData)) `intersect` (M.keys $ sched_running schedData)
        == []
    &&
        let allJobs = uncurry (++) $ getAllJobs schedData
        in
            not $ sched_jobCounter schedData `elem` (map fromJobId $ allJobs)
    &&
        sched_maxThreads schedData > 0
    &&
        M.size (sched_running schedData) < sched_maxThreads schedData

{-
maximumWithDef def (x:xs) = maximum (x:xs)
maximumWithDef def _ = def
-}
isError :: ErrM err a -> Bool
isError = isLeft . runError

isJustLeft (Just (Left _)) = True
isJustLeft _ = False
isJustRight (Just (Right _)) = True
isJustRight _ = False

isLeft (Left _) = True
isLeft _ = False


instance (Arbitrary a) => Arbitrary (Q.Queue (JobId,a)) where
    arbitrary = return . Q.fromList =<< arbitrary

instance (Arbitrary a, Arbitrary threadId) => Arbitrary (M.Map JobId (Thread a threadId)) where
    arbitrary = return . M.fromList =<< arbitrary

instance (Arbitrary  a, Arbitrary threadId) => Arbitrary (Thread a threadId) where
    arbitrary =
        do task <- arbitrary
           threadId <- arbitrary
           return $ Thread task threadId

instance (Arbitrary a, Arbitrary threadId) => Arbitrary (SchedData a threadId) where
    arbitrary =
        do maxThreads <- return . getPositive =<< arbitrary
           tasks <- arbitrary -- :: Gen [Task a]
           randomSeed <- arbitrary
           threadIds <- listOf $ arbitrary -- :: [threadId]
           let
             (taskList, runningListX) =
                 evalRand (randomDivide (map JobId [0..] `zip` tasks))
                     (mkStdGen randomSeed) -- :: ([(Int,Task a)], [(Int, Task a)])
             runningListY = zipWith (\(a,b) c -> (a,b,c)) runningListX threadIds
             runningList = map (\(jobId, task, threadId) -> (jobId, Thread task threadId)) runningListY
           jobCounter <-
               return . (+ length tasks)
               =<< return . getPositive
               =<< arbitrary
           threadId <- arbitrary
           return $
               SchedData
               { sched_maxThreads = maxThreads
               , sched_tasks = Q.fromList taskList
               , sched_running = M.fromList runningList
               , sched_threadId = threadId
               , sched_jobCounter = jobCounter
                }

randomDivide :: forall m a . (MonadRandom m) => [a] -> m ([a],[a])
randomDivide values =
    do listOfBools <- getRandoms :: m [Bool]
       return $
           partitionEithers $ 
           zipWith zipF values listOfBools
    where
       zipF val bool = if bool then Left val else Right val 

instance (Show a, Show threadId) => Show (SchedData a threadId) where
    show SchedData
        { sched_maxThreads = maxThreads
        , sched_tasks = tasks
        , sched_running = running
        , sched_threadId = mThreadId
        , sched_jobCounter = jobCounter
        } =
        "{ " ++
            intercalate ", "
            [ "sched_maxThreads = " ++ show maxThreads
            , "sched_tasks = " ++ show tasks
            , "sched_running = " ++ show running
            , "sched_threadId = " ++ show mThreadId
            , "sched_jobCounter = " ++ show jobCounter
            ]
        ++ " }"
