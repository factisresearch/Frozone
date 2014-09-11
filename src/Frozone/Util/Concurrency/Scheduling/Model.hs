{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Frozone.Util.Concurrency.Scheduling.Model where

import qualified Frozone.Util.Queue as Q
import Frozone.Util.ErrorHandling

--import Frozone.Util.ErrorHandling
import qualified Data.Map as M

import qualified Data.Foldable as F
--import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Error
import Test.Framework

type ErrMsg = String


type Tasks a = Q.Queue (JobId, Task a)
type Running a threadId = M.Map JobId (Thread a threadId)

newtype JobId = JobId { fromJobId :: Int }
    deriving (Show, Eq, Ord, Arbitrary)

data JobState = JobWaiting | JobRunning | JobFinished
    deriving (Eq)


newtype Task a = Task { fromTask :: a }
    deriving (Eq, Arbitrary, Show)
data Thread a threadId
    = Thread
    { thread_task :: Task a
    , thread_id :: threadId
    }
    deriving (Eq, Show)

data SchedData a threadId 
    = SchedData
    { sched_maxThreads :: Int
    , sched_tasks :: Tasks a
    , sched_running :: Running a threadId
    , sched_threadId :: Maybe threadId -- id of the scheduler thread, if running
    , sched_jobCounter :: Int
    }
    deriving (Eq)

emptySchedulerData :: Int -> SchedData a threadId
emptySchedulerData maxThreads =
    SchedData
    { sched_maxThreads = maxThreads
    , sched_tasks = Q.empty
    , sched_running = M.empty
    , sched_threadId = Nothing
    , sched_jobCounter = 0
    }


setSchedulerThread :: threadId -> SchedData a threadId -> SchedData a threadId
setSchedulerThread threadId schedData = schedData{ sched_threadId = Just threadId }

addToTasks :: Task a -> State (SchedData a threadId) JobId
addToTasks task = state $ \schedData ->
    let
        jobId = JobId $ sched_jobCounter schedData
        newSchedData =
            mapToTasks (Q.put (jobId, task)) $
            mapToJobCounter (+1) $
            schedData
    in (jobId, newSchedData)

removeJob :: forall a threadId . JobId -> StateT (SchedData a threadId) (ErrM ErrMsg) (Maybe threadId)
removeJob jobId = 
    do model <- get
       let tasks = sched_tasks model :: Tasks a
           newTasks = Q.filter (\(jobId', _) -> jobId'/=jobId) tasks :: Tasks a
       case (fmap fst newTasks /= fmap fst tasks) of
         True ->
             do put $ model{ sched_tasks = newTasks }
                return $ Nothing
         False -> 
             let running = sched_running model in
             case M.lookup jobId running of
               Just thread ->
                   do put $ model{ sched_running = M.delete jobId running }
                      return $ Just $ thread_id thread
               Nothing ->
                   lift $ throwError "job not found!"

{- if there is a task, AND (#threads < threadMax), move one task from "tasks" to "running" -}
nextToRunning :: threadId -> State (SchedData a threadId) (Maybe (JobId, Task a))
nextToRunning threadId =
    do model <- get
       let
         tasks = sched_tasks model
         running = sched_running model
         maxThreads = sched_maxThreads model
       if M.size running < maxThreads
         then
             flip (maybe (return Nothing)) (Q.take tasks) $ \((jobId,task), newTasks) ->
             do put $
                    mapToTasks (const newTasks) $
                    mapToRunning (M.insert jobId (Thread { thread_task = task, thread_id = threadId })) $
                    model
                return $ Just (jobId, task)
         else
             return $ Nothing
              
getAllJobs :: SchedData a threadId -> ([JobId], [JobId]) -- (tasks, running)
getAllJobs model =
       let
         tasks = F.toList $ fmap fst $ sched_tasks model
         running = M.keys $ sched_running model
       in
         (tasks, running)

getJob :: forall a threadId . JobId -> SchedData a threadId -> Maybe (Either (Task a) (Thread a threadId))
getJob jobId model =
    let
        tasks = F.toList $ sched_tasks model
        running = sched_running model
    in
        case lookup jobId $ F.toList tasks of
          Just task -> Just $ Left $ task
          Nothing ->
              case M.lookup jobId running of
                Just thread -> Just $ Right $ thread
                Nothing -> Nothing

-- internal helper functions:
mapToTasks f schedData = schedData{ sched_tasks = f (sched_tasks schedData) }
mapToRunning f schedData = schedData{ sched_running = f (sched_running schedData) }
mapToThreadId f schedData = schedData{ sched_threadId = f (sched_threadId schedData) }
mapToJobCounter f schedData = schedData{ sched_jobCounter = f (sched_jobCounter schedData) }

instance Arbitrary JobState where
    arbitrary = elements [JobWaiting, JobRunning, JobFinished]
