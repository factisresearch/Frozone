{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.Util.Concurrency.Scheduling.Model where

import qualified Frozone.Util.Queue as Q

--import Frozone.Util.ErrorHandling
import qualified Data.Map as M

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Control.Monad
--import Control.Monad.Error


type Tasks a = Q.Queue (JobId, Task a)
type Running a = M.Map JobId (Thread a)

newtype JobId = JobId { fromJobId :: Int }
    deriving (Show, Eq, Ord)

data JobState = JobWaiting | JobRunning | JobFinished
    deriving (Eq)

newtype Task a = Task { fromTask :: a }
    deriving (Eq)
data Thread a
    = Thread
    { thread_task :: Task a
    , thread_id :: ThreadId
    }
    deriving (Eq)

data SchedData a
    = SchedData
    { sched_maxThreads :: Int
    , sched_tasks :: TVar (Tasks a)
    , sched_running :: TVar (Running a)
    , sched_threadId :: Maybe ThreadId -- id of the scheduler thread, if running
    , sched_jobCounter :: TVar Int
    }

emptySchedulerData :: Int -> STM (SchedData a)
emptySchedulerData maxThreads =
    do
        refTasks <- newTVar Q.empty
        refRunning <- newTVar M.empty
        refJobCounter <- newTVar 0
        return $
            SchedData
            { sched_maxThreads = maxThreads
            , sched_tasks = refTasks
            , sched_running = refRunning
            , sched_threadId = Nothing
            , sched_jobCounter = refJobCounter
            }

addToTasks :: SchedData a -> Task a -> STM JobId
addToTasks SchedData{ sched_tasks = refTasks, sched_jobCounter = refCounter } task =
    do jobId <- liftM JobId $ readTVar refCounter
       modifyTVar refTasks (Q.put (jobId, task))
       modifyTVar refCounter (+1)
       return jobId

removeJobFromModel :: SchedData a -> JobId -> STM (Maybe ())
removeJobFromModel SchedData{ sched_tasks = refTasks, sched_running = refRunning } jobId =
    do tasks <- readTVar refTasks
       running <- readTVar refRunning
       case (Q.filter (/=jobId) (fmap fst tasks) /= fmap fst tasks) of
         True ->
            do writeTVar refTasks $ Q.filter (\(jobId', _) -> jobId'/=jobId) tasks
               return $ Just ()
         False ->
           if jobId `M.member` running
           then
             do writeTVar refRunning $ M.delete jobId running
                return $ Just ()
           else
             return $ Nothing

{- blocks until there is a new task, AND (#threads < threadMax), move next task from tasks to running -}
nextToRunning :: SchedData a -> ThreadId -> STM (JobId, Task a)
nextToRunning SchedData { sched_maxThreads = maxThreads, sched_tasks = refTasks, sched_running = refRunning } threadId =
    do tasks <- readTVar refTasks
       running <- readTVar refRunning
       if (M.size running < maxThreads)
         then
             flip (maybe retry) (Q.take tasks) $ \((jobId, task), restTasks) ->
             do writeTVar refTasks $ restTasks
                writeTVar refRunning $ M.insert jobId (Thread { thread_task = task, thread_id = threadId }) $
                    running
                return (jobId, task)
         else retry

removeFromRunning :: SchedData a -> JobId -> STM ()
removeFromRunning SchedData{ sched_running = refRunning } jobId =
    modifyTVar refRunning $ M.delete jobId

getJob :: forall a . SchedData a -> JobId -> STM (Maybe (Either (Task a) (Thread a)))
getJob SchedData{ sched_tasks = refTasks, sched_running = refRunning } jobId =
    do tasks <- readTVar refTasks
       running <- readTVar refRunning
       let mTask = liftM snd $ Q.get $ (Q.filter (\(jobId',_) -> jobId'==jobId)) tasks :: Maybe (Task a)
       case mTask of
         Just task ->
             return $ Just $ Left $ task
         Nothing ->
           case M.lookup jobId running of
             Just thread ->
                 return $ Just $ Right $ thread
             Nothing -> return $ Nothing
