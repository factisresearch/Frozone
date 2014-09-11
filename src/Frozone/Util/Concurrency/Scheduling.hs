{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.Util.Concurrency.Scheduling(
    Model.Task(..), -- Tasks,
    --Thread(..), -- Running,
    Model.JobId(), Model.JobState(..),
    Forkable, fork,
    SchedRef(),
    runScheduler,
    stopScheduler,
    addTask,
    removeJob,
    killAllJobs,
    waitForJob, waitForJobMaxTime,
) where

--import Frozone.Util.Concurrency.Scheduling.Model hiding (SchedData)
import qualified Frozone.Util.Concurrency.Scheduling.Model as Model

import Frozone.Util.Concurrency
import Frozone.Util.Logging
import Frozone.Util.ErrorHandling
--import qualified Frozone.Util.Queue as Q
--import qualified Data.Map as M
import Data.Maybe
import Data.Either

import Control.Monad.Error
import Control.Monad.State
--import Control.Monad.Identity
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent


class (MonadIO m) => Forkable m where
    fork :: m () -> m ThreadId

--newtype SchedData a = SchedData { fromSchedData :: Model.SchedData a ThreadId }
newtype SchedRef a = SchedRef { fromSchedRef :: TVar (Model a) }

type Model a = Model.SchedData a ThreadId
type Task a = Model.Task a
type JobId = Model.JobId
type JobState = Model.JobState
type ErrMsg = Model.ErrMsg


runScheduler :: Forkable m => Int -> (a -> m ()) -> m (SchedRef a)
runScheduler maxThreads f =
    do doLog LogInfo $ "SCHEDULER: runScheduler"
       refModel <- liftIO $ atomically $
           liftM SchedRef $ newTVar $ Model.emptySchedulerData maxThreads
       threadId <- fork (scheduler refModel f)
       doLog LogInfo $ "SCHEDULER: end of runScheduler"
       liftIO $ atomically $
           modifySchedData refModel $ modify $ Model.setSchedulerThread threadId
       return refModel

{- |this stops the scheduler thread. If you want to kill all jobs as well, use killAllJobs before -}
stopScheduler :: (SchedRef a) -> ErrorT ErrMsg IO ()
stopScheduler schedRef =
    do mThreadId <- lift $ return . Model.sched_threadId =<< atomically (readTVar $ fromSchedRef schedRef)
       doLog LogInfo $ "SCHEDULER: stop"
       case mThreadId of
         Nothing -> throwError "scheduler not running!"
         Just threadId ->
             do doLog LogInfo $ "SCHEDULER: killing scheduler thread"
                lift $ killThread threadId
       doLog LogInfo $ "SCHEDULER: end of stop"

scheduler :: Forkable m => SchedRef a -> (a -> m b) -> m ()
scheduler schedRef f =
    forever $
    do
       threadId <- liftIO $ myThreadId
       (jobId, nextTask) <- liftIO $ atomically $
           do mStartedJob <- modifySchedData schedRef $ Model.nextToRunning threadId
              handleMaybe mStartedJob retry return
       doLog LogInfo $ "SCHEDULER: adding " ++ show jobId
       fork $
           do f $ Model.fromTask nextTask
              err <- liftIO $ atomically $ modifySchedDataErr schedRef $ Model.removeJob jobId
              handleEither (runError err)
                  (const $ doLog LogInfo $ "SCHEDULER: consistency error while removing job")
                  (const $ doLog LogInfo $ "SCHEDULER: finished job " ++ show jobId)


addTask :: SchedRef a -> Task a -> IO JobId
addTask schedRef task =
    atomically $ modifySchedData schedRef $ Model.addToTasks task 

removeJob :: MonadIO m => SchedRef a -> JobId -> ErrorT ErrMsg m ()
removeJob schedRef jobId =
    do errOrMaybeThreadId <- liftIO $ atomically $
           modifySchedDataErr schedRef $ Model.removeJob jobId
       case runError errOrMaybeThreadId of
         Left err -> throwError err
         Right (Just threadId) -> liftIO $ killThread threadId
         Right Nothing -> return ()


killAllJobs :: SchedRef a -> IO ()
killAllJobs schedRef = 
    do allThreadIds <-
           atomically $
           do (tasks, running) <- return . Model.getAllJobs =<< readTVar (fromSchedRef schedRef)
              mapM (modifySchedDataErr schedRef . Model.removeJob) $ tasks
              mapM (modifySchedDataErr schedRef . Model.removeJob) $ running
       let temp = map runError allThreadIds -- :: [Either (Maybe ThreadId)]
       mapM_ killThread $ catMaybes $ rights $ temp

waitForJobMaxTime :: SchedRef a -> TimeMs -> JobState -> JobId -> IO AwaitRes
waitForJobMaxTime = undefined

waitForJob :: SchedRef a -> JobState -> JobId -> IO ()
waitForJob = undefined


-----------------------------------------------------------------------------
-- Internals
-----------------------------------------------------------------------------

modifySchedData :: SchedRef a -> (State (Model a) res) -> STM res
modifySchedData schedRef stateTransf = 
    do let ref = fromSchedRef schedRef
       model <- readTVar ref
       let (res, newModel) = runState stateTransf model
       writeTVar ref newModel
       return res

modifySchedDataErr :: forall err a res . Error err => SchedRef a -> StateT (Model a) (ErrM err) res -> STM (ErrM err res)
modifySchedDataErr schedRef stateTransf =
    do let ref = fromSchedRef schedRef
       model <- readTVar ref
       let errOrResAndNewModel = runError $ runStateT stateTransf model -- :: (Either err (res, (SchedData a)))
       --errOrResAndNewModel <- return $ mNewRes
       case errOrResAndNewModel of
         Left err ->
             return $ throwError err
         Right (res, newModel) -> 
             do writeTVar ref newModel
                return $ return $ res

{-
waitForJobMaxTime :: SchedData a -> TimeMs -> JobState -> JobId -> IO AwaitRes
waitForJobMaxTime schedData maxTime jobState jobId =
    case jobState of
      JobWaiting ->
          awaitMaxTime
            maxTime
            (\tasks -> not $ Q.null $ Q.filter (\(jobId',_) -> jobId' == jobId) $ tasks)
            (sched_tasks schedData)
      JobRunning -> awaitMaxTime maxTime (M.member jobId) (sched_running schedData)
      JobFinished ->
          awaitMaxTime maxTime (not . M.member jobId) (sched_running schedData)

waitForJob :: SchedData a -> JobState -> JobId -> IO ()
waitForJob schedData jobState jobId =
    case jobState of
      JobWaiting ->
          atomically $ await
            (\tasks -> not $ Q.null $ Q.filter (\(jobId',_) -> jobId' == jobId) $ tasks)
            (sched_tasks schedData)
      JobRunning ->
          atomically $
              await (M.member jobId) (sched_running schedData)
      JobFinished ->
          atomically $
              await (not . M.member jobId) (sched_running schedData)

killAllJobs :: SchedData a -> IO ()
killAllJobs schedData =
    do allThreadIds <-
           atomically $
           do (tasks, running) <- getAllJobs schedData
              mapM (removeJobPrivate schedData) $ tasks
              mapM (removeJobPrivate schedData) $ running
       mapM_ killThread $ catMaybes $ rights allThreadIds

removeJobPrivate schedData jobId =
    do mTaskOrThread <- getJob schedData jobId
       case mTaskOrThread of
         Nothing -> return $ Left $ "job " ++ show jobId ++ " not found!" :: STM (Either ErrMsg (Maybe ThreadId))
         Just taskOrThread ->
             case taskOrThread of
               Left _ ->
                 do removeJobFromModel schedData jobId
                    return $ Right $ Nothing
               Right thread -> 
                 do removeJobFromModel schedData jobId
                    return $ Right $ Just $ thread_id thread
-}
 
{-
waitForAll :: TVar (Tasks a) -> TVar (Running a) -> IO ()
waitForAll refTasks refRunning =
    atomically $
    do tasks <- readTVar refTasks
       running <- readTVar refRunning
       if not (Q.null tasks && M.null running)
         then retry
         else return ()
-}
