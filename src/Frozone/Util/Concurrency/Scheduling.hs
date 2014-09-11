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
    waitForAll
) where

import Frozone.Util.Concurrency.Scheduling.Model(Task, JobId, JobState(..))
import qualified Frozone.Util.Concurrency.Scheduling.Model as Model

import Frozone.Util.Concurrency
import Frozone.Util.Logging( LogLevel(..) )
import qualified Frozone.Util.Logging as Log
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

instance Forkable IO where
    fork = forkIO

--newtype SchedData a = SchedData { fromSchedData :: Model.SchedData a ThreadId }
newtype SchedRef a = SchedRef { fromSchedRef :: TVar (Model a) }

type Model a = Model.SchedData a ThreadId
{-
type Task a = Model.Task a
type JobId = Model.JobId
type JobState = Model.JobState
-}
type ErrMsg = Model.ErrMsg


runScheduler :: Forkable m => Int -> (a -> m ()) -> m (SchedRef a)
runScheduler maxThreads f =
    do doLog LogInfo $ "runScheduler"
       refModel <- liftIO $ atomically $
           liftM SchedRef $ newTVar $ Model.emptySchedulerData maxThreads
       threadId <- fork (scheduler refModel f)
       doLog LogInfo $ "end of runScheduler"
       liftIO $ atomically $
           modifySchedData refModel $ modify $ Model.setSchedulerThread threadId
       return refModel

{- |this stops the scheduler thread. If you want to kill all jobs as well, use killAllJobs before -}
stopScheduler :: SchedRef a -> IO ()
stopScheduler schedRef =
    do mThreadId <- return . Model.sched_threadId =<< atomically (readTVar $ fromSchedRef schedRef)
       doLog LogInfo $ "stop"
       case mThreadId of
         Nothing -> doLog LogError "consistency error: scheduler not running!"
         Just threadId ->
             do doLog LogInfo $ "killing scheduler thread"
                killThread threadId
       doLog LogInfo $ "end of stop"

scheduler :: Forkable m => SchedRef a -> (a -> m b) -> m ()
scheduler schedRef f =
    forever $
    do
       threadId <- liftIO $ myThreadId
       (jobId, nextTask) <- liftIO $ atomically $
           do mStartedJob <- modifySchedData schedRef $ Model.nextToRunning threadId
              handleMaybe mStartedJob retry return
       doLog LogInfo $ "adding " ++ show jobId
       fork $
           do f $ Model.fromTask nextTask
              err <- liftIO $ atomically $ modifySchedDataErr schedRef $ Model.removeJob jobId
              handleEither (runError err)
                  (const $ doLog LogInfo $ "consistency error while removing job")
                  (const $ doLog LogInfo $ "finished job " ++ show jobId)


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
waitForJobMaxTime schedRef maxTime jobState jobId =
    case jobState of
      JobWaiting -> awaitMaxTime maxTime (\model -> isJustLeft $ Model.getJob jobId model) (fromSchedRef schedRef)
      JobRunning -> awaitMaxTime maxTime (\model -> isJustRight $ Model.getJob jobId model) (fromSchedRef schedRef)
      JobFinished -> awaitMaxTime maxTime (\model -> isNothing $ Model.getJob jobId model) (fromSchedRef schedRef)
    where
        isJustLeft (Just (Left _)) = True
        isJustLeft _ = False
        isJustRight (Just (Right _)) = True
        isJustRight _ = False

waitForJob :: SchedRef a -> JobState -> JobId -> IO ()
waitForJob schedRef jobState jobId =
    case jobState of
      JobWaiting -> atomically $ await (\model -> isJustLeft $ Model.getJob jobId model) (fromSchedRef schedRef)
      JobRunning -> atomically $ await (\model -> isJustRight $ Model.getJob jobId model) (fromSchedRef schedRef)
      JobFinished -> atomically $ await (\model -> isNothing $ Model.getJob jobId model) (fromSchedRef schedRef)
    where
        isJustLeft (Just (Left _)) = True
        isJustLeft _ = False
        isJustRight (Just (Right _)) = True
        isJustRight _ = False


waitForAll :: SchedRef a -> IO ()
waitForAll schedRef = 
    do allJobs <- atomically ( return . uncurry (++) =<< return . Model.getAllJobs =<< readTVar (fromSchedRef schedRef))
       mapM_ (waitForJob schedRef JobFinished) $ allJobs

-----------------------------------------------------------------------------
-- Internals
-----------------------------------------------------------------------------

doLog logLevel msg = Log.doLog logLevel $ "SCHEDULER: " ++ msg

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
       let errOrResAndNewModel = runError $ runStateT stateTransf model
       case errOrResAndNewModel of
         Left err ->
             return $ throwError err
         Right (res, newModel) -> 
             do writeTVar ref newModel
                return $ return $ res
