{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.BuildSystem.ThreadMonad where

import Frozone.BuildSystem.Intern.Types
import Frozone.BuildSystem.API

import Frozone.Util.Concurrency.SafeGlob
import qualified Frozone.Util.Concurrency.Scheduling as Sched

import Frozone.Util.Logging
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import Control.Concurrent.STM.TVar


type PersistSched = Sched.SchedRef BuildSystemState

instance MonadIO m => Sched.Forkable (SafeGlobT glob m) where
    fork = forkThread

newtype ThreadMonadT m a = ThreadMonadT { fromThreadMonadT :: SafeGlobT (BuildSystemState, PersistSched) (ReaderT BuildSystemConfig m) a }
    deriving(Monad, MonadIO, Sched.Forkable)

instance MonadTrans ThreadMonadT where
    lift x = ThreadMonadT $ lift $ lift x


{-
evalThreadMonadT :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> BuildSystemState -> (forall b . m b -> IO b) -> m a
evalThreadMonadT threadMonad config startState toIO =
    liftM fst $ runThreadMonadT threadMonad config startState toIO 

execThreadMonadT :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> BuildSystemState -> (forall b . m b -> IO b) -> m BuildSystemState
execThreadMonadT threadMonad config startState toIO =
    liftM snd $ runThreadMonadT threadMonad config startState toIO 

runThreadMonadT :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> BuildSystemState -> (forall b . m b -> IO b) -> m (a, BuildSystemState)
runThreadMonadT threadMonad config startState toIO =
    runReaderT ((runSafeGlob $ fromThreadMonadT threadMonad) startState toIO') config
    where
        toIO' x = toIO $ runReaderT x config
-}

evalThreadMonadTWithTVar :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> TVar (BuildSystemState, PersistSched) -> (forall b . m b -> IO b) -> m a
evalThreadMonadTWithTVar threadMonad config ref toIO =
    do (res, _) <- runThreadMonadTWithTVar threadMonad config ref toIO 
       return res

{-
execThreadMonadTWithTVar :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> TVar BuildSystemState -> (forall b . m b -> IO b) -> m BuildSystemState
execThreadMonadTWithTVar threadMonad config ref toIO =
    do (_, newModel) <- runThreadMonadTWithTVar threadMonad config ref toIO 
       return newModel
-}

runThreadMonadTWithTVar :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> TVar (BuildSystemState, PersistSched) -> (forall b . m b -> IO b) -> m (a, BuildSystemState)
runThreadMonadTWithTVar threadMonad config ref toIO =
    do (res, (model, _)) <- runReaderT ((runSafeGlobWithTVar $ fromThreadMonadT threadMonad) ref toIO') config
       return $ (res, model)
    where
        toIO' x = toIO $ runReaderT x config

evalThreadMonadTUnsafe :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> BuildSystemState -> (forall b . m b -> IO b) -> m (a, TVar (BuildSystemState, PersistSched))
evalThreadMonadTUnsafe threadMonad config startState toIO =
    do (res, _, ref) <- runThreadMonadTUnsafe threadMonad config startState toIO 
       return (res, ref)

execThreadMonadTUnsafe :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> BuildSystemState -> (forall b . m b -> IO b) -> m (BuildSystemState, TVar (BuildSystemState, PersistSched))
execThreadMonadTUnsafe threadMonad config startState toIO =
    do (_, newModel, ref) <- runThreadMonadTUnsafe threadMonad config startState toIO 
       return (newModel, ref)

runThreadMonadTUnsafe :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> BuildSystemState -> (forall b . m b -> IO b) -> m (a, BuildSystemState, TVar (BuildSystemState, PersistSched))
runThreadMonadTUnsafe threadMonad config startState toIO =
    do let modelPath = bsc_storage config
       doLog LogInfo $ "starting safe scheduler"
       persistSched <- liftIO $ Sched.runScheduler 1 (safeModelWorker modelPath)
       (ret, (model, _), ref) <- runReaderT ((runSafeGlobUnsafe $ fromThreadMonadT threadMonad) (startState, persistSched) toIO') config
       --liftIO $ Sched.stopScheduler persistSched
       return $ (ret, model, ref)
    where
        toIO' x = toIO $ runReaderT x config

getModel :: MonadIO m => ThreadMonadT m BuildSystemState
getModel = ThreadMonadT $ return . fst =<< getGlob

getConfig :: Monad m => ThreadMonadT m BuildSystemConfig
getConfig = ThreadMonadT $ lift $ ask

modifyModel :: MonadIO m => (BuildSystemState -> BuildSystemState) -> ThreadMonadT m ()
modifyModel f =
    do ThreadMonadT $ modifyGlob $ mapToFst f
       safeModel

safeModel :: MonadIO m => ThreadMonadT m ()
safeModel =
    do --storagePath <- return . bsc_storage =<< getConfig
       model <- getModel
       sched <- getScheduler
       liftIO $ Sched.addTask sched (Sched.Task model)
       return ()

getScheduler :: MonadIO m => ThreadMonadT m PersistSched
getScheduler =
    ThreadMonadT $ return . snd =<< getGlob

modifyModelErr :: (Error err, MonadIO m) => (BuildSystemState -> ErrorT err Identity BuildSystemState) -> ErrorT err (ThreadMonadT m) ()
modifyModelErr f =
    let temp = runErrorT (modifyGlobErr $ mapToFstM f) -- :: SafeGlobT BuildSystemState (ReaderT BuildSystemConfig m) (Either err ())
    in
        (ErrorT $ ThreadMonadT $ temp)
        >> lift safeModel

modifyRepo :: MonadIO m => BuildId -> (BuildRepository -> BuildRepository) -> ErrorT ErrMsg (ThreadMonadT m) ()
modifyRepo buildRepoId f =
    modifyModelErr $ \model -> updateBuildRepository buildRepoId f $ model

safeModelWorker :: MonadIO m => Maybe FilePath -> BuildSystemState -> m ()
safeModelWorker mPath model =
    flip (maybe (return ())) mPath $ \path ->
        do doLog LogInfo "saving model!"
           liftIO $ writeFile path (show model)

mapToFst :: (a -> b) -> (a, c) -> (b, c)
mapToFst f (a,b) = (f a, b)

mapToFstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
mapToFstM f (a,c) =
    do b <- f a
       return $ (b, c)
