{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.BuildSystem.ThreadMonad where

import Frozone.BuildSystem.Intern.Types
import Frozone.BuildSystem.API

import Frozone.Util.Concurrency.SafeGlob
import qualified Frozone.Util.Concurrency.Scheduling as Sched

import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import Control.Concurrent.STM.TVar



instance MonadIO m => Sched.Forkable (SafeGlobT glob m) where
    fork = forkThread

newtype ThreadMonadT m a = ThreadMonadT { fromThreadMonadT :: SafeGlobT BuildSystemState (ReaderT BuildSystemConfig m) a }
    deriving(Monad, MonadIO, Sched.Forkable)

instance MonadTrans ThreadMonadT where
    lift x = ThreadMonadT $ lift $ lift x



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

evalThreadMonadTWithTVar :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> TVar BuildSystemState -> (forall b . m b -> IO b) -> m a
evalThreadMonadTWithTVar threadMonad config ref toIO =
    do (res, _) <- runThreadMonadTWithTVar threadMonad config ref toIO 
       return res

execThreadMonadTWithTVar :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> TVar BuildSystemState -> (forall b . m b -> IO b) -> m BuildSystemState
execThreadMonadTWithTVar threadMonad config ref toIO =
    do (_, newModel) <- runThreadMonadTWithTVar threadMonad config ref toIO 
       return newModel

runThreadMonadTWithTVar :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> TVar BuildSystemState -> (forall b . m b -> IO b) -> m (a, BuildSystemState)
runThreadMonadTWithTVar threadMonad config ref toIO =
    runReaderT ((runSafeGlobWithTVar $ fromThreadMonadT threadMonad) ref toIO') config
    where
        toIO' x = toIO $ runReaderT x config

evalThreadMonadTUnsafe :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> BuildSystemState -> (forall b . m b -> IO b) -> m (a, TVar BuildSystemState)
evalThreadMonadTUnsafe threadMonad config startState toIO =
    do (res, _, ref) <- runThreadMonadTUnsafe threadMonad config startState toIO 
       return (res, ref)

execThreadMonadTUnsafe :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> BuildSystemState -> (forall b . m b -> IO b) -> m (BuildSystemState, TVar BuildSystemState)
execThreadMonadTUnsafe threadMonad config startState toIO =
    do (_, newModel, ref) <- runThreadMonadTUnsafe threadMonad config startState toIO 
       return (newModel, ref)

runThreadMonadTUnsafe :: MonadIO m => ThreadMonadT m a -> BuildSystemConfig -> BuildSystemState -> (forall b . m b -> IO b) -> m (a, BuildSystemState, TVar BuildSystemState)
runThreadMonadTUnsafe threadMonad config startState toIO =
    runReaderT ((runSafeGlobUnsafe $ fromThreadMonadT threadMonad) startState toIO') config
    where
        toIO' x = toIO $ runReaderT x config

getModel :: MonadIO m => ThreadMonadT m BuildSystemState
getModel = ThreadMonadT $ getGlob

getConfig :: Monad m => ThreadMonadT m BuildSystemConfig
getConfig = ThreadMonadT $ lift $ ask

modifyModel :: MonadIO m => (BuildSystemState -> BuildSystemState) -> ThreadMonadT m ()
modifyModel f = ThreadMonadT $ modifyGlob f

modifyModelErr :: (Error err, MonadIO m) => (BuildSystemState -> ErrorT err Identity BuildSystemState) -> ErrorT err (ThreadMonadT m) ()
modifyModelErr f =
    let temp = runErrorT (modifyGlobErr f) -- :: SafeGlobT BuildSystemState (ReaderT BuildSystemConfig m) (Either err ())
    in
      ErrorT $ ThreadMonadT $ temp

modifyRepo :: MonadIO m => BuildId -> (BuildRepository -> BuildRepository) -> ErrorT ErrMsg (ThreadMonadT m) ()
modifyRepo buildRepoId f =
    modifyModelErr $ \model -> updateBuildRepository buildRepoId f $ model
