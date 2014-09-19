{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
module Frozone.Util.Concurrency.SafeGlob(
    SafeGlobT(),
    getGlob, modifyGlob, modifyGlobErr,
    forkThread,
    runSafeGlob, evalSafeGlob, execSafeGlob,
    runSafeGlobWithTVar, evalSafeGlobWithTVar, execSafeGlobWithTVar,
    runSafeGlobUnsafe, evalSafeGlobUnsafe, execSafeGlobUnsafe -- just for testing!!
) where

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Control.Monad.Error
import Control.Monad.Identity

import Control.Monad.Reader



newtype SafeGlobT glob m a = SafeGlobT { fromSafeGlob :: ReaderT (ReadState glob m) m a }
    deriving (Monad, MonadIO)

instance MonadTrans (SafeGlobT glob) where
    lift x = SafeGlobT $ lift x

data ReadState glob m
    = ReadState
    { rs_glob :: TVar glob
    , rs_toIO :: forall a . m a -> IO a
    }

{-
awaitSafeGlob :: TimeMs -> (glob -> Bool) -> SafeGlobT glob m a -> IO AwaitRes
awaitSafeGlob maxTime cond safeGlob =
    fromSafeGlob safeGlob 
-}

evalSafeGlob :: MonadIO m => SafeGlobT glob m a -> glob -> (forall b . m b -> IO b) -> m a
evalSafeGlob safeGlob startGlob toIO = liftM fst $ runSafeGlob safeGlob startGlob toIO

execSafeGlob :: MonadIO m => SafeGlobT glob m a -> glob -> (forall b . m b -> IO b) -> m glob
execSafeGlob safeGlob startGlob toIO = liftM snd $ runSafeGlob safeGlob startGlob toIO

runSafeGlob :: MonadIO m => SafeGlobT glob m a -> glob -> (forall b . m b -> IO b) -> m (a, glob)
runSafeGlob safeGlob startGlob toIO =
    do ref <- liftIO $ atomically $ newTVar startGlob
       res <- runReaderT (fromSafeGlob safeGlob) $ ReadState ref toIO
       newGlob <- liftIO $ atomically $ readTVar ref
       return (res, newGlob)

evalSafeGlobWithTVar :: MonadIO m => SafeGlobT glob m a -> TVar glob -> (forall b . m b -> IO b) -> m a
evalSafeGlobWithTVar safeGlob ref toIO = liftM fst $ runSafeGlobWithTVar safeGlob ref toIO
execSafeGlobWithTVar :: MonadIO m => SafeGlobT glob m a -> TVar glob -> (forall b . m b -> IO b) -> m glob
execSafeGlobWithTVar safeGlob ref toIO = liftM snd $ runSafeGlobWithTVar safeGlob ref toIO

runSafeGlobWithTVar :: MonadIO m => SafeGlobT glob m a -> TVar glob -> (forall b . m b -> IO b) -> m (a, glob)
runSafeGlobWithTVar safeGlob ref toIO =
    do res <- runReaderT (fromSafeGlob safeGlob) $ ReadState ref toIO
       newGlob <- liftIO $ atomically $ readTVar ref
       return (res, newGlob)

evalSafeGlobUnsafe :: MonadIO m => SafeGlobT glob m a -> glob -> (forall b . m b -> IO b) -> m (a, TVar glob)
evalSafeGlobUnsafe safeGlob startGlob toIO =
    do (res, _, ref) <- runSafeGlobUnsafe safeGlob startGlob toIO
       return (res, ref)

execSafeGlobUnsafe :: MonadIO m => SafeGlobT glob m a -> glob -> (forall b . m b -> IO b) -> m (glob, TVar glob)
execSafeGlobUnsafe safeGlob startGlob toIO =
    do (_, glob, ref) <- runSafeGlobUnsafe safeGlob startGlob toIO
       return (glob, ref)

runSafeGlobUnsafe :: MonadIO m => SafeGlobT glob m a -> glob -> (forall b . m b -> IO b) -> m (a, glob, TVar glob)
runSafeGlobUnsafe safeGlob startGlob toIO =
    do ref <- liftIO $ atomically $ newTVar startGlob
       res <- runReaderT (fromSafeGlob safeGlob) $ ReadState ref toIO
       newGlob <- liftIO $ atomically $ readTVar ref
       return (res, newGlob, ref)

getGlob :: MonadIO m => SafeGlobT glob m glob
getGlob =
    SafeGlobT $
        do ref <- asks rs_glob
           lift $ liftIO $ atomically $ readTVar ref

modifyGlob :: MonadIO m => (glob -> glob) -> SafeGlobT glob m ()
modifyGlob f =
    SafeGlobT $ 
        do ref <- asks rs_glob
           lift $ liftIO $ atomically $ modifyTVar ref f

modifyGlobErr :: (Error err, MonadIO m) => (glob -> ErrorT err Identity glob) -> ErrorT err (SafeGlobT glob m) ()
modifyGlobErr f = do
    do mErr <- lift $ SafeGlobT $ 
           do ref <- asks rs_glob
              lift $ liftIO $ atomically $
                  do glob <- readTVar ref
                     case runIdentity $ runErrorT $ f glob of
                       Left err -> return $ Just err
                       Right res ->
                           do writeTVar ref res
                              return $ Nothing
       case mErr of
         Nothing -> return ()
         Just err -> throwError err 

forkThread :: MonadIO m
    => SafeGlobT s m ()
    -> SafeGlobT s m ThreadId
forkThread thread =
    SafeGlobT $
    do var <- ask
       let toIO = rs_toIO var
       liftIO $ forkIO $ toIO (runReaderT (fromSafeGlob thread) var)

