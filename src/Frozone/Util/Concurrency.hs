{-# LANGUAGE Rank2Types #-}
module Frozone.Util.Concurrency(
    AwaitRes(..), TimeMs,
    await, awaitMaxTime, awaitMaxTimeOrErr
) where

import Frozone.Util.ErrorHandling

import Control.Monad.Error
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent


data AwaitRes
    = StateReached
    | TimeOut
  deriving (Show, Eq)

type TimeMs = Int

awaitMaxTimeOrErr :: Error err => TimeMs -> (forall m . Monad m => a -> ErrorT err m Bool) -> TVar a -> ErrorT err IO AwaitRes
awaitMaxTimeOrErr maxTime cond ref =
    do timeOutVar <- lift $ atomically $ newTVar $ False
       lift $ forkIO $ threadDelay (maxTime * ms) >> atomically (writeTVar timeOutVar True)
       ErrorT $ atomically $
           do var <- readTVar ref
              eitherCond <- runErrorT $ cond var
              handleEither eitherCond (return . Left) $ \res ->
                  if res
                    then return $ Right $ StateReached
                    else
                        (readTVar timeOutVar) >>= \isTimeOut ->
                            if isTimeOut
                              then return $ Right $ TimeOut
                              else retry

awaitMaxTime :: TimeMs -> (a -> Bool) -> TVar a -> IO AwaitRes
awaitMaxTime maxTime cond ref =
    do timeOutVar <- atomically $ newTVar $ False
       forkIO $ threadDelay (maxTime * ms) >> atomically (writeTVar timeOutVar True)
       atomically $
           do var <- readTVar ref
              if cond var 
                then return $ StateReached
                else
                    (readTVar timeOutVar) >>= \isTimeOut ->
                        if isTimeOut
                          then return $ TimeOut
                          else retry

{-
awaitOrErr :: (a -> Bool) -> TVar a -> STM ()
awaitOrErr cond ref =
    do var <- readTVar ref
       if cond var 
         then return () 
         else retry
-}

await :: (a -> Bool) -> TVar a -> STM ()
await cond ref =
    do var <- readTVar ref
       if cond var 
         then return () 
         else retry

ms = 1000

