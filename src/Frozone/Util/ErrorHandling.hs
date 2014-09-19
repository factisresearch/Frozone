module Frozone.Util.ErrorHandling(
    module Frozone.Util.ErrorHandling
    , module Control.Monad.Error
) where

import Control.Monad.Error
import Control.Monad.Trans.Maybe
import Control.Monad.Identity

import qualified Data.Var.IO as IO
import Control.Exception


--type ErrT m = ErrorT String m
type ErrM err = ErrorT err Identity

runError = runIdentity . runErrorT

handleMaybe :: Maybe a -> b -> (a -> b) -> b
handleMaybe mVal def f = maybe def f mVal
handleEither :: Either l r -> (l -> res) -> (r -> res) -> res
handleEither eitherVal fl fr = either fl fr eitherVal

handleError :: (Error err, Monad m) => ErrorT err m a -> (err -> m a) -> m a
handleError errVal handler =
    do eitherVal <- runErrorT errVal
       case eitherVal of
         Left err -> handler err
         Right a -> return a

errToException :: ErrorT String IO a -> IO a
errToException errVal =
    do errOrVal <- runErrorT errVal
       case errOrVal of
         Left err -> fail $ err
         Right val -> return val

catchAsErr :: IO a -> ErrorT String IO a
catchAsErr ioVal =
    do ioVar <- lift $ (IO.newVar Nothing :: IO (IO.IOVar (Maybe String)))
       val <- lift $ ioVal `catch` \e  ->
           do IO.writeVar ioVar (Just $ show (e :: SomeException))
              return $ undefined
       mError <- lift $ IO.readVar ioVar
       case mError of
         Nothing ->
             return val
         Just err ->
             throwError err

catchAsMaybe :: IO a -> MaybeT IO a
catchAsMaybe ioVal =
    do errOrVal <- lift $ runErrorT $ catchAsErr ioVal
       MaybeT $ return $
           either (const $ Nothing) Just $ errOrVal

{-
returnError :: Monad m => ErrorT err m () -> -> m (Maybe err)
returnError errVal handler =
    do eitherVal <- runErrorT errVal
       case eitherVal of
         Left err -> Just err
         Right () -> Nothing
-}

maybeToErrorT :: (Error err, Monad m) => err -> Maybe a -> ErrorT err m a
maybeToErrorT err mVal =
       case mVal of
         Just val -> return val
         Nothing -> throwError err

eitherToErrorT :: (Error err', Monad m) => (err -> err') -> Either err a -> ErrorT err' m a
eitherToErrorT fl eitherVal =
       case eitherVal of
         Left err -> throwError $ fl err
         Right val -> return val

mapLeft :: (l -> l') -> Either l r -> Either l' r
mapLeft f eitherVal = case eitherVal of
    Left l -> Left $ f l
    Right r -> Right r
