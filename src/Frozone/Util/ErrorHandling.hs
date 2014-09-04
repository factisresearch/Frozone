module Frozone.Util.ErrorHandling where

import Control.Monad.Error


--type ErrT m = ErrorT String m

handleMaybe :: Maybe a -> b -> (a -> b) -> b
handleMaybe mVal def f = maybe def f mVal
handleEither :: Either l r -> (l -> res) -> (r -> res) -> res
handleEither eitherVal fl fr = either fl fr eitherVal

handleError :: (Error err, Monad m) => ErrorT err m a -> (err -> m a) -> m a
handleError err handler =
    do eitherVal <- runErrorT err 
       case eitherVal of
         Left err -> handler err
         Right a -> return a

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
