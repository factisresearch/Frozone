{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.Util.Process where

import Frozone.Util.Logging

import Control.Exception
import Data.Conduit
import Data.Conduit.Process
import Control.Monad.Trans
import Data.List (intercalate)
import System.Exit
import System.Process
import qualified Data.ByteString as BS

runConduitProc :: String -> [String]
               -> (Consumer BS.ByteString (ResourceT IO) ())
               -> IO ExitCode
runConduitProc p args consumer =
    actionRunner `catch` (\(exitCode :: ExitCode) -> return exitCode)
    where
      procSpec = proc p args
      actionRunner =
          do doLog LogNote (p ++ " " ++ (intercalate " " args))
             runResourceT $ sourceProcess procSpec $$ consumer
             return ExitSuccess

runProc :: String -> [String] -> IO (ExitCode, String, String)
runProc p args =
    do doLog LogNote (p ++ " " ++ (intercalate " " args))
       d@(ec, stdout, stderr) <- readProcessWithExitCode p args ""
       case ec of
         ExitSuccess ->
             doLog LogTrace stdout
         ExitFailure _ ->
             doLog LogError stderr
       return d

withProgResult :: forall m a. MonadIO m
               => String -> (String -> m a) -> [String] -> (String -> m a) -> m a
withProgResult p errHandler cmd onSuccess =
    do (ec, resp, stderr) <- liftIO $ runProc p cmd
       case ec of
         ExitSuccess ->
             onSuccess resp
         ExitFailure _ ->
             errHandler stderr
