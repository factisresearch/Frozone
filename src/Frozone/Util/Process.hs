{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frozone.Util.Process where

import Frozone.Util.Logging

--import Control.Monad.Trans
import Data.List (intercalate)
import System.Exit
import System.Process
import Control.Monad.Error


runProcErr :: MonadIO m => (String -> IO ()) -> String -> [String] -> ErrorT String m (String, String)
runProcErr logFunction prog args =
    do (exitCode, stdOut, stdErr) <- liftIO $ runProc logFunction prog args
       case exitCode of
         ExitSuccess -> return $ (stdOut, stdErr)
         ExitFailure code ->
             throwError $
                 "error while executing \"" ++ procString prog args ++ "\"! exitCode: " ++ show code ++ ", stderr:"
                 ++ stdErr

runProc :: (String -> IO ()) -> String -> [String] -> IO (ExitCode, String, String)
runProc logFunction p args =
    do logFunction $ procString p args
       d@(ec, stdout, stderr) <- readProcessWithExitCode p args ""
       case ec of
         ExitSuccess ->
             doLog LogTrace stdout
         ExitFailure _ ->
             doLog LogError stderr
       return d

procString p args = (p ++ " " ++ (intercalate " " args))

{-
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
-}
