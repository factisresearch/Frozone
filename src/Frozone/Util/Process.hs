{-# LANGUAGE OverloadedStrings #-}
module Frozone.Util.Process where

import Frozone.Util.Logging

import Control.Monad.Trans
import Data.List (intercalate)
import System.Exit
import System.Process

runProc p args =
    do doLog LogNote (p ++ " " ++ (intercalate " " args))
       d@(ec, stdout, stderr) <- readProcessWithExitCode p args ""
       case ec of
         ExitSuccess ->
             doLog LogTrace stdout
         ExitFailure _ ->
             doLog LogError stderr
       return d

withProgResult p errHandler cmd onSuccess =
    do (ec, resp, stderr) <- liftIO $ runProc p cmd
       case ec of
         ExitSuccess ->
             onSuccess resp
         ExitFailure _ ->
             errHandler stderr
