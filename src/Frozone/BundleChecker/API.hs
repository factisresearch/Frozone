{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Frozone.BundleChecker.API ( bundleApi ) where

import Frozone.BundleChecker.Implementation

import Frozone.Types

import Frozone.Util.Db
import Frozone.Util.Rest
import Frozone.Util.Logging
import Frozone.Util.Email

import Web.Spock
import Web.Spock.Worker

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as BS
import Control.Monad.IO.Class


bundleApi :: String -> FrozoneApp (WorkQueue BuildRepositoryId)
bundleApi currentRoute =
    do st <- getState
       runSQL $ closeDangelingActions (fc_mailConfig $ fs_config st)
       let concurrentBuilds = fc_concurrentBuilds $ fs_config st
           bundleQueueLength = concurrentBuilds * 3
           patchQueueLenght = bundleQueueLength * 5

       bundleQueue <- startBundleWorker bundleQueueLength
         newBundleArrived
       patchQueue <- startPatchWorker patchQueueLenght concurrentBuilds
         buildPatchBundle -- FrozoneApp (WorkQueue BuildRepositoryId)

       userRoute POST [] currentRoute "/check" $ \route ->
         withProjectFromShortName "projShortName" "project not found" $ \(projId,proj) (userId,user) ->
           do bundleCheckAction route ((userId,user), (projId,proj)) bundleQueue patchQueue
              answerAndLog (Just $ userName user)
                ("checked in patch bundle for project \"" ++ T.unpack (projectName proj)++ "\"")
                FrozoneCmdCheck
       return patchQueue

startBundleWorker :: Int -> (FrozoneQueueWorker NewBundleArrived) -> FrozoneApp (WorkQueue NewBundleArrived)
startBundleWorker queueLength bundleHandler = 
    do st <- getState
       newWorker
         (WorkerConfig queueLength WorkerNoConcurrency)
         bundleHandler $
         ErrorHandlerIO $ \errorMsg newBundle ->
           do doLog LogError $ "BundleWorker error: " ++ errorMsg
              sendEmail (fc_mailConfig $ fs_config st) emailFrom [userEmail $ snd $ _nba_user newBundle] "Bundle Error" (T.concat ["Build failed! \n\n ", T.pack errorMsg])
              return WorkError

startPatchWorker :: Int -> Int -> (FrozoneQueueWorker BuildRepositoryId) -> FrozoneApp (WorkQueue BuildRepositoryId)
startPatchWorker queueLength concurrencyBound repoHandler =
    do st <- getState
       newWorker
         (WorkerConfig queueLength (WorkerConcurrentBounded concurrencyBound))
         repoHandler $
         ErrorHandlerSpock $ \errorMsg buildRepoId ->
           do doLog LogError $ "Build in " ++ show buildRepoId ++ " failed: " ++ errorMsg
              runSQL $
                     do updateBuildState buildRepoId BuildFailed (T.pack errorMsg)
                        sendNotifications (fc_mailConfig $ fs_config st) buildRepoId
              return WorkError

{- |* read patch bundle from http message
    * pack patch bundle into a "NewBundleArrived"
    * add this "NewBundleArrived" to bundleQueue
-}
bundleCheckAction :: String -> ((UserId,User),(ProjectId,Project)) -> WorkQueue NewBundleArrived -> WorkQueue BuildRepositoryId -> FrozoneAction ()
bundleCheckAction route ((userId,user),(projId,proj)) bundleQueue patchQueue =
    do let usersInProject = projectUsers proj :: [UserId]
       if not (userId `elem` usersInProject)
       then errorInRoute LogNote (Just $ userName user) route "check action not executed. reason: user not part of the project" "user not in project"
       else
         do allFiles <- files
            case HM.lookup "patch-bundle" allFiles of
              Just patchBundleBS ->
                  do bs <- liftIO $ BS.readFile (uf_tempLocation patchBundleBS)
                     addWork WorkNow (NewBundleArrived (projId,proj) (userId,user) bs patchQueue) bundleQueue
                     json (FrozoneInfo "Patch bundle will now be processed!")
              Nothing ->
                  errorInRoute LogError (Just $ userName user) route
                    "no patch-bundle sent" "no patch-bundle sent"
