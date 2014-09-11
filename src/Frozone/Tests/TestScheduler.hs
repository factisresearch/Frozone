{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Frozone.Tests.TestScheduler_Model

main = htfMain htf_importedTests
