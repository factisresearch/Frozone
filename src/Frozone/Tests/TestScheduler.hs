{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Frozone.Tests.TestScheduler_Model
import {-@ HTF_TESTS @-} Frozone.Tests.TestScheduler_Impl

main = htfMain htf_importedTests
