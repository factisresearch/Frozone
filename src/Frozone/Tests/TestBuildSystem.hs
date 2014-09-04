{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
--import Test.Framework.BlackBoxTest

import {-@ HTF_TESTS @-} Frozone.Tests.TestBuildSystem_Model
import {-@ HTF_TESTS @-} Frozone.Tests.TestBuildSystem_Impl

main = htfMain htf_importedTests
