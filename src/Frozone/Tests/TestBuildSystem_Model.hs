{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Frozone.Tests.TestBuildSystem_Model(
    htf_thisModulesTests
) where

import Test.Framework
import Frozone.Util.Testing

import Frozone.BuildSystem.API
import Frozone.BuildSystem.Intern.Model


test_AddBuildRepository =
    do assertNoError $ addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing) emptyBuildSystemState

test_AddBuildRepositoryTwice =
    assertError $
        addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing) 
        =<< addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing) emptyBuildSystemState

test_DeleteBuildRepository =
    assertError $
        deleteBuildRepository (BuildId 0) emptyBuildSystemState


test_AddAndDeleteBuildRepository =
    assertNoError $
        deleteBuildRepository (BuildId 0) 
        =<< addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing) emptyBuildSystemState

{-
test_mapToAllBuilds =
    mapM assertEqual $
        (mapToBuildState (const BuildSuccess) $ originalBuildRep)
        emptyBuildSystemState{ buildSysSt_allBuilds = originalFieldValue }
    where
        originalFieldValue =
            [ ((BuildId 0), buildRepository (Just "test/bla") BuildPreparing)
            , ((BuildId 1), buildRepository Nothing BuildPreparing)
            ]
-}

test_mapToBuildState =
    assertEqual
        (mapToBuildState (const BuildSuccess) $ originalBuildRep)
        (originalBuildRep{ br_buildState = BuildSuccess })
    where
        originalBuildRep = buildRepository path BuildPreparing
        path = Just "test/bla"

--        =<< addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing) emptyBuildSystemState
