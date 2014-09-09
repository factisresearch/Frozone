{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Frozone.Tests.TestBuildSystem_Model(
    htf_thisModulesTests
) where

import Test.Framework
import Frozone.BuildTypes
import Frozone.Util.Testing

import Frozone.BuildSystem.API
import Frozone.BuildSystem.Intern.Model


test_AddBuildRepository =
    do assertSUCCESS $ addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing (TarFile "dummy.tar")) emptyBuildSystemState

test_AddBuildRepositoryTwice =
    assertERROR $
        addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing (TarFile "dummy.tar")) 
        =<< addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing (TarFile "dummy.tar")) emptyBuildSystemState

test_DeleteBuildRepository =
    assertERROR $
        deleteBuildRepository (BuildId 0) emptyBuildSystemState


test_AddAndDeleteBuildRepository =
    assertSUCCESS $
        deleteBuildRepository (BuildId 0) 
        =<< addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing (TarFile "dummy.tar")) emptyBuildSystemState

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
        originalBuildRep = buildRepository path BuildPreparing (TarFile "dummy.tar")
        path = Just "test/bla"

--        =<< addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing) emptyBuildSystemState
