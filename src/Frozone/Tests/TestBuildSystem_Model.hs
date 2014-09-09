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

test_getBuildsInState =
    do let allLists = map (flip getBuildsInState emptyBuildSystemState) allBuildStates
       mapM (assertEqual []) allLists
       buildSys <- assertSUCCESS $ addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing (TarFile "dummy.tar")) emptyBuildSystemState
       assertEqual [(BuildId 0)] $ getBuildsInState BuildPreparing buildSys

test_mapToBuildState =
    assertEqual
        (mapToBuildState (const BuildSuccess) $ originalBuildRep)
        (originalBuildRep{ br_buildState = BuildSuccess })
    where
        originalBuildRep = buildRepository path BuildPreparing (TarFile "dummy.tar")
        path = Just "test/bla"

--        =<< addBuildRepository (BuildId 0) (buildRepository Nothing BuildPreparing) emptyBuildSystemState
