module Frozone.BuildSystem.Intern.Model where

import Frozone.BuildSystem.API
import qualified Frozone.Util.Concurrency.Scheduling as Sched

import qualified Data.Map.Strict as M

import Control.Monad
import Control.Monad.Error
import Frozone.Util.ErrorHandling
--import Control.Concurrent



data BuildSystemState
    = BuildSystemState
    { buildSysSt_allBuilds :: M.Map BuildId BuildRepository
    }
  deriving (Show, Eq)

data BuildRepository
    = BuildRepository
    { br_path :: Maybe FilePath -- relative to bsc_baseDir (!)
    , br_buildState :: BuildState
    , br_incoming :: TarFilePath
    , br_thread :: Maybe Sched.JobId
    }
  deriving (Show, Eq)

type TarFilePath = FilePath

emptyBuildSystemState =
    BuildSystemState
    { buildSysSt_allBuilds = M.empty
    }

buildRepository mPath buildState tarFile =
    BuildRepository
    { br_path = mPath
    , br_buildState = buildState
    , br_incoming = tarFile
    , br_thread = Nothing
    }

addBuildRepository :: Monad m => BuildId -> BuildRepository -> BuildSystemState -> ErrT m BuildSystemState
addBuildRepository buildId buildRep buildSystem =
    do when (buildId `M.member` (buildSysSt_allBuilds buildSystem)) $
           throwError "build repository already exists!"
       return $ mapToAllBuilds (M.insert buildId buildRep) buildSystem

deleteBuildRepository :: Monad m => BuildId -> BuildSystemState -> ErrT m BuildSystemState
deleteBuildRepository buildId buildSystem =
    do when (not $ buildId `M.member` (buildSysSt_allBuilds buildSystem)) $
           throwError "could not delete build repository: repository not found!"
       return $ mapToAllBuilds (M.delete buildId) buildSystem

updateBuildRepository :: Monad m => BuildId -> (BuildRepository -> BuildRepository) -> BuildSystemState -> ErrT m BuildSystemState
updateBuildRepository buildId f buildSystem =
    do when (not $ buildId `M.member` (buildSysSt_allBuilds buildSystem)) $
           throwError "could not update build repository: repository not found!"
       return $ mapToAllBuilds (M.adjust f buildId) buildSystem

getBuildsInState :: BuildState -> BuildSystemState -> [BuildId]
getBuildsInState buildState buildSystem = 
    M.keys $
    M.filterWithKey (\_ buildRep -> (==buildState) $ br_buildState buildRep) $
    buildSysSt_allBuilds buildSystem

getBuildRepository :: Monad m => BuildId -> BuildSystemState -> ErrT m BuildRepository
getBuildRepository buildId buildSystem =
    handleMaybe (buildId `M.lookup` buildSysSt_allBuilds buildSystem)
        (throwError "getBuildRepository: repository not found!") return
        

-- map over buildSysSt_allBuilds
mapToAllBuilds f buildSysStData = buildSysStData{ buildSysSt_allBuilds = f (buildSysSt_allBuilds buildSysStData) }

-- map over components of BuildRepository:
mapToPath f buildRepo = buildRepo{ br_path = f (br_path buildRepo) }
mapToBuildState f buildRepo = buildRepo{ br_buildState = f (br_buildState buildRepo) }
mapToIncoming f buildRepo = buildRepo{ br_incoming = f (br_incoming buildRepo) }
mapToThread f buildRepo = buildRepo{ br_thread = f (br_thread buildRepo) }
