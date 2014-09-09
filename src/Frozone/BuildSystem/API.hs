module Frozone.BuildSystem.API where

import Frozone.BuildTypes

import Control.Monad.Error


data BuildSystem
    = BuildSystem
    { bs_addBuild :: BuildId -> TarFile -> ErrorT ErrMsg IO () -- > BuildScheduled ... BuildPreparing ... Building ... BuildSuccess|BuildFailed
    , bs_getBuildRepositoryState :: BuildId -> ErrorT ErrMsg IO BuildState -- for any known Build: return its state

    --, bs_startBuild :: BuildId -> ErrorT ErrMsg IO () -- BuildReady -> BuildScheduled ... Building ... BuildSuccess|BuildFailed

    , bs_getBuildQueue :: BuildState -> IO [BuildId] -- return all builds which are in a specific state

    , bs_stopBuild :: BuildId -> ErrorT ErrMsg IO () -- any State -> BuildStopped
    , bs_archiveBuild :: BuildId -> ErrorT ErrMsg IO () -- any State -> BuildArchived
    --, bs_setBuildQueue :: [BuildId] -> IO () -- 
    , bs_restartBuild :: BuildId -> ErrorT ErrMsg IO () -- BuildStopped|BuildFailed -> BuildScheduled ... BuildPreparing ... Building ... BuildSuccess|BuildFailed
    }

data BuildState
    = BuildScheduled -- waiting for cpu time
    | BuildPreparing -- untar the incoming archive
    | Building -- running build script
    | BuildStopped -- build script interrupted
    | BuildSuccess -- build script finished with exit code == 0
    | BuildFailed -- build script finished with exit code /= 0
    | BuildArchived -- deleted from disk
    deriving (Show, Eq, Enum)

allBuildStates = [(BuildScheduled)..(BuildArchived)]
-- states in which stopBuild is a valid action:
validStopStates = [BuildScheduled, BuildPreparing, Building]

newtype BuildId = BuildId { fromBuildId :: Int }
    deriving (Show, Eq, Ord)

type ErrT = ErrorT ErrMsg
type ErrMsg = String
