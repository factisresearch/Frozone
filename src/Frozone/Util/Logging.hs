module Frozone.Util.Logging where

import Data.Time
import Control.Monad
import System.Locale

data LogLevel
   = LogTrace
   | LogNote
   | LogInfo
   | LogWarn
   | LogError
   deriving (Show, Eq, Ord, Enum)

doLog :: LogLevel -> String -> IO ()
doLog ll s =
    do now <- getCurrentTime
       when (ll > LogTrace) $ putStrLn ("[" ++ tStr now ++ " " ++ llTStr ++ "] " ++ s)
    where
      tStr now = formatTime defaultTimeLocale "%c" now
      llTStr =
          case ll of
            LogTrace -> "TRACE"
            LogNote -> "NOTE"
            LogInfo -> "INFO"
            LogWarn -> "WARN"
            LogError -> "ERROR"
