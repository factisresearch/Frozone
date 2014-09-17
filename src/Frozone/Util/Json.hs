module Frozone.Util.Json
    ( module Data.Aeson
    , deriveJSON
    , jDrop, dropPrefix
    )
where

import Data.Aeson
import Data.Aeson.TH

jDrop :: Int -> Options
jDrop i =
    defaultOptions
    { fieldLabelModifier = drop i
    }

dropPrefix str =
    case span (/='_') str of
      (_, []) -> str
      (_, suffix) -> suffix
