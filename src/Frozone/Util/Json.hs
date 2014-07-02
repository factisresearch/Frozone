module Frozone.Util.Json
    ( module Data.Aeson
    , deriveJSON, jDrop
    )
where

import Data.Aeson
import Data.Aeson.TH

jDrop i =
    defaultOptions
    { fieldLabelModifier = drop i
    }
