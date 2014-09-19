{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Frozone.BuildTypes where

import Frozone.Util.Json
import Data.Aeson.TH

import qualified Data.ByteString as BS


newtype PatchID = PatchID { unPatchID :: String }
                deriving (Eq,Ord,Show,Read)

data MicroBranchInfo
    = MicroBranchInfo
    { microBranch_id :: MicroBranchHash
    , microBranch_name :: String -- name/describtion of the corresponding patch
    , microBranch_user :: String -- author of the patch
    , microBranch_description :: String -- some additional info (dependency information, ...) in text form
    }
    deriving (Show, Eq)

data MicroBranchHash
    = MicroBranchHash
    { microBranchHash_dpm :: PatchID
    , microBranchHash_extension :: String
    }
    deriving (Show, Read, Eq, Ord)

data Tar = Tar { fromTar :: BS.ByteString }
    deriving (Show, Eq)

microBranchHashToString :: MicroBranchHash -> String
microBranchHashToString hash = unPatchID (microBranchHash_dpm hash) ++ "_" ++ microBranchHash_extension hash

microBranchHashFromString :: String -> Maybe MicroBranchHash
microBranchHashFromString str =
    case split str of
      ([],_) -> Nothing
      (_,[]) -> Nothing
      (dpmHash, extension) -> Just $ MicroBranchHash (PatchID dpmHash) extension
      where
          split str' =
              let (x,y) = span (/='_') str'
              in (x, drop 1 y)

$(deriveJSON (defaultOptions{ fieldLabelModifier = dropPrefix }) ''PatchID)
$(deriveJSON (defaultOptions{ fieldLabelModifier = dropPrefix }) ''MicroBranchHash)

$(deriveJSON (defaultOptions{ fieldLabelModifier = dropPrefix }) ''MicroBranchInfo)
