{-# LANGUAGE OverloadedStrings #-}
module Frozone.VCS.Darcs (darcsVCS) where

import qualified Frozone.VCS.Darcs.Api as Api

import Frozone.VCS.Types
import Frozone.Util.File

import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM

darcsVCS :: VCSApi
darcsVCS =
    VCSApi
    { vcs_patchesFromBundle = readBundle
    , vcs_cloneRepository = cloneFun
    , vcs_applyPatch = applyFun
    , vcs_changeLog = changeLogFun
    , vcs_changedFiles = changedFilesFun
    }

readBundle :: VCSRepository -> VCSPatchBundle -> IO (VCSResponse [VCSPatch])
readBundle (VCSRepository repo) (VCSPatchBundle bs) =
    do r <- Api.readPatchBundle repo bs
       case r of
         Left errMsg ->
             return $ VCSResponse
                        { vcs_stdOut = BS.empty
                        , vcs_stdErr = BSC.pack errMsg
                        , vcs_success = False
                        , vcs_data = Nothing
                        }
         Right patchList ->
             return $ VCSResponse
                        { vcs_stdOut = BS.empty
                        , vcs_stdErr = BS.empty
                        , vcs_success = True
                        , vcs_data = Just patchList
                        }

cloneFun :: VCSSource -> VCSRepository -> IO (VCSResponse ())
cloneFun (VCSSource source) (VCSRepository target) =
    runVCS "darcs" ["get", "--lazy", source, target] "" (const ())

applyFun :: VCSPatchId -> VCSPatchBundle -> VCSRepository -> IO (VCSResponse ())
applyFun patchId (VCSPatchBundle bs) (VCSRepository target) =
    let bundleLoc = target </> "patches.dpatch"
    in do BS.writeFile bundleLoc bs
          r <- Api.apply target patchId False bundleLoc
          removeIfExists bundleLoc
          case r of
            Left errMsg ->
                return $ VCSResponse
                        { vcs_stdOut = BS.empty
                        , vcs_stdErr = BSC.pack errMsg
                        , vcs_success = False
                        , vcs_data = Nothing
                        }
            Right () ->
                return $ VCSResponse
                           { vcs_stdOut = BS.empty
                           , vcs_stdErr = BS.empty
                           , vcs_success = True
                           , vcs_data = Nothing
                           }

changeLogFun :: VCSRepository -> IO (VCSResponse ())
changeLogFun (VCSRepository target) =
    runVCS "darcs" ["changes", "--repodir", target] "" (const ())

changedFilesFun :: VCSPatchId -> VCSPatchBundle -> IO (VCSResponse FileChangeMap)
changedFilesFun (VCSPatchId patchId) (VCSPatchBundle fullBundleBS) =
    return $ VCSResponse
               { vcs_stdOut = BS.empty
               , vcs_stdErr = BS.empty
               , vcs_success = True
               , vcs_data = Just r
               }
    where
      bundleBS =
          let findNeedle = BS.concat ["Ignore-this: ", patchId]
              (_, patchAndJunk) = BS.breakSubstring findNeedle fullBundleBS
              (patch, _) = BS.breakSubstring "Ignore-this: " (BS.drop (BS.length findNeedle) patchAndJunk)
          in patch

      r = foldl handleLine HM.empty (BSC.split '\n' bundleBS)
      handleLine hm bs =
          if BS.isPrefixOf "] " bs
          then handleLine' hm (BS.drop 2 bs)
          else handleLine' hm bs
      handleLine' hm bs
          | BS.isPrefixOf "hunk" bs =
              case splitBS of
                ["hunk", filename, _] ->
                    HM.insertWith (\new old -> if old == FileChangeCreated then old else new) (BSC.unpack filename) FileChangeModified hm
                _ -> hm
          | BS.isPrefixOf "rmfile" bs =
              case splitBS of
                ["rmfile", filename] ->
                    HM.insert (BSC.unpack filename) FileChangeDeleted hm
                _ -> hm
          | BS.isPrefixOf "addfile" bs =
              case splitBS of
                ["addfile", filename] ->
                    HM.insert (BSC.unpack filename) FileChangeCreated hm
                _ -> hm
          | otherwise = hm
          where
            splitBS = BSC.split ' ' bs
