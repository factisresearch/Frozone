{-# LANGUAGE OverloadedStrings #-}
module Frozone.VCS.Darcs (darcsVCS) where

import Frozone.VCS.Types
import Frozone.Util.File

import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM

darcsVCS :: VCSApi
darcsVCS =
    VCSApi
    { vcs_cloneRepository = cloneFun
    , vcs_applyPatch = applyFun
    , vcs_changeLog = changeLogFun
    , vcs_changedFiles = changedFilesFun
    }

cloneFun :: VCSSource -> VCSRepository -> IO (VCSResponse ())
cloneFun (VCSSource source) (VCSRepository target) =
    runVCS "darcs" ["get", "--lazy", source, target] "" (const ())

applyFun :: VCSPatch -> VCSRepository -> IO (VCSResponse ())
applyFun (VCSPatch bs) (VCSRepository target) =
    let bundleLoc = target </> "patches.dpatch"
    in do BS.writeFile bundleLoc bs
          r <- runVCS "darcs" ["apply", "--repodir", target, bundleLoc] "" (const ())
          removeIfExists bundleLoc
          return r

changeLogFun :: VCSRepository -> IO (VCSResponse ())
changeLogFun (VCSRepository target) =
    runVCS "darcs" ["changes", "--repodir", target] "" (const ())

changedFilesFun :: VCSPatch -> IO (VCSResponse FileChangeMap)
changedFilesFun (VCSPatch bundleBS) =
    return $ VCSResponse
               { vcs_stdOut = BS.empty
               , vcs_stdErr = BS.empty
               , vcs_success = True
               , vcs_data = Just r
               }
    where
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
