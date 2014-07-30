{-# LANGUAGE ScopedTypeVariables, RankNTypes, GADTs #-}
module Frozone.VCS.Darcs.Api
    ( readPatchBundle, apply
    )
where

--
-- ripped with permission from DPM [1], then adjusted for frozone
-- [1] http://stefanwehr.de/software/#DPM_-_Darcs_Patch_Manager
--
import Frozone.VCS.Types
import qualified Data.Text as T

import Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.FilePath (dropExtensions)
import Control.Exception (SomeException, ErrorCall(..), IOException,
                          handle, fromException, catches,
                          Handler(..))
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Exit
import System.Path
import System.Directory
import qualified Data.List as List
import Data.Convertible
import Data.List ( (\\) )

-- imports from darcs
import Progress ( setProgressMode )
import Darcs.Patch (RepoPatch, commute)
import Darcs.Email (readEmail)
import Darcs.Commands (commandCommand)
import qualified Darcs.Commands.Apply
import Darcs.External (verifyPS)
import Darcs.Flags (DarcsFlag(FixFilePath,
                              NoAllowConflicts, Test, OnePattern))
import Darcs.Patch.MatchData ( PatchMatch(..) )
import Darcs.Patch.Set ( newset2RL )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.PatchInfoAnd (PatchInfoAnd, hopefullyM, info, n2pia,
                                 conscientiously)
import Darcs.Witnesses.Ordered (FL(..), RL(..), mapFL_FL,
                                (:>)(..), (:\/:)(..),
                                mapRL)
import Darcs.Witnesses.Eq (MyEq(unsafeCompare))
import Darcs.Witnesses.Unsafe (unsafeCoercePStart)
import Darcs.Patch.Bundle (scanBundle)
import Darcs.Patch.Info (PatchInfo(..), humanFriendly,
                         makeFilename, piAuthor, piDate, piName)
import Darcs.Patch.Set (SealedPatchSet, Origin)
import Darcs.Patch.Commute (Commute)
import Darcs.RepoPath (makeAbsolute,
                       rootDirectory)
import Darcs.Patch.Depends ( findUncommon, findCommonWithThem )
import Darcs.Repository (Repository, RepoJob(..))
import Darcs.Repository.Internal (readRepo, withRepositoryDirectory)
import Darcs.Witnesses.Sealed (Sealed(..))
import qualified Printer as P (text, ($$), renderString,
                               vcat)
import ByteStringUtils (linesPS, unlinesPS)
import Storage.Hashed.Tree (Tree)

data DependsOn a = DependsOn { do_patch :: a
                             , do_directDeps :: [a]
                             , do_transDeps :: [a] }
                 deriving Show

instance Functor DependsOn where
    fmap f dp = DependsOn (f (do_patch dp)) (map f (do_directDeps dp)) (map f (do_transDeps dp))

data ExPatchInfoAnd p = forall a b . ExPatchInfoAnd (PatchInfoAnd p a b)

eqExPatchInfoAnd :: (Commute p, MyEq p) => ExPatchInfoAnd p -> ExPatchInfoAnd p -> Bool
eqExPatchInfoAnd (ExPatchInfoAnd p) (ExPatchInfoAnd q) = unsafeCompare p q

exInfo :: ExPatchInfoAnd p -> PatchInfo
exInfo (ExPatchInfoAnd p) = info p

makePatch :: DependsOn PatchInfo -> VCSPatch
makePatch (DependsOn i ps _) =
    let pid = getPatchID i
    in VCSPatch
       { vp_id = pid
       , vp_date = convert $ piDate i
       , vp_name = T.pack (piName i)
       , vp_author = T.pack $ piAuthor i
       , vp_dependents = map getPatchID ps
       }

computeDependencies :: forall p a b . RepoPatch p => FL (PatchInfoAnd p) a b
                    -> [DependsOn PatchInfo]
computeDependencies fl = worker fl []
    where
      worker :: forall c d . FL (PatchInfoAnd p) c d -> [DependsOn (ExPatchInfoAnd p)] -> [DependsOn PatchInfo]
      worker NilFL deps = map (fmap exInfo) (reverse deps)
      worker (p :>: ps) deps =
          let (rdir, rtrans) = getDeps p deps
              directDeps = reverse rdir
              transDeps = List.nubBy eqExPatchInfoAnd $ reverse rtrans
          in worker ps (DependsOn (ExPatchInfoAnd p) directDeps transDeps : deps)
      getDeps :: forall c d . RepoPatch p
              => PatchInfoAnd p c d
              -> [DependsOn (ExPatchInfoAnd p)]
              -> ([ExPatchInfoAnd p], [ExPatchInfoAnd p])
      getDeps _ [] = ([], [])
      getDeps p (DependsOn (ExPatchInfoAnd q) qs qsTrans : rest) =
          case commute (q :> unsafeCoercePStart p) of
            Just _ ->  -- p does NOT depend on q
              getDeps p rest
            Nothing -> -- p depends on q
                let rest' = filter (\ (DependsOn x _ _) ->
                                        not (any (eqExPatchInfoAnd x) qs))
                                   rest
                    (otherDirect, otherTrans) = getDeps p rest'
                in (ExPatchInfoAnd q : otherDirect, ExPatchInfoAnd q : qsTrans ++ otherTrans)

getPatchID :: PatchInfo -> VCSPatchId
getPatchID p = VCSPatchId (BC.pack $ dropExtensions $ makeFilename p)

exceptionAsString :: SomeException -> String
exceptionAsString e =
    case fromException e of
      Just (ErrorCall s) -> s
      _ ->
          case fromException e of
            Just (ioExc :: IOException) ->
                ioeGetErrorString ioExc
            _ -> show e

processPatchBundle :: forall a . String -> [DarcsFlag] -> B.ByteString
                   -> (forall p b c r u .
                       (RepoPatch p)
                       => Repository p r u r
                       -> FL (PatchInfoAnd p) b c   -- bundle patches
                       -> IO a)
                   -> IO a
processPatchBundle repoDir opts bundleData fun =
    bracketCWD repoDir (withRepositoryDirectory opts "." (RepoJob run))
    where
      run :: forall p r u . (RepoPatch p, Tree ~ ApplyState p) => Repository p r u r -> IO a
      run repository = do
        let ps = bundleData
        repoPatches <- readRepo repository
        bundlePatchesEither <- getPatchBundle opts ps
        case bundlePatchesEither of
          Left err -> fail err
          Right (Sealed bundlePatches) ->
            case findCommonWithThem repoPatches bundlePatches of
              (common :> _) -> do
                -- all patches that are in "them" and not in "common" need to
                -- be available; check that
                let common_i = mapRL info $ newset2RL common
                    them_i = mapRL info $ newset2RL bundlePatches
                    required = them_i \\ common_i -- FIXME quadratic?
                    check :: forall c d . RL (PatchInfoAnd p) c d -> [PatchInfo] -> IO ()
                    check (p :<: ps') bad = case hopefullyM p of
                      Nothing | info p `elem` required -> check ps' (info p : bad)
                      _ -> check ps' bad
                    check NilRL [] = return ()
                    check NilRL bad = fail . P.renderString $ P.vcat $ map humanFriendly bad ++
                                      [P.text "\nFATAL: Cannot apply this bundle. We are missing the above patches." ]
                check (newset2RL bundlePatches) []
                (_:\/:bundlePatches') <- return $ findUncommon repoPatches bundlePatches
                let bundlePatches'' =
                        mapFL_FL (n2pia . conscientiously
                                  (P.text ("We cannot process this patch "
                                           ++ "bundle, since we're "
                                           ++ "missing:") P.$$))
                               $ bundlePatches'
                fun repository bundlePatches''

readPatchBundle :: String -> B.ByteString
                -> IO (Either String [VCSPatch])
readPatchBundle repoDir bundleData =
    do setProgressMode True
       let opts = []
       handle (\ e -> return (Left (exceptionAsString e))) $
         processPatchBundle repoDir opts bundleData $
           \_ bundlePatches ->
             do let deps = computeDependencies bundlePatches
                return $! Right $! map makePatch deps

getPatchBundle :: RepoPatch p => [DarcsFlag] -> B.ByteString
                 -> IO (Either String (SealedPatchSet p Origin))
getPatchBundle opts fps = do
    mps <- verifyPS opts $ readEmail fps
    mops <- verifyPS opts fps
    case (mps, mops) of
      (Nothing, Nothing) ->
          return $ Left "Patch bundle not properly signed, or gpg failed."
      (Just ps, Nothing) -> return $ scanBundle ps
      (Nothing, Just ps) -> return $ scanBundle ps
      -- We use careful_scanBundle only below because in either of the two
      -- above case we know the patch was signed, so it really shouldn't
      -- need stripping of CRs.
      (Just ps1, Just ps2) -> case careful_scanBundle ps1 of
                              Left _ -> return $ careful_scanBundle ps2
                              Right x -> return $ Right x
          where careful_scanBundle ps =
                    case scanBundle ps of
                    Left e -> case scanBundle $ stripCrPS ps of
                              Right x -> Right x
                              _ -> Left e
                    x -> x
                stripCrPS :: B.ByteString -> B.ByteString
                stripCrPS ps = unlinesPS $ map stripline $ linesPS ps
                stripline p | B.null p = p
                            | BC.last p == '\r' = B.init p
                            | otherwise = p

apply :: FilePath -> VCSPatchId -> Bool -> FilePath
      -> IO (Either String ())
apply repoDir (VCSPatchId patchIdBs) runTests patchFile =
    do setProgressMode False
       cur <- getCurrentDirectory
       let current = makeAbsolute rootDirectory cur
           opts = [FixFilePath current current, NoAllowConflicts] ++
                  (if runTests then [Test] else []) ++
                  ([OnePattern (PatternMatch ("hash " ++
                                              BC.unpack patchIdBs))])
           args = [patchFile]
       bracketCWD repoDir $
           do commandCommand Darcs.Commands.Apply.apply opts args
              hFlush stdout
              hFlush stderr
              return (Right ())
       `catches`
       [Handler (\(e::ExitCode) ->
                     case e of
                       ExitSuccess -> return $ Right ()
                       ExitFailure n ->
                           return $ Left ("darcs exited with exit code " ++
                                          show n))
       ,Handler (\e -> return $ Left (exceptionAsString e))]
