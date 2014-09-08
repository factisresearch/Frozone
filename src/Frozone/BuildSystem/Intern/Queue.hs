{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Frozone.BuildSystem.Intern.Queue(
    Queue(),
    empty, fromList,
    put, get, take,
    delete,
    null, length,
    --map,
    elem, filter
) where

import qualified Data.Sequence as S
import Prelude hiding (take, null, length, elem, filter, map)
--import qualified Data.List as L

import Control.Monad


newtype Queue a = Queue { fromQueue :: S.Seq a }
    deriving (Eq, Functor)

empty = Queue $ S.empty

fromList = Queue . S.fromList

put x = Queue . (S.|> x) . fromQueue

get = liftM fst . take

take q = case S.viewl (fromQueue q) of
    S.EmptyL -> Nothing
    (x S.:< xs) -> Just (x, Queue xs)

delete x q =
    let newQ = filter (/=x) q
    in
      case newQ == q of
        True -> Nothing
        False -> Just newQ

null = S.null . fromQueue

length = S.length . fromQueue

elem :: (Eq a) => a -> Queue a -> Bool
elem x = (/=S.empty) . S.filter (==x) . fromQueue

{-
map :: (a -> b) -> Queue a -> Queue b
map f q = Queue $ map f $ fromQueue q
-}

filter :: (a -> Bool) -> Queue a -> Queue a
filter cond = Queue . S.filter cond . fromQueue
