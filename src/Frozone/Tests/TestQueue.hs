{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
--import Test.Quickcheck.Function
import Text.Show.Functions()

import qualified Frozone.Util.Queue as Q
import Data.Foldable(toList)
import Control.Monad.State
import Data.Maybe


main = htfMain htf_thisModulesTests

instance (Arbitrary a) => Arbitrary (Q.Queue a) where
    arbitrary = return . Q.fromList =<< arbitrary

test_empty :: IO ()
test_empty =
    do assertEqual [] $ (toList Q.empty :: [Int])

prop_toList values =
    toList (insertAllValues values Q.empty) == values
    where
        _ = (values :: [Int])
prop_fromList values =
    (insertAllValues values Q.empty) == Q.fromList values
    where
        _ = (values :: [Int])

prop_put values =
        (takeAllValuesFromQueue . insertAllValues values) Q.empty == values
    where
        _ = (values :: [Int])

prop_get q =
    Q.get q == liftM fst (Q.take q)
    where
        _ = (q :: Q.Queue Int)

prop_onlyDeleteWhenInQ x q =
    (x `elem` toList q) ==> isJust (Q.delete x q)
    where
        _ = (x :: Int)

prop_delete x q =
    let deleted = Q.delete x q
    in
        isJust deleted ==>
            toList (fromJust deleted) == filter (/=x) (toList q)
    where
        _ = x :: Int

prop_null q =
    Q.null q == null (toList q)
    where
        _ = q :: Q.Queue Int

prop_length q =
    Q.length q == length (toList q)
    where
        _ = q :: Q.Queue Int

prop_elem x q =
    (x `Q.elem` q) == (x `elem` toList q)
    where
        _ = x :: Int

prop_filter prop q =
    toList (Q.filter prop q) == filter prop (toList q)
    where
        _ = q :: Q.Queue Int

leftToRight :: (a -> b) -> (b -> c) -> (a -> c)
leftToRight = flip (.)

--equivalent t1 t2 = (t1 ==> t2) (not t1 ==> not t2)

insertAllValues values q = (foldr leftToRight id $ map Q.put values) q

takeAllValuesFromQueue :: Q.Queue a -> [a]
takeAllValuesFromQueue q =
    case Q.take q of
      Nothing -> []
      Just (x, newQ) -> x : takeAllValuesFromQueue newQ
