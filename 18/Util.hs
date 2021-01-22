{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util where

import Control.Applicative
import Control.Monad
import Data.List (foldl')
import Data.Map.Strict (Map, (!?), assocs, fromList, fromListWith, keys)
import Data.Maybe (catMaybes, maybe)
import Data.Set (Set, delete)
import Data.Vector (Vector)
import qualified Data.Vector as V ((!), (!?), fromList, toList)

between :: Ord a => a -> a -> a -> Bool
between from to x = x >= from && x <= to

juxt :: (a -> b) -> (a -> c) -> a -> (b, c)
juxt = liftM2 (,)

juxtM :: Monad m => (a -> m b) -> (a -> m c) -> a -> m (b, c)
juxtM = liftM2 (liftM2 (,))

mapWith :: (a -> b) -> a -> (a, b)
mapWith = juxt id

mapBy :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map k v
mapBy kf vf xs = fromList $ juxt kf vf <$> xs 

lookup2 :: (Ord a, Ord b) => Map a (Map b v) -> a -> b -> Maybe v
lookup2 m k1 k2 = (m !? k1) >>= (!? k2)

pickEvery :: [a] -> [(a, [a])]
pickEvery [x]    = [(x, [])]
pickEvery (x:xs) = (x, xs) : (fmap.fmap) (x:) (pickEvery xs)

groupBy :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
groupBy kf vf = fromListWith (++) . fmap (juxt kf (return . vf))

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

rst3 :: (a, b, c) -> (b, c)
rst3 (_, y, z) = (y, z)

init3 :: (a, b, c) -> (a, b)
init3 (x, y, _) = (x, y)

dropSnd3 :: (a, b, c) -> (a, c)
dropSnd3 (x, _, z) = (x, z)

tailInit :: [a] -> [a]
tailInit = tail . init

deleteAll :: Ord a => [a] -> Set a -> Set a
deleteAll xs s = foldl' (flip delete) s xs

combine :: [Maybe a] -> Maybe [a]
combine [] = Just []
combine ((Just x):xs) = (x :) <$> combine xs
combine (Nothing:xs) = Nothing

keypairs :: forall k1 k2 a. Ord k1 => Ord k2 => Map k1 (Map k2 a) -> [(k1, k2)]
keypairs m = step . fmap keys =<< assocs m
  where
    step :: (k1, [k2]) -> [(k1, k2)]
    step (k, ks) = (k ,) <$> ks

from2DList :: [[a]] -> Vector (Vector a)
from2DList = V.fromList . (V.fromList <$>)

to2DList :: Vector (Vector a) -> [[a]]
to2DList = (V.toList <$>) . V.toList

(!!?) :: Vector (Vector a) -> (Int, Int) -> Maybe a
(!!?) v (k1, k2) = (v V.!? k1) >>= (V.!? k2)

(!!!) :: Vector (Vector a) -> (Int, Int) -> a
(!!!) v (k1, k2) = (v V.! k1) V.! k2

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : maybe [] (iterateMaybe f) (f x)

mapT2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
mapT2 f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)

addT2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
addT2 = mapT2 (+)

andPred :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
andPred = liftM2 (&&)

orPred :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
orPred = liftM2 (||)

indexed :: [a] -> [(Int, a)]
indexed = zipIndexed (,)

zipIndexed :: (Int -> a -> b) -> [a] -> [b]
zipIndexed f = zipWith f [0..]

zipIndexedMaybe :: (Int -> a -> Maybe b) -> [a] -> [b]
zipIndexedMaybe = (catMaybes .) . zipIndexed

pairs :: Ord a => [a] -> [(a, a)]
pairs xs = [(a, b) | a <- xs
                   , b <- xs
                   , a < b
                   ]

foldMaybe :: (a -> b -> b) -> Maybe a -> b -> b
foldMaybe _ Nothing b = b
foldMaybe f (Just a) b = f a b
