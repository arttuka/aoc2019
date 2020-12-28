module Util where

import Control.Applicative
import Control.Monad
import Data.Map.Strict (Map, (!), fromListWith)

between :: Ord a => a -> a -> a -> Bool
between from to x = x >= from && x <= to

juxt :: (a -> b) -> (a -> c) -> a -> (b, c)
juxt = liftM2 (,)

juxtM :: Monad m => (a -> m b) -> (a -> m c) -> a -> m (b, c)
juxtM = liftM2 (liftM2 (,))

mapWith :: (a -> b) -> a -> (a, b)
mapWith = juxt id

lookup2 :: (Ord a, Ord b) => Map a (Map b v) -> a -> b -> v
lookup2 m k1 k2 = (m ! k1) ! k2

pickEvery :: [a] -> [(a, [a])]
pickEvery [x]    = [(x, [])]
pickEvery (x:xs) = (x, xs) : (fmap.fmap) (x:) (pickEvery xs)

groupBy :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
groupBy kf vf = fromListWith (++) . fmap (juxt kf (return . vf))

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

rst3 :: (a, b, c) -> (b, c)
rst3 (_, y, z) = (y, z)

tailInit :: [a] -> [a]
tailInit = tail . init
