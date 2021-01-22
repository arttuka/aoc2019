{-# LANGUAGE ScopedTypeVariables #-}
module Util where

import Control.Monad (liftM2)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, fromListWith)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Vector (Vector, (!), (!?), (//), fromList, toList)

getLines :: IO [String]
getLines = fmap lines getContents

readWith :: ([String] -> a) -> IO a
readWith = (<$> getLines)

readLinesWith :: (String -> a) -> IO [a]
readLinesWith = readWith . fmap

readLines :: Read a => IO [a]
readLines = readLinesWith read

readGroupsWith :: ([String] -> a) -> IO [a]
readGroupsWith f = fmap f . splitOn [""] <$> getLines

juxt :: (a -> b) -> (a -> c) -> (a -> (b, c))
juxt = liftM2 (,)

groupBy :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
groupBy kf vf = fromListWith (++) . fmap (juxt kf (return . vf))

from2DList :: [[a]] -> Vector (Vector a)
from2DList = fromList . (fromList <$>)

to2DList :: Vector (Vector a) -> [[a]]
to2DList = (toList <$>) . toList

(!!?) :: Vector (Vector a) -> (Int, Int) -> Maybe a
(!!?) v (x, y) = (v !? y) >>= (!? x)

(///) :: forall a. Vector (Vector a) -> [((Int, Int), a)] -> Vector (Vector a)
(///) v vals = v // (toUpdate <$> M.toList gVals)
  where
    gVals = groupBy toKey toSubval vals
    toKey :: ((Int, Int), a) -> Int
    toKey ((_, y), _) = y
    toSubval :: ((Int, Int), a) -> (Int, a)
    toSubval ((x, _), a) = (x, a)
    toUpdate :: (Int, [(Int, a)]) -> (Int, Vector a)
    toUpdate (y, vs) = (y, (v ! y) // vs)

combine :: [Maybe a] -> Maybe [a]
combine [] = Just []
combine ((Just x):xs) = (x :) <$> combine xs
combine (Nothing:xs) = Nothing

addT2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
addT2 (a, b) (c, d) = (a + c, b + d)

subT2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
subT2 (a, b) (c, d) = (a - c, b - d)

find' :: Foldable t => (a -> Bool) -> t a -> a
find' = (fromJust .) . find

pairs :: [a] -> [b] -> [(a, b)]
pairs t1 t2 = (,) <$> t1 <*> t2
