module Util where

import Data.List.Split (splitOn)
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

from2DList :: [[a]] -> Vector (Vector a)
from2DList = fromList . (fromList <$>)

to2DList :: Vector (Vector a) -> [[a]]
to2DList = (toList <$>) . toList

(!!?) :: Vector (Vector a) -> (Int, Int) -> Maybe a
(!!?) v (x, y) = (v !? y) >>= (!? x)

(!!!) :: Vector (Vector a) -> (Int, Int) -> a
(!!!) v (x, y) = (v ! y) ! x 

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True = t

positions :: Int -> Int -> [[(Int, Int)]]
positions w h = [[(x, y) | x <- [0..w-1]] | y <- [0..h-1]]

filterBy :: [Bool] -> [a] -> [a]
filterBy bs as = map snd $ filter fst $ zip bs as
