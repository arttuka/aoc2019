module Main where

import Control.Applicative
import Control.Monad
import Data.List.Split
import Line

data PathSegment = PathUp Int | PathDown Int | PathLeft Int | PathRight Int deriving (Show)

toInt :: String -> Int
toInt s = read s :: Int

pathToLines :: Int -> Int -> [PathSegment] -> [Line]
pathToLines _ _ [] = []
pathToLines x y (segment:segments) = line : pathToLines x1 y1 segments
  where
    (x1, y1, line) = case segment of
      PathUp d    -> (x, y - d, VLine x (y - d) d)
      PathDown d  -> (x, y + d, VLine x y d)
      PathLeft d  -> (x - d, y, HLine (x - d) y d)
      PathRight d -> (x + d, y, HLine x y d)

readLines :: IO [String]
readLines = fmap lines getContents

readPathSegment :: String -> PathSegment
readPathSegment (x:xs) = case x of
  'U' -> PathUp dist
  'D' -> PathDown dist
  'L' -> PathLeft dist
  'R' -> PathRight dist
  where dist = toInt xs

readPath :: String -> [PathSegment]
readPath s = readPathSegment <$> splitOn "," s

closestIntersection :: [Intersection] -> Intersection
closestIntersection = foldl1 closerIntersection

main :: IO ()
main = do ls <- readLines
          let paths = fmap readPath ls
          let [lines1, lines2] = fmap (pathToLines 0 0) paths
          let intersections = findIntersections lines1 lines2
          print $ closestIntersection intersections
