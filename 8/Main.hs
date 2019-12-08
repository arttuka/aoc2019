module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Ord

pixelsPerLayer = 25 * 6

numberOf :: Int -> [Int] -> Int
numberOf i layer = length $ filter (==i) layer

valueOfLayer :: [Int] -> Int
valueOfLayer layer = numberOf 1 layer * numberOf 2 layer

splitToLayers :: [Int] -> [[Int]]
splitToLayers = chunksOf pixelsPerLayer

splitToDigits :: String -> [Int]
splitToDigits = map digitToInt 

main :: IO ()
main = do contents <- getContents
          let digits = splitToDigits contents
              layers = splitToLayers digits
              minLayer = minimumBy (comparing $ numberOf 0) layers
          print $ valueOfLayer minLayer
