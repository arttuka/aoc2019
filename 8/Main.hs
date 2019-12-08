module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.List.Split

width = 25
height = 6

numberOf :: Int -> [Int] -> Int
numberOf i layer = length $ filter (==i) layer

valueOfLayer :: [Int] -> Int
valueOfLayer layer = numberOf 1 layer * numberOf 2 layer

showPixel :: Int -> String
showPixel 0 = "#"
showPixel _ = " "

showLine :: [Int] -> String
showLine l = intercalate "" $ map showPixel l

showImage :: [Int] -> String
showImage image = intercalate "\n" $ map showLine lines
  where lines = chunksOf width image

stackPixels :: Int -> Int -> Int
stackPixels 0 _ = 0
stackPixels 1 _ = 1
stackPixels 2 x = x

stackLayers :: [[Int]] -> [Int]
stackLayers = foldl1 (zipWith stackPixels)

splitToLayers :: String -> [[Int]]
splitToLayers s = chunksOf (width * height) $ map digitToInt s

main :: IO ()
main = do contents <- getContents
          let image = showImage . stackLayers . splitToLayers $ contents
          putStr image
