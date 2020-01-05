module Main where

import Control.Applicative
import Control.Monad
import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)
import Intcode (readProgram, runProgram)

size :: Int
size = 50

showMap :: [Int] -> String
showMap nums = intercalate "\n" $ (map . map) showCell rows
  where
    rows = transpose $ chunksOf size nums
    showCell :: Int -> Char
    showCell 0 = '.'
    showCell 1 = '#'

main :: IO ()
main = do contents <- getContents
          let program = readProgram contents
              inputs  = sequence [[0..(pred size)], [0..(pred size)]]
              output  = runProgram program =<< inputs
          putStrLn $ showMap output
          print $ sum output
