module Main where

import Control.Applicative
import Control.Monad
import Data.List.Split
import Intcode

toInt :: String -> Int
toInt s = read s :: Int

readCode :: IO [Int]
readCode = fmap (fmap toInt . splitOn ",") getContents

main :: IO ()
main = do program <- readCode
          let result = runProgram program [1]
          print result
