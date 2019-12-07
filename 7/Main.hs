module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
import Intcode

toInt :: String -> Int
toInt s = read s :: Int

readCode :: IO [Int]
readCode = fmap (fmap toInt . splitOn ",") getContents

settings :: [[Int]]
settings = permutations [0, 1, 2, 3, 4]

runProgramWithSettings :: Program -> Int -> [Int] -> Int
runProgramWithSettings _ input [] = input
runProgramWithSettings program input (x:xs) = runProgramWithSettings program nextInput xs
  where
    (Result _ [nextInput]) = runProgram program [x, input]
    
main :: IO ()
main = do program <- readCode
          let result = maximum $ map (runProgramWithSettings program 0) settings
          print result
