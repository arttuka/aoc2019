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

rotate :: [a] -> [a]
rotate lst = last lst : init lst

settings :: [[Int]]
settings = permutations [5, 6, 7, 8, 9]

runAmplifiersWithSettings :: Program -> [Int] -> Int
runAmplifiersWithSettings program (sA:settings) =
  let n = length settings
      inputA = sA : 0 : last outputs
      inputs = inputA : zipWith (:) settings (take n outputs)
      outputs = map (runProgram program) inputs
  in last (last outputs)
    
main :: IO ()
main = do program <- readCode
          let result = maximum $ map (runAmplifiersWithSettings program) settings
          print result
