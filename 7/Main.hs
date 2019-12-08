module Main where

import Control.Applicative
import Control.Monad
import Data.List (permutations)
import Data.List.Split (splitOn)
import Data.Vector (fromList)
import Intcode

toInt :: String -> Int
toInt s = read s :: Int

readProgram :: String -> Program
readProgram s = fromList $ map toInt $ splitOn "," s

rotate :: [a] -> [a]
rotate lst = last lst : init lst

settings :: [[Int]]
settings = permutations [5, 6, 7, 8, 9]

runAmplifiersWithSettings :: Program -> [Int] -> Int
runAmplifiersWithSettings program (settingA:settings) = last (last outputs)
  where
    n       = length settings
    inputA  = settingA : 0 : last outputs
    inputs  = inputA : zipWith (:) settings (take n outputs)
    outputs = map (runProgram program) inputs
    
main :: IO ()
main = do contents <- getContents
          let program = readProgram contents
              result  = maximum $ map (runAmplifiersWithSettings program) settings
          print result
