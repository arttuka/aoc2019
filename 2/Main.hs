module Main where

import Control.Applicative
import Control.Monad
import Data.List.Split

data State = Run Int [Int] | Result Int

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth 0 val (x:xs) = val:xs
replaceNth i val (x:xs) = x : replaceNth (i - 1) val xs

doOperation :: (Int -> Int -> Int) -> Int -> Int -> Int -> [Int] -> [Int]
doOperation f i j k program = replaceNth k res program
  where res = f (program !! i) (program !! j)

step :: Int -> [Int] -> State
step counter program = case opcode of
  99 -> Result $ head program
  _  -> Run (counter + 4) newProgram
        where
          _:i:j:k:_ = drop counter program
          operation = case opcode of
            1 -> doOperation (+)
            2 -> doOperation (*)
          newProgram = operation i j k program
  where
    opcode = program!!counter

runProgram :: State -> Int
runProgram (Run counter program) = runProgram $ step counter program
runProgram (Result i) = i

toInt :: String -> Int
toInt s = read s :: Int

readCode :: IO [Int]
readCode = fmap (fmap toInt . splitOn ",") getContents

pairs :: [(Int, Int)]
pairs = [(i, t - i) | t <- [0..], i <- [0..t]]

runProgramWithInput :: [Int] -> (Int, Int) -> Int
runProgramWithInput program (x, y) = runProgram $ Run 0 fixedProgram
  where fixedProgram = replaceNth 1 x $ replaceNth 2 y program

main :: IO ()
main = do program <- readCode
          let results = takeWhile (/= 19690720) $ fmap (runProgramWithInput program) pairs
          print $ pairs !! length results
