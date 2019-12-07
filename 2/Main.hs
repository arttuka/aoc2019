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
  where res = f (program!!i) (program!!j)

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

main :: IO ()
main = do program <- readCode
          let fixedProgram = replaceNth 1 12 $ replaceNth 2 2 program
          print $ runProgram $ Run 0 fixedProgram
