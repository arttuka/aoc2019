module Main where

import Control.Applicative
import Control.Monad
import Data.Char (chr, ord)
import Data.List.Split (splitOn)
import Intcode (readProgram, runProgram)
import Util

data Reg = A | B | C | D | E | F | G | H | I | J | T deriving Show
data Instruction = AND Reg Reg | OR Reg Reg | NOT Reg Reg deriving Show

scProgram :: [Instruction]
scProgram = [ NOT A T
            , OR  T J
            , NOT B T
            , OR  T J
            , NOT C T
            , OR  T J
            , AND D J
            ]

toInput :: [Instruction] -> [Int]
toInput cmds = fmap ord =<< interleave strs (repeat "\n")
  where
    strs = map show cmds ++ ["RUN"]

main :: IO ()
main = do contents <- getContents
          let program      = readProgram contents
              inputs       = toInput scProgram
              output       = runProgram program inputs
          mapM_ putStrLn $ splitOn "\n" $ map chr output
