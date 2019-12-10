module Main where

import Control.Applicative
import Control.Monad
import Data.List.Split (splitOn)
import Data.Vector (fromList)
import Intcode

toInt :: String -> Int
toInt s = read s :: Int

readProgram :: String -> Program
readProgram s = fromList $ map toInt $ splitOn "," s
    
main :: IO ()
main = do contents <- getContents
          let program = readProgram contents
              result  = runProgram program [2]
          print result
