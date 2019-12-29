module Main where

import Control.Applicative
import Control.Monad
import Data.Char (digitToInt)
import Debug.Trace

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = iterate f x !! n

getPattern :: Int -> [Int]
getPattern n = tail . cycle $ replicate (succ n) =<< [0, 1, 0, -1]

onesDigit :: Int -> Int
onesDigit i = (rem . abs) i 10

fftPhase :: [Int] -> [Int]
fftPhase input = trace "fftPhase" $ map calculateDigit [0 .. pred n]
  where
    n                = length input
    calculateDigit i = onesDigit $ sum $ zipWith (*) input $ getPattern i

readInputDigits :: String -> [Int]
readInputDigits = map digitToInt

cycle10k :: [a] -> [a]
cycle10k lst = take (10000 * n) $ cycle lst
  where
    n = length lst

mergeNum :: [Int] -> Int
mergeNum i = read $ show =<< i

main :: IO ()
main = do contents <- getContents
          let digits = cycle10k $ readInputDigits contents
              offset = mergeNum $ take 7 digits
              result = applyN 2 fftPhase digits
          print $ show =<< take 8 (drop offset result)
