module Main where

import Data.Char (digitToInt)
import Data.List (scanl, scanl', scanl1, scanr, scanr1)
import Debug.Trace

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = iterate f x !! n

readInputDigits :: String -> [Int]
readInputDigits = map digitToInt

cycle10k :: [a] -> [a]
cycle10k = concat . replicate 10000

mergeNum :: [Int] -> Int
mergeNum i = read $ show =<< i

add :: Int -> Int -> Int
add x y = (x + y) `mod` 10

fftPhase :: [Int] -> [Int]
fftPhase (x:xs) = scanl' add x xs

applyFFT :: Int -> [Int] -> [Int]
applyFFT n = reverse . applyN n fftPhase . reverse

main :: IO ()
main = do input <- readInputDigits <$> getContents
          let digits = drop offset $ cycle10k input
              offset = mergeNum $ take 7 input
              result = applyFFT 100 digits
          putStrLn (show =<< take 8 result)