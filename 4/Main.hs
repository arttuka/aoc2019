module Main where

import Control.Applicative
import Control.Monad
import Data.List

toInt :: String -> Integer
toInt s = read s :: Integer

validateRange :: String -> String -> String -> Bool
validateRange from to n = from <= n && n <= to

listOfLength2 :: [a] -> Bool
listOfLength2 l = length l == 2

validateSameAdjacentDigits :: String -> Bool
validateSameAdjacentDigits = any listOfLength2 . group

validateNonDecreasing :: String -> Bool
validateNonDecreasing [_] = True
validateNonDecreasing (x:y:xs) = x <= y && validateNonDecreasing (y:xs)

validatePassword :: String -> String -> String -> Bool
validatePassword rangeFrom rangeTo s = validateRange rangeFrom rangeTo s && validateSameAdjacentDigits s && validateNonDecreasing s

readRange :: IO [String]
readRange = fmap lines getContents

main :: IO ()
main = do [rangeFrom, rangeTo] <- readRange
          let candidates = fmap show [(toInt rangeFrom)..(toInt rangeTo)]
          print $ length $ filter (validatePassword rangeFrom rangeTo) candidates
