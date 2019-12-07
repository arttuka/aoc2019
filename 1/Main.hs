module Main where

import Control.Applicative
import Control.Monad

toInt :: String -> Integer
toInt s = read s :: Integer

massToFuel :: Integer -> Integer
massToFuel mass
  | mass < 9  = 0
  | otherwise = fuel + massToFuel fuel
  where fuel = (mass `div` 3) - 2

calculateTotalFuel :: [String] -> Integer
calculateTotalFuel strMasses = sum masses
  where masses = massToFuel . toInt <$> strMasses

readLines :: IO [String]
readLines = fmap lines getContents

main :: IO ()
main = do masses <- readLines
          print $ calculateTotalFuel masses
