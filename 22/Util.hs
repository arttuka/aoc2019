module Util where

import Debug.Trace (trace)

import Control.Monad (join)
import Data.List (foldl1)
import Data.Tuple.Select (sel1)

type Euclid = (Integer, Integer, Integer)

euclid :: Integer -> Integer -> Euclid
euclid a b = last $ takeWhile ((0 /=) . sel1) results
  where
    results = (a, 1, 0) : (b, 0, 1) : zipWith step results (tail results)
    step :: Euclid -> Euclid -> Euclid
    step (r0, s0, t0) (r1, s1, t1) = (r2, s2, t2)
      where
        (q, r2) = quotRem r0 r1
        s2      = s0 - (q * s1)
        t2      = t0 - (q * t1)

mmiLcm :: Integer -> Integer -> (Integer, Integer)
mmiLcm n m = (s, (n * m) `div` r)
  where
    (r, s, _) = euclid n m

mmi :: Integer -> Integer -> Integer
mmi n m = mmi' `absMod` m
  where
    (mmi', _) = mmiLcm n m 

absMod :: Integer -> Integer -> Integer
absMod = (abs .) . mod

combineTimes :: Integer -> (a -> a -> a) -> a -> a
combineTimes n f x = foldl1 f $ map snd $ filter fst $ zip bin fs
  where
    fs  = iterate (join f) x
    bin = dec2bin n

dec2bin :: Integer -> [Bool]
dec2bin 0 = []
dec2bin n = odd n : dec2bin (n `div` 2)
