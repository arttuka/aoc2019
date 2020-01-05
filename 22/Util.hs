module Util where

import Control.Monad
import Data.Tuple (swap)

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith const (drop n (cycle xs)) xs

range :: Int -> Int -> Int -> [Int]
range start end step = [start, start + step .. end]

getFactor :: Int -> Int -> Int
getFactor a b = s `mod` b
  where
    (_, _, s, _) = euclid a b

euclid :: Int -> Int -> (Int, Int, Int, Int)
euclid a b = last $ takeWhile pred results
  where
    results = (0, a, 1, 0) : (0, b, 0, 1) : zipWith step results (tail results)
    step :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
    step (q0, r0, s0, t0) (q1, r1, s1, t1) = (q2, r2, s2, t2)
      where
        (q2, r2) = quotRem r0 r1 
        s2       = s0 - (q2 * s1)
        t2       = t0 - (q2 * t1)
    pred :: (Int, Int, Int, Int) -> Bool
    pred (_, r, _, _) = r /= 0
