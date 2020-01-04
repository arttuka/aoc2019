module Util where

import Data.Maybe (listToMaybe)
import Data.List (isPrefixOf, tails, unfoldr)

between :: Ord a => a -> a -> a -> Bool
between x from to = x > from && x < to

filterBy :: [Bool] -> [a] -> [a]
filterBy conds as = [a | (cond, a) <- zip conds as, cond]

findIndexOfPattern :: Eq a => [a] -> [a] -> Maybe Int
findIndexOfPattern pattern list = listToMaybe [i | (i, xs) <- zip [0..] (tails list), isPrefixOf pattern xs]

countPattern :: Eq a => [a] -> [a] -> Int
countPattern pattern list = length $ unfoldr (removePattern pattern) list
  where
    patternLength = length pattern
    removePattern :: Eq a => [a] -> [a] -> Maybe ((), [a])
    removePattern pattern list = (\i -> ((), drop (i + patternLength) list)) <$> findIndexOfPattern pattern list

replacePattern :: Eq a => [a] -> a -> [a] -> [a]
replacePattern _ _ [] = []
replacePattern pattern x list
    | isPrefixOf pattern list = x : replacePattern pattern x (drop (length pattern) list)
    | otherwise               = (head list) : replacePattern pattern x (tail list)

asList :: a -> a -> [a]
asList x y = [x, y]

interleave :: [a] -> [a] -> [a]
interleave as bs = concat $ zipWith asList as bs
