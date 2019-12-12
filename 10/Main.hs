module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe (mapMaybe)
import Data.Set (Set, fromList, member)

type Asteroid = (Int, Int)

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

readChar :: Int -> (Int, Char) -> Maybe Asteroid
readChar i (j, '#') = Just (j, i)
readChar _ _        = Nothing

readRow :: (Int, String) -> [Asteroid]
readRow (i, s) = mapMaybe (readChar i) (indexed s)

readMap :: [String] -> [Asteroid]
readMap ls = indexed ls >>= readRow

slope :: (Int, Int) -> (Int, Int) -> (Int, Int)
slope (x0, y0) (x1, y1) = (dx `div` g, dy `div` g)
  where
    dx = x1 - x0
    dy = y1 - y0
    g = dy `gcd` dx

midpoints :: Asteroid -> Asteroid -> [Asteroid]
midpoints from to = step from to
  where (dx, dy)         = slope from to
        step :: Asteroid -> Asteroid -> [Asteroid]
        step (x0, y0) end
            | nxt == end = []
            | otherwise  = nxt : step nxt end
          where
            nxt = (x0 + dx, y0 + dy)

isSight :: Set Asteroid -> Asteroid -> Asteroid -> Bool
isSight asteroids from to
    | from == to = False
    | otherwise  = all isNotAsteroid $ midpoints from to
  where
    isNotAsteroid a = not $ member a asteroids

countWhere :: (a -> Bool) -> [a] -> Int
countWhere pred = length . filter pred

findBest :: [Asteroid] -> (Int, Asteroid)
findBest asteroids = foldl step (0, (0, 0)) asteroids
  where
    asteroidSet = fromList asteroids
    numSeen :: Asteroid -> Int
    numSeen asteroid = countWhere (isSight asteroidSet asteroid) asteroids
    step :: (Int, Asteroid) -> Asteroid -> (Int, Asteroid)
    step (i, best) a = if i > j then (i, best) else (j, a)
      where
        j = numSeen a

main :: IO ()
main = do contents <- fmap lines getContents
          let w = length $ head contents
              h = length contents
              asteroids = readMap contents
              asteroidSet = fromList asteroids
              (i, best) = findBest asteroids
          putStrLn $ show best ++ ": " ++ show i
