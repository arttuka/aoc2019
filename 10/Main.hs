module Main where

import Prelude hiding (null)
import Control.Applicative
import Control.Monad
import Data.List ((!!))
import Data.List.Extra (nubSort)
import Data.Maybe (mapMaybe)
import Data.Ord (Ordering (LT, GT, EQ))
import Data.Ratio (Rational, (%))
import Data.Set (Set, delete, fromList, member, null, toList)

type Asteroid = (Int, Int)
data AMap = AMap (Set Asteroid) Int Int
data Half = H1 | H2 deriving (Eq, Enum, Ord, Show)
data Slope = Slope Half Int Int deriving (Eq, Show)

instance Ord Slope where
    compare s1@(Slope h1 x1 y1) s2@(Slope h2 x2 y2)
        | s1 == s2             = EQ
        | h1 /= h2             = compare h1 h2
        | x1 == 0              = LT
        | x2 == 0              = GT
        | otherwise            = compare (y1 % x1) (y2 % x2)

within :: Ord a => a -> (a, a) -> Bool
within x (from, to) = (x >= from) && (x < to)

between :: Ord a => a -> a -> a -> Bool
between x from to = (x >= from) && (x < to)

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

readChar :: Int -> (Int, Char) -> Maybe Asteroid
readChar i (j, '#') = Just (j, i)
readChar _ _        = Nothing

readRow :: (Int, String) -> [Asteroid]
readRow (i, s) = mapMaybe (readChar i) (indexed s)

readMap :: [String] -> [Asteroid]
readMap ls = indexed ls >>= readRow

half :: Int -> Int -> Half
half x y
    | x > 0 || (x == 0 && y < 0) = H1
    | otherwise                  = H2

slope :: Asteroid -> Asteroid -> Slope
slope (x0, y0) (x1, y1) = Slope (half dx dy) (dx `div` g) (dy `div` g)
  where
    dx = x1 - x0
    dy = y1 - y0
    g = dy `gcd` dx

asteroidInDirection :: AMap -> Asteroid -> Slope -> Maybe Asteroid
asteroidInDirection (AMap asteroids width height) from (Slope _ dx dy) = step from
  where step :: Asteroid -> Maybe Asteroid
        step (x, y)
            | member nxt asteroids                                  = Just nxt
            | not $ x `within` (0, width) && y `within` (0, height) = Nothing
            | otherwise                                             = step nxt
          where
            nxt = (x + dx, y + dy)

isSight :: AMap -> Asteroid -> Asteroid -> Bool
isSight aMap from to
    | from == to = False
    | otherwise  = case nextAsteroid of
        Just a  -> a == to
        Nothing -> False
  where
    nextAsteroid = asteroidInDirection aMap from (slope from to)

countWhere :: (a -> Bool) -> [a] -> Int
countWhere pred = length . filter pred

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = foldl1 max'
  where max' x y = case compare (f x) (f y) of
                      GT -> x
                      _  -> y

findBest :: AMap -> Asteroid
findBest aMap@(AMap asteroidSet _ _) = maximumOn numSeen asteroids
  where
    asteroids = toList asteroidSet
    numSeen :: Asteroid -> Int
    numSeen asteroid = countWhere (isSight aMap asteroid) asteroids

deleteAsteroid :: Asteroid -> AMap -> AMap
deleteAsteroid a (AMap asteroids w h) = AMap (delete a asteroids) w h

vaporizedAsteroids :: Asteroid -> [Slope] -> AMap -> [Asteroid]
vaporizedAsteroids from slopes = step slopes
  where
    step :: [Slope] -> AMap -> [Asteroid]
    step [] aMap                          = step slopes aMap
    step (s:ss) aMap@(AMap asteroids w h)
        | null asteroids = []
        | otherwise      = case asteroidInDirection aMap from s of
            Nothing -> step ss aMap
            Just a  -> a : step ss (deleteAsteroid a aMap)

main :: IO ()
main = do contents <- fmap lines getContents
          let w = length $ head contents
              h = length contents
              asteroids = readMap contents
              asteroidMap = AMap (fromList asteroids) w h
              best = findBest asteroidMap
              slopes = [slope best a | a <- asteroids, a /= best]
              sortedSlopes = nubSort slopes
              vaporizeOrder = vaporizedAsteroids best sortedSlopes (deleteAsteroid best asteroidMap)
          print $ vaporizeOrder !! 199
    