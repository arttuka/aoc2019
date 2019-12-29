module Main where

import Control.Applicative
import Control.Monad
import Data.List ((!!), foldl', intercalate)
import Text.Regex.PCRE ((=~), getAllTextSubmatches)

type Coord = (Int, Int, Int)
type Moon = (Coord, Coord)

toInt :: String -> Int
toInt s = read s :: Int

rowRegex = "<x=([0-9-]+), y=([0-9-]+), z=([0-9-]+)>"

parseRow :: String -> Moon
parseRow row = ((toInt x, toInt y, toInt z), (0, 0, 0))
  where
    _:x:y:z:_ = getAllTextSubmatches $ row =~ rowRegex :: [String]

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = iterate f x !! n

add3 :: Coord -> Coord -> Coord
add3 (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

abssum :: Coord -> Int
abssum (a, b, c) = abs a + abs b + abs c

calculateEnergy :: Moon -> Int
calculateEnergy (pos, v) = abssum pos * abssum v

applyGravityOnAxis :: Int -> Int -> Int
applyGravityOnAxis a b
  | a < b     = 1
  | a > b     = -1
  | otherwise = 0

applyGravity :: Moon -> Moon -> Moon
applyGravity (pos@(x1, y1, z1), v) ((x2, y2, z2), _) = (pos, add3 v (dvx, dvy, dvz))
  where
    dvx = applyGravityOnAxis x1 x2
    dvy = applyGravityOnAxis y1 y2
    dvz = applyGravityOnAxis z1 z2

applyVelocity :: Moon -> Moon
applyVelocity (pos, v) = (add3 pos v, v)

simulateStep :: [Moon] -> [Moon]
simulateStep moons = map applyVelocity gravityApplied
  where
    gravityApplied = map (\m -> foldl' applyGravity m moons) moons

main :: IO ()
main = do contents <- fmap lines getContents
          let moons = map parseRow contents
          print $ sum $ map calculateEnergy $ applyN 1000 simulateStep moons
