module Main where

import Control.Applicative
import Control.Monad
import Data.List ((!!), foldl', intercalate)
import Text.Regex.PCRE ((=~), getAllTextSubmatches)

type Coord = (Int, Int, Int)
type Moon = (Coord, Coord)
type Axis = (Int, Int)

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

gravityOnAxis :: Int -> Int -> Int
gravityOnAxis a b
  | a < b     = 1
  | a > b     = -1
  | otherwise = 0

applyGravity :: Moon -> Moon -> Moon
applyGravity (pos@(x1, y1, z1), v) ((x2, y2, z2), _) = (pos, add3 v (dvx, dvy, dvz))
  where
    dvx = gravityOnAxis x1 x2
    dvy = gravityOnAxis y1 y2
    dvz = gravityOnAxis z1 z2

applyVelocity :: Moon -> Moon
applyVelocity (pos, v) = (add3 pos v, v)

simulateStep :: [Moon] -> [Moon]
simulateStep moons = map applyVelocity gravityApplied
  where
    gravityApplied = map (\m -> foldl' applyGravity m moons) moons

applyGravityOnAxis :: Axis -> Axis -> Axis
applyGravityOnAxis (t1, v1) (t2, _) = (t1, v1 + gravityOnAxis t1 t2)

applyVelocityOnAxis :: Axis -> Axis
applyVelocityOnAxis (t, v) = (t + v, v)

simulateStepOnAxis :: [Axis] -> [Axis]
simulateStepOnAxis axes = map applyVelocityOnAxis gravityApplied
  where
    gravityApplied = map (\a -> foldl' applyGravityOnAxis a axes) axes

moonToAxes :: Moon -> (Axis, Axis, Axis)
moonToAxes ((x, y, z), (dx, dy, dz)) = ((x, dx), (y, dy), (z, dz))

findCycle :: (Eq a) => (a -> a) -> a -> Int
findCycle f x = step 1 x (f x)
  where
    step n t h
        | t == h    = n
        | otherwise = step (succ n) (f t) (f (f h))

main :: IO ()
main = do contents <- fmap lines getContents
          let moons = map parseRow contents
              (xs, ys, zs) = unzip3 $ map moonToAxes moons
              cycles = map (findCycle simulateStepOnAxis) [xs, ys, zs]
          print $ foldl1 lcm cycles
