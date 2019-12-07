module Main where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad
import Data.List.Split
import Data.Map.Strict
import Data.Tree

type Orbit = (String, String)
type OrbitTree = Tree String

findOrbitRoot :: [Orbit] -> String
findOrbitRoot orbits =
  let childToParent = fromList $ Prelude.map (\(parent, child) -> (child, parent)) orbits
      findRoot x = maybe x findRoot (lookup x childToParent)
  in findRoot (fst $ head orbits)

buildOrbitTree :: [Orbit] -> OrbitTree
buildOrbitTree orbits =
  let parentToChildren = fromListWith (++) $ Prelude.map (\(parent, child) -> (parent, [child])) orbits
      root = findOrbitRoot orbits
      buildNode x = (x, findWithDefault [] x parentToChildren)
  in unfoldTree buildNode root

countOrbits :: OrbitTree -> Int
countOrbits orbitTree = 
  let sumLevels _ [] = 0
      sumLevels n (lvl:lvls) = (n * length lvl) + sumLevels (n + 1) lvls
  in sumLevels 0 $ levels orbitTree

readOrbit :: String -> Orbit
readOrbit s = (a, b)
  where [a, b] = splitOn ")" s

readOrbits :: String -> [Orbit]
readOrbits s = readOrbit <$> lines s

main :: IO ()
main = do orbits <- fmap readOrbits getContents
          let orbitTree = buildOrbitTree orbits
          print $ countOrbits orbitTree
