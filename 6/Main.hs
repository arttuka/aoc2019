module Main where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad
import Data.List.Split
import Data.Map.Strict hiding (map, drop)
import Data.Tree

type Orbit = (String, String)
type OrbitTree = Tree String

longestCommonPrefix :: Eq a => [a] -> [a] -> [a]
longestCommonPrefix [] _ = []
longestCommonPrefix _ [] = []
longestCommonPrefix (x:xs) (y:ys) = if x == y then x : longestCommonPrefix xs ys else []

findOrbitRoot :: [Orbit] -> Map String String -> String
findOrbitRoot orbits childToParent =
  let findRoot x = maybe x findRoot (lookup x childToParent)
  in findRoot (fst $ head orbits)

buildOrbitTree :: String -> [Orbit] -> OrbitTree
buildOrbitTree root orbits =
  let parentToChildren = fromListWith (++) $ map (\(parent, child) -> (parent, [child])) orbits
      buildNode x = (x, findWithDefault [] x parentToChildren)
  in unfoldTree buildNode root

countOrbits :: OrbitTree -> Int
countOrbits orbitTree = 
  let sumLevels _ [] = 0
      sumLevels n (lvl:lvls) = (n * length lvl) + sumLevels (n + 1) lvls
  in sumLevels 0 $ levels orbitTree

findRouteFromRoot :: String -> OrbitTree -> Maybe [String]
findRouteFromRoot end (Node label children)
  | end == label = Just [end]
  | otherwise    = fmap ((:) label) $ msum $ fmap (findRouteFromRoot end) children

findRoute :: String -> String -> OrbitTree -> Maybe [String]
findRoute start end tree = do
  routeToStart <- findRouteFromRoot start tree
  routeToEnd   <- findRouteFromRoot end tree
  let prefix = longestCommonPrefix routeToStart routeToEnd
      len    = length prefix
      startPath = drop (len - 1) routeToStart
      endPath   = drop len routeToEnd
  Just $ reverse startPath ++ endPath

findRouteFromYouToSan :: Map String String -> OrbitTree -> Maybe [String]
findRouteFromYouToSan childToParent tree = do
  start <- lookup "YOU" childToParent
  end <- lookup "SAN" childToParent
  findRoute start end tree

readOrbit :: String -> Orbit
readOrbit s = (a, b)
  where [a, b] = splitOn ")" s

readOrbits :: String -> [Orbit]
readOrbits s = readOrbit <$> lines s

main :: IO ()
main = do orbits <- fmap readOrbits getContents
          let childToParent = fromList $ map (\(parent, child) -> (child, parent)) orbits
              root = findOrbitRoot orbits childToParent
              orbitTree = buildOrbitTree root orbits
              route = findRouteFromYouToSan childToParent orbitTree
          print $ length <$> route
