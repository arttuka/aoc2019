{-# LANGUAGE TupleSections #-}
module Main where

import Debug.Trace (trace)

import Data.List (foldl1, minimum)
import Data.Map.Strict (Map, (!), findWithDefault)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe, fromJust)
import Data.Set (Set, insert, isSubsetOf, notMember)
import qualified Data.Set as Set
import Data.Vector (Vector, findIndex)
import qualified Data.Vector as Vector (fromList, toList)
import Route 
import Util

findEntrance :: Tiles -> Position
findEntrance tiles = position
  where
    position = head $ catMaybes $ zipWith findFromRow [0..] (Vector.toList tiles)
    findFromRow :: Int -> Vector Tile -> Maybe Position
    findFromRow y row = (,y) <$> findIndex ((== Entrance) . _type) row

readWorld :: String -> World
readWorld input = World { _tiles  = tiles
                        , _player = entrance
                        }
  where
    tiles    = readRows $ lines input
    entrance = findEntrance tiles
    readRows :: [String] -> Tiles
    readRows = Vector.fromList . zipWith readRow [0..]
    readRow :: Int -> String -> Vector Tile
    readRow y = Vector.fromList . zipWith (readTile y) [0..]
    readTile :: Int -> Int -> Char -> Tile
    readTile y x c = Tile { _type = toTileType c, _position = (x, y)}

getRoutesFromKey :: Tiles -> [Tile] -> Tile -> Map Position KeyRoute
getRoutesFromKey tiles allKeys from = Map.fromList $ map (juxt (_to . _route) id) routes
  where
    routes = mapMaybe (getRouteBetweenKeys tiles from) $ filter (/= from) allKeys

getRoutesBetween :: Tiles -> Tile -> Tile -> [(Position, Position, KeyRoute)]
getRoutesBetween tiles from to = case getRouteBetweenKeys tiles from to of
  Nothing    -> []
  Just route -> [(_position from, _position to, route), (_position to, _position from, reverseKeyRoute route)] 

getRoutesBetweenKeys :: Tiles -> [Tile] -> Map Position (Map Position KeyRoute)
getRoutesBetweenKeys tiles allKeys = Map.fromList <$> groupBy fst3 rst3 allRoutes
  where
    allRoutes = [route | k1 <- allKeys
                       , k2 <- allKeys
                       , k1 < k2
                       , route <- getRoutesBetween tiles k1 k2
                       ]

generatePossibleRoutes :: Map Position KeyRoute -> Map Position (Map Position KeyRoute) -> [Position] -> [Route]
generatePossibleRoutes initialRoutes routesBetweenKeys allKeys = foldl1 joinRoutes . map _route <$> (start =<< pickEvery allKeys)
  where
    start :: (Position, [Position]) -> [[KeyRoute]]
    start (p, ps)
        | not (Set.null (_keys r)) = []
        | Set.null (_doors r)      = (r:) <$> step keys p ps
        | otherwise                = []
      where
        r    = initialRoutes ! p
        keys = Set.singleton (_keyTo r)
    step :: Set Int -> Position -> [Position] -> [[KeyRoute]]
    step _ _ [] = [[]]
    step keys from ps = pickEvery ps >>= innerStep keys from
    innerStep :: Set Int -> Position -> (Position, [Position]) -> [[KeyRoute]]
    innerStep keys from (to, ps)
        | not (_keys r `isSubsetOf` keys) = [] 
        | _doors r `isSubsetOf` keys      = (r:) <$> step nextKeys to ps
        | otherwise                       = []
      where
        r        = lookup2 routesBetweenKeys from to
        nextKeys = insert (_keyTo r) keys 

collectAllKeys :: World -> Route
collectAllKeys world @ World { _tiles = tiles, _player = from } = trace ("Number of routes " ++ show (length possibleRoutes)) $ minimum possibleRoutes
  where
    allTiles          = Vector.toList tiles >>= Vector.toList
    allKeys           = filter isKey allTiles
    initialRoutes     = getRoutesFromKey tiles allKeys $ fromJust $ getTile tiles from
    routesBetweenKeys = getRoutesBetweenKeys tiles allKeys
    possibleRoutes    = generatePossibleRoutes initialRoutes routesBetweenKeys $ map _position allKeys

main :: IO ()
main = do contents <- getContents
          let world = readWorld contents
              route = collectAllKeys world
          print world
          print route
