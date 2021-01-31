{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad ((<=<), liftM2)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector, elemIndex)
import qualified Data.Vector as Vector
import Route
import Util

readTiles :: String -> ([Position], Tiles)
readTiles input = (entrances, tiles)
  where
    tiles     = from2DList $ fmap readTile <$> lines input
    entrances = findTiles (== Entrance) tiles

findTiles :: (Tile -> Bool) -> Tiles -> [Position]
findTiles pred = catMaybes . mapcatIndexed (mapIndexed . findTile) . to2DList
  where
    findTile :: Int -> Int -> Tile -> Maybe Position
    findTile y x tile = if pred tile then Just (x, y) else Nothing

findKeys :: Tiles -> [Position]
findKeys = findTiles (\case (Key _) -> True; _ -> False)

makeQuadrantRouteIndex :: Tiles -> Position -> [Position] -> RouteIndex
makeQuadrantRouteIndex tiles entrance keys = makeRouteIndex $ findAllRoutes tiles intersections entrance
  where
    routePositions = Set.fromList $ fromMaybe [] . findSimpleRoute tiles =<< pairs (entrance : keys)
    intersections = findIntersections tiles routePositions

main :: IO ()
main = do contents <- getContents
          let (entrances, tiles) = readTiles contents
              width              = Vector.length (Vector.head tiles)
              height             = Vector.length tiles
              keys               = ($ findKeys tiles) . filter . (. toQuadrant width height) . (==) <$> [0..3]
              keyNums            = Set.fromList . mapMaybe (getKey tiles) <$> keys
              routeIndexes       = zipWith (makeQuadrantRouteIndex tiles) entrances keys
              bestRoute          = findBestRoute routeIndexes keyNums entrances
          putStrLn $ showTiles tiles
          print $ sum $ map _length bestRoute
