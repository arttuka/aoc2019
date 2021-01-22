{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad ((<=<))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector, elemIndex)
import qualified Data.Vector as Vector
import Route
import Util

findEntrance :: Tiles -> Position
findEntrance tiles = position
  where
    position = head $ zipIndexedMaybe findFromRow (Vector.toList tiles)
    findFromRow :: Int -> Vector Tile -> Maybe Position
    findFromRow y row = (,y) <$> elemIndex Entrance row

readTiles :: String -> (Position, Tiles)
readTiles input = (entrance, tiles)
  where
    tiles    = from2DList $ fmap readTile <$> lines input
    entrance = findEntrance tiles

getKeyPositions :: Tiles -> [Position]
getKeyPositions = uncurry getFromRow <=< (indexed . to2DList)
  where
    getFromRow :: Int -> [Tile] -> [Position]
    getFromRow = zipIndexedMaybe . getFromTile
    getFromTile :: Int -> Int -> Tile -> Maybe Position
    getFromTile y x (Key _) = Just (x, y)
    getFromTile _ _ _       = Nothing

main :: IO ()
main = do contents <- getContents
          let (entrance, tiles) = readTiles contents
              keys              = getKeyPositions tiles
              keyNums           = Set.fromList $ mapMaybe (getKey tiles) keys
              keypairs          = pairs (entrance:keys)
              routes            = findSimpleRoute tiles <$> keypairs
              routePositions    = Set.fromList $ fromMaybe [] . findSimpleRoute tiles =<< pairs (entrance : keys)
              intersections     = entrance `Set.insert` findIntersections tiles routePositions
              allRoutes         = findAllRoutes tiles intersections entrance
              routeIndex        = makeRouteIndex allRoutes
              bestRoute         = findBestRoute routeIndex keyNums entrance
          putStrLn $ showTiles tiles
          print (toSimpleRoute bestRoute)
          print (_prLength bestRoute)
