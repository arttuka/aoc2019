module Main where

import Control.Applicative
import Control.Monad
import Data.Char (chr, ord)
import Data.List (find, intercalate, minimumBy, permutations, scanl)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, findWithDefault)
import qualified Data.Map.Strict as Map (fromList)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set, insert, isSubsetOf, notMember)
import qualified Data.Set as Set (empty, fromList)
import Data.Vector (Vector, (!?), findIndex)
import qualified Data.Vector as Vector (fromList, toList)
import Route (Position, Route(..), compareRoutes, findRouteBetween, findRouteVia, move)
import Util

data TileType = Wall | Empty | Entrance | Key Int | Door Int deriving Eq
data Tile = Tile { _type     :: TileType
                 , _position :: Position
                 } deriving Eq
type Tiles = Vector (Vector Tile)
data World = World { _tiles  :: Tiles
                   , _player :: Position
                   }

isKey :: Tile -> Bool
isKey tile = case _type tile of
    Key _ -> True
    _     -> False

instance Show Tile where
    show Tile { _type = tile } = case tile of
      Wall     -> "#"
      Empty    -> "."
      Entrance -> "@"
      Key i    -> [chr (97 + i)]
      Door i   -> [chr (65 + i)]

instance Ord Tile where
    compare t1 t2 = compare (_position t1) (_position t2)

showWorld :: World -> String
showWorld World{_tiles=tiles}
    = intercalate "\n" $ showRow <$> Vector.toList tiles
  where
    showRow :: Vector Tile -> String
    showRow = (show =<<) . Vector.toList

instance Show World where
    show = showWorld

toTileType :: Char -> TileType
toTileType '#'          = Wall
toTileType '.'          = Empty
toTileType '@'          = Entrance
toTileType c
    | between c 'a' 'z' = Key (ord c - 97)
    | otherwise         = Door (ord c - 65)

findEntrance :: Tiles -> Position
findEntrance tiles = position
  where
    position = head $ catMaybes $ zipWith findFromRow [0..] (Vector.toList tiles)
    findFromRow :: Int -> Vector Tile -> Maybe Position
    findFromRow y row = (\x -> (x, y)) <$> findIndex ((== Entrance) . _type) row

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

getTile :: Tiles -> Position -> Maybe Tile
getTile tiles (x, y) = (tiles !? y) >>= (!? x)

tileMatches :: (Tile -> Bool) -> Tiles -> Position -> Bool
tileMatches pred tiles pos = maybe False pred $ getTile tiles pos

isPassable :: Tiles -> Position -> Bool
isPassable = tileMatches ((/= Wall) . _type)

getKeyNumber :: Tile -> Maybe Int
getKeyNumber tile = case _type tile of
    Key i -> Just i
    _     -> Nothing

getDoorNumber :: Tile -> Maybe Int
getDoorNumber tile = case _type tile of
    Door i -> Just i
    _      -> Nothing

doorsOnRoute :: Tiles -> Route -> Set Int
doorsOnRoute tiles Route { _from = from, _route = route } = Set.fromList doors
  where
    doors = mapMaybe ((getDoorNumber =<<) . getTile tiles) $ scanl move from route

getRequiredKeys :: Tiles -> Position -> [Tile] -> Map Int (Set Int)
getRequiredKeys tiles from keys = Map.fromList entries
  where
    entries = catMaybes $ juxtM getKeyNumber (fmap (doorsOnRoute tiles) . findRouteTo . _position) <$> keys
    findRouteTo :: Position -> Maybe Route
    findRouteTo = findRouteBetween (isPassable tiles) from

routeIsPossible :: Map Int (Set Int) -> [Tile] -> Bool
routeIsPossible requiredKeys = step Set.empty
  where
    seenAll :: Int -> Set Int -> Bool
    seenAll num = isSubsetOf (findWithDefault Set.empty num requiredKeys)
    step :: Set Int -> [Tile] -> Bool
    step _ []            = True
    step seenKeys (t:ts) = case _type t of
        Key i  | not (seenAll i seenKeys) -> False
        Key i                             -> step (insert i seenKeys) ts
        Door i | notMember i seenKeys     -> False
        _                                 -> step seenKeys ts

collectAllKeys :: World -> Route
collectAllKeys world @ World { _tiles = tiles, _player = from } = bestRoute
  where
    allTiles        = Vector.toList tiles >>= Vector.toList
    allKeys         = filter isKey allTiles
    requiredKeys    = getRequiredKeys tiles from allKeys
    candidateRoutes = (from:) . fmap _position <$> filter (routeIsPossible requiredKeys) (permutations allKeys)
    routes          = catMaybes $ findRouteVia (isPassable tiles) <$> candidateRoutes
    bestRoute       = minimumBy compareRoutes routes

main :: IO ()
main = do contents <- getContents
          let world = readWorld contents
              route = collectAllKeys world
          --print world
          print route
