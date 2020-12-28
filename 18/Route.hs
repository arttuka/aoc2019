module Route where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad ((<=<), zipWithM)
import Data.Char (chr, ord)
import Data.List (intercalate, unfoldr)
import Data.Map.Strict (Map, lookup)
import qualified Data.Map.Strict as Map (empty, fromList, union)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.Set (Set, notMember, member, union)
import qualified Data.Set as Set (fromList, singleton, toList, union)
import Data.Vector (Vector, (!?), findIndex)
import qualified Data.Vector as Vector (fromList, toList)
import Queue (Queue, pushAll, peek, pop)
import qualified Queue (singleton)
import Util

data TileType = Wall | Empty | Entrance | Key Int | Door Int deriving Eq
data Tile = Tile { _type     :: TileType
                 , _position :: Position
                 } deriving Eq
type Tiles = Vector (Vector Tile)
data World = World { _tiles  :: Tiles
                   , _player :: Position
                   }

data Direction = North | South | West | East deriving (Enum, Eq, Show)
type Position = (Int, Int)
data Route = Route { _from   :: Position
                   , _to     :: Position
                   , _moves  :: [Direction]
                   , _length :: Int
                   } deriving (Eq, Show)

data KeyRoute = KeyRoute { _keyFrom :: Int
                         , _keyTo   :: Int
                         , _doors   :: Set Int
                         , _keys    :: Set Int
                         , _route   :: Route
                         } deriving (Show)

instance Ord Route where
  compare r1 r2 = compare (_length r1) (_length r2)

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
    showRow = show <=< Vector.toList

instance Show World where
    show = showWorld

toTileType :: Char -> TileType
toTileType '#'          = Wall
toTileType '.'          = Empty
toTileType '@'          = Entrance
toTileType c
    | between 'a' 'z' c = Key (ord c - 97)
    | otherwise         = Door (ord c - 65)

getTile :: Tiles -> Position -> Maybe Tile
getTile tiles (x, y) = (tiles !? y) >>= (!? x)

tileMatches :: (Tile -> Bool) -> Tiles -> Position -> Bool
tileMatches pred = (maybe False pred .) . getTile

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

doorsAndKeysOnRoute :: Tiles -> Route -> (Set Int, Set Int)
doorsAndKeysOnRoute tiles Route { _from = from, _moves = moves } = (Set.fromList doors, Set.fromList keys)
  where
    visited = tailInit $ scanl move from moves
    (doors, keys) = doorsAndKeys visited
    doorsAndKeys :: [Position] -> ([Int], [Int])
    doorsAndKeys [] = ([], [])
    doorsAndKeys (p:ps) = case (getDoorNumber =<< tile, getKeyNumber =<< tile) of
        (Just d, Nothing) -> (d:ds, ks)
        (Nothing, Just k) -> (ds, k:ks)
        _                 -> (ds, ks)
      where
        tile = getTile tiles p
        (ds, ks) = doorsAndKeys ps

getRouteBetweenKeys :: Tiles -> Tile -> Tile -> Maybe KeyRoute
getRouteBetweenKeys tiles from to = do
    route   <- findRouteBetween (isPassable tiles) (_position from) (_position to)
    keyTo   <- getKeyNumber to
    let keyFrom       = fromMaybe (-1) $ getKeyNumber from
        (doors, keys) = doorsAndKeysOnRoute tiles route
    return KeyRoute { _keyFrom = keyFrom
                    , _keyTo   = keyTo
                    , _doors   = doors
                    , _keys    = keys
                    , _route   = route
                    }

joinRoutes :: Route -> Route -> Route
joinRoutes r1 r2 = Route { _from   = _from r1
                         , _to     = _to r2
                         , _moves  = _moves r1 ++ _moves r2
                         , _length = _length r1 + _length r2
                         }

reverseRoute :: Route -> Route
reverseRoute r = Route { _from   = _to r
                       , _to     = _from r
                       , _moves  = map reverseDirection $ _moves r
                       , _length = _length r
                       }

reverseKeyRoute :: KeyRoute -> KeyRoute
reverseKeyRoute r = KeyRoute { _keyFrom = _keyTo r
                             , _keyTo   = _keyFrom r
                             , _doors   = _doors r
                             , _keys    = _keys r
                             , _route   = reverseRoute $ _route r
                             }

directions :: [Direction]
directions = [North, South, West, East]

dirToDelta :: Direction -> (Int, Int)
dirToDelta North = ( 0, -1)
dirToDelta South = ( 0,  1)
dirToDelta West  = (-1,  0)
dirToDelta East  = ( 1,  0)

reverseDirection :: Direction -> Direction
reverseDirection North = South
reverseDirection South = North 
reverseDirection West  = East
reverseDirection East  = West

move :: Position -> Direction -> Position
move (x, y) dir = (x + dx, y + dy)
  where
    (dx, dy) = dirToDelta dir

moveBack :: Position -> Direction -> Position
moveBack (x, y) dir = (x - dx, y - dy)
  where
    (dx, dy) = dirToDelta dir

generateRoute :: Position -> Position -> Map Position Direction -> Route
generateRoute from to parents = Route { _from = from, _to = to, _moves = moves, _length = length moves }
  where
    moves    = reverse $ unfoldr step to
    step :: Position -> Maybe (Direction, Position)
    step pos = mapWith (moveBack pos) <$> lookup pos parents

getNewMoves :: Set Position -> Position -> [(Position, Direction)]
getNewMoves discovered pos = [ (p, d) | d <- directions,
                                        let p = move pos d,
                                        notMember p discovered
                             ]

findRoute :: Position -> (Position -> Bool) -> (Position -> Bool) -> Maybe Route
findRoute from isPassable isGoal = searchNextPos (Queue.singleton from) (Set.singleton from) Map.empty
  where
    searchNextPos :: Queue Position -> Set Position -> Map Position Direction -> Maybe Route
    searchNextPos queue discovered parents = peek queue >>= expandPos
      where
        expandPos :: Position -> Maybe Route
        expandPos pos
            | isGoal pos     = Just $ generateRoute from pos parents
            | isPassable pos = let newMoves      = getNewMoves discovered pos
                                   newPositions  = map fst newMoves
                                   newDiscovered = Set.union discovered $ Set.fromList newPositions
                                   newQueue      = pushAll newPositions (pop queue)
                                   newParents    = Map.union parents $ Map.fromList newMoves
                               in searchNextPos newQueue newDiscovered newParents
            | otherwise      = searchNextPos (pop queue) discovered parents

findRouteBetween :: (Position -> Bool) -> Position -> Position -> Maybe Route
findRouteBetween isPassable from to = findRoute from isPassable (== to)

findRouteVia :: (Position -> Bool) -> [Position] -> Maybe Route
findRouteVia isPassable waypoints = foldl1 joinRoutes <$> zipWithM (findRouteBetween isPassable) waypoints (tail waypoints)
