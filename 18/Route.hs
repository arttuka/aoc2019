module Route where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad
import Data.List (unfoldr)
import Data.Map.Strict (Map, lookup)
import qualified Data.Map.Strict as Map (empty, fromList, union)
import Data.Set (Set, notMember, member)
import qualified Data.Set as Set (fromList, singleton, toList, union)
import Queue (Queue, pushAll, peek, pop)
import qualified Queue (singleton)
import Util

data Direction = North | South | West | East deriving (Enum, Eq, Show)
type Position = (Int, Int)
data Route = Route { _from   :: Position
                   , _to     :: Position
                   , _route  :: [Direction]
                   , _length :: Int
                   } deriving (Eq, Show)

compareRoutes :: Route -> Route -> Ordering
compareRoutes r1 r2 = compare (_length r1) (_length r2)

joinRoutes :: Route -> Route -> Route
joinRoutes r1 r2 = Route { _from   = _from r1
                         , _to     = _to r2
                         , _route  = _route r1 ++ _route r2
                         , _length = _length r1 + _length r2
                         }

directions :: [Direction]
directions = [North, South, West, East]

dirToDelta :: Direction -> (Int, Int)
dirToDelta dir = case dir of
    North -> ( 0, -1)
    South -> ( 0,  1)
    West  -> (-1,  0)
    East  -> ( 1,  0)

move :: Position -> Direction -> Position
move (x, y) dir = (x + dx, y + dy)
  where
    (dx, dy) = dirToDelta dir

moveBack :: Position -> Direction -> Position
moveBack (x, y) dir = (x - dx, y - dy)
  where
    (dx, dy) = dirToDelta dir

generateRoute :: Position -> Position -> Map Position Direction -> Route
generateRoute from to parents = Route { _from = from, _to = to, _route = route, _length = length route }
  where
    route    = reverse $ unfoldr step to
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
