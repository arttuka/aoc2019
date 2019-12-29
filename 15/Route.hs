module Route where

import Prelude hiding (lookup)
import Data.Map.Strict (Map, lookup)
import qualified Data.Map.Strict as Map (empty, fromList, union)
import Data.Set (Set, notMember, member)
import qualified Data.Set as Set (fromList, singleton, toList, union)
import Queue (Queue, push, pushAll, peek, pop)
import qualified Queue (singleton)

data Direction = North | South | West | East deriving (Enum, Show)
type Position = (Int, Int)

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

generateRoute :: Position -> Map Position Direction -> [Direction]
generateRoute pos parents = reverse $ step pos
  where
    step :: Position -> [Direction]
    step pos = case lookup pos parents of
        Nothing  -> []
        Just dir -> dir : step (moveBack pos dir)

getNewMoves :: Set Position -> Position -> [(Position, Direction)]
getNewMoves discovered pos = [ (p, d) | d <- [North, South, West, East],
                                        let p = move pos d,
                                        notMember p discovered
                             ]
  
findRoute :: Position -> (Position -> Bool) -> (Position -> Bool) -> Maybe [Direction]
findRoute from isPassable isGoal = step (Queue.singleton from) (Set.singleton from) Map.empty
  where
    step :: Queue Position -> Set Position -> Map Position Direction -> Maybe [Direction]
    step queue discovered parents = case peek queue of
        Nothing                   -> Nothing
        Just pos | isGoal pos     -> Just $ generateRoute pos parents
        Just pos | isPassable pos -> step newQueue newDiscovered newParents
            where
              newMoves = getNewMoves discovered pos
              newPositions = map fst newMoves
              newDiscovered = Set.union discovered $ Set.fromList newPositions
              newQueue = pushAll newPositions (pop queue)
              newParents = Map.union parents $ Map.fromList newMoves
        _                         -> step (pop queue) discovered parents

floodFill :: (Position -> Bool) -> Position -> Int
floodFill isPassable from = step (-1) [from] (Set.singleton from)
  where
    step :: Int -> [Position] -> Set Position -> Int
    step i [] _            = i
    step i next discovered = step (succ i) (Set.toList newPositions) newDiscovered
      where
        newPositions = Set.fromList $ filter isPassable $ fst <$> (next >>= getNewMoves discovered)
        newDiscovered = Set.union discovered newPositions
