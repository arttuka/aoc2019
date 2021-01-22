{-# LANGUAGE TupleSections #-}
module Route where

import Prelude hiding (lookup)
import Data.Map.Strict (Map, lookup)
import qualified Data.Map.Strict as Map (empty, fromList, union)
import Data.Maybe (maybeToList)
import Data.Set (Set, notMember, member)
import qualified Data.Set as Set (fromList, singleton, toList, union)
import Queue (Queue, push, pushAll, peek, pop)
import qualified Queue (singleton)

data Direction = North | South | West | East deriving (Enum, Show)
type Position = (Int, Int)
type Position3D = (Int, Int, Int)
type Move = Position3D -> Direction -> Maybe Position3D

generateRoute :: Position3D -> Map Position3D Position3D -> [Position3D]
generateRoute pos parents = reverse $ step pos
  where
    step :: Position3D -> [Position3D]
    step pos = case lookup pos parents of
        Nothing  -> []
        Just newPos -> pos : step newPos

getNewMoves :: Set Position3D -> Move -> Position3D -> [Position3D]
getNewMoves discovered move pos = [ p | d <- [North, South, West, East],
                                        p <- maybeToList $ move pos d,
                                        notMember p discovered
                                  ]
  
findRoute :: Position3D -> Position3D -> Move -> Maybe [Position3D]
findRoute from to move = step (Queue.singleton from) (Set.singleton from) Map.empty
  where
    step :: Queue Position3D -> Set Position3D -> Map Position3D Position3D -> Maybe [Position3D]
    step queue discovered parents = case peek queue of
        Nothing              -> Nothing
        Just pos | pos == to -> Just $ generateRoute pos parents
        Just pos             -> step newQueue newDiscovered newParents
            where
              newMoves = getNewMoves discovered move pos
              newDiscovered = Set.union discovered $ Set.fromList newMoves
              newQueue = pushAll newMoves (pop queue)
              newParents = Map.union parents $ Map.fromList $ (, pos) <$> newMoves
