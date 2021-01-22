{-# LANGUAGE TupleSections #-}

module Route where

import Debug.Trace

import Prelude hiding (lookup)
import Control.Monad ((<=<), liftM2)
import Data.Char (chr, ord)
import Data.List (intercalate, minimumBy, sort)
import Data.Map.Strict (Map, lookup)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set, member, notMember)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Data.Vector (Vector, (!?))
import Queue (Queue, pushAll, peek, pop)
import qualified Queue
import Util

data Tile = Wall | Empty | Entrance | Key Int | Door Int deriving Eq
instance Show Tile where
  show Wall     = "#"
  show Empty    = "."
  show Entrance = "@"
  show (Key i)  = [chr (97 + i)]
  show (Door i) = [chr (65 + i)]

type Tiles = Vector (Vector Tile)
type Position = (Int, Int)
type SimpleRoute = [Position]
data Route = Route { _from    :: Position
                   , _to      :: Position
                   , _keyFrom :: Maybe Int
                   , _keyTo   :: Maybe Int
                   , _doors   :: Set Int
                   , _route   :: [Position]
                   , _length  :: Int
                   } deriving (Eq, Show)

instance Ord Route where
  compare r1 r2 = compare (_length r1) (_length r2)

type RouteIndex = Map Position [Route]

data PartialRoute = PartialRoute { _prFrom   :: Position
                                 , _prTo     :: Position
                                 , _prKeys   :: Set Int
                                 , _prRoutes :: [Route]
                                 , _prLength :: Int
                                 } deriving (Eq, Show)

instance Ord PartialRoute where
  compare r1 r2 = compare (_prLength r1) (_prLength r2)

readTile :: Char -> Tile
readTile '#'            = Wall
readTile '.'            = Empty
readTile '@'            = Entrance
readTile c
    | between 'a' 'z' c = Key (ord c - 97)
    | otherwise         = Door (ord c - 65)

showTiles :: Tiles -> String
showTiles tiles = intercalate "\n" $ (show =<<) <$> to2DList tiles

isPassable :: Tiles -> Position -> Bool
isPassable tiles pos = case getTile tiles pos of
  Nothing   -> False
  Just Wall -> False
  _         -> True

getTile :: Tiles -> Position -> Maybe Tile
getTile tiles = (tiles !!?) . swap

getKey :: Tiles -> Position -> Maybe Int
getKey tiles pos = case getTile tiles pos of
  Just (Key k) -> Just k
  _            -> Nothing

getDoor :: Tiles -> Position -> Maybe Int
getDoor tiles pos = case getTile tiles pos of
  Just (Door d) -> Just d
  _             -> Nothing

reverseRoute :: Route -> Route
reverseRoute r = Route { _from    = _to r
                       , _to      = _from r
                       , _keyFrom = _keyTo r
                       , _keyTo   = _keyFrom r
                       , _doors   = _doors r
                       , _route   = tail $ reverse (_from r : _route r)
                       , _length  = _length r
                       }

addToPartial :: Route -> PartialRoute -> PartialRoute
addToPartial r pr = PartialRoute { _prFrom = _prFrom pr
                                 , _prTo   = _to r
                                 , _prKeys = foldMaybe Set.insert (_keyTo r) (_prKeys pr)
                                 , _prLength = _length r + _prLength pr
                                 , _prRoutes = _prRoutes pr ++ [r]
                                 }

toSimpleRoute :: PartialRoute -> SimpleRoute
toSimpleRoute = _route <=< _prRoutes

makeRouteIndex :: [Route] -> RouteIndex
makeRouteIndex routes = Map.fromListWith (++) $ map (juxt _from pure) routes

adjacentPositions :: Position -> [Position]
adjacentPositions pos = addT2 pos <$> [(1, 0), (-1, 0), (0, 1), (0, -1)]

getNextPositions :: Tiles -> Set Position -> Position -> [Position]
getNextPositions tiles discovered = filter (andPred (isPassable tiles) (`notMember` discovered)) . adjacentPositions

generateSimpleRoute :: Map Position Position -> Position -> SimpleRoute
generateSimpleRoute parents = reverse . iterateMaybe (`lookup` parents)

generateRoute :: Tiles -> Map Position Position -> Position -> Route
generateRoute tiles parents to = Route { _from    = from
                                       , _to      = to
                                       , _keyFrom = getKey tiles from
                                       , _keyTo   = getKey tiles to
                                       , _doors   = Set.fromList $ mapMaybe (getDoor tiles) simpleRoute
                                       , _route   = simpleRoute
                                       , _length  = length simpleRoute
                                       }
  where
    (from:simpleRoute) = generateSimpleRoute parents to

type SearchFn a = Queue Position -> Set Position -> Map Position Position -> a

expandPosition :: Tiles -> a -> SearchFn a -> SearchFn ((Position -> Bool) -> Position -> a)
expandPosition tiles goalVal recurFn queue discovered parents isGoal pos
    | isGoal pos = goalVal
    | otherwise  = recurFn nextQueue nextDiscovered nextParents
  where
    nextPositions  = getNextPositions tiles discovered pos
    nextDiscovered = Set.union discovered $ Set.fromList nextPositions
    nextQueue      = pushAll nextPositions (pop queue)
    nextParents    = Map.union parents $ Map.fromList $ fmap (,pos) nextPositions  

findSimpleRoute :: Tiles -> (Position, Position) -> Maybe SimpleRoute
findSimpleRoute tiles (from, to) = searchNextPos (Queue.singleton from) (Set.singleton from) Map.empty
  where
    searchNextPos :: SearchFn (Maybe SimpleRoute)
    searchNextPos queue discovered parents = peek queue >>= expandPosition tiles goalVal searchNextPos queue discovered parents (== to)
      where
        goalVal = Just $ generateSimpleRoute parents to

findIntersections :: Tiles -> Set Position -> Set Position
findIntersections tiles routePositions = Set.filter (orPred keyPred intersectionPred) routePositions
  where
    keyPred :: Position -> Bool
    keyPred = isJust . getKey tiles
    intersectionPred :: Position -> Bool
    intersectionPred = (3 <=) . length . filter (`member` routePositions) . adjacentPositions

findAllRoutes :: Tiles -> Set Position -> Position -> [Route]
findAllRoutes tiles intersections from = allRoutes ++ map reverseRoute allRoutes
  where
    allRoutes = findNextRoutes (Set.singleton from) from
    findNextRoutes :: Set Position -> Position -> [Route]
    findNextRoutes discovered from
        | null routes = []
        | otherwise   = routes ++ (findNextRoutes finalDiscovered . _to =<< routes)
      where
        (finalDiscovered, routes) = searchNextPos [] (andPred (`Set.member` intersections) (/= from)) (Queue.singleton from) discovered Map.empty
    searchNextPos :: [Position] -> (Position -> Bool) -> SearchFn (Set Position, [Route])
    searchNextPos goals isGoal queue discovered parents = case peek queue of
        Just pos -> let goalVal = searchNextPos (pos : goals) isGoal (pop queue) discovered parents
                    in expandPosition tiles goalVal (searchNextPos goals isGoal) queue discovered parents isGoal pos
        Nothing  -> (discovered, generateRoute tiles parents <$> goals)

findBestRoute :: RouteIndex -> Set Int -> Position -> PartialRoute
findBestRoute routeIndex keys from = minimumBy (comparing _prLength) possibleRoutes
  where
    possibleRoutes = searchNextPos (getNextQueue Queue.empty emptyPartial from) (Map.singleton from (Map.singleton Set.empty 0)) 
    emptyPartial = PartialRoute { _prFrom   = from
                                , _prTo     = from
                                , _prKeys   = Set.empty
                                , _prLength = 0
                                , _prRoutes = []}
    getNextQueue :: Queue (PartialRoute, Route) -> PartialRoute -> Position -> Queue (PartialRoute, Route)
    getNextQueue queue pr pos = pushAll (map (pr,) (routeIndex Map.! pos)) (pop queue)
    isDiscovered :: Map Position (Map (Set Int) Int) -> Set Int -> Int -> Position -> Bool
    isDiscovered discovered keys length pos = case discovered Map.!? pos of
      Nothing -> False
      Just m  -> any (length >=) [ l | (ks, l) <- Map.toList m
                                     , keys `Set.isSubsetOf` ks
                                     ]
    searchNextPos :: Queue (PartialRoute, Route) -> Map Position (Map (Set Int) Int) -> [PartialRoute]
    searchNextPos queue discovered = maybe [] expandRoute (peek queue)
      where
        alterDiscovered :: Set Int -> Int -> Maybe (Map (Set Int) Int) -> Maybe (Map (Set Int) Int)
        alterDiscovered keys length Nothing  = Just $ Map.singleton keys length
        alterDiscovered keys length (Just m) = Just $ Map.insert keys length m
        expandRoute :: (PartialRoute, Route) -> [PartialRoute]
        expandRoute (pr, r)
            | not (_doors r `Set.isSubsetOf` _prKeys pr)      = searchNextPos (pop queue) discovered
            | isDiscovered discovered nextKeys nextLength pos = searchNextPos (pop queue) discovered
            | keys == nextKeys                                = nextRoute : searchNextPos (pop queue) nextDiscovered
            | otherwise                                       = searchNextPos (getNextQueue queue nextRoute pos) nextDiscovered
          where
            prevDiscovered = lookup2 discovered pos nextKeys
            pos = _to r
            nextRoute = addToPartial r pr
            nextKeys = _prKeys nextRoute
            nextLength = _prLength nextRoute
            nextDiscovered = Map.alter (alterDiscovered nextKeys nextLength) pos discovered
