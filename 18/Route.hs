{-# LANGUAGE TupleSections #-}

module Route where

import Debug.Trace

import Prelude hiding (lookup)
import Control.Applicative ((<|>))
import Control.Monad ((<=<), liftM2, liftM3)
import Data.Char (chr, ord)
import Data.List (find, intercalate, minimumBy, null, sort)
import Data.Map.Strict (Map, lookup)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set, member, notMember)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Data.Vector (Vector, (!?))
import Queue (Queue, push, pushAll, peek, pop)
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
type RouteIndex = Map Position [Route]
data Route = Route { _from    :: Position
                   , _to      :: Position
                   , _keyFrom :: Maybe Int
                   , _keyTo   :: Maybe Int
                   , _doors   :: [Int]
                   , _route   :: [Position]
                   , _length  :: Int
                   } deriving (Eq, Show)
data PartialRoute = PartialRoute { _prFrom   :: Position
                                 , _prTo     :: Position
                                 , _prKeys   :: Set Int
                                 , _prRoutes :: [Route]
                                 , _prLength :: Int
                                 } deriving (Eq, Show)

readTile :: Char -> Tile
readTile '#'            = Wall
readTile '.'            = Empty
readTile '@'            = Entrance
readTile c
    | between 'a' 'z' c = Key (ord c - 97)
    | otherwise         = Door (ord c - 65)

showTiles :: Tiles -> String
showTiles = intercalate "\n" . ((show =<<) <$>) . to2DList

isPassable :: Tiles -> Position -> Bool
isPassable = (maybe False (/= Wall) .) . getTile

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
addToPartial r pr = PartialRoute { _prFrom   = _prFrom pr
                                 , _prTo     = _to r
                                 , _prKeys   = insertMaybe (_keyTo r) (_prKeys pr)
                                 , _prLength = _length r + _prLength pr
                                 , _prRoutes = _prRoutes pr ++ [r]
                                 }

makeRouteIndex :: [Route] -> RouteIndex
makeRouteIndex = Map.fromListWith (++) . map (juxt _from pure)

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
                                       , _doors   = mapMaybe (getDoor tiles) simpleRoute
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
    nextDiscovered = insertAllSet discovered nextPositions
    nextQueue      = pushAll (pop queue) nextPositions
    nextParents    = insertAllMap parents $ fmap (,pos) nextPositions

findSimpleRoute :: Tiles -> (Position, Position) -> Maybe SimpleRoute
findSimpleRoute tiles (from, to) = searchNextPos (Queue.singleton from) (Set.singleton from) Map.empty
  where
    searchNextPos :: SearchFn (Maybe SimpleRoute)
    searchNextPos queue discovered parents = peek queue >>= expandPosition tiles goalVal searchNextPos queue discovered parents (== to)
      where
        goalVal = Just $ generateSimpleRoute parents to

findIntersections :: Tiles -> Set Position -> Set Position
findIntersections tiles routePositions = Set.filter (orPred tilePred intersectionPred) routePositions
  where
    isIntersection :: Tile -> Bool
    isIntersection (Key _)  = True
    isIntersection Entrance = True
    isIntersection _        = False
    tilePred :: Position -> Bool
    tilePred = maybe False isIntersection . getTile tiles
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


alterDiscovered :: Set Int -> Int -> Maybe (Map (Set Int) Int) -> Maybe (Map (Set Int) Int)
alterDiscovered keys length = return . maybe (Map.singleton keys length) (Map.insert keys length)

findPossibleRoutes :: RouteIndex -> Set Int -> Position -> [[Route]]
findPossibleRoutes routeIndex keys from = _prRoutes <$> searchNextPos (getNextQueue Queue.empty emptyPartial from) (Map.singleton from (Map.singleton Set.empty 0))
  where
    emptyPartial = PartialRoute { _prFrom   = from
                                , _prTo     = from
                                , _prKeys   = Set.empty
                                , _prLength = 0
                                , _prRoutes = []}
    getNextQueue :: Queue (PartialRoute, Route) -> PartialRoute -> Position -> Queue (PartialRoute, Route)
    getNextQueue queue pr pos = pushAll (pop queue) $ map (pr,) (routeIndex Map.! pos)
    isDiscovered :: Map Position (Map (Set Int) Int) -> Set Int -> Int -> Position -> Bool
    isDiscovered discovered keys length = maybe False (any (\(ks, l) -> length >= l && keys `Set.isSubsetOf` ks) . Map.toList) . (discovered Map.!?)
    searchNextPos :: Queue (PartialRoute, Route) -> Map Position (Map (Set Int) Int) -> [PartialRoute]
    searchNextPos queue discovered = maybe [] expandRoute (peek queue)
      where
        expandRoute :: (PartialRoute, Route) -> [PartialRoute]
        expandRoute (pr@PartialRoute{ _prKeys = foundKeys }, r@Route{ _to = pos })
            | not ((Set.fromList (_doors r) `Set.intersection` keys) `Set.isSubsetOf` foundKeys) || isDiscovered discovered nextKeys nextLength pos
                               = searchNextPos (pop queue) discovered
            | keys == nextKeys = nextRoute : searchNextPos (pop queue) nextDiscovered
            | otherwise        = searchNextPos (getNextQueue queue nextRoute pos) nextDiscovered
          where
            nextRoute@PartialRoute{ _prKeys = nextKeys, _prLength = nextLength } = addToPartial r pr
            nextDiscovered = Map.alter (alterDiscovered nextKeys nextLength) pos discovered

findBestRoute :: [RouteIndex] -> [Set Int] -> [Position] -> [Route]
findBestRoute routeIndexes allKeys from = minimumBy (comparing (sum . fmap _length)) interleavedRoutes
  where
    interleavedRoutes = mapMaybe (interleaveRoutes (Set.singleton 0) Set.empty 0) $ cartesian possibleRoutes
    possibleRoutes = zipWith3 findPossibleRoutes routeIndexes allKeys from
    keyToQ = mapBy snd fst $ mapcatIndexed (zip . repeat) (Set.toList <$> allKeys)
    interleaveRoutes :: Set Int -> Set Int -> Int -> [[Route]] -> Maybe [Route]
    interleaveRoutes _ _ _ [[], [], [], []] = Just []
    interleaveRoutes tried keys q routes = (xs ++) <$> (flip (interleaveRoutes nextTried nextKeys) (replaceNth q ys routes) =<< nextQ)
      where
        (nextKeys, xs, ys) = spanPossible keys (routes !! q)
        nextTried = if null xs then q `Set.insert` tried else Set.singleton q
        nextQ = (safeHead ys >>= getDoorQ) <|> find (`notMember` tried) [0..3]
        getDoorQ :: Route -> Maybe Int
        getDoorQ = find (`notMember` tried) . fmap (keyToQ Map.!) . filter (`notMember` nextKeys) . _doors
    spanPossible :: Set Int -> [Route] -> (Set Int, [Route], [Route])
    spanPossible keys [] = (keys, [], [])
    spanPossible keys xs@(x:xs')
      | Set.fromList (_doors x) `Set.isSubsetOf` keys = let (ks, ys, zs) = spanPossible (insertMaybe (_keyTo x) keys) xs' in (ks, x:ys, zs)
      | otherwise                                     = (keys, [], xs)
