module Main where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad
import Data.List (find, intercalate)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, insert, keys, lookup, singleton)
import qualified Data.Map.Strict as Map (notMember, toList)
import Data.Maybe (mapMaybe, maybe)
import Data.Set (Set, member)
import qualified Data.Set as Set (fromList)
import Data.Vector (fromList)
import Intcode (Program, Result(..), runProgram, startProgram, inputValue, lastOutput)
import qualified Intcode (State)
import Input (Key(..), getKeys, setupStdin)
import Route (Direction(..), Position, findRoute, floodFill, move)

data Tile = Wall | Empty | Oxygen deriving (Eq, Enum)
type Tiles = Map Position Tile
data World = World { _tiles :: Tiles
                   , _droid :: Position
                   }

data State = State { _world    :: World
                   , _intcode  :: Intcode.State
                   , _finished :: Bool}

instance Show Tile where
    show tile = case tile of
      Wall   -> "#"
      Empty  -> "."
      Oxygen -> "x"

showWorld :: World -> String
showWorld world = intercalate "\n" $ map showRow [-1 .. height]
  where
    droid    = _droid world
    tiles    = _tiles world
    (xs, ys) = unzip $ keys tiles
    dx       = negate (minimum xs)
    dy       = negate (minimum ys)
    width    = dx + maximum xs + 1
    height   = dy + maximum ys + 1
    showPosition :: Int -> Int -> String
    showPosition y x
        | pos == droid = "D"
        | otherwise    = maybe " " show $ lookup pos tiles
      where
        pos = (x - dx, y - dy)
    showRow :: Int -> String
    showRow y = [-1 .. width] >>= showPosition y

instance Show World where
    show = showWorld

initialWorld = World { _tiles = singleton (0, 0) Empty
                     , _droid = (0, 0)
                     }

tileMatches :: (Tile -> Bool) -> World -> Position -> Bool
tileMatches pred world pos = maybe False pred $ lookup pos (_tiles world)

isPassable :: World -> Position -> Bool
isPassable = tileMatches (/= Wall)

toInt :: String -> Int
toInt s = read s :: Int

readProgram :: String -> Program
readProgram s = fromList $ map toInt $ splitOn "," s

updateTile :: World -> Position -> Tile -> World
updateTile world position tile = world { _tiles = insert position tile (_tiles world) }

updateWorld :: World -> (Direction, Int) -> World
updateWorld world (input, output) = World { _tiles = newTiles, _droid = newPosition }
  where
    prevPosition  = _droid world
    triedPosition = move prevPosition input
    tile          = toEnum output :: Tile
    newTiles      = insert triedPosition tile (_tiles world)
    newPosition   = if output == 0 then prevPosition else triedPosition

makeMove :: State -> Direction -> State
makeMove state dir = case result of
    Continue s -> state { _intcode = s, _world = updateWorld world (dir, lastOutput s)}
    Finished _ -> state { _finished = True }
  where
    world  = _world state
    result = inputValue (directionToInput dir) (_intcode state)

toNearestUnknown :: World -> Maybe [Direction]
toNearestUnknown world = findRoute pos (isPassable world) isGoal
  where
    pos          = _droid world
    isGoal p     = Map.notMember p (_tiles world)

explore :: State -> [State]
explore state = state : step state []
  where
    step :: State -> [Direction] -> [State]
    step state []         = case toNearestUnknown (_world state) of
                              Just route -> step state route
                              Nothing    -> []
    step state (dir:dirs) = if _finished newState then [newState] else newState : step newState dirs
      where
        newState = makeMove state dir

directionToInput :: Direction -> Int
directionToInput dir = case dir of
    North -> 1
    South -> 2
    West  -> 3
    East  -> 4

keyToDirection :: Key -> Maybe Direction
keyToDirection key = case key of
    ArrowUp    -> Just North
    ArrowDown  -> Just South
    ArrowLeft  -> Just West
    ArrowRight -> Just East
    _          -> Nothing

keyToInput :: Key -> Maybe Int
keyToInput key = case key of
    ArrowUp    -> Just 1
    ArrowDown  -> Just 2
    ArrowLeft  -> Just 3
    ArrowRight -> Just 4
    _          -> Nothing

runExplore :: Program -> [State]
runExplore program = case startProgram program of
    Finished _ -> []
    Continue s -> explore $ State { _world = initialWorld, _intcode = s, _finished = False }

routeToOxygen :: World -> Maybe Int
routeToOxygen world = length <$> findRoute (0, 0) (isPassable world) isOxygen
  where
    isOxygen = tileMatches (== Oxygen) world

main :: IO ()
main = do setupStdin
          contents <- readFile "input.txt"
          keys     <- getKeys
          let program         = readProgram contents
              inputs          = mapMaybe keyToInput keys
              inputDirections = mapMaybe keyToDirection keys
--              outputs         = runProgram program inputs
--              worlds          = scanl updateWorld initialWorld $ zip inputDirections outputs
              states          = runExplore program
              lastWorld       = _world $ last states
              tiles           = Map.toList $ _tiles lastWorld
              oxygenPos       = fst <$> find (\(pos, tile) -> tile == Oxygen) tiles
              fillTime        = floodFill (isPassable lastWorld) <$> oxygenPos
          print lastWorld
          print fillTime