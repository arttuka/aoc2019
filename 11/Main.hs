module Main where

import Control.Monad
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Set (Set, delete, empty, fromList, insert, member, singleton, toList)
import qualified Data.Vector (fromList)
import Intcode (Program, runProgram)

data Direction = UP | RIGHT | DOWN | LEFT deriving (Eq, Enum, Ord, Show)
type Position = (Int, Int)
data State = State Position Direction (Set Position) deriving (Show)

toInt :: String -> Int
toInt s = read s :: Int

readProgram :: String -> Program
readProgram s = Data.Vector.fromList $ map toInt $ splitOn "," s

uniq :: Ord a => [a] -> [a]
uniq = step empty
  where
    step :: Ord a => Set a -> [a] -> [a]
    step _ [] = []
    step seen (x:xs)
        | member x seen = step seen xs
        | otherwise     = x : step (insert x seen) xs

toPairs :: [a] -> [(a, a)]
toPairs (x:y:xs) = (x, y) : toPairs xs
toPairs _        = []

getInput :: State -> Int
getInput (State position _ whitePanels)
    | member position whitePanels = 1
    | otherwise                   = 0

getPosition :: State -> Position
getPosition (State pos _ _) = pos

moveRobot :: Position -> Direction -> Position
moveRobot (x, y) dir = (x + dx, y + dy)
  where
    (dx, dy) = case dir of
                    UP    -> ( 0, -1)
                    RIGHT -> ( 1,  0)
                    DOWN  -> ( 0,  1)
                    LEFT  -> (-1,  0)

turnRobot :: Int -> Direction -> Direction
turnRobot 0 UP   = LEFT
turnRobot 1 LEFT = UP
turnRobot 0 dir  = pred dir
turnRobot 1 dir  = succ dir 

stepRobot :: State -> (Int, Int) -> State
stepRobot (State pos dir whitePanels) (color, turn) = State nextPos nextDir nextPanels
  where
    nextPanels = case color of
                    0 -> delete pos whitePanels
                    1 -> insert pos whitePanels
    nextDir    = turnRobot turn dir
    nextPos    = moveRobot pos nextDir

runRobot :: Program -> [State]
runRobot program = states
  where
    initialState = State (0, 0) UP (singleton (0, 0))
    inputs = map getInput states
    states = scanl stepRobot initialState (toPairs outputs)
    outputs = runProgram program inputs

generateMap :: Set Position -> String
generateMap whitePanels = intercalate "\n" $ map generateRow [-1..h+1]
  where
    (xs, ys)      = unzip $ toList whitePanels
    dx            = negate (minimum xs)
    dy            = negate (minimum ys)
    w             = dx + maximum xs
    h             = dy + maximum ys
    showPosition :: Int -> Int -> String
    showPosition y x
        | member (x - dx, y - dy) whitePanels = "#"
        | otherwise                           = "."
    generateRow :: Int -> String
    generateRow y = [-1..w+1] >>= showPosition y

main :: IO ()
main = do contents <- getContents
          let program               = readProgram contents
              states                = runRobot program
              State _ _ whitePanels = last states
          putStrLn $ generateMap whitePanels
