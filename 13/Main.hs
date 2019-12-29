module Main where

import Prelude hiding (replicate)
import Control.Applicative
import Control.Monad
import Data.Foldable (toList)
import Data.List (foldl', intercalate)
import Data.List.Split (splitOn)
import qualified Data.List.Split as LSplit (chunksOf)
import Data.Maybe (isJust, mapMaybe)
import Data.Sequence (Seq, replicate, update)
import qualified Data.Sequence as Seq (chunksOf)
import Data.Vector (fromList)
import Intcode (Program, runProgram)
import Input (Key(..), getKeys, setupStdin)

type Position  = (Int, Int)
data Direction = DirLeft | DirRight
data TileType  = Empty | Wall | Block | Paddle | Ball deriving (Enum, Eq)
data Output    = Tile Position TileType | Score Int deriving (Show)
data Map       = Map
                    { _score    :: Int
                    , _tiles    :: Seq TileType
                    , _paddleX  :: Int
                    , _ballX    :: Int
                    , _ballDir  :: Direction
                    , _input    :: Maybe Int
                    , _newScore :: Bool
                    }

instance Show TileType where
    show Empty  = "."
    show Wall   = "#"
    show Block  = "B"
    show Paddle = "-"
    show Ball   = "o"

showMap :: Map -> String
showMap m = show (_input m) ++ "   " ++ show (_score m) ++ "\n" ++ intercalate "\n" rows ++ "\n"
  where
    showRow :: [TileType] -> String
    showRow row = row >>= show
    rows :: [String]
    rows        = map showRow $ LSplit.chunksOf width $ toList (_tiles m)

instance Show Map where
    show = showMap

width        = 36
height       = 24
initialCount = 865
emptyMap     = Map {_score = 0, _tiles = replicate (width * height) Empty, _paddleX = 0, _ballX = 0, _ballDir = DirLeft, _input = Nothing, _newScore = False }

countWhere :: (a -> Bool) -> [a] -> Int
countWhere pred = length . filter pred

toInt :: String -> Int
toInt s = read s :: Int

readProgram :: String -> Program
readProgram s = fromList $ map toInt $ splitOn "," s

readOutputs :: [Int] -> [Output]
readOutputs xs = map readOutput $ LSplit.chunksOf 3 xs
  where
    readOutput :: [Int] -> Output
    readOutput [-1, 0, score] = Score score
    readOutput [x, y, t]      = Tile (x, y) (toEnum t)

positionToIndex :: Position -> Int
positionToIndex (x, y) = (width * y) + x

getBallDir :: Int -> Int -> Direction
getBallDir prevX newX
    | prevX < newX = DirRight
    | otherwise    = DirLeft

nextInput :: Int -> Int -> Direction -> Int
nextInput ballX paddleX ballDir
    | ballX < paddleX = -1
    | ballX > paddleX =  1
    | otherwise       = case ballDir of
        DirLeft  -> -1
        DirRight ->  1

updateMap :: Map -> Output -> Map
updateMap m (Tile pos tile) = m { _tiles = tiles, _ballX = ballX, _paddleX = paddleX, _ballDir = ballDir, _input = input, _newScore = False }
  where
    index   = positionToIndex pos
    tiles   = update index tile (_tiles m)
    ballX   = if tile == Ball then fst pos else _ballX m
    paddleX = if tile == Paddle then fst pos else _paddleX m
    ballDir = if tile == Ball then getBallDir (_ballX m) (fst pos) else _ballDir m
    input   = if tile == Ball then Just (nextInput ballX paddleX ballDir) else Nothing
updateMap m (Score newScore) = m { _score = newScore, _newScore = True, _input = Nothing}

makeInitialMap :: [Output] -> Map
makeInitialMap outputs = m { _input = Just 0 }
  where
    m = foldl' updateMap emptyMap outputs

filterPrintedMaps :: [Map] -> [Map]
filterPrintedMaps []             = []
filterPrintedMaps (m:ms)
    | blocks == 0 && newScore    = [m]
    | blocks > 0 && isJust input = m : filterPrintedMaps ms
    | otherwise                  = filterPrintedMaps ms
  where
    blocks   = countWhere (== Block) $ toList $ _tiles m
    input    = _input m
    newScore = _newScore m

keyToInput :: Key -> Int
keyToInput ArrowLeft  = -1
keyToInput ArrowRight =  1
keyToInput _          =  0

main :: IO ()
main = do setupStdin
          contents <- readFile "input.txt"
          keys     <- getKeys
          let program                   = readProgram contents
              inputs                    = map keyToInput keys
              autoInputs                = mapMaybe _input printedMaps
              (initialOutputs, outputs) = splitAt initialCount $ readOutputs $ runProgram program autoInputs
              initialMap                = makeInitialMap initialOutputs
              maps                      = scanl updateMap initialMap outputs
              printedMaps               = filterPrintedMaps maps
          mapM_ print printedMaps
