module Main where

import Prelude hiding (replicate)
import Control.Applicative
import Control.Monad
import Data.Foldable (toList)
import Data.List (foldl', intercalate)
import Data.List.Split (splitOn)
import qualified Data.List.Split as LSplit (chunksOf)
import Data.Sequence (Seq, replicate, update)
import qualified Data.Sequence as Seq (chunksOf)
import Data.Vector (fromList)
import Intcode (Program, runProgram)
import Input (Key(..), getKeys, setupStdin)

type Position = (Int, Int)
data TileType = Empty | Wall | Block | Paddle | Ball deriving (Enum, Eq)
data Output   = Tile Position TileType | Score Int deriving (Show)
data Map      = Map Int (Seq TileType)

instance Show TileType where
    show Empty  = "."
    show Wall   = "#"
    show Block  = "B"
    show Paddle = "-"
    show Ball   = "o"

showMap :: Map -> String
showMap (Map score tiles) = show score ++ "\n" ++ intercalate "\n" rows ++ "\n"
  where
    showRow :: [TileType] -> String
    showRow row = row >>= show
    rows :: [String]
    rows        = map showRow $ LSplit.chunksOf width $ toList tiles 

instance Show Map where
    show = showMap

width        = 36
height       = 24
initialCount = 865
emptyMap     = Map 0 $ replicate (width * height) Empty

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

updateMap :: Map -> Output -> Map
updateMap (Map score tiles) (Tile pos tile)  = Map score $ update index tile tiles
  where
    index = positionToIndex pos
updateMap (Map score tiles) (Score newScore) = Map newScore tiles

makeInitialMap :: [Output] -> Map
makeInitialMap = foldl' updateMap emptyMap

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
              (initialOutputs, outputs) = splitAt initialCount $ readOutputs $ runProgram program inputs
              initialMap                = makeInitialMap initialOutputs
              maps                      = scanl updateMap initialMap outputs
          mapM_ print maps
