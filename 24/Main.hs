module Main where

import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (maybe)
import Data.Set (Set, empty, insert, member)
import Data.Vector (Vector)

import Util ((!!?), (!!!), bool, filterBy, from2DList, positions, readWith, to2DList)

type Position = (Int, Int)
type Tiles = Vector (Vector Bool)

readTiles :: [String] -> Tiles
readTiles = from2DList . fmap (fmap (== '#'))

showTiles :: Tiles -> String
showTiles tiles = intercalate "\n" (fmap (bool '.' '#') <$> to2DList tiles) ++ "\n"

adjacentPositions :: Position -> [Position]
adjacentPositions (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

countBugs :: Tiles -> Position -> Int
countBugs tiles position = sum $ maybe 0 (bool 0 1) . (tiles !!?) <$> adjacentPositions position

nextTile :: Tiles -> Position -> Bool
nextTile tiles position = case countBugs tiles position of
  1 -> True
  2 -> not $ tiles !!! position
  _ -> False

nextTiles :: Tiles -> Tiles
nextTiles tiles = from2DList $ (nextTile tiles <$>) <$> positions 5 5

biodiversity :: [Integer]
biodiversity = take 25 $ iterate (* 2) 1

calculateBiodiversity :: Tiles -> Integer
calculateBiodiversity tiles = sum $ filterBy (concat $ to2DList tiles) biodiversity

findDuplicate :: Tiles -> Tiles
findDuplicate = step empty
  where
    step :: Set Integer -> Tiles -> Tiles
    step seen tiles 
        | bd `member` seen = tiles
        | otherwise        = step (insert bd seen) (nextTiles tiles)
      where
        bd = calculateBiodiversity tiles

main :: IO ()
main = do tiles <- readWith readTiles
          let duplicate = findDuplicate tiles
          putStrLn $ showTiles duplicate
          print $ calculateBiodiversity duplicate