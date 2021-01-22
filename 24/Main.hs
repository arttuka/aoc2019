module Main where

import Data.Bits (popCount, setBit, testBit, zeroBits)
import Data.Int (Int32)
import Data.List (dropWhileEnd, foldl', intercalate)
import Data.List.Split (chunksOf)
import Data.Vector (Vector, (!), fromList)

import Util (bool, filterBy, readWith)

bitsToNum :: [Int] -> Int32
bitsToNum = foldl' setBit zeroBits

readGrid :: [String] -> Int32
readGrid lines = bitsToNum $ filterBy setBits [0..24]
  where
    setBits = fmap (== '#') =<< lines

showGrid :: Int32 -> String
showGrid grid = intercalate "\n" (chunksOf 5 $ bool '.' '#' . testBit grid <$> [0..24]) ++ "\n"

checkBits :: Vector [[Int]]
checkBits = fromList [ [[7, 11], [1, 5], []]
                     , [[7], [0, 2, 6], []]
                     , [[7], [1, 3, 7], []]
                     , [[7], [2, 4, 8], []]
                     , [[7, 13], [3, 9], []]
                     , [[11], [0, 6, 10], []]
                     , [[], [1, 5, 7, 11], []]
                     , [[], [2, 6, 8], [0, 1, 2, 3, 4]]
                     , [[], [3, 7, 9, 13], []]
                     , [[13], [4, 8, 14], []]
                     , [[11], [5, 11, 15], []]
                     , [[], [6, 10, 16], [0, 5, 10, 15, 20]]
                     , [[], [], []]
                     , [[], [8, 14, 18], [4, 9, 14, 19, 24]]
                     , [[13], [9, 13, 19], []]
                     , [[11], [10, 16, 20], []]
                     , [[], [11, 15, 17, 21], []]
                     , [[], [16, 18, 22], [20, 21, 22, 23, 24]]
                     , [[], [13, 17, 19, 23], []]
                     , [[13], [14, 18, 24], []]
                     , [[11, 17], [15, 21], []]
                     , [[17], [16, 20, 22], []]
                     , [[17], [17, 21, 23], []]
                     , [[17], [18, 22, 24], []]
                     , [[13, 17], [19, 23], []]
                     ]

countBits :: Int32 -> [Int] -> Int
countBits = (length .) . filter . testBit

nextTile :: [Int32] -> Int -> Bool
nextTile _ 12                 = False
nextTile grids@[_, curr, _] i = case bugs of
    1 -> True
    2 -> not $ testBit curr i
    _ -> False
  where
    bugs = sum $ zipWith countBits grids (checkBits ! i)

nextGrid :: Int32 -> Int32 -> Int32 -> Int32
nextGrid upper current lower = bitsToNum $ filter (nextTile [upper, current, lower]) [0..24]

nextGrids :: [Int32] -> [Int32]
nextGrids grids = dropWhileEnd (== 0) $ dropWhile (== 0) $ zipWith3 nextGrid grids' (tail grids') (tail (tail grids'))
  where
    grids' = 0 : 0 : grids ++ [0, 0]

countBugs :: [Int32] -> Int
countBugs = sum . map popCount

main :: IO ()
main = do grid <- readWith readGrid
          let grids = iterate nextGrids [grid]
              result = grids !! 200
          print $ countBugs result
