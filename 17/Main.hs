module Main where

import Control.Applicative
import Control.Monad
import Data.Char (chr)
import Data.List (elem, group, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Set (Set, member)
import qualified Data.Set as Set (fromList)
import Intcode (readProgram, runProgram)

type Position = (Int, Int)
data Scaffold = Scaffold { _num :: Int, _start :: Int, _length :: Int } deriving Show

between :: Ord a => a -> a -> a -> Bool
between x from to = x > from && x < to

filterBy :: [Bool] -> [a] -> [a]
filterBy conds as = [a | (cond, a) <- zip conds as, cond]

isScaffold :: Int -> Bool
isScaffold i = i `elem` [35, 60, 62, 94, 118]

getScaffolds :: Int -> [Int] -> [Scaffold]
getScaffolds num line = filterBy filters $ zipWith3 Scaffold (repeat num) starts lengths
  where
    groups = group $ map isScaffold line
    lengths = map length groups
    starts = scanl (+) 0 lengths
    filters = map (\xs -> head xs && 1 < length xs) groups

intersection :: Scaffold -> Scaffold -> Maybe Position
intersection Scaffold{ _num=y, _start=startH, _length=lengthH} Scaffold{ _num=x, _start=startV, _length=lengthV}
    | isIntersection = Just (x, y)
    | otherwise      = Nothing
  where
    isIntersection = between x startH (startH + lengthH - 1) && between y startV (startV + lengthV - 1)

findIntersections :: [Scaffold] -> [Scaffold] -> [Position]
findIntersections hScaffolds vScaffolds = catMaybes $ liftA2 intersection hScaffolds vScaffolds

main :: IO ()
main = do contents <- getContents
          let program       = readProgram contents
              outputs       = runProgram program []
              hLines        = splitOn [10] outputs
              hScaffolds    = concat $ zipWith getScaffolds [0..] hLines
              vScaffolds    = concat $ zipWith getScaffolds [0..] $ transpose hLines
              intersections = findIntersections hScaffolds vScaffolds
--              intersections = Set.fromList $ findIntersections hScaffolds vScaffolds
--              isIntersection x = member x intersections
--              showChar :: Int -> Int -> Int -> Char
--              showChar y x c = if isIntersection (x, y) then 'O' else chr c
--              showLine :: Int -> [Int] -> String
--              showLine y = zipWith (showChar y) [0 ..]
--          mapM_ putStrLn $ zipWith showLine [0..] hLines
          print $ sum $ map (uncurry (*)) intersections
