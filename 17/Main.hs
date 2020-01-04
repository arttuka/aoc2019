module Main where

import Control.Applicative
import Control.Monad
import Data.Char (chr, ord)
import Data.List (concat, elem, find, findIndex, group, nub, intercalate, transpose, unfoldr, zipWith4)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Set (Set, member)
import qualified Data.Set as Set (fromList)
import Intcode (readProgram, runProgram)
import Util

type Position  = (Int, Int)
data Scaffold  = Scaffold { _horizontal :: Bool, _line :: Int, _start :: Int, _length :: Int } deriving Show
data Direction = R | L | U | D deriving (Eq, Show)
class Input a where
  toInput :: a -> [Int]
data Movement  = Movement Direction Int | Call Int deriving Eq
data Function  = Function Int [Movement]
data TurnPoint = TurnPoint Direction Position deriving Show

instance Show Movement where
    show (Movement dir n) = show dir ++ show n
    show (Call name)      = [chr name]

instance Show Function where
    show (Function name code) = "Function " ++ [chr name] ++ " " ++ show code

instance Input Movement where
    toInput (Movement dir n) = [toAscii dir, 44] ++ (ord <$> show n)
    toInput (Call name)      = [name]

instance Input a => Input [a] where
    toInput xs = intercalate [44] $ map toInput xs

instance Input Function where
    toInput (Function _ code) = toInput code

isStart :: Int -> Bool
isStart i = i `elem` [60, 62, 94, 118]

startDirection :: Int -> Direction
startDirection i = case i of
    60  -> L
    62  -> R
    94  -> U
    118 -> D

toAscii :: Direction -> Int
toAscii R = 82
toAscii L = 76

turnDirection :: Direction -> Direction -> Direction
turnDirection L U = R
turnDirection U R = R
turnDirection R D = R
turnDirection D L = R
turnDirection _ _ = L

isScaffold :: Int -> Bool
isScaffold i = i == 35 || isStart i

isHorizontal :: Direction -> Bool
isHorizontal R = True
isHorizontal L = True
isHorizontal _ = False 

end :: Int -> Int -> Int
end start length = start + length - 1

getScaffolds :: Bool -> Int -> [Int] -> [Scaffold]
getScaffolds horizontal lineNum line = filterBy filters $ zipWith4 Scaffold (repeat horizontal) (repeat lineNum) starts lengths
  where
    groups = group $ map isScaffold line
    lengths = map length groups
    starts = scanl (+) 0 lengths
    filters = map (\xs -> head xs && 1 < length xs) groups

getStart :: [[Int]] -> TurnPoint
getStart = step 0
  where
    findStart :: [Int] -> Maybe (Int, Direction)
    findStart line = (\i -> (i, startDirection $ line !! i)) <$> findIndex isStart line
    step :: Int -> [[Int]] -> TurnPoint
    step y (line:lines) = case findStart line of
        Just (x, dir) -> TurnPoint dir (x, y)
        Nothing       -> step (succ y) lines

intersection :: Scaffold -> Scaffold -> Maybe Position
intersection Scaffold{ _line=y, _start=startH, _length=lengthH} Scaffold{ _line=x, _start=startV, _length=lengthV}
    | isIntersection = Just (x, y)
    | otherwise      = Nothing
  where
    isIntersection = between x startH (end startH lengthH) && between y startV (end startV lengthV)

findIntersections :: [Scaffold] -> [Scaffold] -> [Position]
findIntersections hScaffolds vScaffolds = catMaybes $ liftA2 intersection hScaffolds vScaffolds

findNextScaffold :: Int -> Int -> [Scaffold] -> Maybe (Scaffold, Direction)
findNextScaffold prevLine prevPos scaffolds = (\s -> (s, getDirection s)) <$> find isNext scaffolds
  where
    isNext :: Scaffold -> Bool
    isNext Scaffold{ _line=line, _start=start, _length=length}
        = prevPos == line && (prevLine == start || prevLine == end start length)
    getDirection :: Scaffold -> Direction
    getDirection Scaffold{_horizontal=horizontal, _start=start}
        | horizontal && prevLine == start = R
        | horizontal                      = L
        | prevLine == start               = D
        | otherwise                       = U

toMovementAndTurnPoint :: Direction -> Scaffold -> Direction -> (Movement, TurnPoint)
toMovementAndTurnPoint prevDir scaffold@Scaffold{ _line=line, _start=start, _length=length} dir = (movement, turnPoint)
  where
    movement  = Movement (turnDirection prevDir dir) (length - 1)
    turnPoint = TurnPoint dir endPos
    endPos    = case dir of
        R -> (end start length, line)
        L -> (start, line)
        U -> (line, start)
        D -> (line, end start length)

getMovements :: [Scaffold] -> [Scaffold] -> TurnPoint -> [Movement]
getMovements hScaffolds vScaffolds = unfoldr getNextMovement
  where
    getNextMovement :: TurnPoint -> Maybe (Movement, TurnPoint)
    getNextMovement (TurnPoint dir (x, y)) = uncurry (toMovementAndTurnPoint dir) <$> nextScaffold
      where
        nextScaffold = if isHorizontal dir then findNextScaffold y x vScaffolds else findNextScaffold x y hScaffolds

findFunctions :: [Movement] -> [Function]
findFunctions list = zipWith Function [65..] $ nub $ unfoldr findNextPattern list
  where
    findNextPattern :: Eq a => [a] -> Maybe ([a], [a])
    findNextPattern [] = Nothing
    findNextPattern list = (\p -> (p, drop (length p) list)) <$> step 1 0 list
    step :: Eq a => Int -> Int -> [a] -> Maybe [a]
    step n prevCnt list
        | n == length list  = Nothing
        | prevCnt > currCnt = Just $ take (pred n) list
        | otherwise         = step (succ n) currCnt list
      where
        pattern = take n list
        currCnt = n * countPattern pattern list

applyFunctions :: [Movement] -> [Function] -> [Movement]
applyFunctions movements []     = movements
applyFunctions movements (f:fs) = applyFunctions (replacePattern code call movements) fs
  where
    Function name code = f
    call               = Call name

makeInput :: [Movement] -> [Function] -> [Int]
makeInput code functions = input ++ [110, 10]
  where
    rows  = toInput code : (map toInput functions)
    input = zipWith asList rows (repeat [10]) >>= join

fromAscii :: [Int] -> String
fromAscii = map chr

readMap :: [Int] -> [Int]
readMap (10:10:_) = []
readMap (x:xs)    = x : readMap xs

main :: IO ()
main = do contents <- getContents
          let program       = readProgram contents
              inputs        = makeInput mainCode functions
              outputs       = runProgram program inputs
              mapOutputs    = readMap outputs
              restOutputs   = drop (length mapOutputs + 2) outputs
              hLines        = splitOn [10] mapOutputs
              hScaffolds    = concat $ zipWith (getScaffolds True) [0..] hLines
              vScaffolds    = concat $ zipWith (getScaffolds False) [0..] $ transpose hLines
              start         = getStart hLines
              movements     = getMovements hScaffolds vScaffolds start
              functions     = findFunctions movements
              mainCode      = applyFunctions movements functions
          mapM_ putStrLn $ splitOn "\n" $ map chr (init restOutputs)
          print $ last restOutputs
