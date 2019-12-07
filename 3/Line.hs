module Line where

import Control.Applicative

type Intersection = (Int, Int)
data Line = VLine Int Int Int [Intersection] | HLine Int Int Int [Intersection]

instance Show Line where
  show (VLine x y d ints) = "VLine (" ++ show x ++ "," ++ show y ++ ") (" ++ show x ++ "," ++ show (y + d) ++ ") " ++ show ints
  show (HLine x y d ints) = "HLine (" ++ show x ++ "," ++ show y ++ ") (" ++ show (x + d) ++ "," ++ show y ++ ") " ++ show ints

intersections :: Line -> [Intersection]
intersections (VLine _ _ _ ints) = ints
intersections (HLine _ _ _ ints) = ints

lineLength :: Line -> Int
lineLength (VLine _ _ d _) = abs d
lineLength (HLine _ _ d _) = abs d

between :: Int -> Int -> Int -> Bool
between a b x = (a <= x && x <= b) || (b <= x && x <= a)

intersect :: Line -> Line -> (Line, Line)
intersect l1@(VLine x0 y0 d0 i0) l2@(HLine x1 y1 d1 i1)
  | int == (0, 0) = (l1, l2)
  | between y0 (y0 + d0) y1 && between x1 (x1 + d1) x0 = (VLine x0 y0 d0 (int : i0), HLine x1 y1 d1 (int : i1))
  | otherwise = (l1, l2)
  where int = (x0, y1)
intersect l1@HLine {} l2@VLine {} = (line1, line2)
  where (line2, line1) = l2 `intersect` l1
intersect l1 l2 = (l1, l2)

intersectOneFolder :: Line -> (Line, [Line]) -> (Line, [Line])
intersectOneFolder nextLine (currentLine, previousLines) = (newLine1, newLine2 : previousLines)
  where
    (newLine1, newLine2) = currentLine `intersect` nextLine

intersectOne :: Line -> [Line] -> (Line, [Line])
intersectOne line = Prelude.foldr intersectOneFolder (line, [])

intersectAllFolder :: Line -> ([Line], [Line]) -> ([Line], [Line])
intersectAllFolder line (firstLines, secondLines) = (newLine : firstLines, newSecondLines)
  where
    (newLine, newSecondLines) = line `intersectOne` secondLines

intersectAll :: [Line] -> [Line] -> ([Line], [Line])
intersectAll lines1 lines2 = Prelude.foldr intersectAllFolder ([], lines2) lines1

calculateDistanceForIntersection :: Int -> Line -> Intersection -> (Intersection, Int)
calculateDistanceForIntersection distance (VLine _ y _ _) int@(_, y0) = (int, distance + abs (y - y0))
calculateDistanceForIntersection distance (HLine x _ _ _) int@(x0, _) = (int, distance + abs (x - x0))

calculateDistancesForLine :: Int -> Line -> [(Intersection, Int)]
calculateDistancesForLine distance line = calculateDistanceForIntersection distance line <$> intersections line

calculateDistances :: Int -> [Line] -> [(Intersection, Int)]
calculateDistances distance [] = []
calculateDistances distance (line:lines) = calculateDistancesForLine distance line ++ calculateDistances (distance + lineLength line) lines 