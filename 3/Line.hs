module Line where

import Data.Maybe

data Line = VLine Int Int Int | HLine Int Int Int deriving (Show)
data Intersection = Intersection Int Int Int deriving (Show)

between :: Int -> Int -> Int -> Bool
between a b x = a <= x && x <= b

intersection :: Line -> Line -> Maybe Intersection
intersection (VLine x0 y0 d0) (HLine x1 y1 d1) =
  if between y0 (y0 + d0) y1 && between x1 (x1 + d1) x0
    then Just $ Intersection x0 y1 (abs x0 + abs y1)
    else Nothing
intersection l1@HLine {} l2@VLine {} = intersection l2 l1
intersection VLine {} VLine {} = Nothing
intersection HLine {} HLine {} = Nothing

isNotOrigin :: Intersection -> Bool
isNotOrigin (Intersection _ _ d) = d /= 0

findIntersections :: [Line] -> [Line] -> [Intersection]
findIntersections lines1 lines2 = filter isNotOrigin intersections
  where intersections = catMaybes [intersection l1 l2 | l1 <- lines1, l2 <- lines2]

closerIntersection :: Intersection -> Intersection -> Intersection
closerIntersection i1@(Intersection _ _ d0) i2@(Intersection _ _ d1)
  | d0 < d1   = i1
  | otherwise = i2
