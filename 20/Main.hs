{-# LANGUAGE TupleSections #-}
module Main where

import Prelude hiding (head, length)
import Control.Monad ((<=<), liftM2)
import Data.List (intercalate)
import qualified Data.List as L (length)
import Data.Map.Strict (Map, elems)
import qualified Data.Map.Strict as M ((!), fromList)
import Data.Maybe (catMaybes, mapMaybe, maybe)
import Data.Tuple.Select (sel4)
import Data.Vector (Vector, head, length)
import Route (Direction(..), Move, Position, Position3D, findRoute)
import Util ((!!?), (///), addT2, combine, find', from2DList, groupBy, pairs, readWith, subT2, to2DList)

data Tile = Wall | Open | Empty | Portal Bool Position | Letter Char

toLetter :: Tile -> Maybe Char
toLetter (Letter c) = Just c
toLetter _          = Nothing

type Tiles = Vector (Vector Tile)

readTiles :: [String] -> Tiles
readTiles = from2DList . fmap (fmap readTile)
  where
    readTile :: Char -> Tile
    readTile '#' = Wall
    readTile '.' = Open
    readTile ' ' = Empty
    readTile c   = Letter c

type Portal' = (Position, Position, Bool, String)

findPortals :: Tiles -> (Position3D, Position3D, [(Position, Tile)])
findPortals tiles = ((xEn, yEn, 0), (xEx, yEx, 0), makePair =<< elems portals)
  where
    width                 = length (head tiles)
    height                = length tiles
    positions             = pairs [0..width-1] [0..height-1]
    allPortals            = catMaybes $ findPortal <$> [True, False] <*> positions
    (_, (xEn, yEn), _, _) = find' (hasLetters "AA") allPortals
    (_, (xEx, yEx), _, _) = find' (hasLetters "ZZ") allPortals
    portals               = groupBy sel4 id $ filter (not . liftM2 (||) (hasLetters "AA") (hasLetters "ZZ")) allPortals

    isOuter :: Position -> Bool
    isOuter (x, y) = x == 0 || x == width - 1 || y == 0 || y == height - 1
    hasLetters :: String -> Portal' -> Bool
    hasLetters l = (== l) . sel4
    getLetter :: Position -> Maybe Char
    getLetter = toLetter <=< (tiles !!?)
    findPortal :: Bool -> Position -> Maybe Portal'
    findPortal isHorizontal pos = (portalPos, exitPos, any isOuter [t2, t3], ) <$> letters
      where
        dt                      = if isHorizontal then (1, 0) else (0, 1)
        [t1, t2, t3, t4]        = take 4 $ iterate (addT2 dt) (subT2 pos dt)
        letters                 = combine [getLetter t2, getLetter t3]
        (portalPos, exitPos, i) = case tiles !!? t4 of
            (Just Open) -> (t3, t4, 1)
            _           -> (t2, t1, 0)
    makePair :: [Portal'] -> [(Position, Tile)]
    makePair [(pPos1, ePos1, o1, _), (pPos2, ePos2, o2, _)] = [(pPos1, Portal o1 ePos2), (pPos2, Portal o2 ePos1)]

dirToDelta :: Direction -> (Int, Int)
dirToDelta dir = case dir of
    North -> ( 0, -1)
    South -> ( 0,  1)
    West  -> (-1,  0)
    East  -> ( 1,  0)

move :: Tiles -> Move
move tiles (x, y, z) dir = case tiles !!? (x', y') of
    Just Open                  -> Just (x', y', z)
    Just (Portal isOuter (ex, ey))
        | not isOuter || z > 0 -> Just (ex, ey, if isOuter then z - 1 else z + 1)
    _                          -> Nothing
  where
    (x', y') = addT2 (x, y) $ dirToDelta dir

main :: IO ()
main = do initialTiles <- readWith readTiles
          let (entrance, exit, portals) = findPortals initialTiles
              tiles                     = initialTiles /// portals
              route                     = findRoute entrance exit (move tiles)
          print $ maybe 0 L.length route

