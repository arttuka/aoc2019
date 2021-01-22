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
import Route (Direction(..), Move, findRoute)
import Util ((!!?), (///), Position, addT2, combine, find', from2DList, groupBy, pairs, readWith, subT2, to2DList)

data Tile = Wall | Open | Empty | Portal Char Position | Letter Char
instance Show Tile where
  show Wall         = "#"
  show Open         = "."
  show Empty        = " "
  show (Portal c _) = [c]
  show (Letter c)   = [c]

toLetter :: Tile -> Maybe Char
toLetter (Letter c) = Just c
toLetter _          = Nothing

type Tiles = Vector (Vector Tile)

showTiles :: Tiles -> String
showTiles tiles = intercalate "\n" $ (show =<<) <$> to2DList tiles

readTiles :: [String] -> Tiles
readTiles = from2DList . fmap (fmap readTile)
  where
    readTile :: Char -> Tile
    readTile '#' = Wall
    readTile '.' = Open
    readTile ' ' = Empty
    readTile c   = Letter c

type Portal' = (Position, Position, Char, String)

findPortals :: Tiles -> (Position, Position, [(Position, Tile)])
findPortals tiles = (entrance, exit, makePair =<< elems portals)
  where
    width               = length (head tiles)
    height              = length tiles
    positions           = pairs [0..width-1] [0..height-1]
    allPortals          = catMaybes $ findPortal <$> [True, False] <*> positions
    (_, entrance, _, _) = find' (hasLetters "AA") allPortals
    (_, exit, _, _)     = find' (hasLetters "ZZ") allPortals
    portals             = groupBy sel4 id $ filter (liftM2 (||) (hasLetters "AA") (hasLetters "ZZ")) allPortals

    hasLetters :: String -> Portal' -> Bool
    hasLetters l = (== l) . sel4
    getLetter :: Position -> Maybe Char
    getLetter = toLetter <=< (tiles !!?)
    findPortal :: Bool -> Position -> Maybe Portal'
    findPortal isHorizontal pos = (\l -> (portalPos, exitPos, l !! i, l)) <$> letters
      where
        dt                      = if isHorizontal then (1, 0) else (0, 1)
        [t1, t2, t3, t4]        = take 4 $ iterate (addT2 dt) (subT2 pos dt)
        letters                 = combine [getLetter t2, getLetter t3]
        (portalPos, exitPos, i) = case tiles !!? t4 of
            (Just Open) -> (t3, t4, 1)
            _           -> (t2, t1, 0)
    makePair :: [Portal'] -> [(Position, Tile)]
    makePair [(pPos1, ePos1, c1, _), (pPos2, ePos2, c2, _)] = [(pPos1, Portal c1 ePos2), (pPos2, Portal c2 ePos1)]

dirToDelta :: Direction -> (Int, Int)
dirToDelta dir = case dir of
    North -> ( 0, -1)
    South -> ( 0,  1)
    West  -> (-1,  0)
    East  -> ( 1,  0)

move :: Tiles -> Move
move tiles pos dir = case tiles !!? newPos of
    Just Open            -> Just newPos
    Just (Portal _ exit) -> Just exit
    _                    -> Nothing
  where
    newPos = addT2 pos $ dirToDelta dir

main :: IO ()
main = do initialTiles <- readWith readTiles
          let (entrance, exit, portals) = findPortals initialTiles
              tiles                     = initialTiles /// portals
              route                     = findRoute entrance exit (move tiles)
          putStrLn $ showTiles tiles
          putStrLn $ "Finding route from " ++ show entrance ++ " to " ++ show exit
          print route
          print $ maybe 0 L.length route

