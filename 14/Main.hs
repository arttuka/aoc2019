module Main where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad
import Data.List (delete, elem, find)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!), adjust, fromList, keys, lookup, singleton, unionWith)
import Data.Maybe (fromMaybe, maybe)
import Debug.Trace

-- rowRegex = "(?:([0-9]+) ([A-Z]+),? )+=> ([0-9]+) ([A-Z]+)"

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll items = filter $ not . \x -> x `elem` items

data Chemical = Chemical Int String deriving Show
data Reaction = Reaction Int String [Chemical] deriving Show
type ReactionMap = Map String Reaction
type AmountMap = Map String Int

toInt :: String -> Int
toInt s = read s :: Int

divUp :: Int -> Int -> Int
divUp x y = if r == 0 then q else succ q
  where
    (q, r) = x `quotRem` y

parseChemical :: String -> Chemical
parseChemical c = Chemical (toInt i) name
  where
    [i, name] = splitOn " " c

parseRow :: String -> Reaction
parseRow row = Reaction amount result chemicals
  where
    [from, to]             = splitOn " => " row
    chemicals              = map parseChemical $ splitOn ", " from
    Chemical amount result = parseChemical to

makeReactionMap :: [Reaction] -> ReactionMap
makeReactionMap reactions = fromList [(name, reaction) | reaction@(Reaction _ name _) <- reactions]

calculateReaction :: Int -> Reaction -> (Int, AmountMap)
calculateReaction needed (Reaction amount _ chemicals) = (m * amount, amounts)
  where
    m       = needed `divUp` amount
    amounts = fromList [(name, m * n) | Chemical n name <- chemicals]

updateNames :: String -> [String] -> [String] -> [String]
updateNames current oldNames newNames = removeAll (current : newNames) oldNames ++ delete "ORE" newNames

chooseReaction :: AmountMap -> [String] -> Maybe String
chooseReaction amounts = find positiveAmount
  where
    positiveAmount :: String -> Bool
    positiveAmount s = maybe False (>0) $ lookup s amounts

calculateAmounts :: ReactionMap -> Int -> AmountMap -> Maybe AmountMap
calculateAmounts reactions fuel amounts = if newOre < 1000000000000 then Just newAmounts else Nothing
  where
    step :: AmountMap -> [String] -> AmountMap
    step amounts names = case chooseReaction amounts names of
        Nothing   -> amounts
        Just name -> step newAmounts newNames
          where 
            reaction = reactions ! name
            amount   = amounts ! name
            (added, required) = calculateReaction amount reaction
            newAmounts = unionWith (+) required $ adjust (subtract added) name amounts
            newNames = updateNames name names $ keys required
    newAmounts = step (adjust (+ fuel) "FUEL" amounts) ["FUEL"]
    newOre     = newAmounts ! "ORE"

calculateFuel :: ReactionMap -> Int
calculateFuel reactions = step (singleton "FUEL" 0) 0 1 True
  where
    step :: AmountMap -> Int -> Int -> Bool -> Int
    step amounts totalFuel addFuel increase = case newAmounts of
        Just as                -> step as (totalFuel + addFuel) newAddFuel increase
        Nothing | addFuel <= 1 -> totalFuel
        Nothing                -> step amounts totalFuel (addFuel `div` 2) False
      where
        newAmounts = calculateAmounts reactions addFuel amounts
        newAddFuel = if increase then 2 * addFuel else addFuel

main :: IO ()
main = do contents <- fmap lines getContents
          let reactions = map parseRow contents
              rmap      = makeReactionMap reactions
              amount    = calculateFuel rmap
          print amount
