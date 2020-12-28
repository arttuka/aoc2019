module Main where

import Control.Monad (join)
import Data.List (foldl', foldr, isPrefixOf, scanl', sortOn)
import Util (absMod, combineTimes, dec2bin, mmi)

data Technique = NewStack | Cut Integer | Deal Integer deriving Show

deckSize = 119315717514047
rounds   = 101741582076661

m :: Integer -> Integer
m = (`absMod` deckSize)

readLine :: String -> Technique
readLine "deal into new stack"             = NewStack
readLine s
    | "deal with increment" `isPrefixOf` s = Deal $ read (drop 20 s)
    | "cut" `isPrefixOf` s                 = Cut $ read (drop 4 s)

applyTechnique :: Technique -> (Integer, Integer)
applyTechnique NewStack = (-1, deckSize - 1)
applyTechnique (Cut n)  = (1, -n)
applyTechnique (Deal n) = (n, 0)

reverseApplyTechnique :: Technique -> (Integer, Integer)
reverseApplyTechnique NewStack = (-1, deckSize - 1)
reverseApplyTechnique (Cut n)  = (1, n)
reverseApplyTechnique (Deal n) = (mmi n deckSize, 0)

combineTransforms :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
combineTransforms (a1, b1) (a2, b2) = (m (a1 * a2), m (b1 * a2 + b2))

applyTransform :: (Integer, Integer) -> Integer -> Integer
applyTransform (a, b) x = m (a * x + b)

makeTransform :: [Technique] -> (Integer, Integer)
makeTransform = foldl' combineTransforms (1, 0) . map applyTechnique

makeReverseTransform :: [Technique] -> (Integer, Integer)
makeReverseTransform = foldl' combineTransforms (1, 0) . reverse . map reverseApplyTechnique

main :: IO ()
main = do ls <- lines <$> getContents
          let techniques = map readLine ls
              tf = combineTimes rounds combineTransforms $ makeReverseTransform techniques
          print $ applyTransform tf 2020
