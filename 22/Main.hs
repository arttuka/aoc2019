module Main where

import Control.Applicative
import Control.Monad
import Data.List (elemIndex, foldl', isPrefixOf, transpose)
import Data.List.Split (chunksOf)
import Util

data Technique = NewStack | Cut Int | Deal Int deriving Show
type Deck = [Int]

readLine :: String -> Technique
readLine "deal into new stack"             = NewStack
readLine s
    | "deal with increment" `isPrefixOf` s = Deal $ read (drop 20 s)
    | "cut" `isPrefixOf` s                 = Cut $ read (drop 4 s)

applyTechnique :: Int -> Deck -> Technique -> Deck
applyTechnique deckSize deck technique = case technique of
    NewStack -> reverse deck
    Cut i    -> rotate (i `mod` deckSize) deck
    Deal i   -> let factor        = getFactor i deckSize
                    transformDeck = (join <$> transpose) . chunksOf factor
                in take deckSize $ transformDeck $ take (i * factor) $ cycle deck

main :: IO ()
main = do ls <- lines <$> getContents
          let techniques = map readLine ls
              deckSize   = 10007
              deck       = [0..(pred deckSize)]
              result     = foldl' (applyTechnique deckSize) deck techniques
          print $ elemIndex 2019 result