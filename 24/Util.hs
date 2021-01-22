module Util where

import Data.List.Split (splitOn)

getLines :: IO [String]
getLines = fmap lines getContents

readWith :: ([String] -> a) -> IO a
readWith = (<$> getLines)

readLinesWith :: (String -> a) -> IO [a]
readLinesWith = readWith . fmap

readLines :: Read a => IO [a]
readLines = readLinesWith read

readGroupsWith :: ([String] -> a) -> IO [a]
readGroupsWith f = fmap f . splitOn [""] <$> getLines

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True = t

filterBy :: [Bool] -> [a] -> [a]
filterBy bs as = map snd $ filter fst $ zip bs as
