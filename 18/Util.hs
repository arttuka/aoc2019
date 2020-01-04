module Util where

import Control.Applicative
import Control.Monad

between :: Ord a => a -> a -> a -> Bool
between x from to = x >= from && x <= to

juxt :: (a -> b) -> (a -> c) -> a -> (b, c)
juxt f g a = (f a, g a)

juxtM :: Monad m => (a -> m b) -> (a -> m c) -> a -> m (b, c)
juxtM f g a = liftM2 (,) (f a) (g a)

mapWith :: (a -> b) -> a -> (a, b)
mapWith = juxt id
