module Queue where

import Prelude hiding (drop, take)
import qualified Prelude
import Data.Foldable (Foldable, foldl')
import Data.Maybe (catMaybes)

data Queue a = Empty | Value Int [a] [a]

instance Show a => Show (Queue a) where
  show Empty = "<=[]<="
  show q     = "<=" ++ show (toList q) ++ "<="

instance Eq a => Eq (Queue a) where
  q1 == q2 = toList q1 == toList q2

instance Ord a => Ord (Queue a) where
  compare q1 q2 = compare (toList q1) (toList q2)

push :: a -> Queue a -> Queue a
push x Empty            = singleton x
push x (Value len ins outs) = Value (len + 1) (x:ins) outs

pushAll :: Foldable t => t a -> Queue a -> Queue a
pushAll xs q = foldl' (flip push) q xs

peek :: Queue a -> Maybe a
peek Empty              = Nothing
peek (Value _ _ (x:outs)) = Just x

pop :: Queue a -> Queue a
pop Empty                    = Empty
pop (Value _ [] [x])         = Empty
pop (Value len ins [x])      = Value (len - 1) [] (reverse ins)
pop (Value len ins (x:outs)) = Value (len - 1) ins outs

take :: Int -> Queue a -> Queue a
take n q = fromList $ Prelude.take n $ toList q

drop :: Int -> Queue a -> Queue a
drop 0 q = q
drop n q = drop (n - 1) (pop q)

singleton :: a -> Queue a
singleton x = Value 1 [] [x]

empty :: Queue a
empty = Empty

null :: Queue a -> Bool
null Empty = True
null _     = False

toList :: Queue a -> [a]
toList Empty            = []
toList (Value _ ins outs) = outs ++ reverse ins

fromList :: [a] -> Queue a
fromList list = Value (Prelude.length list) [] list

length :: Queue a -> Int
length Empty = 0
length (Value len _ _) = len
