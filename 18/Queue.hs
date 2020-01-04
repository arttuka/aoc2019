module Queue where

import Data.Foldable (Foldable, foldl')

data Queue a = Empty | Value [a] [a]

instance Show a => Show (Queue a) where
  show Empty = "=>[]=>"
  show (Value ins outs) = "=>" ++ show (ins ++ reverse outs) ++ "=>"

push :: a -> Queue a -> Queue a
push x Empty            = singleton x
push x (Value ins outs) = Value (x:ins) outs

pushAll :: Foldable t => t a -> Queue a -> Queue a
pushAll xs q = foldl' (flip push) q xs

peek :: Queue a -> Maybe a
peek Empty              = Nothing
peek (Value _ (x:outs)) = Just x

pop :: Queue a -> Queue a
pop Empty                = Empty
pop (Value [] [x])       = Empty
pop (Value ins [x])      = Value [] (reverse ins)
pop (Value ins (x:outs)) = Value ins outs

singleton :: a -> Queue a
singleton x = Value [] [x]

empty :: Queue a
empty = Empty

null :: Queue a -> Bool
null Empty = True
null _     = False
