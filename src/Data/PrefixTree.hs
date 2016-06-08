module Data.PrefixTree
       (
         PrefixTree(..)
       ) where

class Monoid a => PrefixTree a where
  isEmpty      :: a -> Bool
  root         :: a -> Maybe Int
  leftSubtree  :: a -> a
  rightSubtree :: a -> a
  singleton    :: Maybe Int -> a
  merge        :: Maybe Int -> a -> a -> a
  collapse     :: a -> a
  delSubtree   :: a -> a -> a
  size         :: a -> Int
