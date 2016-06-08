module Data.PrefixTree
       (
         PrefixTree(..)
       ) where

class Monoid a => PrefixTree a where
  isEmpty       :: a -> Bool
  collapse      :: a -> a
  delSubtree    :: a -> a -> a
  bRoot         :: a -> Maybe Int
  bLeftSubtree  :: a -> a
  bRightSubtree :: a -> a
  bSingleton    :: Maybe Int -> a
  bMerge        :: Maybe Int -> a -> a -> a
  size          :: a -> Int
