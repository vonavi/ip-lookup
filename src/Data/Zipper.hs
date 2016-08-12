module Data.Zipper
  (
    Zipper(..)
  ) where

class Zipper a where
  goLeft   :: a -> a
  goRight  :: a -> a
  goUp     :: a -> a
  isLeaf   :: a -> Bool
  getLabel :: a -> Maybe Int
  setLabel :: Maybe Int -> a -> a
  size     :: a -> Int
  insert   :: a -> a -> a
  delete   :: a -> a
