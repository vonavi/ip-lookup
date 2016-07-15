module Data.Zipper
  (
    Zipper(..)
  ) where

class Zipper a where
  goLeft   :: a -> a
  goRight  :: a -> a
  goUp     :: a -> a
  isRoot   :: a -> Bool
  isLeaf   :: a -> Bool
  getLabel :: a -> Maybe Int
  nodeSize :: a -> Int
  delete   :: a -> a
