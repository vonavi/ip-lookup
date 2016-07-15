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
  isPrefix :: a -> Bool
  nodeSize :: a -> Int
  delete   :: a -> a
