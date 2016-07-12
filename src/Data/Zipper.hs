module Data.Zipper
  (
    Zipper(..)
  ) where

class Zipper a where
  goLeft  :: a -> Maybe a
  goRight :: a -> Maybe a
  goUp    :: a -> Maybe a
