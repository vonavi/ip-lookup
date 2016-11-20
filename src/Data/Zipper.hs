-- | Traverses a tree of memory pages by zipper.
module Data.Zipper
  (
    Zipper(..)
  ) where

-- | Zipper typeclass
class Zipper a where
  goLeft   :: a -> a              -- ^ Goes left.
  goRight  :: a -> a              -- ^ Goes right.
  goUp     :: a -> a              -- ^ Goes up.
  isLeaf   :: a -> Bool           -- ^ Is a leaf reached or not.
  getLabel :: a -> Maybe Int      -- ^ Gets label.
  setLabel :: Maybe Int -> a -> a -- ^ Sets label.
  size     :: a -> Int            -- ^ Gets the size of page.
  insert   :: a -> a -> a         -- ^ Inserts a page here.
  delete   :: a -> a              -- ^ Deletes page here.
