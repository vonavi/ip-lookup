{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Zipper
  (
    Zipper(..)
  ) where

class Zipper a where
  goLeft     :: a -> a
  goRight    :: a -> a
  goUp       :: a -> a
  isRoot     :: a -> Bool
  isLeaf     :: a -> Bool
  isNodeFull :: a -> Bool
  getLabel   :: a -> Maybe Int
  setLabel   :: Maybe Int -> a -> a
  size       :: a -> Int
  insert     :: a -> a -> a
  delete     :: a -> a

  isNodeFull = const True


instance {-# OVERLAPPABLE #-} Zipper a => Show a where
  show z | isLeaf z  = ""
         | otherwise = show (getLabel z) ++ leftStr ++ rightStr
    where leftStr  = let s = show . goLeft $ z
                     in if null s then "" else " (L: " ++ s ++ ")"
          rightStr = let s = show . goRight $ z
                     in if null s then "" else " (R: " ++ s ++ ")"
