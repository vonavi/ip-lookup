{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Zipper
  (
    Zipper(..)
  ) where

import           Data.Function (on)

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

instance {-# OVERLAPPABLE #-} Zipper a => Show a where
  show z | isLeaf z  = ""
         | otherwise = show (getLabel z) ++ leftStr ++ rightStr
    where leftStr  = let s = show . goLeft $ z
                     in if null s then "" else " (L: " ++ s ++ ")"
          rightStr = let s = show . goRight $ z
                     in if null s then "" else " (R: " ++ s ++ ")"

instance {-# OVERLAPPABLE #-} Zipper a => Eq a where
  z1 == z2
    | isLeaf z1 && isLeaf z2 = True
    | Nothing <- getLabel z1
    , Nothing <- getLabel z2 = cmpSubtrees z1 z2
    | Just s1 <- getLabel z1
    , Just s2 <- getLabel z2
    , s1 == s2               = cmpSubtrees z1 z2
    | otherwise              = False
    where cmpSubtrees x1 x2 = ((==) `on` goLeft) x1 x2 &&
                              ((==) `on` goRight) x1 x2
