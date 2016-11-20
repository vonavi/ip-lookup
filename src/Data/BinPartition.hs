{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Partitions a binary tree into a tree of memory pages.
module Data.BinPartition
  (
    BinMinHeight(..)
  , showBinMinHeight
  , BinMinSize(..)
  , showBinMinSize
  ) where

import           Data.IpRouter
import           Data.Partition
import           Data.Trees.BinTree (BinZipper)
import           Data.Zipper

-- | Minimum-height partition of binary tree
newtype BinMinHeight = BinMinHeight BinZipper
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible BinMinHeight where
  memTreeMerge = minHeightMerge

-- | Shows the minimum-height partition of binary tree.
showBinMinHeight :: MemTree BinMinHeight -> String
showBinMinHeight = ("Min-height partition of binary tree\n" ++)
                   . showPartition

-- | Minimum-size partition of binary tree
newtype BinMinSize = BinMinSize BinZipper
                   deriving (Eq, Show, IpRouter, Zipper)
instance Partible BinMinSize where
  memTreeMerge = minSizeMerge

-- | Shows the minimum-size partition of binary tree.
showBinMinSize :: MemTree BinMinSize -> String
showBinMinSize = ("Min-size partition of binary tree\n" ++)
                 . showPartition
