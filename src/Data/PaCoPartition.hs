{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Partitions a path-compressed tree into a tree of memory pages.
module Data.PaCoPartition
  (
    PaCoMinHeight(..)
  , showPaCoMinHeight
  , PaCoMinSize(..)
  , showPaCoMinSize
  ) where

import           Data.IpRouter
import           Data.Partition
import           Data.Trees.PaCoTree (PaCoZipper)
import           Data.Zipper

-- | Minimum-height partition of path-compressed tree
newtype PaCoMinHeight = PaCoMinHeight PaCoZipper
                      deriving (Eq, Show, IpRouter, Zipper)
instance Partible PaCoMinHeight where
  memTreeMerge = minHeightMerge

-- | Shows the minimum-height partition of path-compressed tree.
showPaCoMinHeight :: MemTree PaCoMinHeight -> String
showPaCoMinHeight = ("Min-height partition of path-compressed tree\n" ++)
                    . showPartition

-- | Minimum-size partition of path-compressed tree
newtype PaCoMinSize = PaCoMinSize PaCoZipper
                    deriving (Eq, Show, IpRouter, Zipper)
instance Partible PaCoMinSize where
  memTreeMerge = minSizeMerge

-- | Shows the minimum-size partition of path-compressed tree.
showPaCoMinSize :: MemTree PaCoMinSize -> String
showPaCoMinSize = ("Min-size partition of path-compressed tree\n" ++)
                  . showPartition
