{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

newtype PaCoMinHeight = PaCoMinHeight PaCoZipper
                      deriving (Eq, Show, IpRouter, Zipper)
instance Partible PaCoMinHeight where
  memTreeMerge = minHeightMerge

showPaCoMinHeight :: MemTree PaCoMinHeight -> String
showPaCoMinHeight = ("Min-height partition of path-compressed tree\n" ++)
                    . showPartition

newtype PaCoMinSize = PaCoMinSize PaCoZipper
                    deriving (Eq, Show, IpRouter, Zipper)
instance Partible PaCoMinSize where
  memTreeMerge = minSizeMerge

showPaCoMinSize :: MemTree PaCoMinSize -> String
showPaCoMinSize = ("Min-size partition of path-compressed tree\n" ++)
                  . showPartition
