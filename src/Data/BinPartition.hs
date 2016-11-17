{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

newtype BinMinHeight = BinMinHeight BinZipper
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible BinMinHeight where
  memTreeMerge = minHeightMerge

showBinMinHeight :: MemTree BinMinHeight -> String
showBinMinHeight = ("Min-height partition of binary tree\n" ++)
                   . showPartition

newtype BinMinSize = BinMinSize BinZipper
                   deriving (Eq, Show, IpRouter, Zipper)
instance Partible BinMinSize where
  memTreeMerge = minSizeMerge

showBinMinSize :: MemTree BinMinSize -> String
showBinMinSize = ("Min-size partition of binary tree\n" ++)
                 . showPartition
