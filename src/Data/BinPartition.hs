{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.BinPartition
  (
    BinMinHeight(..)
  , putBinMinHeight
  , BinMinSize(..)
  , putBinMinSize
  ) where

import           Data.BinTree   (BinZipper)
import           Data.IpRouter
import           Data.Partition
import           Data.Zipper

newtype BinMinHeight = BinMinHeight BinZipper
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible BinMinHeight where
  memTreeMerge = minHeightMerge

putBinMinHeight :: MemTree BinMinHeight -> IO ()
putBinMinHeight t = do
  putStrLn "Min-height partition of binary tree"
  putPartition t

newtype BinMinSize = BinMinSize BinZipper
                   deriving (Eq, Show, IpRouter, Zipper)
instance Partible BinMinSize where
  memTreeMerge = minSizeMerge

putBinMinSize :: MemTree BinMinSize -> IO ()
putBinMinSize t = do
  putStrLn "Min-size partition of binary tree"
  putPartition t
