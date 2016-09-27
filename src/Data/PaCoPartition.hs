{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.PaCoPartition
  (
    PaCoMinHeight(..)
  , putPaCoMinHeight
  , PaCoMinSize(..)
  , putPaCoMinSize
  ) where

import           Data.IpRouter
import           Data.PaCoTree   (PaCoZipper)
import           Data.PartitionM
import           Data.Zipper

newtype PaCoMinHeight = PaCoMinHeight PaCoZipper
                      deriving (Eq, Show, IpRouter, Zipper)
instance Partible PaCoMinHeight where
  memTreeMerge = minHeightMerge

putPaCoMinHeight :: MemTree PaCoMinHeight -> IO ()
putPaCoMinHeight t = do
  putStrLn "Min-height partition of path-compressed tree"
  putPartition t

newtype PaCoMinSize = PaCoMinSize PaCoZipper
                    deriving (Eq, Show, IpRouter, Zipper)
instance Partible PaCoMinSize where
  memTreeMerge = minSizeMerge

putPaCoMinSize :: MemTree PaCoMinSize -> IO ()
putPaCoMinSize t = do
  putStrLn "Min-size partition of path-compressed tree"
  putPartition t
