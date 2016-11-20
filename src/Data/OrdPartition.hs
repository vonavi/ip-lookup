{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Partitions ordinal trees into a tree of memory pages.
module Data.OrdPartition
  (
    OrdMinHeightT1(..)
  , showOrdMinHeightT1
  , OrdMinSizeT1(..)
  , showOrdMinSizeT1
  , OrdMinHeightT2(..)
  , showOrdMinHeightT2
  , OrdMinSizeT2(..)
  , showOrdMinSizeT2
  , OrdMinHeightT3(..)
  , showOrdMinHeightT3
  , OrdMinSizeT3(..)
  , showOrdMinSizeT3
  , OrdMinHeightT4(..)
  , showOrdMinHeightT4
  , OrdMinSizeT4(..)
  , showOrdMinSizeT4
  ) where

import           Data.IpRouter
import           Data.Partition
import           Data.Trees.OrdTree (OrdZipperT1, OrdZipperT2, OrdZipperT3,
                                     OrdZipperT4)
import           Data.Zipper

-- | Minimum-height partition of ordinal tree given by transformation
--   T1
newtype OrdMinHeightT1 = OrdMinHeightT1 OrdZipperT1
                       deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinHeightT1 where
  memTreeMerge = minHeightMerge

-- | Shows the minimum-height partition of ordinal tree T1.
showOrdMinHeightT1 :: MemTree OrdMinHeightT1 -> String
showOrdMinHeightT1 = ("Min-height partition of ordinal tree T1\n" ++)
                     . showPartition

-- | Minimum-size partition of ordinal tree given by transformation T1
newtype OrdMinSizeT1 = OrdMinSizeT1 OrdZipperT1
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinSizeT1 where
  memTreeMerge = minSizeMerge

-- | Shows the minimum-size partition of ordinal tree T1.
showOrdMinSizeT1 :: MemTree OrdMinSizeT1 -> String
showOrdMinSizeT1 = ("Min-size partition of ordinal tree T1\n" ++)
                   . showPartition


-- | Minimum-height partition of ordinal tree given by transformation
--   T2
newtype OrdMinHeightT2 = OrdMinHeightT2 OrdZipperT2
                       deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinHeightT2 where
  memTreeMerge = minHeightMerge

-- | Shows the minimum-height partition of ordinal tree T2.
showOrdMinHeightT2 :: MemTree OrdMinHeightT2 -> String
showOrdMinHeightT2 = ("Min-height partition of ordinal tree T2\n" ++)
                     . showPartition

-- | Minimum-size partition of ordinal tree given by transformation T2
newtype OrdMinSizeT2 = OrdMinSizeT2 OrdZipperT2
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinSizeT2 where
  memTreeMerge = minSizeMerge

-- | Shows the minimum-size partition of ordinal tree T2.
showOrdMinSizeT2 :: MemTree OrdMinSizeT2 -> String
showOrdMinSizeT2 = ("Min-size partition of ordinal tree T2\n" ++)
                   . showPartition


-- | Minimum-height partition of ordinal tree given by transformation
--   T3
newtype OrdMinHeightT3 = OrdMinHeightT3 OrdZipperT3
                       deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinHeightT3 where
  memTreeMerge = minHeightMerge

-- | Shows the minimum-height partition of ordinal tree T3.
showOrdMinHeightT3 :: MemTree OrdMinHeightT3 -> String
showOrdMinHeightT3 = ("Min-height partition of ordinal tree T3\n" ++)
                     . showPartition

-- | Minimum-size partition of ordinal tree given by transformation T3
newtype OrdMinSizeT3 = OrdMinSizeT3 OrdZipperT3
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinSizeT3 where
  memTreeMerge = minSizeMerge

-- | Shows the minimum-size partition of ordinal tree T3.
showOrdMinSizeT3 :: MemTree OrdMinSizeT3 -> String
showOrdMinSizeT3 = ("Min-size partition of ordinal tree T3\n" ++)
                   . showPartition


-- | Minimum-height partition of ordinal tree given by transformation
--   T4
newtype OrdMinHeightT4 = OrdMinHeightT4 OrdZipperT4
                       deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinHeightT4 where
  memTreeMerge = minHeightMerge

-- | Shows the minimum-height partition of ordinal tree T4.
showOrdMinHeightT4 :: MemTree OrdMinHeightT4 -> String
showOrdMinHeightT4 = ("Min-height partition of ordinal tree T4\n" ++)
                     . showPartition

-- | Minimum-size partition of ordinal tree given by transformation T4
newtype OrdMinSizeT4 = OrdMinSizeT4 OrdZipperT4
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinSizeT4 where
  memTreeMerge = minSizeMerge

-- | Shows the minimum-size partition of ordinal tree T4.
showOrdMinSizeT4 :: MemTree OrdMinSizeT4 -> String
showOrdMinSizeT4 = ("Min-size partition of ordinal tree T4\n" ++)
                   . showPartition
