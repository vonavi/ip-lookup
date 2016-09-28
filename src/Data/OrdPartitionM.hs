{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.OrdPartitionM
  (
    OrdMinHeightT1(..)
  , putOrdMinHeightT1
  , OrdMinSizeT1(..)
  , putOrdMinSizeT1
  , OrdMinHeightT2(..)
  , putOrdMinHeightT2
  , OrdMinSizeT2(..)
  , putOrdMinSizeT2
  , OrdMinHeightT3(..)
  , putOrdMinHeightT3
  , OrdMinSizeT3(..)
  , putOrdMinSizeT3
  , OrdMinHeightT4(..)
  , putOrdMinHeightT4
  , OrdMinSizeT4(..)
  , putOrdMinSizeT4
  ) where

import           Data.IpRouter
import           Data.OrdTree    (OrdZipperT1, OrdZipperT2, OrdZipperT3,
                                  OrdZipperT4)
import           Data.PartitionM
import           Data.Zipper

newtype OrdMinHeightT1 = OrdMinHeightT1 OrdZipperT1
                       deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinHeightT1 where
  memTreeMerge = minHeightMerge

putOrdMinHeightT1 :: MemTree OrdMinHeightT1 -> IO ()
putOrdMinHeightT1 t = do
  putStrLn "Min-height partition of ordinal tree T1"
  putPartition t

newtype OrdMinSizeT1 = OrdMinSizeT1 OrdZipperT1
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinSizeT1 where
  memTreeMerge = minSizeMerge

putOrdMinSizeT1 :: MemTree OrdMinSizeT1 -> IO ()
putOrdMinSizeT1 t = do
  putStrLn "Min-size partition of ordinal tree T1"
  putPartition t


newtype OrdMinHeightT2 = OrdMinHeightT2 OrdZipperT2
                       deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinHeightT2 where
  memTreeMerge = minHeightMerge

putOrdMinHeightT2 :: MemTree OrdMinHeightT2 -> IO ()
putOrdMinHeightT2 t = do
  putStrLn "Min-height partition of ordinal tree T2"
  putPartition t

newtype OrdMinSizeT2 = OrdMinSizeT2 OrdZipperT2
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinSizeT2 where
  memTreeMerge = minSizeMerge

putOrdMinSizeT2 :: MemTree OrdMinSizeT2 -> IO ()
putOrdMinSizeT2 t = do
  putStrLn "Min-size partition of ordinal tree T2"
  putPartition t


newtype OrdMinHeightT3 = OrdMinHeightT3 OrdZipperT3
                       deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinHeightT3 where
  memTreeMerge = minHeightMerge

putOrdMinHeightT3 :: MemTree OrdMinHeightT3 -> IO ()
putOrdMinHeightT3 t = do
  putStrLn "Min-height partition of ordinal tree T3"
  putPartition t

newtype OrdMinSizeT3 = OrdMinSizeT3 OrdZipperT3
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinSizeT3 where
  memTreeMerge = minSizeMerge

putOrdMinSizeT3 :: MemTree OrdMinSizeT3 -> IO ()
putOrdMinSizeT3 t = do
  putStrLn "Min-size partition of ordinal tree T3"
  putPartition t


newtype OrdMinHeightT4 = OrdMinHeightT4 OrdZipperT4
                       deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinHeightT4 where
  memTreeMerge = minHeightMerge

putOrdMinHeightT4 :: MemTree OrdMinHeightT4 -> IO ()
putOrdMinHeightT4 t = do
  putStrLn "Min-height partition of ordinal tree T4"
  putPartition t

newtype OrdMinSizeT4 = OrdMinSizeT4 OrdZipperT4
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible OrdMinSizeT4 where
  memTreeMerge = minSizeMerge

putOrdMinSizeT4 :: MemTree OrdMinSizeT4 -> IO ()
putOrdMinSizeT4 t = do
  putStrLn "Min-size partition of ordinal tree T4"
  putPartition t
