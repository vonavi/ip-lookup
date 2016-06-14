{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.OrdPartition
       (
         MhOrdPrtnT1
       , MhOrdPrtnT2
       , MhOrdPrtnT3
       , MhOrdPrtnT4
       , MhOrdPrtnT1M
       , MhOrdPrtnT2M
       , MhOrdPrtnT3M
       , MhOrdPrtnT4M
       , MsOrdPrtnT1
       , MsOrdPrtnT2
       , MsOrdPrtnT3
       , MsOrdPrtnT4
       , MsOrdPrtnT1M
       , MsOrdPrtnT2M
       , MsOrdPrtnT3M
       , MsOrdPrtnT4M
       , putOrdPrtn
       ) where

import           Data.IpRouter
import           Data.OrdTree
import           Data.Partition
import           Data.PrefixTree

putOrdPrtn :: (IpRouter a, Partition a) => a -> IO ()
putOrdPrtn t = do
  putStrLn "Partition of ordered tree"
  putStrLn . (++) "  Number of prefixes: " . show . numOfPrefixes $ t
  putStrLn . (++) "  Height:             " . show . height $ t
  putStrLn . (++) "  Number of pages:    " . show . numOfPages $ t
  putStrLn . (++) "  Memory usage:       " . show . memUsage $ t
  putStrLn . (++) "  Fill size:          " . show . fillSize $ t
  putStrLn . (++) "  Fill ratio:         " . show . fillRatio $ t

newtype MhOrdTreeT1 = MhOrdTreeT1 OrdTreeT1
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhOrdPrtnT1 = MhOrdPrtnT1 (Page MhOrdTreeT1)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MhOrdTreeT1 where
  pageMerge = minHeightMerge False

newtype MhOrdTreeT2 = MhOrdTreeT2 OrdTreeT2
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhOrdPrtnT2 = MhOrdPrtnT2 (Page MhOrdTreeT2)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MhOrdTreeT2 where
  pageMerge = minHeightMerge False

newtype MhOrdTreeT3 = MhOrdTreeT3 OrdTreeT3
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhOrdPrtnT3 = MhOrdPrtnT3 (Page MhOrdTreeT3)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MhOrdTreeT3 where
  pageMerge = minHeightMerge False

newtype MhOrdTreeT4 = MhOrdTreeT4 OrdTreeT4
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhOrdPrtnT4 = MhOrdPrtnT4 (Page MhOrdTreeT4)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MhOrdTreeT4 where
  pageMerge = minHeightMerge False

newtype MhOrdTreeT1M = MhOrdTreeT1M OrdTreeT1
                     deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhOrdPrtnT1M = MhOrdPrtnT1M (Page MhOrdTreeT1M)
                     deriving (Eq, Show, Partition, IpRouter)
instance Partible MhOrdTreeT1M where
  pageMerge = minHeightMerge True

newtype MhOrdTreeT2M = MhOrdTreeT2M OrdTreeT2
                     deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhOrdPrtnT2M = MhOrdPrtnT2M (Page MhOrdTreeT2M)
                     deriving (Eq, Show, Partition, IpRouter)
instance Partible MhOrdTreeT2M where
  pageMerge = minHeightMerge True

newtype MhOrdTreeT3M = MhOrdTreeT3M OrdTreeT3
                     deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhOrdPrtnT3M = MhOrdPrtnT3M (Page MhOrdTreeT3M)
                     deriving (Eq, Show, Partition, IpRouter)
instance Partible MhOrdTreeT3M where
  pageMerge = minHeightMerge True

newtype MhOrdTreeT4M = MhOrdTreeT4M OrdTreeT4
                     deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhOrdPrtnT4M = MhOrdPrtnT4M (Page MhOrdTreeT4M)
                     deriving (Eq, Show, Partition, IpRouter)
instance Partible MhOrdTreeT4M where
  pageMerge = minHeightMerge True

newtype MsOrdTreeT1 = MsOrdTreeT1 OrdTreeT1
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsOrdPrtnT1 = MsOrdPrtnT1 (Page MsOrdTreeT1)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MsOrdTreeT1 where
  pageMerge = minHeightMerge False

newtype MsOrdTreeT2 = MsOrdTreeT2 OrdTreeT2
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsOrdPrtnT2 = MsOrdPrtnT2 (Page MsOrdTreeT2)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MsOrdTreeT2 where
  pageMerge = minHeightMerge False

newtype MsOrdTreeT3 = MsOrdTreeT3 OrdTreeT3
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsOrdPrtnT3 = MsOrdPrtnT3 (Page MsOrdTreeT3)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MsOrdTreeT3 where
  pageMerge = minHeightMerge False

newtype MsOrdTreeT4 = MsOrdTreeT4 OrdTreeT4
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsOrdPrtnT4 = MsOrdPrtnT4 (Page MsOrdTreeT4)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MsOrdTreeT4 where
  pageMerge = minHeightMerge False

newtype MsOrdTreeT1M = MsOrdTreeT1M OrdTreeT1
                     deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsOrdPrtnT1M = MsOrdPrtnT1M (Page MsOrdTreeT1M)
                     deriving (Eq, Show, Partition, IpRouter)
instance Partible MsOrdTreeT1M where
  pageMerge = minHeightMerge True

newtype MsOrdTreeT2M = MsOrdTreeT2M OrdTreeT2
                     deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsOrdPrtnT2M = MsOrdPrtnT2M (Page MsOrdTreeT2M)
                     deriving (Eq, Show, Partition, IpRouter)
instance Partible MsOrdTreeT2M where
  pageMerge = minHeightMerge True

newtype MsOrdTreeT3M = MsOrdTreeT3M OrdTreeT3
                     deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsOrdPrtnT3M = MsOrdPrtnT3M (Page MsOrdTreeT3M)
                     deriving (Eq, Show, Partition, IpRouter)
instance Partible MsOrdTreeT3M where
  pageMerge = minHeightMerge True

newtype MsOrdTreeT4M = MsOrdTreeT4M OrdTreeT4
                     deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsOrdPrtnT4M = MsOrdPrtnT4M (Page MsOrdTreeT4M)
                     deriving (Eq, Show, Partition, IpRouter)
instance Partible MsOrdTreeT4M where
  pageMerge = minHeightMerge True
