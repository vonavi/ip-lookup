{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.PaCo2Partition
       (
         MhPaCo2Prtn
       , MhPaCo2PrtnM
       , MsPaCo2Prtn
       , MsPaCo2PrtnM
       , putPaCo2Prtn
       ) where

import           Data.IpRouter
import           Data.PaCo2Tree  hiding (Tree)
import           Data.Partition
import           Data.PrefixTree

putPaCo2Prtn :: (IpRouter a, Partition a) => a -> IO ()
putPaCo2Prtn t = do
  putStrLn "Partition of path-compressed tree"
  putStrLn . (++) "  Number of prefixes: " . show . numOfPrefixes $ t
  putStrLn . (++) "  Height:             " . show . height $ t
  putStrLn . (++) "  Number of pages:    " . show . numOfPages $ t
  putStrLn . (++) "  Memory usage:       " . show . memUsage $ t
  putStrLn . (++) "  Fill size:          " . show . fillSize $ t
  putStrLn . (++) "  Fill ratio:         " . show . fillRatio $ t

newtype MhPaCo2Tree = MhPaCo2Tree PaCo2Tree
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhPaCo2Prtn = MhPaCo2Prtn (Page MhPaCo2Tree)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MhPaCo2Tree where
  pageMerge = minHeightMerge False

newtype MhPaCo2TreeM = MhPaCo2TreeM PaCo2Tree
                     deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhPaCo2PrtnM = MhPaCo2PrtnM (Page MhPaCo2TreeM)
                     deriving (Eq, Show, Partition, IpRouter)
instance Partible MhPaCo2TreeM where
  pageMerge = minHeightMerge True

newtype MsPaCo2Tree = MsPaCo2Tree PaCo2Tree
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsPaCo2Prtn = MsPaCo2Prtn (Page MsPaCo2Tree)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MsPaCo2Tree where
  pageMerge = minSizeMerge False

newtype MsPaCo2TreeM = MsPaCo2TreeM PaCo2Tree
                     deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsPaCo2PrtnM = MsPaCo2PrtnM (Page MsPaCo2TreeM)
                     deriving (Eq, Show, Partition, IpRouter)
instance Partible MsPaCo2TreeM where
  pageMerge = minSizeMerge True
