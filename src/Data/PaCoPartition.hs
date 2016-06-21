{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.PaCoPartition
       (
         MhPaCoPrtn
       , MhPaCoPrtnM
       , MsPaCoPrtn
       , MsPaCoPrtnM
       , putPaCoPrtn
       ) where

import           Data.IpRouter
import           Data.PaCoTree   hiding (Tree)
import           Data.Partition
import           Data.PrefixTree

putPaCoPrtn :: (IpRouter a, Partition a) => a -> IO ()
putPaCoPrtn t = do
  putStrLn "Partition of path-compressed tree"
  putPartition t

newtype MhPaCoTree = MhPaCoTree PaCoTree
                   deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhPaCoPrtn = MhPaCoPrtn (Page MhPaCoTree)
                   deriving (Eq, Show, Partition, IpRouter)
instance Partible MhPaCoTree where
  pageMerge = minHeightMerge False

newtype MhPaCoTreeM = MhPaCoTreeM PaCoTree
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhPaCoPrtnM = MhPaCoPrtnM (Page MhPaCoTreeM)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MhPaCoTreeM where
  pageMerge = minHeightMerge True

newtype MsPaCoTree = MsPaCoTree PaCoTree
                   deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsPaCoPrtn = MsPaCoPrtn (Page MsPaCoTree)
                   deriving (Eq, Show, Partition, IpRouter)
instance Partible MsPaCoTree where
  pageMerge = minSizeMerge False

newtype MsPaCoTreeM = MsPaCoTreeM PaCoTree
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsPaCoPrtnM = MsPaCoPrtnM (Page MsPaCoTreeM)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MsPaCoTreeM where
  pageMerge = minSizeMerge True
