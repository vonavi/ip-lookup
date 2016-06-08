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
import           Data.PrefixTree
import           Partible

putPaCoPrtn :: (IpRouter a, Partition a) => a -> IO ()
putPaCoPrtn t = do
  putStrLn "Partition of path-compressed tree"
  putStrLn . (++) "  Number of prefixes: " . show . numOfPrefixes $ t
  putStrLn . (++) "  Height:             " . show . height $ t
  putStrLn . (++) "  Number of pages:    " . show . numOfPages $ t
  putStrLn . (++) "  Memory usage:       " . show . memUsage $ t
  putStrLn . (++) "  Fill size:          " . show . fillSize $ t
  putStrLn . (++) "  Fill ratio:         " . show . fillRatio $ t

newtype MhPaCoTree = MhPaCoTree PaCoTree
                   deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhPaCoPrtn = MhPaCoPrtn (Page MhPaCoTree)
                   deriving (Eq, Show, Partition, IpRouter)
instance Partible MhPaCoTree where
  pageMerge = mhMerge False

newtype MhPaCoTreeM = MhPaCoTreeM PaCoTree
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhPaCoPrtnM = MhPaCoPrtnM (Page MhPaCoTreeM)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MhPaCoTreeM where
  pageMerge = mhMerge True

newtype MsPaCoTree = MsPaCoTree PaCoTree
                   deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsPaCoPrtn = MsPaCoPrtn (Page MsPaCoTree)
                   deriving (Eq, Show, Partition, IpRouter)
instance Partible MsPaCoTree where
  pageMerge = msMerge False

newtype MsPaCoTreeM = MsPaCoTreeM PaCoTree
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsPaCoPrtnM = MsPaCoPrtnM (Page MsPaCoTreeM)
                    deriving (Eq, Show, Partition, IpRouter)
instance Partible MsPaCoTreeM where
  pageMerge = msMerge True
