{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.PaCoPartition
       (
         MhPaCoPar
       , MhPaCoParM
       , MsPaCoPar
       , MsPaCoParM
       , putPaCoPar
       ) where

import           Data.IpRouter
import           Data.PaCoTree   hiding (Tree)
import           Data.PrefixTree
import           Partible

putPaCoPar :: (IpRouter a, Partible a) => a -> IO ()
putPaCoPar t = do
  putStrLn "Partition of path-compressed tree"
  putStrLn . (++) "  Number of prefixes: " . show . numOfPrefixes $ t
  putStrLn . (++) "  Height:             " . show . height $ t
  putStrLn . (++) "  Number of pages:    " . show . numOfPages $ t
  putStrLn . (++) "  Memory usage:       " . show . memUsage $ t
  putStrLn . (++) "  Fill size:          " . show . fillSize $ t
  putStrLn . (++) "  Fill ratio:         " . show . fillRatio $ t

newtype MhPaCoTree = MhPaCoTree PaCoTree
                   deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhPaCoPar = MhPaCoPar (Page MhPaCoTree)
                  deriving (Eq, Show, Partible, IpRouter)
instance Mergeable MhPaCoTree where
  pageMerge = mhMerge False

newtype MhPaCoTreeM = MhPaCoTreeM PaCoTree
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhPaCoParM = MhPaCoParM (Page MhPaCoTreeM)
                   deriving (Eq, Show, Partible, IpRouter)
instance Mergeable MhPaCoTreeM where
  pageMerge = mhMerge True

newtype MsPaCoTree = MsPaCoTree PaCoTree
                   deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsPaCoPar = MsPaCoPar (Page MsPaCoTree)
                  deriving (Eq, Show, Partible, IpRouter)
instance Mergeable MsPaCoTree where
  pageMerge = msMerge False

newtype MsPaCoTreeM = MsPaCoTreeM PaCoTree
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsPaCoParM = MsPaCoParM (Page MsPaCoTreeM)
                   deriving (Eq, Show, Partible, IpRouter)
instance Mergeable MsPaCoTreeM where
  pageMerge = msMerge True
