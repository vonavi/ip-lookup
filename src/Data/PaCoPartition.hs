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
newtype MhPaCoPar = MhPaCoPar (Page MhPaCoTree) deriving (Eq, Show, Partible)

instance Mergeable MhPaCoTree where
  pageMerge = mhMerge False

instance IpRouter MhPaCoPar where
  mkTable                     = MhPaCoPar . patSstBuild .
                                (mkTable :: [Entry] -> MhPaCoTree)
  insEntry e (MhPaCoPar t)    = MhPaCoPar $ patSstInsert e t
  delEntry e (MhPaCoPar t)    = MhPaCoPar $ patSstDelete e t
  ipLookup addr (MhPaCoPar t) = patSstLookup addr t
  numOfPrefixes (MhPaCoPar t) = numOfPrefixes' t


newtype MhPaCoTreeM = MhPaCoTreeM PaCoTree
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MhPaCoParM = MhPaCoParM (Page MhPaCoTreeM) deriving (Eq, Show, Partible)

instance Mergeable MhPaCoTreeM where
  pageMerge = mhMerge True

instance IpRouter MhPaCoParM where
  mkTable                      = MhPaCoParM . patSstBuild .
                                 (mkTable :: [Entry] -> MhPaCoTreeM)
  insEntry e (MhPaCoParM t)    = MhPaCoParM $ patSstInsert e t
  delEntry e (MhPaCoParM t)    = MhPaCoParM $ patSstDelete e t
  ipLookup addr (MhPaCoParM t) = patSstLookup addr t
  numOfPrefixes (MhPaCoParM t) = numOfPrefixes' t


newtype MsPaCoTree = MsPaCoTree PaCoTree
                   deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsPaCoPar = MsPaCoPar (Page MsPaCoTree) deriving (Eq, Show, Partible)

instance Mergeable MsPaCoTree where
  pageMerge = msMerge False

instance IpRouter MsPaCoPar where
  mkTable                     = MsPaCoPar . patSstBuild .
                                (mkTable :: [Entry] -> MsPaCoTree)
  insEntry e (MsPaCoPar t)    = MsPaCoPar $ patSstInsert e t
  delEntry e (MsPaCoPar t)    = MsPaCoPar $ patSstDelete e t
  ipLookup addr (MsPaCoPar t) = patSstLookup addr t
  numOfPrefixes (MsPaCoPar t) = numOfPrefixes' t


newtype MsPaCoTreeM = MsPaCoTreeM PaCoTree
                    deriving (Eq, Show, Monoid, IpRouter, PrefixTree)
newtype MsPaCoParM = MsPaCoParM (Page MsPaCoTreeM) deriving (Eq, Show, Partible)

instance Mergeable MsPaCoTreeM where
  pageMerge = msMerge True

instance IpRouter MsPaCoParM where
  mkTable                      = MsPaCoParM . patSstBuild .
                                 (mkTable :: [Entry] -> MsPaCoTreeM)
  insEntry e (MsPaCoParM t)    = MsPaCoParM $ patSstInsert e t
  delEntry e (MsPaCoParM t)    = MsPaCoParM $ patSstDelete e t
  ipLookup addr (MsPaCoParM t) = patSstLookup addr t
  numOfPrefixes (MsPaCoParM t) = numOfPrefixes' t
