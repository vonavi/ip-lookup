module Data.PaCoPartition
       (
         MhPatSst
       , MhPatSstM
       , MsPatSst
       , MsPatSstM
       , putPatSst
       ) where

import           Data.IpRouter
import           Data.PaCoTree hiding (Tree)
import           Partible

putPatSst :: (PatSst a, IpRouter a) => a -> IO ()
putPatSst t = do
  putStrLn "SST for path-compressed tree"
  putStrLn . (++) "  Number of prefixes: " . show . numOfPrefixes $ t
  putStrLn . (++) "  Height:             " . show . height $ t
  putStrLn . (++) "  Number of pages:    " . show . numOfPages $ t
  putStrLn . (++) "  Memory usage:       " . show . memUsage $ t
  putStrLn . (++) "  Fill size:          " . show . fillSize $ t
  putStrLn . (++) "  Fill ratio:         " . show . fillRatio $ t

newtype MhPatSst = MhPatSst (Page PaCoTree) deriving (Eq, Show)

instance IpRouter MhPatSst where
  mkTable                    = MhPatSst . patSstBuild False mhMerge .
                               (mkTable :: [Entry] -> PaCoTree)
  insEntry e (MhPatSst t)    = MhPatSst $ patSstInsert False mhMerge e t
  delEntry e (MhPatSst t)    = MhPatSst $ patSstDelete False mhMerge e t
  ipLookup addr (MhPatSst t) = patSstLookup addr t
  numOfPrefixes (MhPatSst t) = numOfPrefixes' t

instance PatSst MhPatSst where
  height (MhPatSst t)     = pageDepth t
  numOfPages (MhPatSst t) = numOfPages' t
  memUsage (MhPatSst t)   = memUsage' t
  fillSize (MhPatSst t)   = fillSize' t
  checkPages (MhPatSst t) = checkPages' t

newtype MhPatSstM = MhPatSstM (Page PaCoTree) deriving (Eq, Show)

instance IpRouter MhPatSstM where
  mkTable                     = MhPatSstM . patSstBuild True mhMerge .
                                (mkTable :: [Entry] -> PaCoTree)
  insEntry e (MhPatSstM t)    = MhPatSstM $ patSstInsert True mhMerge e t
  delEntry e (MhPatSstM t)    = MhPatSstM $ patSstDelete True mhMerge e t
  ipLookup addr (MhPatSstM t) = patSstLookup addr t
  numOfPrefixes (MhPatSstM t) = numOfPrefixes' t

instance PatSst MhPatSstM where
  height (MhPatSstM t)     = pageDepth t
  numOfPages (MhPatSstM t) = numOfPages' t
  memUsage (MhPatSstM t)   = memUsage' t
  fillSize (MhPatSstM t)   = fillSize' t
  checkPages (MhPatSstM t) = checkPages' t

newtype MsPatSst = MsPatSst (Page PaCoTree) deriving (Eq, Show)

instance IpRouter MsPatSst where
  mkTable                    = MsPatSst . patSstBuild False msMerge .
                               (mkTable :: [Entry] -> PaCoTree)
  insEntry e (MsPatSst t)    = MsPatSst $ patSstInsert False msMerge e t
  delEntry e (MsPatSst t)    = MsPatSst $ patSstDelete False msMerge e t
  ipLookup addr (MsPatSst t) = patSstLookup addr t
  numOfPrefixes (MsPatSst t) = numOfPrefixes' t

instance PatSst MsPatSst where
  height (MsPatSst t)     = pageDepth t
  numOfPages (MsPatSst t) = numOfPages' t
  memUsage (MsPatSst t)   = memUsage' t
  fillSize (MsPatSst t)   = fillSize' t
  checkPages (MsPatSst t) = checkPages' t

newtype MsPatSstM = MsPatSstM (Page PaCoTree) deriving (Eq, Show)

instance IpRouter MsPatSstM where
  mkTable                     = MsPatSstM . patSstBuild True msMerge .
                                (mkTable :: [Entry] -> PaCoTree)
  insEntry e (MsPatSstM t)    = MsPatSstM $ patSstInsert True msMerge e t
  delEntry e (MsPatSstM t)    = MsPatSstM $ patSstDelete True msMerge e t
  ipLookup addr (MsPatSstM t) = patSstLookup addr t
  numOfPrefixes (MsPatSstM t) = numOfPrefixes' t

instance PatSst MsPatSstM where
  height (MsPatSstM t)     = pageDepth t
  numOfPages (MsPatSstM t) = numOfPages' t
  memUsage (MsPatSstM t)   = memUsage' t
  fillSize (MsPatSstM t)   = fillSize' t
  checkPages (MsPatSstM t) = checkPages' t
