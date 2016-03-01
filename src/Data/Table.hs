module Data.Table
       (
         Table
       ) where

import Data.List
import Data.Function (on)

import Data.IpRouter

newtype Table = Table [Entry] deriving Eq

instance Show Table where
  show (Table t) = concat $ zipWith helper t lenList
    where lenList    = map (length . show . prefix) t
          maxLen     = maximum lenList
          helper p l = (show . prefix) p ++ replicate (maxLen - l + 2) ' ' ++
                       (show . nextHop) p ++ "\n"

instance {-# OVERLAPPING #-} IpRouter Table where
  mkTable = Table

  insEntry e (Table t) = Table (e:t)

  delEntry e (Table t) = Table $ delete e t

  ipLookup a (Table t)
    | null matches = Nothing
    | otherwise    = Just . nextHop .
                     maximumBy (compare `on` (mask . prefix)) $ matches
    where matches = filter (prefixMatch a) t

  numOfPrefixes (Table t) = length t
