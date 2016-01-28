module Data.Table where

import Data.List

import Data.IpRouter

newtype Table = Table [Entry]

instance Show Table where
  show (Table t) = concat $ zipWith helper t lenList
    where lenList    = map (length . show . prefix) t
          maxLen     = maximum lenList
          helper p l = (show . prefix) p ++ replicate (maxLen - l + 2) ' ' ++
                       (show . nextHop) p ++ "\n"

instance IpRouter Table where
  ipInsert e (Table t) = Table (e:t)
  ipLookup a (Table t)
    | null matches = Nothing
    | otherwise    = Just . maximumBy cmp $ matches
    where matches                       = filter (prefixMatch a) t
          cmp (Entry p1 _) (Entry p2 _) = compare (mask p1) (mask p2)
