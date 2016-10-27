module Data.IpRouter
  (
    Entry(..)
  , IpRouter(..)
  , insEntries
  , delEntries
  ) where

import           Data.Prefix

data Entry = Entry { network :: Prefix
                   , nextHop :: Int
                   }
           deriving (Eq, Show)

class IpRouter a where
  mkTable       :: [Entry] -> a
  insEntry      :: Entry   -> a -> a
  delEntry      :: Entry   -> a -> a
  ipLookup      :: Prefix  -> a -> Maybe Int
  numOfPrefixes :: a       -> Int

insEntries :: IpRouter a => a -> [Entry] -> a
insEntries = foldr insEntry

delEntries :: IpRouter a => a -> [Entry] -> a
delEntries = foldr delEntry
