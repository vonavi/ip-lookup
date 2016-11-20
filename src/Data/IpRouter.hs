-- | Defines methods supported by a network router.
module Data.IpRouter
  (
    Entry(..)
  , IpRouter(..)
  , insEntries
  , delEntries
  ) where

import           Data.Prefix

-- | Routing-table entry
data Entry = Entry { network :: Prefix -- ^ Subnetwork
                   , nextHop :: Int    -- ^ Next-hop ID
                   }
           deriving (Eq, Show)

-- | Network-router typeclass
class IpRouter a where
  mkTable       :: [Entry] -> a              -- ^ Builds from a list
                                             --   of routing-table
                                             --   entries.
  insEntry      :: Entry   -> a -> a         -- ^ Inserts a
                                             --   routing-table entry.
  delEntry      :: Entry   -> a -> a         -- ^ Deletes a
                                             --   routing-table entry.
  ipLookup      :: Prefix  -> a -> Maybe Int -- ^ Makes the
                                             --   longest-prefix match
                                             --   (LPM).
  numOfPrefixes :: a       -> Int            -- ^ Returns the number
                                             --   of prefixes.

-- | Inserts a list of routing-table entries into network router.
insEntries :: IpRouter a => a -> [Entry] -> a
insEntries = foldr insEntry

-- | Deletes a list of routing-table entries from network router.
delEntries :: IpRouter a => a -> [Entry] -> a
delEntries = foldr delEntry
