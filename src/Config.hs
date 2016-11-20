-- | Provides configuration parameters.
module Config
  (
    Config(..)
  , config
  ) where

-- | Configuration parameters
data Config = Config
              { minPageSize :: Int  -- ^ Minimum size of memory page
              , maxPageSize :: Int  -- ^ Maximum size of memory page
              , prevNextHop :: Bool -- ^ Is previous next hop stored
                                    --   or not
              , nextHopSize :: Int  -- ^ Size of next-hop data

              , eliasFanoLowerBits :: Int -- ^ Number of lower bits
              }

-- | Keeps the default configuration parameters.
config :: Config
config = Config
         { minPageSize = 128
         , maxPageSize = 6 * 128
         , nextHopSize = 18
         , prevNextHop = True

         , eliasFanoLowerBits = 3
         }
