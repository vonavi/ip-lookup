module Config
  (
    Config(..)
  , config
  ) where

data Config = Config
              { minPageSize :: Int
              , maxPageSize :: Int
              , nextHopSize :: Int
              , prevNextHop :: Bool

              , eliasFanoLowerBits :: Int
              }

config :: Config
config = Config
         { minPageSize = 128
         , maxPageSize = 6 * 128
         , nextHopSize = 18
         , prevNextHop = True

         , eliasFanoLowerBits = 3
         }
