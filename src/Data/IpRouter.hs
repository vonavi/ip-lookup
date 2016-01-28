module Data.IpRouter
       (
         Address(Address)
       , Mask(Mask)
       , Prefix(..)
       , Entry(..)
       , IpRouter(..)
       , prefixMatch
       ) where

import Data.Word
import Data.Bits
import Data.Function (on)

newtype Address = Address Word32

instance Show Address where
  show (Address x) = tail $ concatMap ((++) "." . show) parts
    where parts = map (helper . shiftR x) [24,16,8,0]
          helper y = (fromInteger . toInteger) y :: Word8

newtype Mask = Mask Int deriving (Eq, Ord)

instance Show Mask where
  show (Mask x) = "/" ++ show x

data Prefix = Prefix { address :: Address
                     , mask    :: Mask
                     }

instance Show Prefix where
  show x = (show . address) x ++ (show . mask) x

data Entry = Entry { prefix  :: Prefix
                   , nextHop :: Int
                   } deriving Show

class IpRouter a where
  ipInsert :: Entry   -> a -> a
  ipLookup :: Address -> a -> Maybe Entry

prefixMatch :: Address -> Entry -> Bool
prefixMatch (Address x) (Entry p _) = ((==) `on` (`shiftR` offset)) x a
  where Address a = address p
        Mask m    = mask p
        offset    = 32 - m
