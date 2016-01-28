module Data.IpRouter
       (
         Address(Address)
       , Mask(Mask)
       , Prefix(..)
       , Entry(..)
       , IpRouter(..)
       ) where

import Data.Word
import Data.Bits

newtype Address = Address Word32

instance Show Address where
  show (Address x) = tail $ concatMap ((++) "." . show) parts
    where parts = map (helper . shiftR x) [24,16,8,0]
          helper y = (fromInteger . toInteger) y :: Word8

newtype Mask = Mask Int

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
  ipLookup :: Address -> a -> Entry
