module Data.IpRouter
       (
         Address(Address)
       , Mask(Mask)
       , Prefix(..)
       , Entry(..)
       , IpRouter(..)
       , strToAddr
       , strToMask
       ) where

import           Data.Bits
import           Data.List.Split (splitOn)
import           Data.Word

newtype Address = Address Word32 deriving Eq

instance Show Address where
  show (Address x) = tail $ concatMap ((++) "." . show) parts
    where parts = map (helper . shiftR x) [24, 16, 8, 0]
          helper y = (fromInteger . toInteger) y :: Word8

strToAddr :: String -> Address
strToAddr s = Address $ sum $ zipWith shift parts [24, 16, 8, 0]
  where parts = map (read :: String -> Word32) $ splitOn "." s

newtype Mask = Mask Int deriving (Eq, Ord)

strToMask :: String -> Mask
strToMask ('/' : ss) = Mask (read ss :: Int)
strToMask _          = error "Incorrect network mask"

instance Show Mask where
  show (Mask x) = "/" ++ show x

data Prefix = Prefix { address :: {-# UNPACK #-} !Address
                     , mask    :: {-# UNPACK #-} !Mask
                     }
            deriving Eq

instance Show Prefix where
  show x = (show . address) x ++ (show . mask) x

data Entry = Entry { prefix  :: {-# UNPACK #-} !Prefix
                   , nextHop :: {-# UNPACK #-} !Int
                   }
           deriving (Eq, Show)

class IpRouter a where
  mkTable       :: [Entry] -> a
  insEntry      :: Entry   -> a -> a
  delEntry      :: Entry   -> a -> a
  ipLookup      :: Address -> a -> Maybe Int
  numOfPrefixes :: a       -> Int
