module Data.IpRouter
       (
         Address(Address)
       , Mask(Mask)
       , Prefix(..)
       , Entry(..)
       , IpRouter(..)
       , strToAddr
       , addrBits
       , strToMask
       , prefixBits
       , prefixMatch
       ) where

import Data.Word
import Data.Bits
import Data.Function (on)
import Data.List.Split (splitOn)

newtype Address = Address Word32 deriving Eq

instance Show Address where
  show (Address x) = tail $ concatMap ((++) "." . show) parts
    where parts = map (helper . shiftR x) [24, 16, 8, 0]
          helper y = (fromInteger . toInteger) y :: Word8

strToAddr :: String -> Address
strToAddr s = Address $ sum $ zipWith shift parts [24, 16, 8, 0]
  where parts = map (read :: String -> Word32) $ splitOn "." s

addrBits :: Address -> [Bool]
addrBits (Address a) = map (`testBit` 31) . take 32 . iterate (`shift` 1) $ a

newtype Mask = Mask Int deriving (Eq, Ord)

strToMask :: String -> Mask
strToMask ('/' : ss) = Mask (read ss :: Int)
strToMask _          = error "Incorrect network mask"

instance Show Mask where
  show (Mask x) = "/" ++ show x

data Prefix = Prefix { address :: Address
                     , mask    :: Mask
                     }
            deriving Eq

instance Show Prefix where
  show x = (show . address) x ++ (show . mask) x

prefixBits :: Prefix -> [Bool]
prefixBits p = map (`testBit` 31) . take m . iterate (`shift` 1) $ a
  where Address a = address p
        Mask m    = mask p

data Entry = Entry { prefix  :: Prefix
                   , nextHop :: Int
                   }
           deriving (Eq, Show)

prefixMatch :: Address -> Entry -> Bool
prefixMatch (Address x) (Entry p _) = ((==) `on` (`shiftR` offset)) x a
  where Address a = address p
        Mask m    = mask p
        offset    = 32 - m

class IpRouter a where
  mkTable       :: [Entry] -> a
  insEntry      :: Entry   -> a -> a
  delEntry      :: Entry   -> a -> a
  ipLookup      :: Address -> a -> Maybe Int
  numOfPrefixes :: a       -> Int
