module Data.IpRouter
       (
         Address(Address)
       , Mask(Mask)
       , Prefix(..)
       , Entry(..)
       , IpRouter(..)
       , insEntries
       , delEntries
       ) where

import           Control.Arrow   (first)
import           Data.Bits
import           Data.Char       (isDigit)
import           Data.List       (intercalate)
import           Data.List.Split (splitOn)
import           Data.Word

newtype Address = Address Word32 deriving Eq

listToAddr :: [Word8] -> Address
listToAddr = Address . sum .
             zipWith (flip shift) [24, 16, 8, 0] . map fromIntegral

instance Read Address where
  readsPrec _ s
    | length octets /= 4 = []
    | otherwise          = map (first listToAddr) .
                           (readList :: ReadS [Word8]) $ list
    where (addr, other) = break (\c -> not $ isDigit c || c == '.') s
          octets        = splitOn "." addr
          list          = "[" ++ intercalate "," octets ++ "]" ++ other

instance Show Address where
  show (Address x) = intercalate "." $ map showOctet [24, 16, 8, 0]
    where showOctet = show . (fromIntegral :: Word32 -> Word8) . (x `shiftR`)

newtype Mask = Mask Int deriving (Eq, Ord)

instance Read Mask where
  readsPrec _ s = [ (Mask r, u)
                  | ("/", t) <- lex s
                  , (r, u)   <- (reads :: ReadS Int) t
                  ]

instance Show Mask where
  show (Mask x) = "/" ++ show x

data Prefix = Prefix { address :: {-# UNPACK #-} !Address
                     , mask    :: {-# UNPACK #-} !Mask
                     }
            deriving Eq

instance Read Prefix where
  readsPrec d s = [ (Prefix { address = a, mask = m }, u)
                  | (a, t) <- (readsPrec d :: ReadS Address) s
                  , (m, u) <- (readsPrec d :: ReadS Mask) t
                  ]

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

insEntries :: IpRouter a => a -> [Entry] -> a
insEntries = foldr insEntry

delEntries :: IpRouter a => a -> [Entry] -> a
delEntries = foldr delEntry
