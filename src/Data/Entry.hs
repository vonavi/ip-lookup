{-# LANGUAGE FlexibleInstances #-}

module Data.Entry
  (
  ) where

import           Control.Arrow       (first, second)
import           Control.Monad.State
import           Data.Bits
import           Data.Char
import           Data.List           (intercalate)
import           Data.List.Split     (splitOn)
import           Data.Word

splitBitsAt :: (Num a, Bits a) => Int -> a -> (a, a)
splitBitsAt n x = (h, x - (h `shiftL` n))
  where h = x `shiftR` n

newtype Ipv4Address = Ipv4Address Word32

fromOctetList :: [Word32] -> Ipv4Address
fromOctetList = Ipv4Address . sum . zipWith (flip shiftL) [24, 16, 8, 0]

instance Read Ipv4Address where
  readsPrec _ s
    | length xs /= 4 = []
    | otherwise      = map (first fromOctetList) . (readList :: ReadS [Word32])
                       $ "[" ++ intercalate "," xs ++ "]" ++ other
    where (addr, other) = span (\c -> isDigit c || '.' == c) s
          xs            = splitOn "." addr

instance Show Ipv4Address where
  show (Ipv4Address x) = intercalate "." . map show . (`evalState` x)
                         . mapM (state . splitBitsAt) $ [24, 16, 8, 0]


newtype HexDigit = HexDigit { unHexDigit :: Int }

hexDigitToChar :: HexDigit -> Char
hexDigitToChar = intToDigit . unHexDigit

instance Read HexDigit where
  readsPrec _ (c:cs) | isHexDigit c = [ (HexDigit . digitToInt $ c, cs) ]
  readsPrec _ _                     = []

instance Show HexDigit where
  show = show . hexDigitToChar

type Hex = [HexDigit]

instance {-# OVERLAPPING #-} Read Hex where
  readsPrec _ s = [ (map (read . (:[])) . dropWhile ('0' ==) $ hex, other) ]
    where (digits, str) = span isHexDigit s
          (hex, other)  = second (++ str) . splitAt 4 $ digits

instance {-# OVERLAPPING #-} Show Hex where
  show [] = "0"
  show xs = map hexDigitToChar xs

hexToInt :: Hex -> Int
hexToInt xs = sum . zipWith (flip shiftL) offsets . map unHexDigit $ xs
  where offsets = drop (4 - length xs) [12, 8, 4, 0]

intToHex :: Int -> Hex
intToHex x = map HexDigit . dropWhile (0 ==)
             . (`evalState` x) . mapM (state . splitBitsAt) $ [12, 8, 4, 0]

newtype Ipv6Address = Ipv6Address Integer

fromHexList :: [Hex] -> Ipv6Address
fromHexList = Ipv6Address . sum . zipWith (flip shiftL) offsets
              . map (toInteger . hexToInt)
  where offsets = map (65536 *) [7, 6 .. 0]

instance Read Ipv6Address where
  readsPrec _ s
    | length xs /= 8 = []
    | otherwise      = map (first fromHexList) . (readList :: ReadS [Hex])
                       $ "[" ++ intercalate "," xs ++ "]" ++ other
    where (addr, other) = span (\c -> isHexDigit c || ':' == c) s
          xs            = splitOn ":" addr

instance Show Ipv6Address where
  show (Ipv6Address x) = intercalate ":" . map showHex $ offsets
    where offsets = map (65536 *) [7, 6 .. 0]
          showHex = show . intToHex . (fromInteger :: Integer -> Int) . shiftR x
