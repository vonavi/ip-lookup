{-# LANGUAGE FlexibleInstances #-}

module Data.Prefix
  (
    Address
  , ipv4Address
  , ipv6Address
  , Mask
  , Prefix
  , mkPrefix
  , maskLength
  , null
  , splitAt
  , cons
  , uncons
  , append
  , commonPrefixes
  ) where

import           Control.Arrow       (first)
import           Control.Monad.State
import           Data.Bits
import           Data.Bool           (bool)
import           Data.Char
import           Data.List           (foldl', intercalate, maximumBy)
import           Data.List.Split
import           Data.Ord            (comparing)
import           Data.Word
import           Prelude             hiding (null, splitAt)
import qualified Prelude             as P (null)

newtype IPv4Address = IPv4Address Word32

fromOctetList :: [Word8] -> IPv4Address
fromOctetList = IPv4Address . foldl' accum 0
  where accum r o = (r `shiftL` 8) .|. fromIntegral o

readsOctets :: Int -> Int -> ReadS [Word8]
readsOctets n d s
  | n == 0    = [ ([], s) ]
  | n == 1    = [ ([x], u) | (x, "") <- (readsPrec d :: ReadS Word8) t ]
  | otherwise = [ (x:xs, w)
                | (x, "")  <- (readsPrec d :: ReadS Word8) t
                , (".", v) <- lex u
                , (xs, w)  <- readsOctets (pred n) d v
                ]
  where (t, u) = span isDigit s

instance Read IPv4Address where
  readsPrec d = map (first fromOctetList) . readsOctets 4 d

instance Show IPv4Address where
  show (IPv4Address x) =
    intercalate "."
    . map (show . (fromIntegral :: Word32 -> Word8) . shiftR x) $ [24, 16, 8, 0]


newtype HexDigit = HexDigit { unHexDigit :: Word16 }

hexDigitToChar :: HexDigit -> Char
hexDigitToChar = intToDigit . fromIntegral . unHexDigit

instance Read HexDigit where
  readsPrec _ (c:cs)
    | isHexDigit c = [ (HexDigit . fromIntegral . digitToInt $ c, cs) ]
  readsPrec _ _    = []

instance Show HexDigit where
  show = show . hexDigitToChar

type Hex = [HexDigit]

readsHex :: Int -> Int -> ReadS Hex
readsHex n d = helper n
  where helper k s
          | k == 0             = [ ([], s) ]
          | not (P.null digit) = [ (x:xs, u)
                                 | (x, t)  <- digit
                                 , (xs, u) <- helper (pred k) t
                                 ]
          | k == n             = []
          | otherwise          = [ ([], s) ]
          where digit = (readsPrec d :: ReadS HexDigit) s

instance {-# OVERLAPPING #-} Read Hex where
  readsPrec d = map (first reduceZeros) . readsHex 4 d
    where reduceZeros xs@[HexDigit 0]   = xs
          reduceZeros (HexDigit 0 : xs) = reduceZeros xs
          reduceZeros xs                = xs

instance {-# OVERLAPPING #-} Show Hex where
  show = map hexDigitToChar

hexToWord :: Hex -> Word16
hexToWord = foldl' accum 0
  where accum r h = (r `shiftL` 4) .|. unHexDigit h

wordToHex :: Word16 -> Hex
wordToHex 0 = [HexDigit 0]
wordToHex x = dropWhile ((0 ==) . unHexDigit)
              . map (HexDigit . (15 .&.) . shiftR x) $ [12, 8, 4, 0]


newtype IPv6Address = IPv6Address Integer

fromHexList :: [Hex] -> IPv6Address
fromHexList = IPv6Address . foldl' accum 0
  where accum r h = (r `shiftL` 16) .|. (toInteger . hexToWord $ h)

readsHexList :: Int -> ReadS [Hex]
readsHexList d s
  | P.null hex   = [ ([], s) ]
  | P.null colon = [ ([x], t) | (x, t) <- hex ]
  | otherwise    = [ (x:xs, u)
                   | (x, t)  <- colon
                   , (xs, u) <- readsHexList d t
                   ]
  where hex   = (readsPrec d :: ReadS Hex) s
        colon = [ (x, u) | (x, t) <- hex, (":", u) <- lex t ]

instance Read IPv6Address where
  readsPrec d s = [ let l = 8 - (length xs + length ys)
                    in (fromHexList $ xs ++ replicate l [HexDigit 0] ++ ys, v)
                  | (xs, "") <- readsHexList d t
                  , (ys, v)  <- readsHexList d u
                  ]
    where (t, u) = splitOn2Colon s
          splitOn2Colon (':':':':xs) = ([], xs)
          splitOn2Colon (x:xs)       = first (x :) . splitOn2Colon $ xs
          splitOn2Colon []           = ([], [])

longestGroupIndices :: (a -> Bool) -> [a] -> [Int]
longestGroupIndices p xs
  | P.null indexGroups = []
  | otherwise          = maximumBy (comparing length) . reverse $ indexGroups
  where indexGroups = map (map fst) . wordsBy (not . p . snd) . zip [0 ..] $ xs

instance Show IPv6Address where
  show (IPv6Address x)
    | maxLen <= 1        = intercalate ":" . map (show . wordToHex) $ octets
    | maxLen == 8        = "::"
    | head maxZeros == 0 = ':' : addrStr
    | last maxZeros == 7 = addrStr ++ ":"
    | otherwise          = addrStr
    where offsets  = map (16 *) [7, 6 .. 0]
          octets   = map ((fromInteger :: Integer -> Word16) . shiftR x) offsets
          maxZeros = longestGroupIndices (== 0) octets
          maxLen   = length maxZeros
          addrStr  = tail . concat . (`evalState` maxZeros)
                     . mapM (state . dropGroup) . zip [0 ..] $ octets
          dropGroup :: (Int, Word16) -> [Int] -> (String, [Int])
          dropGroup (n, _) (g:gs) | n == g = (bool "" ":" (P.null gs), gs)
          dropGroup (_, h) gs              = (':' : show (wordToHex h), gs)


data Address = IPv4Addr IPv4Address
             | IPv6Addr IPv6Address

instance Read Address where
  readsPrec d s = [ (IPv4Addr x, t)
                  | (x, t) <- (readsPrec d :: ReadS IPv4Address) s
                  ]
                  ++
                  [ (IPv6Addr x, t)
                  | (x, t) <- (readsPrec d :: ReadS IPv6Address) s
                  ]

instance Show Address where
  show (IPv4Addr x) = show x
  show (IPv6Addr x) = show x

ipv4Address :: Word32 -> Address
ipv4Address = IPv4Addr . IPv4Address

ipv6Address :: Integer -> Address
ipv6Address = IPv6Addr . IPv6Address


type Mask = Int
data Prefix = IPv4  Word32  Mask
            | IPv6  Integer Mask
            deriving Show

instance Eq Prefix where
  (IPv4 a1 m1) == (IPv4 a2 m2) =
    (m1 == m2) && ((a1 `xor` a2) .&. (bit 32 - bit (32 - m1)) == 0)
  (IPv6 a1 m1) == (IPv6 a2 m2) =
    (m1 == m2) && ((a1 `xor` a2) .&. (bit 128 - bit (128 - m1)) == 0)
  _            == _            = False

mkPrefix :: Address -> Mask -> Prefix
mkPrefix (IPv4Addr (IPv4Address x)) = IPv4 x
mkPrefix (IPv6Addr (IPv6Address x)) = IPv6 x

maskLength :: Prefix -> Int
maskLength (IPv4 _ m) = m
maskLength (IPv6 _ m) = m

null :: Prefix -> Bool
null (IPv4 _ 0) = True
null (IPv6 _ 0) = True
null _          = False

splitAt :: Int -> Prefix -> (Prefix, Prefix)
splitAt n p@(IPv4 x m) | n <= 0    = (IPv4 0 0, p)
                       | n >= m    = (p, IPv4 0 0)
                       | otherwise = (IPv4 x n, IPv4 x' (m - n))
  where x' = x `shiftL` n
splitAt n p@(IPv6 x m) | n <= 0    = (IPv6 0 0, p)
                       | n >= m    = (p, IPv6 0 0)
                       | otherwise = (IPv6 x n, IPv6 x' (m - n))
  where x' = (x `shiftL` n) .&. (bit 128 - bit 0)

cons :: Bool -> Prefix -> Prefix
cons b (IPv4 x m) = IPv4 x' (succ m)
  where x' = bool clearBit setBit b (x `shiftR` 1) 31
cons b (IPv6 x m) = IPv6 x' (succ m)
  where x' = bool clearBit setBit b (x `shiftR` 1) 127

uncons :: Prefix -> Maybe (Bool, Prefix)
uncons (IPv4 x m) | m == 0    = Nothing
                  | otherwise = Just (x `testBit` 31, IPv4 x' (pred m))
  where x' = x `shiftL` 1
uncons (IPv6 x m) | m == 0    = Nothing
                  | otherwise = Just (x `testBit` 127, IPv6 x' (pred m))
  where x' = (x `shiftL` 1) `clearBit` 128

append :: Prefix -> Prefix -> Prefix
append (IPv4 x1 m1) (IPv4 x2 m2) = IPv4 x (m1 + m2)
  where x = (x1 .&. (bit 32 - bit (32 - m1))) .|. (x2 `shiftR` m1)
append (IPv6 x1 m1) (IPv6 x2 m2) = IPv6 x (m1 + m2)
  where x = (x1 .&. (bit 128 - bit (128 - m1))) .|. (x2 `shiftR` m1)
append _            _            = error "incompatible prefixes"

commonPrefixes :: Prefix -> Prefix -> (Prefix, Prefix, Prefix)
commonPrefixes (IPv4 x1 m1) (IPv4 x2 m2) =
  (IPv4 x1 m, IPv4 (dropMSBs x1) (m1 - m), IPv4 (dropMSBs x2) (m2 - m))
  where m = minimum [m1, m2, countLeadingZeros (x1 `xor` x2)]
        dropMSBs x = x `shiftL` m
commonPrefixes (IPv6 x1 m1) (IPv6 x2 m2) =
  (IPv6 x1 m, IPv6 (dropMSBs x1) (m1 - m), IPv6 (dropMSBs x2) (m2 - m))
  where m = minimum [m1, m2, 128 - numOfBits (x1 `xor` x2)]
        dropMSBs x = (x `shiftL` m) .&. (bit 128 - bit 0)
        numOfBits = ceiling . logBase (2::Double) . fromInteger . succ
commonPrefixes _            _            = error "incompatible prefixes"
