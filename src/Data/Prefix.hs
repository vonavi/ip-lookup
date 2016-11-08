{-# LANGUAGE FlexibleInstances #-}

module Data.Prefix
  (
    Address
  , ipv4Address
  , ipv6Address
  , Mask
  , Vpn
  , Prefix
  , mkPrefix
  , setVpn
  , maskLength
  , empty
  , breakAt
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

newtype IPv4Address = IPv4Address Word32

fromOctetList :: [Word8] -> IPv4Address
fromOctetList = IPv4Address . foldl' accum 0
  where accum r o = (r `shiftL` 8) .|. fromIntegral o

readsOctets :: Int -> ReadS [Word8]
readsOctets d = helper (4 :: Int)
  where helper 1 s = map (first (:[])) . (readsPrec d :: ReadS Word8) $ s
        helper n s = [ (x:xs, w)
                     | (x, "")  <- (readsPrec d :: ReadS Word8) t
                     , (".", v) <- lex u
                     , (xs, w)  <- helper (pred n) v
                     ]
          where (t, u) = span isDigit s

instance Read IPv4Address where
  readsPrec d = map (first fromOctetList) . readsOctets d

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

readsHex :: Int -> ReadS Hex
readsHex d = helper (4 :: Int)
  where helper 0 s       = [ ([], s) ]
        helper n (c:cs)
          | isHexDigit c = [ (x:xs, t)
                           | (x, "") <- (readsPrec d :: ReadS HexDigit) [c]
                           , (xs, t) <- helper (pred n) cs
                           ]
        helper _ s       = [ ([], s) ]

instance {-# OVERLAPPING #-} Read Hex where
  readsPrec d = map (first reduceZeros) . readsHex d
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

instance Read IPv6Address where
  readsPrec _ s
    | lenx > 8                        = []
    | (lenx /= 8) /= ("::" `elem` xs) = []
    | otherwise                       =
      map (first fromHexList) . (readList :: ReadS [Hex])
      . ("[" ++) . (++ "]" ++ other) . intercalate "," . map addZeros $ xs
    where (ip, other) = span (\c -> isHexDigit c || ':' == c) s
          xs          = filter (":" /=) . split (dropBlanks $ oneOf ":") $ ip
          lenx        = length xs
          addZeros "::" = intercalate "," $ replicate (9 - length xs) "0"
          addZeros x    = x

longestGroupIndices :: (a -> Bool) -> [a] -> [Int]
longestGroupIndices p xs
  | null indexGroups = []
  | otherwise        = maximumBy (comparing length) . reverse $ indexGroups
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
          dropGroup (n, _) (g:gs) | n == g = (bool "" ":" (null gs), gs)
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
type Vpn = Hex
data Prefix = IPv4  Word32  Mask
            | VPNv4 Word64  Mask
            | IPv6  Integer Mask
            | VPNv6 Integer Mask
            deriving Show

instance Eq Prefix where
  (IPv4  a1 m1) == (IPv4  a2 m2) =
    (m1 == m2) && ((a1 `xor` a2) .&. (bit 32 - bit (32 - m1)) == 0)
  (VPNv4 a1 m1) == (VPNv4 a2 m2) =
    (m1 == m2) && ((a1 `xor` a2) .&. (bit 48 - bit (48 - m1)) == 0)
  (IPv6  a1 m1) == (IPv6  a2 m2) =
    (m1 == m2) && ((a1 `xor` a2) .&. (bit 128 - bit (128 - m1)) == 0)
  (VPNv6 a1 m1) == (VPNv6 a2 m2) =
    (m1 == m2) && ((a1 `xor` a2) .&. (bit 144 - bit (144 - m1)) == 0)
  _             == _             = False

mkPrefix :: Address -> Mask -> Prefix
mkPrefix (IPv4Addr (IPv4Address x)) = IPv4 x
mkPrefix (IPv6Addr (IPv6Address x)) = IPv6 x

setVpn :: Vpn -> Prefix -> Prefix
setVpn v (IPv4  x m) = VPNv4 (vpnBits .|. addrBits) (m + 16)
  where vpnBits  = fromIntegral (hexToWord v) `shiftL` 32
        addrBits = fromIntegral x
setVpn v (VPNv4 x m) = VPNv4 (vpnBits .|. addrBits) m
  where vpnBits  = fromIntegral (hexToWord v) `shiftL` 32
        addrBits = x .&. (bit 32 - bit 0)
setVpn v (IPv6  x m) = VPNv6 (vpnBits .|. addrBits) (m + 16)
  where vpnBits  = toInteger (hexToWord v) `shiftL` 128
        addrBits = x
setVpn v (VPNv6 x m) = VPNv6 (vpnBits .|. addrBits) m
  where vpnBits  = toInteger (hexToWord v) `shiftL` 128
        addrBits = x .&. (bit 128 - bit 0)

maskLength :: Prefix -> Int
maskLength (IPv4  _ m) = m
maskLength (VPNv4 _ m) = m
maskLength (IPv6  _ m) = m
maskLength (VPNv6 _ m) = m

empty :: Prefix -> Bool
empty (IPv4  _ 0) = True
empty (VPNv4 _ 0) = True
empty (IPv6  _ 0) = True
empty (VPNv6 _ 0) = True
empty _           = False

breakAt :: Int -> Prefix -> (Prefix, Prefix)
breakAt n p@(IPv4  x m) | n <= 0    = (IPv4 0 0, p)
                        | n >= m    = (p, IPv4 0 0)
                        | otherwise = (IPv4 x n, IPv4 x' (m - n))
  where x' = x `shiftL` n
breakAt n p@(VPNv4 x m) | n <= 0    = (VPNv4 0 0, p)
                        | n >= m    = (p, VPNv4 0 0)
                        | otherwise = (VPNv4 x n, VPNv4 x' (m - n))
  where x' = (x `shiftL` n) .&. (bit 48 - bit 0)
breakAt n p@(IPv6  x m) | n <= 0    = (IPv6 0 0, p)
                        | n >= m    = (p, IPv6 0 0)
                        | otherwise = (IPv6 x n, IPv6 x' (m - n))
  where x' = (x `shiftL` n) .&. (bit 128 - bit 0)
breakAt n p@(VPNv6 x m) | n <= 0    = (VPNv6 0 0, p)
                        | n >= m    = (p, VPNv6 0 0)
                        | otherwise = (VPNv6 x n, VPNv6 x' (m - n))
  where x' = (x `shiftL` n) .&. (bit 144 - bit 0)

cons :: Bool -> Prefix -> Prefix
cons b (IPv4  x m) = IPv4  x' (succ m)
  where x' = bool clearBit setBit b (x `shiftR` 1) 31
cons b (VPNv4 x m) = VPNv4 x' (succ m)
  where x' = bool clearBit setBit b (x `shiftR` 1) 47
cons b (IPv6  x m) = IPv6  x' (succ m)
  where x' = bool clearBit setBit b (x `shiftR` 1) 127
cons b (VPNv6 x m) = VPNv6 x' (succ m)
  where x' = bool clearBit setBit b (x `shiftR` 1) 143

uncons :: Prefix -> Maybe (Bool, Prefix)
uncons (IPv4  x m) | m == 0    = Nothing
                   | otherwise = Just (x `testBit` 31,  IPv4  x' (pred m))
  where x' = x `shiftL` 1
uncons (VPNv4 x m) | m == 0    = Nothing
                   | otherwise = Just (x `testBit` 47,  VPNv4 x' (pred m))
  where x' = (x `shiftL` 1) `clearBit` 48
uncons (IPv6  x m) | m == 0    = Nothing
                   | otherwise = Just (x `testBit` 127, IPv6  x' (pred m))
  where x' = (x `shiftL` 1) `clearBit` 128
uncons (VPNv6 x m) | m == 0    = Nothing
                   | otherwise = Just (x `testBit` 143, VPNv6 x' (pred m))
  where x' = (x `shiftL` 1) `clearBit` 144

append :: Prefix -> Prefix -> Prefix
append (IPv4  x1 m1) (IPv4  x2 m2) = IPv4  x (m1 + m2)
  where x = (x1 .&. (bit 32 - bit (32 - m1))) .|. (x2 `shiftR` m1)
append (VPNv4 x1 m1) (VPNv4 x2 m2) = VPNv4 x (m1 + m2)
  where x = (x1 .&. (bit 48 - bit (48 - m1))) .|. (x2 `shiftR` m1)
append (IPv6  x1 m1) (IPv6  x2 m2) = IPv6  x (m1 + m2)
  where x = (x1 .&. (bit 128 - bit (128 - m1))) .|. (x2 `shiftR` m1)
append (VPNv6 x1 m1) (VPNv6 x2 m2) = VPNv6 x (m1 + m2)
  where x = (x1 .&. (bit 144 - bit (144 - m1))) .|. (x2 `shiftR` m1)
append _             _             = error "incompatible prefixes"

widthOfInteger :: Integer -> Int
widthOfInteger = ceiling . logBase (2::Double) . fromInteger . succ

commonPrefixes :: Prefix -> Prefix -> (Prefix, Prefix, Prefix)
commonPrefixes (IPv4  x1 m1) (IPv4  x2 m2) =
  (IPv4  x1 m, IPv4  (dropMSBs x1) (m1 - m), IPv4  (dropMSBs x2) (m2 - m))
  where m = minimum [m1, m2, countLeadingZeros (x1 `xor` x2)]
        dropMSBs x = x `shiftL` m
commonPrefixes (VPNv4 x1 m1) (VPNv4 x2 m2) =
  (VPNv4 x1 m, VPNv4 (dropMSBs x1) (m1 - m), VPNv4 (dropMSBs x2) (m2 - m))
  where m = minimum [m1, m2, countLeadingZeros (x1 `xor` x2) - 16]
        dropMSBs x = (x `shiftL` m) .&. (bit 48 - bit 0)
commonPrefixes (IPv6  x1 m1) (IPv6  x2 m2) =
  (IPv6  x1 m, IPv6  (dropMSBs x1) (m1 - m), IPv6  (dropMSBs x2) (m2 - m))
  where m = minimum [m1, m2, 128 - widthOfInteger (x1 `xor` x2)]
        dropMSBs x = (x `shiftL` m) .&. (bit 128 - bit 0)
commonPrefixes (VPNv6 x1 m1) (VPNv6 x2 m2) =
  (VPNv6 x1 m, VPNv6 (dropMSBs x1) (m1 - m), VPNv6 (dropMSBs x2) (m2 - m))
  where m = minimum [m1, m2, 144 - widthOfInteger (x1 `xor` x2)]
        dropMSBs x = (x `shiftL` m) .&. (bit 144 - bit 0)
commonPrefixes _             _             = error "incompatible prefixes"
