{-# LANGUAGE FlexibleInstances #-}

module Data.Entry
  (
    Address
  , Mask
  , Vpn
  , Prefix
  , mkPrefix
  , setVpn
  , maskLength
  , testBitOf
  , commonPrefix
  , Entry(..)
  ) where

import           Control.Arrow       (first, second)
import           Control.Monad.State
import           Data.Bits
import           Data.Bool           (bool)
import           Data.Char
import           Data.List           (intercalate, maximumBy)
import           Data.List.Split
import           Data.Ord            (comparing)
import           Data.Word

splitBitsAt :: (Num a, Bits a) => Int -> a -> (a, a)
splitBitsAt n x = (h, x - (h `shiftL` n))
  where h = x `shiftR` n

newtype IPv4Address = IPv4Address Word32

fromOctetList :: [Word32] -> IPv4Address
fromOctetList = IPv4Address . sum . zipWith (flip shiftL) [24, 16, 8, 0]

instance Read IPv4Address where
  readsPrec _ s
    | length xs /= 4 = []
    | otherwise      = map (first fromOctetList) . (readList :: ReadS [Word32])
                       $ "[" ++ intercalate "," xs ++ "]" ++ other
    where (addr, other) = span (\c -> isDigit c || '.' == c) s
          xs            = splitOn "." addr

instance Show IPv4Address where
  show (IPv4Address x) = intercalate "." . map show . (`evalState` x)
                         . mapM (state . splitBitsAt) $ [24, 16, 8, 0]


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

instance {-# OVERLAPPING #-} Read Hex where
  readsPrec _ s
    | null digits = []
    | null hex    = [ ([HexDigit 0], other) ]
    | otherwise   = [ (map (read . (:[])) hex, other) ]
    where (digits, str) = span isHexDigit s
          (hex, other)  = first (dropWhile ('0' ==)) . second (++ str)
                          . splitAt 4 $ digits

instance {-# OVERLAPPING #-} Show Hex where
  show = map hexDigitToChar

hexToWord :: Hex -> Word16
hexToWord xs = sum . zipWith (flip shiftL) offsets . map unHexDigit $ xs
  where offsets = drop (4 - length xs) [12, 8, 4, 0]

wordToHex :: Word16 -> Hex
wordToHex 0 = [HexDigit 0]
wordToHex x = map HexDigit . dropWhile (0 ==)
              . (`evalState` x) . mapM (state . splitBitsAt) $ [12, 8, 4, 0]


newtype IPv6Address = IPv6Address Integer

fromHexList :: [Hex] -> IPv6Address
fromHexList = IPv6Address . sum . zipWith (flip shiftL) offsets
              . map (toInteger . hexToWord)
  where offsets = map (16 *) [7, 6 .. 0]

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

testBitOf :: Prefix -> Int -> Bool
testBitOf (IPv4  x m) n | n >= 0 && n < m = x `testBit` (31 - n)
testBitOf (VPNv4 x m) n | n >= 0 && n < m = x `testBit` (47 - n)
testBitOf (IPv6  x m) n | n >= 0 && n < m = x `testBit` (127 - n)
testBitOf (VPNv6 x m) n | n >= 0 && n < m = x `testBit` (143 - n)
testBitOf _           _                   = error "test outside of mask"

widthOfInteger :: Integer -> Int
widthOfInteger = ceiling . logBase (2::Double) . fromInteger . succ

commonPrefix :: Prefix -> Prefix -> Prefix
commonPrefix (IPv4  a1 m1) (IPv4  a2 m2) = IPv4  a1 m
  where m = minimum [m1, m2, countLeadingZeros (a1 `xor` a2)]
commonPrefix (VPNv4 a1 m1) (VPNv4 a2 m2) = VPNv4 a1 m
  where m = minimum [m1, m2, countLeadingZeros (a1 `xor` a2) - 16]
commonPrefix (IPv6  a1 m1) (IPv6  a2 m2) = IPv6  a1 m
  where m = minimum [m1, m2, 128 - widthOfInteger (a1 `xor` a2)]
commonPrefix (VPNv6 a1 m1) (VPNv6 a2 m2) = VPNv6 a1 m
  where m = minimum [m1, m2, 144 - widthOfInteger (a1 `xor` a2)]
commonPrefix _             _             = error "incompatible prefixes"


data Entry = Entry { prefix  :: Prefix
                   , nextHop :: Int
                   }
