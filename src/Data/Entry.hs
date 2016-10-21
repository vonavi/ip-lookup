{-# LANGUAGE FlexibleInstances #-}

module Data.Entry
  (
    Address
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
  readsPrec _ s
    | null digits = []
    | null hex    = [ ([HexDigit 0], other) ]
    | otherwise   = [ (map (read . (:[])) hex, other) ]
    where (digits, str) = span isHexDigit s
          (hex, other)  = first (dropWhile ('0' ==)) . second (++ str)
                          . splitAt 4 $ digits

instance {-# OVERLAPPING #-} Show Hex where
  show = map hexDigitToChar

hexToInt :: Hex -> Int
hexToInt xs = sum . zipWith (flip shiftL) offsets . map unHexDigit $ xs
  where offsets = drop (4 - length xs) [12, 8, 4, 0]

intToHex :: Int -> Hex
intToHex 0 = [HexDigit 0]
intToHex x = map HexDigit . dropWhile (0 ==)
             . (`evalState` x) . mapM (state . splitBitsAt) $ [12, 8, 4, 0]


newtype IPv6Address = IPv6Address Integer

fromHexList :: [Hex] -> IPv6Address
fromHexList = IPv6Address . sum . zipWith (flip shiftL) offsets
              . map (toInteger . hexToInt)
  where offsets = map (65536 *) [7, 6 .. 0]

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

indicesOfMaxGroup :: (a -> Bool) -> [a] -> [Int]
indicesOfMaxGroup p = maximumBy (comparing length) . reverse . grIndices
  where grIndices = map (map fst) . wordsBy (not . p . snd) . zip [0 ..]

instance Show IPv6Address where
  show (IPv6Address x)
    | maxLen <= 1        = intercalate ":" . map (show . intToHex) $ octets
    | maxLen == 8        = "::"
    | head maxZeros == 0 = ':' : addrStr
    | last maxZeros == 7 = addrStr ++ ":"
    | otherwise          = addrStr
    where offsets  = map (65536 *) [7, 6 .. 0]
          octets   = map ((fromInteger :: Integer -> Int) . shiftR x) offsets
          maxZeros = indicesOfMaxGroup (== 0) octets
          maxLen   = length maxZeros
          addrStr  = tail . concat . (`evalState` maxZeros)
                     . mapM (state . dropGroup) . zip [0 ..] $ octets
          dropGroup :: (Int, Int) -> [Int] -> (String, [Int])
          dropGroup (n, _) (g:gs) | n == g = (bool "" ":" (null gs), gs)
          dropGroup (_, h) gs              = (':' : show (intToHex h), gs)


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
