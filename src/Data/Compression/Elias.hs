module Data.Compression.Elias
       (
         encodeEliasGamma
       , decodeEliasGamma
       , encodeEliasDelta
       , decodeEliasDelta
       ) where

import           Data.Bits
import           Data.Monoid ((<>))

import           Data.Bitmap

encodeEliasGamma :: Int -> Bitmap
encodeEliasGamma x | x <= 0    = error "Not positive number"
                   | otherwise = fromIntExact 0 (pred . size $ bmp) <> bmp
  where bmp = fromInt x

decodeEliasGamma :: Bitmap -> Int
decodeEliasGamma bmp = toInt . takeBits (succ s) . dropBits s $ bmp
  where s = leadingZeros bmp

encodeEliasDelta :: Int -> Bitmap
encodeEliasDelta x | x <= 0    = error "Not positive number"
                   | otherwise = encodeEliasGamma (size bmp) <> dropBits 1 bmp
  where bmp = fromInt x

decodeEliasDelta :: Bitmap -> Int
decodeEliasDelta bmp = 1 `shiftL` n + x
  where l = leadingZeros bmp
        n = pred . toInt . takeBits (2 * l + 1) $ bmp
        x = toInt . takeBits n . dropBits (2 * l + 1) $ bmp
