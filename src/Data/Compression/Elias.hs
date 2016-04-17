module Data.Compression.Elias
       (
         encodeEliasGamma
       , decodeEliasGamma
       ) where

import           Data.Monoid ((<>))

import           Data.Bitmap

encodeEliasGamma :: Int -> Bitmap
encodeEliasGamma x | x <= 0    = error "Not positive number"
                   | otherwise = fromIntExact 0 (pred . size $ bmp) <> bmp
  where bmp = fromInt x

decodeEliasGamma :: Bitmap -> Int
decodeEliasGamma bmp = toInt . takeBits (succ s) . dropBits s $ bmp
  where s = leadingZeros bmp
