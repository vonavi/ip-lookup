module Data.Compression.Elias
       (
         encodeEliasGamma
       , decodeEliasGamma
       , encodeEliasDelta
       , decodeEliasDelta
       , encodeEliasFano
       , decodeEliasFano
       ) where

import           Data.Bits
import           Data.List   (unfoldr)
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

data Bitmap2 = Bitmap2 { highBits :: Bitmap
                       , lowBits  :: Bitmap
                       , lowSize  :: Int
                       }
               deriving Show

encodeEliasFano :: [Int] -> Bitmap2
encodeEliasFano [] = error "Empty monotone sequence"
encodeEliasFano xs = Bitmap2 { highBits = hbmp
                             , lowBits  = lbmp
                             , lowSize  = s
                             }
  where s    = floor . logBase (2 :: Double) $
               fromIntegral (last xs) / fromIntegral (length xs)
        hbs  = map (`shiftR` s) xs
        hbmp = mconcat . map (fromIntExact 1 . succ) $
               zipWith (-) hbs (0 : init hbs)
        lbmp = mconcat . map (`fromIntExact` s) $ xs

decodeEliasFano :: Bitmap2 -> [Int]
decodeEliasFano bmp = zipWith (\h l -> h `shiftL` s + l) hbs lbs
  where s   = lowSize bmp
        hbs = scanl1 (+) . unfoldr getHigh . highBits $ bmp
        lbs = unfoldr getLow . lowBits $ bmp
        getHigh b | size b == 0 = Nothing
                  | otherwise   = let n = leadingZeros b
                                  in Just (n, dropBits (succ n) b)
        getLow b | size b == 0 = Nothing
                 | otherwise   = Just (toInt $ takeBits s b, dropBits s b)
