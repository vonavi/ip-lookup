module Data.Compression.Elias
  (
    Bitmap2(..)
  , encodeUnary
  , decodeUnary
  , encodeEliasGamma
  , decodeEliasGamma
  , encodeEliasDelta
  , decodeEliasDelta
  , encodeEliasFanoWith
  , encodeEliasFano
  , decodeEliasFano
  ) where

import           Data.Bits
import           Data.Function           (on)
import           Data.List               (unfoldr)
import           Data.Monoid             ((<>))

import           Data.Compression.Bitmap

encodeUnary :: Int -> Bitmap
encodeUnary x | x <= 0    = error "Not positive number"
              | otherwise = fromIntExact 1 x

decodeUnary :: Bitmap -> Int
decodeUnary = succ . leadingZeros

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

encodeEliasFanoWith :: Int -> [Int] -> Bitmap2
encodeEliasFanoWith l xs
  | null xs      = error "Empty monotone sequence"
  | last xs == 0 = Bitmap2 { highBits = mconcat . map (\_ -> fromInt 1) $ xs
                           , lowBits  = mempty
                           , lowSize  = 0
                           }
  | otherwise    = Bitmap2 { highBits = hbmp
                           , lowBits  = lbmp
                           , lowSize  = l
                           }
  where hbs  = map (`shiftR` l) xs
        hbmp = mconcat . map (fromIntExact 1 . succ) $
               zipWith (-) hbs (0 : init hbs)
        lbmp = mconcat . map (`fromIntExact` l) $ xs

encodeEliasFano :: [Int] -> Bitmap2
encodeEliasFano xs = encodeEliasFanoWith l xs
  where l =  max 0 . floor . logBase (2 :: Double) $
             ((/) `on` fromIntegral) (last xs) (length xs)

decodeEliasFano :: Bitmap2 -> [Int]
decodeEliasFano bmp
  | s == 0    = hbs
  | otherwise = zipWith (\h l -> h `shiftL` s + l) hbs lbs
  where s   = lowSize bmp
        hbs = scanl1 (+) . unfoldr getHigh . highBits $ bmp
        lbs = unfoldr getLow . lowBits $ bmp
        getHigh b | size b == 0 = Nothing
                  | otherwise   = let n = leadingZeros b
                                  in Just (n, dropBits (succ n) b)
        getLow b | size b == 0 = Nothing
                 | otherwise   = Just (toInt $ takeBits s b, dropBits s b)
