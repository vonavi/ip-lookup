module Data.Bitmap
       (
         fromList
       ) where

import           Data.Bits
import           Data.Word

data Bitmap = Bitmap { word :: [Word8]
                     , size :: Int
                     }

instance Show Bitmap where
  show bmp | size' <= 0 = showIt w [7, 6 .. -size']
           | otherwise  = showIt w [7, 6 .. 0] ++ show bmp'
    where size'  = size bmp - 8
          (w:ws) = word bmp
          bmp'   = Bitmap { word = ws, size = size' }
          showIt x = map (\b -> if x `testBit` b then '1' else '0')

instance Monoid Bitmap where
  mempty              = Bitmap { word = [], size = 0 }
  bmp1 `mappend` bmp2 = Bitmap { word = ws, size = s1 + s2 }
    where Bitmap { word = ws1, size = s1 } = bmp1
          Bitmap { word = ws2, size = s2 } = bmp2
          offset = 8 * length ws1 - s1
          ws     = if offset == 0
                   then ws1 ++ ws2
                   else ws1 `joinIt` shiftIt (0 : ws2)
          shiftIt x = zipWith (.|.) (map (`shiftL` offset) x)
                      (map (`shiftR` (8 - offset)) (tail x) ++ [0])
          joinIt x y = init x ++ [last x .|. head y] ++ tail y

fromList :: [Bool] -> Bitmap
fromList bs = Bitmap { word = ws, size = length bs }
  where ws = tail . foldr helper [0] $ zip (cycle [7, 6 .. 0]) bs
        helper :: (Int, Bool) -> [Word8] -> [Word8]
        helper (n, b) (x:xs')
          | n == 7    = 0 : x' : xs'
          | otherwise =     x' : xs'
          where x' = if b then x `setBit` n else x
