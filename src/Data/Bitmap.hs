module Data.Bitmap
       (
         fromList
       , rank1
       , rank0
       ) where

import           Data.Bits
import qualified Data.Vector as V
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

rank1 :: Int -> Bitmap -> Int
rank1 n bmp
  | s <= 0 || n < 0 = 0
  | n < 7           = prank
  | otherwise       = (xrank +) $ rank1 (n - 8) bmp'
  where Bitmap { word = ws, size = s } = bmp
        (x:xs) = ws
        xrank  = (rankVec V.!) . fromInteger . toInteger $ x
        offset = 7 - n
        mask   = (`shiftL` offset) . (`shiftR` offset) $ 255
        prank  = (rankVec V.!) . fromInteger . toInteger $ x .&. mask
        bmp'   = Bitmap { word = xs, size = s - 8 }

rank0 :: Int -> Bitmap -> Int
rank0 n bmp = rank1 n bmp { word = map complement . word $ bmp }

rankVec :: V.Vector Int
rankVec = V.fromList [ 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
                     , 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
                     , 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
                     , 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
                     , 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
                     , 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
                     , 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
                     , 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
                     , 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
                     , 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
                     , 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
                     , 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
                     , 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
                     , 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
                     , 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
                     , 4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
                     ]
