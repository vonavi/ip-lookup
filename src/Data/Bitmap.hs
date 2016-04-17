module Data.Bitmap
       (
         Bitmap(..)
       , fromList
       , fromIntExact
       , fromInt
       , toInt
       , inverse
       , takeBits
       , dropBits
       , leadingZeros
       , rank1
       , rank0
       , select1
       , select0
       ) where

import           Data.Bits
import           Data.List   (foldl')
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
  bmp1 `mappend` bmp2 = fromIntExact x (s1 + s2)
    where s1 = size bmp1
          s2 = size bmp2
          x  = toInt bmp1 `shiftL` s2 + toInt bmp2

fromList :: [Bool] -> Bitmap
fromList bs = Bitmap { word = ws, size = length bs }
  where ws = tail . foldr helper [0] $ zip (cycle [7, 6 .. 0]) bs
        helper :: (Int, Bool) -> [Word8] -> [Word8]
        helper (n, b) (x:xs')
          | n == 7    = 0 : x' : xs'
          | otherwise =     x' : xs'
          where x' = if b then x `setBit` n else x

fromIntExact :: Int -> Int -> Bitmap
fromIntExact x s = Bitmap { word = ws, size = s }
  where nw = (s + 7) `div` 8
        ws = map helper [nw - 1, nw - 2 .. 0]
        x' = x `shiftL` (7 - (s - 1) `mod` 8)
        helper n = fromIntegral (x' `shiftR` (8 * n)) :: Word8

fromInt :: Int -> Bitmap
fromInt x = fromIntExact x s
  where s = succ . floor . logBase (2 :: Double) . fromIntegral $ x

toInt :: Bitmap -> Int
toInt Bitmap { word = ws, size = s } = x `shiftR` (8 * length ws - s)
  where x = foldl' (\b a -> b `shiftL` 8 + fromIntegral a) (0 :: Int) ws

inverse :: Bitmap -> Bitmap
inverse bmp | offset == 0 = bmp { word = ws }
            | otherwise   = bmp { word = ws' ++ [w'] }
  where ws     = map complement . word $ bmp
        offset = 8 * length ws - size bmp
        mask   = (`shiftL` offset) . (`shiftR` offset) $ 255
        ws'    = init ws
        w'     = last ws .&. mask

takeBits :: Int -> Bitmap -> Bitmap
takeBits n bmp | n >= s    = bmp
               | otherwise = fromIntExact (toInt bmp `shiftR` (s - n)) n
  where s = size bmp

dropBits :: Int -> Bitmap -> Bitmap
dropBits n bmp = fromIntExact (toInt bmp) (size bmp - n)

leadingZeros :: Bitmap -> Int
leadingZeros Bitmap { word = ws, size = s }
  | s <= 0           = 0
  | x == 0 && s >= 8 = (8 +) . leadingZeros $ bmp'
  | x == 0           = s
  | otherwise        = countLeadingZeros x
  where (x:xs) = ws
        bmp'   = Bitmap { word = xs, size = s - 8 }

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
rank0 n = rank1 n . inverse

clearLeadingOnes :: Int -> Word8 -> Word8
clearLeadingOnes n = (!! n) . iterate (\x -> x .&. mask x)
  where mask = (255 `shiftR`) . succ . countLeadingZeros

select1 :: Int -> Bitmap -> Maybe Int
select1 n bmp
  | s <= 0 || n <= 0 = Nothing
  | n <= pcount      = Just psel
  | otherwise        = (8 +) <$> select1 (n - pcount) bmp'
  where Bitmap { word = ws, size = s } = bmp
        (x:xs) = ws
        pcount = popCount x
        psel   = (selectVec V.!) . fromInteger . toInteger .
                 clearLeadingOnes (n - 1) $ x
        bmp'   = Bitmap { word = xs, size = s - 8 }

select0 :: Int -> Bitmap -> Maybe Int
select0 n = select1 n . inverse

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

selectVec :: V.Vector Int
selectVec = V.fromList [ 8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4
                       , 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
                       , 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
                       , 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
                       , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
                       , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
                       , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
                       , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
                       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                       ]
