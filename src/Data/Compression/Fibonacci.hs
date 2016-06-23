module Data.Compression.Fibonacci
       (
         encodeFibonacci
       , decodeFibonacci
       ) where

import           Control.Arrow (second)
import           Data.List     (unfoldr)
import           Data.Monoid   ((<>))

import           Data.Bitmap

fibSeq :: [Int]
fibSeq = 1 : 2 : zipWith (+) fibSeq (tail fibSeq)

encodeFibonacci :: Int -> Bitmap
encodeFibonacci x | x <= 0    = error "Not positive number"
                  | otherwise = bmp <> fromInt 1
  where getMaxFib y = last . takeWhile (\(_, f) -> f <= y) $ zip [1 ..] fibSeq
        getOneBits y | y == 0    = Nothing
                     | otherwise = Just . second (y -) . getMaxFib $ y
        bs  = unfoldr getOneBits x
        bmp = mconcat . map (fromIntExact 1) .
              reverse $ zipWith (-) bs (tail bs ++ [0])

decodeFibonacci :: Bitmap -> Int
decodeFibonacci bmp = sum . map ((fibSeq !!) . pred) $ bs
  where getOneBits bmp' | size bmp' == 0 = Nothing
                        | otherwise      = let o = succ . leadingZeros $ bmp'
                                           in Just (o, dropBits o bmp')
        bs = scanr1 (+) . reverse . unfoldr getOneBits $
             takeBits (pred . size $ bmp) bmp
