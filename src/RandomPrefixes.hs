module RandomPrefixes
       (
         randomEntries
       ) where

import           Data.Bits
import           Data.List
import           Data.Word
import           System.Random

import           Data.IpRouter

newtype Xorshift32 = Xorshift32 { getWord32 :: Word32 }

-- Iterates the random generator for 32 bits
step32 :: Xorshift32 -> Xorshift32
step32 (Xorshift32 a) = Xorshift32 d
  where b = a `xor` shiftL a 13
        c = b `xor` shiftR b 17
        d = c `xor` shiftL c  5

addrSeed :: Xorshift32
addrSeed = Xorshift32 1

maskSeed :: Int
maskSeed = 1

randomEntries :: (Int, Int) -> [Int] -> [Entry]
randomEntries (l, h) = zipWith3 helper addrList maskList
  where helper a m = Entry (Prefix a m)
        maskList   = map Mask . randomRs (l, h) . mkStdGen $ maskSeed
        addrList   = map (Address . getWord32) $ unfoldr step addrSeed
          where step x = let y = step32 x in Just (y, y)
