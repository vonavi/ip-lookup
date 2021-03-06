module RandomEntries
  (
    randomEntries
  , genRandomEntries
  ) where

import           Data.Bits
import           Data.List     (unfoldr)
import           Data.Word
import           System.Random

import           Data.IpRouter
import           Data.Prefix

newtype Xorshift32 = Xorshift32 { getWord32 :: Word32 }

-- Iterates the random generator for 32 bits
step32 :: Xorshift32 -> Xorshift32
step32 (Xorshift32 a) = Xorshift32 d
  where b = a `xor` shiftL a 13
        c = b `xor` shiftR b 17
        d = c `xor` shiftL c 5

addrSeed :: Xorshift32
addrSeed = Xorshift32 1

maskSeed :: Int
maskSeed = 1

randomEntries :: (Int, Int) -> [Int] -> [Entry]
randomEntries (l, h) = zipWith3 helper addrList maskList
  where helper a m n = Entry { network = mkPrefix a m
                             , nextHop = n
                             }
        maskList     = randomRs (l, h) . mkStdGen $ maskSeed
        addrList     = map (ipv4Address . getWord32) $ unfoldr step addrSeed
          where step x = let y = step32 x in Just (y, y)

genRandomEntries :: Int -> [Entry]
genRandomEntries n = randomEntries (32, 32) [1 .. n]
