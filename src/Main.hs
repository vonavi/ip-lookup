module Main where

import Data.IpRouter
import RandomPrefixes
import Data.OrdSst

randomIpRouter :: IpRouter a => Int -> a
randomIpRouter n
  | n == 0    = mkTable []
  | otherwise = mkTable entries
  where zeroEntry = Entry (Prefix (Address 0) (Mask 0)) 0
        entries   = zeroEntry : randomEntries (32, 32) [1 .. pred n]

main :: IO ()
main = do
  putStrLn "Min-height SST for ordinal tree T1"
  putStrLn . (++) "  Height:          " . show . height $ t1
  putStrLn . (++) "  Number of pages: " . show . numOfPages $ t1
  putStrLn . (++) "  Fill size:       " . show . fillSize $ t1

  putStrLn "Min-height SST for ordinal tree T2"
  putStrLn . (++) "  Height:          " . show . height $ t2
  putStrLn . (++) "  Number of pages: " . show . numOfPages $ t2
  putStrLn . (++) "  Fill size:       " . show . fillSize $ t2

  putStrLn "Min-height SST for ordinal tree T3"
  putStrLn . (++) "  Height:          " . show . height $ t3
  putStrLn . (++) "  Number of pages: " . show . numOfPages $ t3
  putStrLn . (++) "  Fill size:       " . show . fillSize $ t3

  putStrLn "Min-height SST for ordinal tree T4"
  putStrLn . (++) "  Height:          " . show . height $ t4
  putStrLn . (++) "  Number of pages: " . show . numOfPages $ t4
  putStrLn . (++) "  Fill size:       " . show . fillSize $ t4
  where n  = 1000000
        t1 = randomIpRouter n :: MhOrdSstT1
        t2 = randomIpRouter n :: MhOrdSstT2
        t3 = randomIpRouter n :: MhOrdSstT3
        t4 = randomIpRouter n :: MhOrdSstT4
