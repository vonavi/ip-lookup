module Main where

import Data.IpRouter
import RandomPrefixes
import Data.PatTree
import qualified Data.PatSst as PS
import Data.PatSst (MhPatSst)
import qualified Data.OrdSst as OS
import Data.OrdSst (MhOrdSstT1, MhOrdSstT2, MhOrdSstT3, MhOrdSstT4)

randomIpRouter :: IpRouter a => Int -> a
randomIpRouter n = mkTable $ randomEntries (32, 32) [1 .. n]

memUsage :: MhPatSst -> Int
memUsage = (*) PS.maxPageSize . PS.numOfPages

fillRatio :: MhPatSst -> Double
fillRatio t = fromIntegral (PS.fillSize t) / fromIntegral (memUsage t)

putPatTree :: IO ()
putPatTree = do
  putStrLn "PATRICIA tree"
  putStrLn . (++) "  Size with gamma code: " . show $ gammaSize t + 18 * n
  putStrLn . (++) "  Size with delta code: " . show $ deltaSize t + 18 * n
    where n = 1000000
          t = randomIpRouter n :: PatTree

putMhPatSst :: IO ()
putMhPatSst = do
  putStrLn "Min-height SST for PATRICIA tree"
  putStrLn . (++) "  Height:          " . show . PS.height $ t
  putStrLn . (++) "  Number of pages: " . show . PS.numOfPages $ t
  putStrLn . (++) "  Memory usage:    " . show . memUsage $ t
  putStrLn . (++) "  Fill size:       " . show . PS.fillSize $ t
  putStrLn . (++) "  Fill ratio:      " . show . fillRatio $ t
    where n = 1000000
          t = randomIpRouter n :: MhPatSst


putMhOrdSst :: IO ()
putMhOrdSst = do
  putStrLn "Min-height SST for ordinal tree T1"
  putStrLn . (++) "  Height:          " . show . OS.height $ t1
  putStrLn . (++) "  Number of pages: " . show . OS.numOfPages $ t1
  putStrLn . (++) "  Fill size:       " . show . OS.fillSize $ t1

  putStrLn "Min-height SST for ordinal tree T2"
  putStrLn . (++) "  Height:          " . show . OS.height $ t2
  putStrLn . (++) "  Number of pages: " . show . OS.numOfPages $ t2
  putStrLn . (++) "  Fill size:       " . show . OS.fillSize $ t2

  putStrLn "Min-height SST for ordinal tree T3"
  putStrLn . (++) "  Height:          " . show . OS.height $ t3
  putStrLn . (++) "  Number of pages: " . show . OS.numOfPages $ t3
  putStrLn . (++) "  Fill size:       " . show . OS.fillSize $ t3

  putStrLn "Min-height SST for ordinal tree T4"
  putStrLn . (++) "  Height:          " . show . OS.height $ t4
  putStrLn . (++) "  Number of pages: " . show . OS.numOfPages $ t4
  putStrLn . (++) "  Fill size:       " . show . OS.fillSize $ t4
  where n  = 1000000
        t1 = randomIpRouter n :: MhOrdSstT1
        t2 = randomIpRouter n :: MhOrdSstT2
        t3 = randomIpRouter n :: MhOrdSstT3
        t4 = randomIpRouter n :: MhOrdSstT4

main :: IO ()
main = putMhPatSst
