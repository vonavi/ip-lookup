module Main where

import Data.IpRouter
import RandomPrefixes
import Data.PatTree
import qualified Data.PatSst as PT
import Data.PatSst (MhPatSst)
import qualified Data.OrdSst as OT
import Data.OrdSst (MhOrdSstT1, MhOrdSstT2, MhOrdSstT3, MhOrdSstT4)

randomIpRouter :: IpRouter a => Int -> a
randomIpRouter n = mkTable $ randomEntries (32, 32) [1 .. n]

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
  putStrLn . (++) "  Height:          " . show . PT.height $ t
  putStrLn . (++) "  Number of pages: " . show . PT.numOfPages $ t
  putStrLn . (++) "  Fill size:       " . show . PT.fillSize $ t
    where n = 1000000
          t = randomIpRouter n :: MhPatSst


putMhOrdSst :: IO ()
putMhOrdSst = do
  putStrLn "Min-height SST for ordinal tree T1"
  putStrLn . (++) "  Height:          " . show . OT.height $ t1
  putStrLn . (++) "  Number of pages: " . show . OT.numOfPages $ t1
  putStrLn . (++) "  Fill size:       " . show . OT.fillSize $ t1

  putStrLn "Min-height SST for ordinal tree T2"
  putStrLn . (++) "  Height:          " . show . OT.height $ t2
  putStrLn . (++) "  Number of pages: " . show . OT.numOfPages $ t2
  putStrLn . (++) "  Fill size:       " . show . OT.fillSize $ t2

  putStrLn "Min-height SST for ordinal tree T3"
  putStrLn . (++) "  Height:          " . show . OT.height $ t3
  putStrLn . (++) "  Number of pages: " . show . OT.numOfPages $ t3
  putStrLn . (++) "  Fill size:       " . show . OT.fillSize $ t3

  putStrLn "Min-height SST for ordinal tree T4"
  putStrLn . (++) "  Height:          " . show . OT.height $ t4
  putStrLn . (++) "  Number of pages: " . show . OT.numOfPages $ t4
  putStrLn . (++) "  Fill size:       " . show . OT.fillSize $ t4
  where n  = 1000000
        t1 = randomIpRouter n :: MhOrdSstT1
        t2 = randomIpRouter n :: MhOrdSstT2
        t3 = randomIpRouter n :: MhOrdSstT3
        t4 = randomIpRouter n :: MhOrdSstT4

main :: IO ()
main = putMhPatSst
