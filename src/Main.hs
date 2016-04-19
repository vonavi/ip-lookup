module Main where

import Data.IpRouter
import RandomPrefixes
import Data.PatTree
import qualified Data.PatSst as PS
import Data.PatSst (PatSst, MhPatSst, MhPatSstM, MsPatSst, MsPatSstM)
import qualified Data.OrdSst as OS
import Data.OrdSst (MhOrdSstT1, MhOrdSstT2, MhOrdSstT3, MhOrdSstT4)

randomIpRouter :: IpRouter a => Int -> a
randomIpRouter n = mkTable $ randomEntries (32, 32) [1 .. n]

main :: IO ()
main = PS.putPatSst (randomIpRouter 1000000 :: MhPatSst)
