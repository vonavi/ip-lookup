module Main where

import           Data.IpRouter
import           Data.OrdSst    (MhOrdSstT1, MhOrdSstT2, MhOrdSstT3, MhOrdSstT4)
import qualified Data.OrdSst    as OS
import           Data.PatSst    (MhPatSst, MhPatSstM, MsPatSst, MsPatSstM,
                                 PatSst)
import qualified Data.PatSst    as PS
import           Data.PatTree
import           RandomPrefixes

randomIpRouter :: IpRouter a => Int -> a
randomIpRouter n = mkTable $ randomEntries (32, 32) [1 .. n]

main :: IO ()
main = PS.putPatSst (randomIpRouter 1000000 :: MhPatSst)
