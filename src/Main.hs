module Main where

import           System.Directory

import           Data.IpRouter
import           Data.PaCoPartition

main :: IO ()
main = do
  pwd   <- getCurrentDirectory
  input <- readFile $ pwd ++ "/1.route"
  let (_ : _ : prefixLines) = lines input
      es                    = map (getEntry . words) prefixLines
  putPaCoPrtn (mkTable es :: MhPaCoPrtn)
  where getEntry (aStr : mStr : _ : _ : nStr : _) = Entry (Prefix a m) n
          where a = strToAddr aStr
                m = strToMask $ '/' : mStr
                n = read nStr :: Int
