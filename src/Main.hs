module Main where

import           System.Directory

import           Data.IpRouter
import           Data.PaCo2PartitionM

main :: IO ()
main = do
  pwd   <- getCurrentDirectory
  input <- readFile $ pwd ++ "/1.route"
  let (_ : _ : prefixLines) = lines input
      es                    = map (getEntry . words) prefixLines
  putPaCo2Prtn (mkTable es :: Maybe Page)
  where getEntry (aStr : mStr : _ : _ : nStr : _) = Entry (Prefix a m) n
          where a = strToAddr aStr
                m = strToMask $ '/' : mStr
                n = read nStr :: Int
