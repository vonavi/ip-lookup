module Main where

import           System.Directory

import           Data.IpRouter
import           Data.PaCo2Partition

main :: IO ()
main = do
  pwd   <- getCurrentDirectory
  input <- readFile $ pwd ++ "/1.route"
  let (_ : _ : prefixLines) = lines input
      es                    = map (getEntry . words) prefixLines
  putPaCo2Prtn (mkTable es :: Maybe Page)
  where getEntry (aStr : mStr : _ : _ : nStr : _) =
          Entry { prefix  = read (aStr ++ ('/' : mStr)) :: Prefix
                , nextHop = read nStr :: Int
                }
