{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import           System.Directory

import           Data.IpRouter
import           Data.PaCoPartition (putPaCoMinHeight)
import           Data.Prefix

main :: IO ()
main = do
  pwd   <- getCurrentDirectory
  input <- readFile $ pwd ++ "/1.route"
  let (_ : _ : prefixLines) = lines input
      es                    = map (getEntry . words) prefixLines
  putPaCoMinHeight . mkTable $ es
  where getEntry (aStr : mStr : _ : _ : nStr : _) =
          Entry { network = mkPrefix (read aStr :: Address) (read mStr :: Mask)
                , nextHop = read nStr :: Int
                }
