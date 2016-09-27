{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import           System.Directory

import           Data.IpRouter
import           Data.PaCoTreeM
import           Data.PartitionM

main :: IO ()
main = do
  pwd   <- getCurrentDirectory
  input <- readFile $ pwd ++ "/1.route"
  let (_ : _ : prefixLines) = lines input
      es                    = map (getEntry . words) prefixLines
  putPrtn (mkTable es :: MemTree PaCoZipper)
  where getEntry (aStr : mStr : _ : _ : nStr : _) =
          Entry { prefix  = read (aStr ++ ('/' : mStr)) :: Prefix
                , nextHop = read nStr :: Int
                }
