{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import           System.Directory

import           Data.IpRouter
import qualified Data.PaCoPartitionM as PaCo
import           Data.PaCoTreeM

main :: IO ()
main = do
  pwd   <- getCurrentDirectory
  input <- readFile $ pwd ++ "/1.route"
  let (_ : _ : prefixLines) = lines input
      es                    = map (getEntry . words) prefixLines
  PaCo.putPrtn (mkTable es :: PaCo.MemTree PaCoZipper)
  where getEntry (aStr : mStr : _ : _ : nStr : _) =
          Entry { prefix  = read (aStr ++ ('/' : mStr)) :: Prefix
                , nextHop = read nStr :: Int
                }
