{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import           System.Directory

import           Data.IpRouter
import           Data.PaCoPartition         (putPaCoMinHeight)
import           Data.Prefix

main :: IO ()
main = do
  pwd   <- getCurrentDirectory
  input <- C.readFile $ pwd ++ "/1.route"
  let (_ : _ : prefixLines) = C.lines input
      es                    = map (getEntry . C.words) prefixLines
  putPaCoMinHeight . mkTable $ es
    where getEntry (aBS : mBS : _ : _ : nBS : _) =
            Entry { network = mkPrefix addr mask
                  , nextHop = nHop
                  }
            where addr = read (C.unpack aBS) :: Address
                  mask = read (C.unpack mBS) :: Mask
                  nHop = read (C.unpack nBS) :: Int
