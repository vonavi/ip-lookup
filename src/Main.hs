{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BC

import           Data.IpRouter
import           Data.PaCoPartition         (showPaCoMinHeight)
import           Data.Prefix

main :: IO ()
main = BC.interact
       $ BC.pack . showPaCoMinHeight . mkTable
       . map (getEntry . BC.words) . BC.lines
  where getEntry (aBS : mBS : _ : nBS : _) =
          Entry { network = mkPrefix addr mask
                , nextHop = nHop
                }
          where addr = read (BC.unpack aBS) :: Address
                mask = read (BC.unpack mBS) :: Mask
                nHop = read (BC.unpack nBS) :: Int
