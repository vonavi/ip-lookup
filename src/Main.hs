{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import           System.Directory

import           Data.IpRouter
import           Data.PaCoPartition         (showPaCoMinHeight)
import           Data.Prefix

main :: IO ()
main = do
  pwd          <- getCurrentDirectory
  (_ : _ : ls) <- C.lines <$> C.readFile (pwd ++ "/1.route")
  putStr . showPaCoMinHeight . mkTable . map (getEntry . C.words) $ ls
    where getEntry (aBS : mBS : _ : _ : nBS : _) =
            Entry { network = mkPrefix addr mask
                  , nextHop = nHop
                  }
            where addr = read (C.unpack aBS) :: Address
                  mask = read (C.unpack mBS) :: Mask
                  nHop = read (C.unpack nBS) :: Int
