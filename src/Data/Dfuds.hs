{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             OverlappingInstances #-}

module Data.Dfuds
       (
         parent
       , firstChild
       , lastChild
       , nextSibling
       , prevSibling
       , subtreeSize
       , degree
       , child
       ) where

import Data.Monoid
import Data.Maybe (fromMaybe)
import Control.Applicative ((<*>))

import Data.Paren
import Data.OrdTree

class Dfuds a where
  getList :: a -> [(Last Int, [Paren])]

instance Dfuds a => Show a where
  show = concatMap helper . getList
    where helper (x, l) = show (getLast x, l) ++ " "

toParens :: Dfuds a => a -> [Paren]
toParens = concatMap snd . getList

parent :: Dfuds a => Int -> a -> Maybe Int
parent n dfuds = do no <- findOpen (pred n) ps
                    Just . succ $ selectClose (rankClose no ps) ps
  where ps = toParens dfuds

firstChild :: Dfuds a => Int -> a -> Maybe Int
firstChild n = child n 0

lastChild :: Dfuds a => Int -> a -> Maybe Int
lastChild n dfuds = Just succ <*> findClose n (toParens dfuds)

nextSibling :: Dfuds a => Int -> a -> Maybe Int
nextSibling n dfuds = do no <- findOpen (pred n) ps
                         Just succ <*> findClose (pred no) ps
  where ps = toParens dfuds

prevSibling :: Dfuds a => Int -> a -> Maybe Int
prevSibling n dfuds = do no <- findOpen (pred n) ps
                         Just succ <*> findClose (succ no) ps
  where ps = toParens dfuds

subtreeSize :: Dfuds a => Int -> a -> Int
subtreeSize n dfuds
  | n < 0 || n >= length ps = 0
  | isClose n ps            = 1
  | otherwise               = succ $ (nc - n) `div` 2
  where ps = toParens dfuds
        nc = fromMaybe (length ps) $ do ne <- enclose n ps
                                        findClose ne ps

degree :: Dfuds a => Int -> a -> Int
degree n dfuds
  | not (isOpen n ps) = 0
  | otherwise         = nc - n
  where ps = toParens dfuds
        nc = selectClose (succ $ rankClose n ps) ps

child :: Dfuds a => Int -> Int -> a -> Maybe Int
child n i dfuds
  | i >= d    = Nothing
  | otherwise = do no <- findClose (n + d - i - 1) ps
                   Just $ succ no
  where ps = toParens dfuds
        d  = degree n dfuds
