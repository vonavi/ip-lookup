{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             OverlappingInstances #-}

module Data.Dfuds
       (
         parent
       , degree
       , child
       ) where

import Data.Monoid

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

degree :: Dfuds a => Int -> a -> Int
degree n dfuds
  | isClose n ps = 0
  | otherwise    = nc - n
  where ps = toParens dfuds
        nc = selectClose (succ $ rankClose n ps) ps

child :: Dfuds a => Int -> Int -> a -> Maybe Int
child n i dfuds
  | i >= d    = Nothing
  | otherwise = do no <- findClose (n + d - i - 1) ps
                   Just $ succ no
  where ps = toParens dfuds
        d  = degree n dfuds
