{-# LANGUAGE FlexibleInstances #-}

module Data.BinTree
       (
         BinTree
       ) where

import Data.Maybe (isJust)
import Control.Applicative ((<|>))
import Control.Monad.State

import Data.IpRouter

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Eq, Show)

newtype BinTree = BinTree { getTree :: Tree (Maybe Int) } deriving (Eq, Show)

instance Monoid (Tree (Maybe Int)) where
  mempty = Tip

  Tip `mappend` x = x
  x `mappend` Tip = x
  (Bin ll x lr) `mappend` (Bin rl y rr) =
    Bin (ll `mappend` rl) (x <|> y) (lr `mappend` rr)

instance Monoid BinTree where
  mempty        = BinTree mempty
  x `mappend` y = BinTree $ getTree x `mappend` getTree y

fromEntry :: Entry -> BinTree
fromEntry (Entry p n) = BinTree $ helper . prefixBits $ p
  where helper :: [Bool] -> Tree (Maybe Int)
        helper []     = Bin Tip (Just n) Tip
        helper (b:bs) = if b
                        then Bin Tip Nothing (helper bs)
                        else Bin (helper bs) Nothing Tip

lookupState :: [Bool] -> Tree (Maybe Int) -> State (Maybe Int) ()
lookupState _      Tip         = return ()
lookupState []     (Bin _ x _) = modify (x <|>)
lookupState (b:bs) (Bin l x r) = do modify (x <|>)
                                    lookupState bs $ if b then r else l

instance {-# OVERLAPPING #-} IpRouter BinTree where
  mkTable = foldr (mappend . fromEntry) mempty

  insEntry e t = t `mappend` fromEntry e

  delEntry e (BinTree a) = let BinTree b = fromEntry e
                           in BinTree $ helper a b
    where helper Tip           _             = Tip
          helper t             Tip           = t
          helper (Bin lx x rx) (Bin ly y ry) =
            collapse $ Bin (helper lx ly) z (helper rx ry)
            where z = if x == y then Nothing else x
                  collapse (Bin Tip Nothing Tip) = Tip
                  collapse t                     = t

  ipLookup a (BinTree t) = execState (lookupState (addrBits a) t) Nothing

  numOfPrefixes (BinTree t) = execState (helperS t) 0
    where helperS :: Tree (Maybe Int) -> State Int ()
          helperS Tip         = return ()
          helperS (Bin l x r) = do helperS l
                                   helperS r
                                   when (isJust x) $ modify succ
