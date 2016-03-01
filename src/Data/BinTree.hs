module Data.BinTree
       (
         BinTree
       ) where

import Data.Maybe (isJust, fromJust)
import Data.Monoid
import Control.Monad.State

import Data.IpRouter

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving Show

instance Monoid a => Monoid (Tree a) where
  mempty = Tip
  Tip `mappend` x = x
  x `mappend` Tip = x
  (Bin ll x lr) `mappend` (Bin rl y rr) =
    Bin (ll `mappend` rl) (x `mappend` y) (lr `mappend` rr)

newtype BinTree = BinTree { getTree :: Tree (Last Int) } deriving Show

instance Monoid BinTree where
  mempty        = BinTree mempty
  x `mappend` y = BinTree $ getTree x `mappend` getTree y

fromEntry :: Entry -> BinTree
fromEntry (Entry p n) = BinTree $ helper . prefixBits $ p
  where helper :: [Bool] -> Tree (Last Int)
        helper []     = Bin Tip (Last (Just n)) Tip
        helper (b:bs) = if b
                        then Bin Tip (Last Nothing) (helper bs)
                        else Bin (helper bs) (Last Nothing) Tip

lookupState :: [Bool] -> Tree (Last Int) -> State (Last Int) ()
lookupState _      Tip         = return ()
lookupState []     (Bin _ x _) = modify (`mappend` x)
lookupState (b:bs) (Bin l x r) = do modify (`mappend` x)
                                    lookupState bs $ if b then r else l

instance {-# OVERLAPPING #-} IpRouter BinTree where
  mkTable = foldr (mappend . fromEntry) mempty

  insEntry e t = t `mappend` fromEntry e

  delEntry (Entry p n) (BinTree bt) = BinTree $ helper (prefixBits p) bt
    where helper _      Tip               = Tip
          helper []     t@(Bin Tip x Tip) = if (fromJust . getLast $ x) == n
                                            then Tip
                                            else t
          helper []     t@(Bin l x r)     = case getLast x of
                                             Nothing -> t
                                             Just n' ->
                                               if n' == n
                                               then Bin l (Last Nothing) r
                                               else t
          helper (b:bs) (Bin l x r)       = if b
                                            then Bin l x (helper bs r)
                                            else Bin (helper bs l) x r

  ipLookup a (BinTree t) =
    getLast $ execState (lookupState (addrBits a) t) (Last Nothing)

  numOfPrefixes (BinTree t) = execState (helperS t) 0
    where helperS :: Tree (Last Int) -> State Int ()
          helperS Tip         = return ()
          helperS (Bin l x r) = do
            helperS l
            helperS r
            when (isJust . getLast $ x) $ modify succ
