{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.BinTree
  (
    BinTree
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Bits
import           Data.Bool           (bool)
import           Data.Function       (on)
import           Data.Maybe          (isJust)
import           Data.Monoid

import           Data.IpRouter

data Tree a = Tip | Bin (Tree a) !a (Tree a) deriving (Eq, Show)

instance Foldable Tree where
  foldMap _ Tip         = mempty
  foldMap f (Bin l x r) = foldMap f l <> f x <> foldMap f r

instance Monoid (Tree (Maybe Int)) where
  mempty = Tip

  Tip           `mappend` x             = x
  x             `mappend` Tip           = x
  (Bin ll x lr) `mappend` (Bin rl y rr) =
    Bin (ll `mappend` rl) (x <|> y) (lr `mappend` rr)

newtype BinTree = BinTree { getTree :: Tree (Maybe Int) }
                deriving (Eq, Show, Monoid)


fromEntry :: Entry -> BinTree
fromEntry (Entry p n) = BinTree $ helper 31 (Bin Tip (Just n) Tip)
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then Bin Tip Nothing y
                            else Bin y Nothing Tip
            where y = helper (pred i) x

lookupState :: Address -> Tree (Maybe Int) -> State (Maybe Int) ()
lookupState (Address a) = helper 31
  where helper _ Tip         = return ()
        helper n (Bin l x r) = do
          modify (x <|>)
          helper (pred n) $ if a `testBit` n then r else l

instance {-# OVERLAPPING #-} IpRouter BinTree where
  mkTable = foldr insEntry mempty

  insEntry = mappend . fromEntry

  delEntry e btree = BinTree $ (helper `on` getTree) btree (fromEntry e)
    where Tip           `helper` _             = Tip
          tree          `helper` Tip           = tree
          (Bin lx x rx) `helper` (Bin ly y ry) =
            delEmptyNode $ Bin (lx `helper` ly) z (rx `helper` ry)
            where delEmptyNode (Bin Tip Nothing Tip) = Tip
                  delEmptyNode t                     = t
                  z = if x == y then Nothing else x

  ipLookup a (BinTree t) = execState (lookupState a t) Nothing

  numOfPrefixes = getSum . foldMap (Sum . bool 0 1 . isJust) . getTree
