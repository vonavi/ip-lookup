{-# LANGUAGE FlexibleInstances #-}

module Data.BinTree
       (
         BinTree
       ) where

import Data.Monoid
import Data.Bits
import Data.Maybe (isJust)
import Control.Applicative ((<|>))
import Control.Monad.State

import Data.IpRouter

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


newtype BinTree = BinTree { getTree :: Tree (Maybe Int) } deriving (Eq, Show)

instance Monoid BinTree where
  mempty        = BinTree mempty
  x `mappend` y = BinTree $ getTree x `mappend` getTree y


fromEntry :: Entry -> BinTree
fromEntry (Entry p n) = BinTree $ helper 31 (Bin Tip (Just n) Tip)
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then Bin Tip Nothing y
                            else Bin y Nothing Tip
            where y = helper (pred i) x

collapse :: BinTree -> BinTree
collapse = BinTree . helper . getTree
  where helper Tip         = Tip
        helper (Bin l x r) = reduce $ Bin (helper l) x (helper r)
          where reduce (Bin Tip Nothing Tip) = Tip
                reduce t                     = t

lookupState :: Address -> Tree (Maybe Int) -> State (Maybe Int) ()
lookupState (Address a) = helper 31
  where helper _ Tip         = return ()
        helper n (Bin l x r) = do
          modify (x <|>)
          helper (pred n) $ if a `testBit` n then r else l

instance {-# OVERLAPPING #-} IpRouter BinTree where
  mkTable = foldr (mappend . fromEntry) mempty

  insEntry e t = t `mappend` fromEntry e

  delEntry e (BinTree a) = collapse . BinTree $ helper a b
    where BinTree b = fromEntry e
          helper Tip           _             = Tip
          helper t             Tip           = t
          helper (Bin lx x rx) (Bin ly y ry) =
            Bin (helper lx ly) z (helper rx ry)
            where z = if x == y then Nothing else x

  ipLookup a (BinTree t) = execState (lookupState a t) Nothing

  numOfPrefixes = getSum . foldMap isPrefix . getTree
    where isPrefix x = if isJust x then Sum 1 else Sum 0
