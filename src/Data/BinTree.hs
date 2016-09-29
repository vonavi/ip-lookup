{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.BinTree
  (
    BinTree
  , BinZipper
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Bits
import           Data.Bool           (bool)
import           Data.Function       (on)
import           Data.Maybe          (isJust)
import           Data.Monoid

import           Data.IpRouter
import           Data.Zipper

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


type BinZipper = (BinTree, [Either (Maybe Int, BinTree) (Maybe Int, BinTree)])

instance {-# OVERLAPPING #-} Show BinZipper where
  show (t, _) = show t

instance IpRouter BinZipper where
  mkTable es           = (mkTable es, [])
  insEntry e (t, _)    = (insEntry e t, [])
  delEntry e (t, _)    = (delEntry e t, [])
  ipLookup e (t, _)    = ipLookup e t
  numOfPrefixes (t, _) = numOfPrefixes t

{-|
The size of binary tree is built from the following parts:

    * balanced parentheses of additional ordinal-root (2 bits);

    * balanced-parentheses sequence (2 bits per inner node);

    * prefix bit (1 bit per inner node);

    * RE indexes (18 bits per prefix).
-}
binSize :: BinTree -> Int
binSize = (2 +) . getSum . foldMap (Sum . nodeSize) . getTree
  where nodeSize x = 3 + if isJust x then 18 else 0

instance Zipper BinZipper where
  goLeft (t, es) =
    case t of BinTree (Bin l x r) -> (BinTree l, Right (x, BinTree r) : es)
              _                   -> error "Tried to go left from a leaf"

  goRight (t, es) =
    case t of BinTree (Bin l x r) -> (BinTree r, Left (x, BinTree l) : es)
              _                   -> error "Tried to go right from a leaf"

  goUp (l, Right (x, r) : es) = (BinTree $ Bin (getTree l) x (getTree r), es)
  goUp (r, Left (x, l) : es)  = (BinTree $ Bin (getTree l) x (getTree r), es)
  goUp _                      = error "Tried to go up from the top"

  isLeaf (BinTree Tip, _) = True
  isLeaf _                = False

  getLabel (BinTree (Bin _ x _), _) = x
  getLabel _                        = Nothing

  setLabel s z@(t, es) =
    case t of BinTree (Bin l _ r) -> (BinTree (Bin l s r), es)
              _                   -> z

  size (t, _) = binSize t

  insert (t, _) (_, es) = (t, es)

  delete (_, es) = (BinTree Tip, es)
