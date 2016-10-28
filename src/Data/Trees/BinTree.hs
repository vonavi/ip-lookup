{-# LANGUAGE FlexibleInstances #-}

module Data.Trees.BinTree
  (
    BinTree
  , BinZipper
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Bool           (bool)
import           Data.List           (unfoldr)
import           Data.Maybe          (isJust)
import           Data.Monoid

import           Data.IpRouter
import           Data.Prefix
import           Data.Zipper

data Tree a = Tip | Bin !a (Tree a) (Tree a) deriving (Eq, Show)
type BinTree = Tree (Maybe Int)

instance Foldable Tree where
  foldMap _ Tip         = mempty
  foldMap f (Bin x l r) = f x <> foldMap f l <> foldMap f r

instance Monoid BinTree where
  mempty = Tip

  Tip           `mappend` x             = x
  x             `mappend` Tip           = x
  (Bin x ll lr) `mappend` (Bin y rl rr) =
    Bin (x <|> y) (ll `mappend` rl) (lr `mappend` rr)

fromEntry :: Entry -> BinTree
fromEntry Entry { network = p, nextHop = n } =
  (`appEndo` Bin (Just n) Tip Tip)
  . foldMap (Endo . bool pushLeft pushRight) . unfoldr uncons $ p
  where pushLeft x  = Bin Nothing x Tip
        pushRight x = Bin Nothing Tip x

lookupState :: Prefix -> BinTree -> State (Maybe Int) ()
lookupState _ Tip         = return ()
lookupState v (Bin x l r) = do modify (x <|>)
                               case uncons v of
                                 Nothing      -> return ()
                                 Just (b, v') -> lookupState v' $ bool l r b

instance {-# OVERLAPPING #-} IpRouter BinTree where
  mkTable = foldr insEntry mempty

  insEntry = mappend . fromEntry

  delEntry e btree = btree `helper` fromEntry e
    where Tip           `helper` _             = Tip
          tree          `helper` Tip           = tree
          (Bin x lx rx) `helper` (Bin y ly ry) =
            delEmptyNode $ Bin z (lx `helper` ly) (rx `helper` ry)
            where delEmptyNode (Bin Nothing Tip Tip) = Tip
                  delEmptyNode t                     = t
                  z = if x == y then Nothing else x

  ipLookup a t = execState (lookupState a t) Nothing

  numOfPrefixes = getSum . foldMap (Sum . fromEnum . isJust)


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
binSize = (2 +) . getSum . foldMap (Sum . nodeSize)
  where nodeSize x = 3 + 18 * (fromEnum . isJust $ x)

instance Zipper BinZipper where
  goLeft (Bin x l r, es) = (l, Right (x, r) : es)
  goLeft _               = error "Tried to go left from a leaf"

  goRight (Bin x l r, es) = (r, Left (x, l) : es)
  goRight _               = error "Tried to go right from a leaf"

  goUp (l, Right (x, r) : es) = (Bin x l r, es)
  goUp (r, Left (x, l) : es)  = (Bin x l r, es)
  goUp _                      = error "Tried to go up from the top"

  isLeaf (Tip, _) = True
  isLeaf _        = False

  getLabel (Bin x _ _, _) = x
  getLabel _              = Nothing

  setLabel s (Bin _ l r, es) = (Bin s l r, es)
  setLabel _ z               = z

  size (t, _) = binSize t

  insert (t, _) (_, es) = (t, es)

  delete (_, es) = (Tip, es)
