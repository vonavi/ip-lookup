{-# LANGUAGE FlexibleInstances #-}

module Data.Trees.BinTree
  (
    BinTree
  , BinZipper
  , showBinTree
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Bool           (bool)
import           Data.Function       (on)
import           Data.List           (unfoldr)
import           Data.Maybe          (isJust)
import           Data.Monoid

import           Config
import           Data.IpRouter
import           Data.Prefix
import           Data.Zipper

data Tree a = Tip
            | Bin { label :: a
                  , left  :: Tree a
                  , right :: Tree a
                  }
            deriving (Eq, Show)
type BinTree = Tree (Maybe Int)

instance Foldable Tree where
  foldMap _ Tip         = mempty
  foldMap f (Bin x l r) = f x <> foldMap f l <> foldMap f r

instance Monoid BinTree where
  mempty = Tip

  Tip `mappend` t   = t
  t   `mappend` Tip = t
  tx  `mappend` ty  = Bin { label = ((<|>) `on` label) tx ty
                          , left  = ((<>)  `on` left)  tx ty
                          , right = ((<>)  `on` right) tx ty
                          }

fromEntry :: Entry -> BinTree
fromEntry Entry { network = p, nextHop = n } =
  (`appEndo` Bin (Just n) Tip Tip)
  . foldMap (Endo . bool pushLeft pushRight) . unfoldr uncons $ p
  where pushLeft x  = Bin Nothing x Tip
        pushRight x = Bin Nothing Tip x

isTip :: Tree a -> Bool
isTip Tip = True
isTip _   = False

instance {-# OVERLAPPING #-} IpRouter BinTree where
  mkTable = foldMap fromEntry

  insEntry = (<>) . fromEntry

  delEntry = flip helper . fromEntry
    where Tip `helper` _   = Tip
          t   `helper` Tip = t
          tx  `helper` ty  = delEmptyNode
                             Bin { label = (unlabel `on` label) tx ty
                                 , left  = (helper  `on` left)  tx ty
                                 , right = (helper  `on` right) tx ty
                                 }
            where delEmptyNode (Bin Nothing Tip Tip) = Tip
                  delEmptyNode t                     = t
                  unlabel x y = if x == y then Nothing else x

  ipLookup v t = getLast . foldMap (Last . label)
                 . takeWhile (not . isTip) . (t :) . (`evalState` t)
                 . mapM (act . bool left right) . unfoldr uncons $ v
    where act f = state $ \s -> let s' = f s in (s', s')

  numOfPrefixes = getSum . foldMap (Sum . fromEnum . isJust)


{-|
The size of binary tree is built from the following parts:

    * balanced parentheses of additional ordinal-root (2 bits);

    * balanced-parentheses sequence (2 bits per inner node);

    * prefix bit (1 bit per inner node);

    * next-hop size per prefix.
-}
binSize :: BinTree -> Int
binSize = (2 +) . getSum . foldMap (Sum . nodeSize)
  where nodeSize x = 3 + nextHopSize config * (fromEnum . isJust $ x)

showBinTree :: BinTree -> String
showBinTree t = "Size of binary tree " ++ show (binSize t) ++ "\n"


type BinZipper = (BinTree, [Either (Maybe Int, BinTree) (Maybe Int, BinTree)])

instance {-# OVERLAPPING #-} Show BinZipper where
  show (t, _) = show t

instance IpRouter BinZipper where
  mkTable es           = (mkTable es, [])
  insEntry e (t, _)    = (insEntry e t, [])
  delEntry e (t, _)    = (delEntry e t, [])
  ipLookup e (t, _)    = ipLookup e t
  numOfPrefixes (t, _) = numOfPrefixes t

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
