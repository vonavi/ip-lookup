{-# LANGUAGE FlexibleInstances #-}

module Data.PaCo2TreeM
  (
  ) where

import           Control.Applicative ((<|>))
import           Data.Bits
import           Data.Maybe          (isJust)
import           Data.Monoid         ((<>))
import           Data.Word

import           Data.Zipper

data Node = Node { skip   :: Int
                 , string :: Word32
                 , label  :: Maybe Int
                 }
          deriving Show

instance Eq Node where
  x == y = kx == ky && n >= kx && label x == label y
    where kx = skip x
          ky = skip y
          n  = countLeadingZeros $ string x `xor` string y


data Tree a = Tip | Bin a (Tree a) (Tree a) deriving (Show, Eq)
type PCTree = Tree Node

emptyBranch :: PCTree
emptyBranch = Bin emptyRoot Tip Tip
  where emptyRoot = Node { skip   = 0
                         , string = 0
                         , label  = Nothing
                         }

balanceRoot :: PCTree -> PCTree
balanceRoot t@(Bin _ Tip Tip) = t
balanceRoot (Bin x Tip r)     = Bin x emptyBranch r
balanceRoot (Bin x l Tip)     = Bin x l emptyBranch
balanceRoot t                 = t

resizeRoot :: Int -> PCTree -> PCTree
resizeRoot k (Bin x l r) | k < kx = tree
  where kx    = skip x
        vx    = string x
        xhead = Node { skip   = k
                     , string = vx
                     , label  = Nothing
                     }
        xtail = Node { skip   = kx - k - 1
                     , string = vx `shiftL` (k + 1)
                     , label  = label x
                     }
        tree  = if vx `testBit` (31 - k)
                then Bin xhead Tip (Bin xtail l r)
                else Bin xhead (Bin xtail l r) Tip
resizeRoot _ tree = tree

instance Monoid PCTree where
  mempty = Tip

  Tip `mappend` t  = t
  t  `mappend` Tip = t
  t1 `mappend` t2  = balanceRoot $ t1 `helper` t2
    where tx `helper` ty = Bin node (lx <> ly) (rx <> ry)
            where Bin x _ _    = tx
                  Bin y _ _    = ty
                  kmin         = minimum [ skip x
                                         , skip y
                                         , countLeadingZeros $
                                           string x `xor` string y
                                         ]
                  Bin x' lx rx = resizeRoot kmin tx
                  Bin y' ly ry = resizeRoot kmin ty
                  node         = x' { label = label x' <|> label y' }


type NodeZipper = (PCTree, [Either (Node, PCTree) (Node, PCTree)])

instance Zipper NodeZipper where
  goLeft (Bin x l r, es) = Just (l, Right (x, r) : es)
  goLeft (Tip, _)        = Nothing

  goRight (Bin x l r, es) = Just (r, Left (x, l) : es)
  goRight (Tip, _)        = Nothing

  goUp (l, Right (x, r) : es) = Just (Bin x l r, es)
  goUp (r, Left (x, l) : es)  = Just (Bin x l r, es)
  goUp (_, [])                = Nothing


root :: PCTree -> Maybe Int
root (Bin x _ _) | skip x == 0 = label x
root _                         = Nothing

leftChild :: PCTree -> PCTree
leftChild Tip            = Tip
leftChild (Bin x l r)
  | k == 0               = l
  | v `testBit` 31       = Tip
  | otherwise            = Bin x' l r
  where k  = skip x
        v  = string x
        x' = x { skip   = pred k
               , string = v `shiftL` 1
               }

rightChild :: PCTree -> PCTree
rightChild Tip            = Tip
rightChild (Bin x l r)
  | k == 0                = r
  | v `testBit` 31        = Bin x' l r
  | otherwise             = Tip
  where k  = skip x
        v  = string x
        x' = x { skip   = pred k
               , string = v `shiftL` 1
               }

merge :: Maybe Int -> PCTree -> PCTree -> PCTree
merge x l r | isJust x  = Bin xroot Tip Tip <> lsub <> rsub
            | otherwise = lsub <> rsub
  where xroot = Node { skip   = 0
                     , string = 0
                     , label  = x
                     }
        lsub  = case l of
                  Tip          -> Tip
                  Bin xl ll rl ->
                    let xl' = xl { skip   = succ $ skip xl
                                 , string = (`clearBit` 31) . (`shiftR` 1) $
                                            string xl
                                 }
                    in Bin xl' ll rl
        rsub  = case r of
                  Tip          -> Tip
                  Bin xr lr rr ->
                    let xr' = xr { skip   = succ $ skip xr
                                 , string = (`setBit` 31) . (`shiftR` 1) $
                                            string xr
                                 }
                    in Bin xr' lr rr

type BitZipper = (PCTree, [Either (Maybe Int, PCTree) (Maybe Int, PCTree)])

instance Zipper BitZipper where
  goLeft (t, es) = case l of
                     Tip -> Nothing
                     _   -> Just (l, Right (root t, r) : es)
    where l = leftChild t
          r = rightChild t

  goRight (t, es) = case r of
                      Tip -> Nothing
                      _   -> Just (r, Left (root t, l) : es)
    where l = leftChild t
          r = rightChild t

  goUp (l, Right (x, r) : es) = Just (merge x l r, es)
  goUp (r, Left (x, l) : es)  = Just (merge x l r, es)
  goUp (_, [])                = Nothing
