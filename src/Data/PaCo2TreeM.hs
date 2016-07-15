{-# LANGUAGE FlexibleInstances #-}

module Data.PaCo2TreeM
  (
    Node(..)
  , Tree(..)
  , PaCo2Tree
  ) where

import           Control.Applicative     ((<|>))
import           Control.Monad.State
import           Data.Bits
import           Data.Maybe              (isJust)
import           Data.Monoid
import           Data.Word

import qualified Data.Compression.Bitmap as BMP
import           Data.Compression.Elias
import           Data.IpRouter
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
type PaCo2Tree = Tree Node

instance Foldable Tree where
  foldMap _ Tip         = mempty
  foldMap f (Bin x l r) = f x <> foldMap f l <> foldMap f r


isRootFull :: PaCo2Tree -> Bool
isRootFull (Bin _ Tip Tip) = True
isRootFull (Bin _ Tip _)   = False
isRootFull (Bin _ _ Tip)   = False
isRootFull _               = True

emptyBranch :: PaCo2Tree
emptyBranch = Bin emptyRoot Tip Tip
  where emptyRoot = Node { skip   = 0
                         , string = 0
                         , label  = Nothing
                         }

mkRootFull :: PaCo2Tree -> PaCo2Tree
mkRootFull t@(Bin _ Tip Tip) = t
mkRootFull (Bin x Tip r)     = Bin x emptyBranch r
mkRootFull (Bin x l Tip)     = Bin x l emptyBranch
mkRootFull t                 = t

joinNodes :: Node -> Bool -> Node -> Node
joinNodes xhead b xlast = Node { skip   = succ $ skip xhead + skip xlast
                               , string = str
                               , label  = label xlast
                               }
  where w      = succ $ skip xhead
        m      = complement $ (maxBound :: Word32) `shiftR` w
        shead  = string xhead .&. m
        slast  = string xlast `shiftR` w
        setter = if b then setBit else clearBit
        str    = (shead .|. slast) `setter` (32 - w)

uniteRoot :: PaCo2Tree -> PaCo2Tree
uniteRoot t@(Bin x _ _) | isJust (label x) = t
uniteRoot (Bin _ Tip Tip)                  = Tip
uniteRoot (Bin x Tip (Bin y l r))          = Bin (joinNodes x True y) l r
uniteRoot (Bin x (Bin y l r) Tip)          = Bin (joinNodes x False y) l r
uniteRoot t                                = t

resizeRoot :: Int -> PaCo2Tree -> PaCo2Tree
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

instance Monoid PaCo2Tree where
  mempty = Tip

  Tip `mappend` t  = t
  t  `mappend` Tip = t
  t1 `mappend` t2  = mkRootFull . uniteRoot $ t1 `helper` t2
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


fromEntry :: Entry -> PaCo2Tree
fromEntry (Entry p n) = Bin node Tip Tip
  where Prefix (Address a) (Mask m) = p
        node                        = Node { skip   = m
                                           , string = a
                                           , label  = Just n
                                           }

lookupState :: Address -> PaCo2Tree -> State (Maybe Int) ()
lookupState (Address a) = helper a
  where helper :: Word32 -> PaCo2Tree -> State (Maybe Int) ()
        helper v (Bin x l r) | k < kx    = return ()
                             | otherwise = do
                                 modify (label x <|>)
                                 helper (v `shiftL` (kx + 1)) $
                                   if v `testBit` (31 - kx) then r else l
          where kx = skip x
                k  = countLeadingZeros $ v `xor` string x
        helper _ Tip                     = return ()

delSubtree :: PaCo2Tree -> PaCo2Tree -> PaCo2Tree
Tip `delSubtree` _ = Tip
t `delSubtree` Tip = uniteRoot t
tx `delSubtree` ty = mkRootFull . uniteRoot $
                     Bin node (lx `delSubtree` ly) (rx `delSubtree` ry)
  where Bin x _ _    = tx
        Bin y _ _    = ty
        kmin         = minimum [ skip x
                               , skip y
                               , countLeadingZeros $
                                 string x `xor` string y
                               ]
        Bin x' lx rx = resizeRoot kmin tx
        Bin y' ly ry = resizeRoot kmin ty
        node         = x' { label = labelDiff (label x') (label y') }
        labelDiff :: Maybe Int -> Maybe Int -> Maybe Int
        labelDiff (Just sx) (Just sy) | sx == sy = Nothing
        labelDiff sx        _                    = sx

instance IpRouter PaCo2Tree where
  mkTable = foldr insEntry mempty

  insEntry = mappend . fromEntry

  delEntry = flip delSubtree . fromEntry

  ipLookup a t = execState (lookupState a t) Nothing

  numOfPrefixes = getSum . foldMap addPrefix
    where addPrefix x = if (isJust . label) x then Sum 1 else Sum 0


-- | The node size of path-compressed 2-tree is built from the
--   following parts: open/close parenthesis (1 bit), internal prefix
--   (1 bit), Elias gamma code of skip value (the skip value should be
--   increased by one), and node string.
nodeSizeFromSkip :: Int -> Int
nodeSizeFromSkip k = 2 + (BMP.size . encodeEliasGamma . succ $ k) + k

type NodeZipper = (PaCo2Tree, [Either (Node, PaCo2Tree) (Node, PaCo2Tree)])

instance Zipper NodeZipper where
  goLeft (Bin x l r, es) = (l, Right (x, r) : es)

  goRight (Bin x l r, es) = (r, Left (x, l) : es)

  goUp (l, Right (x, r) : es) = (Bin x l r, es)
  goUp (r, Left (x, l) : es)  = (Bin x l r, es)

  isRoot (_, []) = True
  isRoot _       = False

  isLeaf (Tip, _) = True
  isLeaf _        = False

  isNodeFull = isRootFull . fst

  getLabel (Bin x _ _, _) = label x
  getLabel (Tip, _)       = Nothing

  nodeSize (Bin Node { skip = k } _ _, _) = nodeSizeFromSkip k
  nodeSize (Tip, _)                       = 0

  delete (_, es) = (Tip, es)


root :: PaCo2Tree -> Maybe Int
root (Bin x _ _) | skip x == 0 = label x
root _                         = Nothing

leftChild :: PaCo2Tree -> PaCo2Tree
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

rightChild :: PaCo2Tree -> PaCo2Tree
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

merge :: Maybe Int -> PaCo2Tree -> PaCo2Tree -> PaCo2Tree
merge x l r = Bin xroot Tip Tip <> lsub <> rsub
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

type BitZipper =
  (PaCo2Tree, [Either (Maybe Int, PaCo2Tree) (Maybe Int, PaCo2Tree)])

instance Zipper BitZipper where
  goLeft (t, es) = (leftChild t, Right (root t, rightChild t) : es)

  goRight (t, es) = (rightChild t, Left (root t, leftChild t) : es)

  goUp (l, Right (x, r) : es) = (merge x l r, es)
  goUp (r, Left (x, l) : es)  = (merge x l r, es)

  isRoot (_, []) = True
  isRoot _       = False

  isLeaf (Tip, _) = True
  isLeaf _        = False

  isNodeFull = isRootFull . fst

  getLabel (Bin x _ _, _) | skip x == 0 = label x
  getLabel _                            = Nothing

  nodeSize (Bin Node { skip = k } _ _, _)
    | k == 0        = nodeSizeFromSkip k
    | otherwise     = nodeSizeFromSkip k - nodeSizeFromSkip (pred k)
  nodeSize (Tip, _) = 0

  delete (_, es) = (Tip, es)
