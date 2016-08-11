{-# LANGUAGE FlexibleInstances #-}

module Data.PaCoTreeM
  (
    Node(..)
  , Tree(..)
  , PaCoTree
  , PaCoZipper
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
type PaCoTree = Tree Node

instance Foldable Tree where
  foldMap _ Tip         = mempty
  foldMap f (Bin x l r) = f x <> foldMap f l <> foldMap f r


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

uniteRoot :: PaCoTree -> PaCoTree
uniteRoot t@(Bin x _ _) | isJust (label x) = t
uniteRoot (Bin x Tip (Bin y l r))          = Bin (joinNodes x True y) l r
uniteRoot (Bin x (Bin y l r) Tip)          = Bin (joinNodes x False y) l r
uniteRoot t                                = t

resizeRoot :: Int -> PaCoTree -> PaCoTree
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

instance Monoid PaCoTree where
  mempty = Tip

  Tip `mappend` t  = t
  t  `mappend` Tip = t
  t1 `mappend` t2  = uniteRoot $ t1 `helper` t2
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


fromEntry :: Entry -> PaCoTree
fromEntry (Entry p n) = Bin node Tip Tip
  where Prefix (Address a) (Mask m) = p
        node                        = Node { skip   = m
                                           , string = a
                                           , label  = Just n
                                           }

lookupState :: Address -> PaCoTree -> State (Maybe Int) ()
lookupState (Address a) = helper a
  where helper :: Word32 -> PaCoTree -> State (Maybe Int) ()
        helper v (Bin x l r) | k < kx    = return ()
                             | otherwise = do
                                 modify (label x <|>)
                                 helper (v `shiftL` (kx + 1)) $
                                   if v `testBit` (31 - kx) then r else l
          where kx = skip x
                k  = countLeadingZeros $ v `xor` string x
        helper _ Tip                     = return ()

delEmptyNode :: PaCoTree -> PaCoTree
delEmptyNode t | Bin x Tip Tip <- t
               , Nothing <- label x = Tip
               | otherwise          = t

delSubtree :: PaCoTree -> PaCoTree -> PaCoTree
Tip `delSubtree` _ = Tip
t `delSubtree` Tip = uniteRoot t
tx `delSubtree` ty = delEmptyNode . uniteRoot $
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

instance IpRouter PaCoTree where
  mkTable = foldr insEntry mempty

  insEntry = mappend . fromEntry

  delEntry = flip delSubtree . fromEntry

  ipLookup a t = execState (lookupState a t) Nothing

  numOfPrefixes = getSum . foldMap addPrefix
    where addPrefix x = if (isJust . label) x then Sum 1 else Sum 0


{-|
The node size of path-compressed 2-tree is built from the following
parts:

    * parenthesis expression (2 bits);

    * Elias gamma code of skip value (the skip value should be
      increased by one);

    * node string;

    * prefix bit (1 bit);

    * RE index (18 bits) if the prefix bit is set.
-}
nodeSize :: Node -> Int
nodeSize Node { skip = k, label = s } =
  2 + (BMP.size . encodeEliasGamma . succ $ k) +
  k + 1 + if isJust s then 18 else 0


type PaCoZipper = (,,) PaCoTree
                  [Either (Node, PaCoTree) (Node, PaCoTree)]
                  [Bool]

instance IpRouter PaCoZipper where
  mkTable es              = (mkTable es, [], [])
  insEntry e (t, _, _)    = (insEntry e t, [], [])
  delEntry e (t, _, _)    = (delEntry e t, [], [])
  ipLookup e (t, _, _)    = ipLookup e t
  numOfPrefixes (t, _, _) = numOfPrefixes t

instance Zipper PaCoZipper where
  goLeft (t, es, bs) = case resizeRoot 0 t of
                         Bin x l r -> (l, Right (x, r) : es, True : bs)
                         Tip       -> error "Tried to go left from a leaf"

  goRight (t, es, bs) = case resizeRoot 0 t of
                          Bin x l r -> (r, Left (x, l) : es, True : bs)
                          Tip       -> error "Tried to go right from a leaf"

  goUp z =
    case z of
      (l, Right (x, r) : es, b : bs) -> (mbUnite b $ Bin x l r, es, bs)
      (r, Left (x, l) : es, b : bs)  -> (mbUnite b $ Bin x l r, es, bs)
      (_, [], _)                     -> error "Tried to go up from the top"
    where mbUnite b = if b then uniteRoot else id

  isLeaf (Tip, _, _) = True
  isLeaf _           = False

  getLabel (Bin x _ _, _, _) | skip x == 0 = label x
  getLabel _                               = Nothing

  setLabel s (t@(Bin x l r), es, bs)
    | skip x == 0 = (Bin x { label = s } l r, es, bs)
    | isJust s    = (Bin x' { label = s } l' r', es, bs)
    where Bin x' l' r' = resizeRoot 0 t
  setLabel _ z    = z

  size (t, _, _) = getSum . foldMap (Sum . nodeSize) $ t

  insert (t, _, _) (_, es, _ : bs) = (t, es, True : bs)
  insert (t, _, _) (_, es, [])     = (t, es, [])

  delete (_, es, _ : bs) = (Tip, es, False : bs)
  delete (_, es, [])     = (Tip, es, [])
