{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Implements a path-compressed tree.
module Data.Trees.PaCoTree
  (
    Node(..)
  , Tree(..)
  , PaCoTree
  , PaCoZipper
  , eliasGammaSize
  , eliasDeltaSize
  , eliasFanoSize
  , showPaCoTree
  ) where

import           Control.Applicative        ((<|>))
import           Control.Monad.State
import           Data.Bool                  (bool)
import           Data.Maybe                 (isJust)
import           Data.Monoid

import           Config
import qualified Data.Compression.Bitmap    as BMP
import           Data.Compression.Elias
import           Data.Compression.Fibonacci
import           Data.IpRouter
import qualified Data.Prefix                as P
import           Data.Zipper

-- | Node of path-compressed tree
data Node = Node { prefix :: P.Prefix  -- ^ Skipped bits
                 , label  :: Maybe Int -- ^ may or may not contain
                                       --   'Data.IpRouter.nextHop'
                 }
          deriving (Show, Eq)
-- | Structure of path-compressed tree
data Tree a = Tip
            | Bin a (Tree a) (Tree a)
            deriving (Show, Eq, Foldable)
-- | Path-compressed tree
type PaCoTree = Tree Node


-- | Joins bit strings of two consequent nodes.
joinNodes :: Node -> Bool -> Node -> Node
joinNodes Node { prefix = px } b y
  = Node { prefix = (px `P.append`) . P.cons b . prefix $ y
         , label  = label y
         }

-- | Unites the root with a child node if possible.
uniteRoot :: PaCoTree -> PaCoTree
uniteRoot t@(Bin x _ _) | isJust (label x) = t
uniteRoot (Bin x Tip (Bin y l r))          = Bin (joinNodes x True y) l r
uniteRoot (Bin x (Bin y l r) Tip)          = Bin (joinNodes x False y) l r
uniteRoot t                                = t

-- | Reduces the skip value of the root down to the given value.
resizeRoot :: Int -> PaCoTree -> PaCoTree
resizeRoot _ Tip           = Tip
resizeRoot k t@(Bin x l r) = case P.uncons pb of
                               Nothing         -> t
                               Just (True, p)  -> Bin xa Tip (subtree p)
                               Just (False, p) -> Bin xa (subtree p) Tip
  where (pa, pb)  = P.splitAt k . prefix $ x
        xa        = Node { prefix = pa
                         , label  = Nothing
                         }
        subtree p = Bin x { prefix = p } l r

instance Monoid PaCoTree where
  mempty = Tip

  Tip `mappend` t  = t
  t  `mappend` Tip = t
  t1 `mappend` t2  = uniteRoot $ t1 `helper` t2
    where tx `helper` ty = Bin node (lx <> ly) (rx <> ry)
            where Bin x _ _    = tx
                  Bin y _ _    = ty
                  (p, _, _)    = P.commonPrefixes (prefix x) (prefix y)
                  kmin         = P.maskLength p
                  Bin x' lx rx = resizeRoot kmin tx
                  Bin y' ly ry = resizeRoot kmin ty
                  node         = x' { label = label x' <|> label y' }


-- | Builds a path-compressed tree from routing entry.
fromEntry :: Entry -> PaCoTree
fromEntry Entry { network = p, nextHop = n } =
  Bin Node { prefix = p, label = Just n } Tip Tip

-- | Makes the longest-prefix match (LPM).
lookupState :: P.Prefix -> PaCoTree -> State (Maybe Int) ()
lookupState _ Tip         = return ()
lookupState v (Bin x l r)
  | (not . P.null) pa     = return ()
  | otherwise             = do modify (label x <|>)
                               case P.uncons va of
                                 Nothing      -> return ()
                                 Just (b, vb) -> lookupState vb $ bool l r b
  where (_, va, pa) = P.commonPrefixes v (prefix x)

-- | Deletes the empty root.
delEmptyNode :: PaCoTree -> PaCoTree
delEmptyNode t | Bin x Tip Tip <- t
               , Nothing <- label x = Tip
               | otherwise          = t

-- | Deletes one path-compressed tree from another.
delSubtree :: PaCoTree -> PaCoTree -> PaCoTree
Tip `delSubtree` _ = Tip
t `delSubtree` Tip = uniteRoot t
tx `delSubtree` ty = delEmptyNode . uniteRoot $
                     Bin node (lx `delSubtree` ly) (rx `delSubtree` ry)
  where Bin x _ _    = tx
        Bin y _ _    = ty
        (p, _, _)    = P.commonPrefixes (prefix x) (prefix y)
        kmin         = P.maskLength p
        Bin x' lx rx = resizeRoot kmin tx
        Bin y' ly ry = resizeRoot kmin ty
        node         = x' { label = labelDiff (label x') (label y') }
        labelDiff :: Maybe Int -> Maybe Int -> Maybe Int
        labelDiff (Just sx) (Just sy) | sx == sy = Nothing
        labelDiff sx        _                    = sx

instance IpRouter PaCoTree where
  mkTable       = foldMap fromEntry
  insEntry      = (<>) . fromEntry
  delEntry      = flip delSubtree . fromEntry
  ipLookup a t  = execState (lookupState a t) Nothing
  numOfPrefixes = getSum . foldMap (Sum . fromEnum . isJust . label)


{-|
The size of path-compressed tree is built from the following parts:

    * balanced parentheses of additional ordinal-tree root (2 bits);

    * balanced-parentheses sequence (2 bits per node);

    * Elias gamma codes of skip values of the nodes (the skip value
      should be increased by one);

    * node strings of the nodes;

    * prefix bits (1 bit per node);

    * next-hop size per prefix.
-}
eliasGammaSize :: PaCoTree -> Int
eliasGammaSize = (2 +) . getSum . foldMap (Sum . nodeSize)
  where nodeSize x = 2 + (BMP.size . encodeEliasGamma . succ $ k) + k
                     + 1 + nextHopSize config * (fromEnum . isJust $ s)
          where k = P.maskLength . prefix $ x
                s = label x

{-|
The size of path-compressed tree is built from the following parts:

    * balanced parentheses of additional ordinal-tree root (2 bits);

    * balanced-parentheses sequence (2 bits per node);

    * Elias delta codes of skip values of the nodes (the skip value
      should be increased by one);

    * node strings of the nodes;

    * prefix bits (1 bit per node);

    * next-hop size per prefix.
-}
eliasDeltaSize :: PaCoTree -> Int
eliasDeltaSize = (2 +) . getSum . foldMap (Sum . nodeSize)
  where nodeSize x = 2 + (BMP.size . encodeEliasDelta . succ $ k) + k
                     + 1 + nextHopSize config * (fromEnum . isJust $ s)
          where k = P.maskLength . prefix $ x
                s = label x

{-|
The size of path-compressed tree is built from the following parts:

    * balanced parentheses of additional ordinal-tree root (2 bits);

    * balanced-parentheses sequence (2 bits per node);

    * Elias-Fano sequence of skip values of the nodes;

    * node strings of the nodes;

    * prefix bits (1 bit per node);

    * next-hop size per prefix.
-}
eliasFanoSize :: PaCoTree -> Int
eliasFanoSize t =
  2 + (getSum . foldMap (Sum . nodeSize) $ t) + eliasFanoSeqSize t
  where nodeSize x = 2 + k + 1 + nextHopSize config * (fromEnum . isJust $ s)
          where k = P.maskLength . prefix $ x
                s = label x

-- | Returns the size of skip values encoded as an Elias-Fano
--   sequence.
eliasFanoSeqSize :: PaCoTree -> Int
eliasFanoSeqSize t
  | null ks   = 0
  | otherwise = BMP.size $ highBits bmp2 <> lowBits bmp2
  where ks   = foldMap ((:[]) . P.maskLength . prefix) t
        bmp2 = encodeEliasFanoWith (eliasFanoLowerBits config) . scanl1 (+) $ ks

{-|
The size of path-compressed tree is built from the following parts:

    * balanced parentheses of additional ordinal-tree root (2 bits);

    * balanced-parentheses sequence (2 bits per node);

    * Fibonacci codes of skip values of the nodes (the skip value
      should be increased by one);

    * node strings of the nodes;

    * prefix bits (1 bit per node);

    * next-hop size per prefix.
-}
fibonacciSize :: PaCoTree -> Int
fibonacciSize = (2 +) . getSum . foldMap (Sum . nodeSize)
  where nodeSize x = 2 + (BMP.size . encodeFibonacci . succ $ k) + k
                     + 1 + nextHopSize config * (fromEnum . isJust $ s)
          where k = P.maskLength . prefix $ x
                s = label x

-- | Shows characteristics of path-compressed tree.
showPaCoTree :: PaCoTree -> String
showPaCoTree t =
  "Size of path-compressed tree\n" ++
  "  Elias gamma coding " ++ show (eliasGammaSize t) ++ "\n" ++
  "  Elias delta coding " ++ show (eliasDeltaSize t) ++ "\n" ++
  "  Elias-Fano coding  " ++ show (eliasFanoSize t)  ++ "\n" ++
  "  Fibonacci coding   " ++ show (fibonacciSize t)  ++ "\n"


-- | Zipper of path-compressed tree
type PaCoZipper = (,,) PaCoTree
                  [Either (Node, PaCoTree) (Node, PaCoTree)]
                  [Bool]

instance {-# OVERLAPPING #-} Show PaCoZipper where
  show (t, _, _) = show t

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
      (_, _, [])                     -> error "Tried to go up from the top"
    where mbUnite b = if b then uniteRoot else id

  isLeaf (Tip, _, _) = True
  isLeaf _           = False

  getLabel (Bin x _ _, _, _)
    | P.null (prefix x) = label x
  getLabel _            = Nothing

  setLabel s (t@(Bin x l r), es, bs)
    | P.null (prefix x) = (Bin x { label = s } l r, es, bs)
    | isJust s          = (Bin x' { label = s } l' r', es, bs)
    where Bin x' l' r' = resizeRoot 0 t
  setLabel _ z    = z

  size (t, _, _) = eliasFanoSize t

  insert (t, _, _) (_, es, _ : bs) = (t, es, True : bs)
  insert (t, _, _) (_, es, [])     = (t, es, [])

  delete (_, es, _ : bs) = (Tip, es, False : bs)
  delete (_, es, [])     = (Tip, es, [])
