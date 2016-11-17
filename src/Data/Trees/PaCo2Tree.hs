{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Trees.PaCo2Tree
  (
    Node(..)
  , Tree(..)
  , PaCo2Tree
  , Zipper(..)
  , PaCo2Zipper
  , eliasGammaSize
  , eliasDeltaSize
  , eliasFanoSize
  , showPaCo2Tree
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

data Node = Node { prefix :: P.Prefix
                 , label  :: Maybe Int
                 }
          deriving (Show, Eq)
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
  where emptyRoot = Node { prefix = P.mkPrefix (P.ipv4Address 0) 0
                         , label  = Nothing
                         }

mkRootFull :: PaCo2Tree -> PaCo2Tree
mkRootFull t@(Bin _ Tip Tip) = t
mkRootFull (Bin x Tip r)     = Bin x emptyBranch r
mkRootFull (Bin x l Tip)     = Bin x l emptyBranch
mkRootFull t                 = t

joinNodes :: Node -> Bool -> Node -> Node
joinNodes Node { prefix = px } b y
  = Node { prefix = (px `P.append`) . P.cons b . prefix $ y
         , label  = label y
         }

uniteRoot :: PaCo2Tree -> PaCo2Tree
uniteRoot t@(Bin x _ _) | isJust (label x) = t
uniteRoot (Bin x Tip (Bin y l r))          = Bin (joinNodes x True y) l r
uniteRoot (Bin x (Bin y l r) Tip)          = Bin (joinNodes x False y) l r
uniteRoot t                                = t

resizeRoot :: Int -> PaCo2Tree -> PaCo2Tree
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

instance Monoid PaCo2Tree where
  mempty = Tip

  Tip `mappend` t  = t
  t  `mappend` Tip = t
  t1 `mappend` t2  = mkRootFull . uniteRoot $ t1 `helper` t2
    where tx `helper` ty = Bin node (lx <> ly) (rx <> ry)
            where Bin x _ _    = tx
                  Bin y _ _    = ty
                  (p, _, _)    = P.commonPrefixes (prefix x) (prefix y)
                  kmin         = P.maskLength p
                  Bin x' lx rx = resizeRoot kmin tx
                  Bin y' ly ry = resizeRoot kmin ty
                  node         = x' { label = label x' <|> label y' }


fromEntry :: Entry -> PaCo2Tree
fromEntry Entry { network = p, nextHop = n } =
  Bin Node { prefix = p, label = Just n } Tip Tip

lookupState :: P.Prefix -> PaCo2Tree -> State (Maybe Int) ()
lookupState _ Tip         = return ()
lookupState v (Bin x l r)
  | (not . P.null) pa     = return ()
  | otherwise             = do modify (label x <|>)
                               case P.uncons va of
                                 Nothing      -> return ()
                                 Just (b, vb) -> lookupState vb $ bool l r b
  where (_, va, pa) = P.commonPrefixes v (prefix x)

delEmptyNode :: PaCo2Tree -> PaCo2Tree
delEmptyNode t | Bin x Tip Tip <- t
               , Nothing <- label x = Tip
               | otherwise          = t

delSubtree :: PaCo2Tree -> PaCo2Tree -> PaCo2Tree
Tip `delSubtree` _ = Tip
t `delSubtree` Tip = delEmptyNode . uniteRoot $ t
tx `delSubtree` ty = delEmptyNode . mkRootFull . uniteRoot $
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

instance IpRouter PaCo2Tree where
  mkTable       = foldMap fromEntry
  insEntry      = (<>) . fromEntry
  delEntry      = flip delSubtree . fromEntry
  ipLookup a t  = execState (lookupState a t) Nothing
  numOfPrefixes = getSum . foldMap (Sum . fromEnum . isJust . label)


{-|
The node size of path-compressed 2-tree is built from the following
parts:

    * open/close parenthesis (1 bit);

    * Elias gamma code of skip value (the skip value should be
      increased by one);

    * node string;

    * prefix bit (1 bit);

    * next-hop size per prefix.
-}
eliasGammaSize :: PaCo2Tree -> Int
eliasGammaSize = getSum . foldMap (Sum . nodeSize)
  where nodeSize x = 1 + (BMP.size . encodeEliasGamma . succ $ k) + k
                     + 1 + nextHopSize config * (fromEnum . isJust $ s)
          where k = P.maskLength . prefix $ x
                s = label x

{-|
The node size of path-compressed 2-tree is built from the following
parts:

    * open/close parenthesis (1 bit);

    * Elias delta code of skip value (the skip value should be
      increased by one);

    * node string;

    * prefix bit (1 bit);

    * next-hop size per prefix.
-}
eliasDeltaSize :: PaCo2Tree -> Int
eliasDeltaSize = getSum . foldMap (Sum . nodeSize)
  where nodeSize x = 1 + (BMP.size . encodeEliasDelta . succ $ k) + k
                     + 1 + nextHopSize config * (fromEnum . isJust $ s)
          where k = P.maskLength . prefix $ x
                s = label x

{-|
The size of path-compressed 2-tree is built from the following parts:

    * balanced-parentheses expression (1 bit per node);

    * Elias-Fano sequence of skip values;

    * node strings;

    * prefix bits (1 bit per node);

    * next-hop size per prefix.
-}
eliasFanoSize :: PaCo2Tree -> Int
eliasFanoSize t = (getSum . foldMap (Sum . nodeSize) $ t) + eliasFanoSeqSize t
  where nodeSize x = 1 + k + 1 + nextHopSize config * (fromEnum . isJust $ s)
          where k = P.maskLength . prefix $ x
                s = label x

eliasFanoSeqSize :: PaCo2Tree -> Int
eliasFanoSeqSize t
  | null ks   = 0
  | otherwise = BMP.size $ (encodeUnary . succ . lowSize $ bmp2) <>
                highBits bmp2 <> lowBits bmp2
  where ks   = foldMap ((:[]) . P.maskLength . prefix) t
        bmp2 = encodeEliasFano . scanl1 (+) $ ks

{-|
The node size of path-compressed 2-tree is built from the following
parts:

    * open/close parenthesis (1 bit);

    * Fibonacci code of skip value (the skip value should be increased
      by one);

    * node string;

    * prefix bit (1 bit);

    * next-hop size per prefix.
-}
fibonacciSize :: PaCo2Tree -> Int
fibonacciSize = getSum . foldMap (Sum . nodeSize)
  where nodeSize x = 1 + (BMP.size . encodeFibonacci . succ $ k) + k
                     + 1 + nextHopSize config * (fromEnum . isJust $ s)
          where k = P.maskLength . prefix $ x
                s = label x

showPaCo2Tree :: PaCo2Tree -> String
showPaCo2Tree t =
  "Size of path-compressed 2-tree\n" ++
  "  Elias gamma coding " ++ show (eliasGammaSize t) ++ "\n" ++
  "  Elias delta coding " ++ show (eliasDeltaSize t) ++ "\n" ++
  "  Elias-Fano coding  " ++ show (eliasFanoSize t)  ++ "\n" ++
  "  Fibonacci coding   " ++ show (fibonacciSize t)  ++ "\n"


class Zipper a where
  goLeft     :: a -> a
  goRight    :: a -> a
  goUp       :: a -> a
  isLeaf     :: a -> Bool
  getLabel   :: a -> Maybe Int
  setLabel   :: Maybe Int -> a -> a
  size       :: a -> Int
  insert     :: a -> a -> a
  delete     :: a -> a
  isNodeFull :: a -> Bool
  mkNodeFull :: a -> a

type PaCo2Zipper = (,,) PaCo2Tree
                   [Either (Node, PaCo2Tree) (Node, PaCo2Tree)]
                   [Bool]

instance {-# OVERLAPPING #-} Show PaCo2Zipper where
  show (t, _, _) = show t

instance IpRouter PaCo2Zipper where
  mkTable es              = (mkTable es, [], [])
  insEntry e (t, _, _)    = (insEntry e t, [], [])
  delEntry e (t, _, _)    = (delEntry e t, [], [])
  ipLookup e (t, _, _)    = ipLookup e t
  numOfPrefixes (t, _, _) = numOfPrefixes t

instance Zipper PaCo2Zipper where
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

  size (t, _, _) = eliasGammaSize t

  insert (t, _, _) (_, es, _ : bs) = (t, es, True : bs)
  insert (t, _, _) (_, es, [])     = (t, es, [])

  delete (_, es, _ : bs) = (Tip, es, False : bs)
  delete (_, es, [])     = (Tip, es, [])

  isNodeFull (t, _, _) = isRootFull t

  mkNodeFull (t, es, bs) = (mkRootFull t, es, bs)
