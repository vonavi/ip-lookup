{-# LANGUAGE FlexibleInstances,
             UndecidableInstances #-}

module Data.OrdTree
       (
         OrdTree( isEmpty
                , fromEntry
                , collapse
                , size
                , bRoot
                , bLeftSubtree
                , bRightSubtree
                , bInsertRoot
                )
       , ordToBp
       , ordToDfuds
       , OrdTreeT1
       , OrdTreeT2
       , OrdTreeT3
       , OrdTreeT4
       ) where

import Data.Bits
import Data.Maybe (isJust)
import Control.Applicative ((<|>))
import Control.Monad.State
import Control.Arrow (second)

import Data.IpRouter
import Data.Paren

newtype OldForest a = OldForest { getNodes :: [(a, OldForest a)] } deriving (Eq, Show)

class OrdTree t where
  toOldForest :: t       -> OldForest (Maybe Int)
  isEmpty     :: t       -> Bool
  fromEntry   :: Entry   -> t
  lookupState :: Address -> t -> State (Maybe Int) ()
  collapse    :: t       -> t
  delSubtree  :: t       -> t -> t

  size          :: t         -> Int
  bRoot         :: t         -> Maybe Int
  bLeftSubtree  :: t         -> t
  bRightSubtree :: t         -> t
  bInsertRoot   :: Maybe Int -> t -> t -> t

  isEmpty = null . getNodes . toOldForest

  size x = pred $ execState (helper . toOldForest $ x) 0
    where helper :: OldForest (Maybe Int) -> State Int ()
          helper (OldForest xs) = do
            mapM_ (helper . snd) xs
            modify succ

instance {-# OVERLAPPABLE #-} (Monoid a, OrdTree a) => IpRouter a where
  mkTable = foldr (mappend . fromEntry) mempty

  insEntry e t = t `mappend` fromEntry e

  delEntry e t = t `delSubtree` fromEntry e

  ipLookup a t = execState (lookupState a t) Nothing

  numOfPrefixes t = execState (helper . toOldForest $ t) 0
    where helper :: OldForest (Maybe Int) -> State Int ()
          helper (OldForest [])           = return ()
          helper (OldForest ((x, l) : r)) = do
            helper l
            helper (OldForest r)
            when (isJust x) $ modify succ

forestToBp :: OldForest a -> [(a, Paren)]
forestToBp = helper . getNodes
  where helper = concatMap (\(x, OldForest l)
                            -> [(x, Open)] ++ helper l ++ [(x, Close)])

ordToBp :: OrdTree a => a -> [(Maybe Int, Paren)]
ordToBp x = [(Nothing, Open)] ++ forestToBp (toOldForest x) ++ [(Nothing, Close)]

forestToDfuds :: OldForest a -> [(a, [Paren])]
forestToDfuds (OldForest t) = helper t
  where helper []                     = []
        helper ((x, OldForest l) : r) =
          let p = replicate (length l) Open ++ [Close]
          in (x, p) : helper l ++ helper r

ordToDfuds :: OrdTree a => a -> [(Maybe Int, [Paren])]
ordToDfuds x = (Nothing, ps) : forestToDfuds f
  where f  = toOldForest x
        ps = replicate (length $ getNodes f) Open ++ [Close]


newtype OrdTreeT1 = OrdTreeT1 (OldForest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT1 where
  mempty = OrdTreeT1 $ OldForest []

  tx `mappend` ty = OrdTreeT1 . OldForest $
                    helper (getNodes $ toOldForest tx) (getNodes $ toOldForest ty)
    where helper []                   ys                         = ys
          helper xs                   []                         = xs
          helper ((a, OldForest x) : xs) ((b, OldForest y) : ys) =
            (a <|> b, OldForest (helper x y)) : helper xs ys

instance OrdTree OrdTreeT1 where
  toOldForest (OrdTreeT1 x) = x

  fromEntry (Entry p n) = OrdTreeT1 . OldForest $ helper 31 [(Just n, OldForest [])]
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then (Nothing, OldForest []) : y
                            else [(Nothing, OldForest y)]
            where y = helper (pred i) x

  lookupState (Address a) = helper 31 . getNodes . toOldForest
    where helper _ []                     = return ()
          helper n ((x, OldForest l) : r) = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l

  collapse = OrdTreeT1 . collapseF . toOldForest
    where collapseF x = OldForest $ helper m y
            where y = map (second collapseF) . getNodes $ x
                  m = length . filter nonempty $ y
                  nonempty (Nothing, OldForest []) = False
                  nonempty _                       = True
                  helper _ []     = []
                  helper 0 z      = z
                  helper n (z:zs) = if nonempty z
                                    then helper (pred n) zs
                                    else helper n zs

  delSubtree a b = collapse . OrdTreeT1 . OldForest $
                   helper (getNodes . toOldForest $ a) (getNodes . toOldForest $ b)
    where helper []                    _                           = []
          helper t                     []                          = t
          helper ((x, OldForest lx) : rx) ((y, OldForest ly) : ry) =
            (z, OldForest (helper lx ly)) : helper rx ry
            where z = if x == y then Nothing else x

  bRoot = helper . getNodes . toOldForest
    where helper []           = Nothing
          helper ((x, _) : _) = x

  bLeftSubtree = OrdTreeT1 . OldForest . helper . toOldForest
    where helper (OldForest [])                     = []
          helper (OldForest ((_, OldForest l) : _)) = l

  bRightSubtree = OrdTreeT1 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = tail x

  bInsertRoot x ltree rtree =
    OrdTreeT1 . OldForest $
    (x, toOldForest ltree) : (getNodes . toOldForest $ rtree)


newtype OrdTreeT2 = OrdTreeT2 (OldForest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT2 where
  mempty = OrdTreeT2 $ OldForest []

  tx `mappend` ty = OrdTreeT2 . OldForest $
                    helper (getNodes $ toOldForest tx) (getNodes $ toOldForest ty)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a <|> b, OldForest (helper x y))]
            where (a, OldForest x) = last xs
                  (b, OldForest y) = last ys

instance OrdTree OrdTreeT2 where
  toOldForest (OrdTreeT2 x) = x

  fromEntry (Entry p n) = OrdTreeT2 . OldForest $ helper 31 [(Just n, OldForest [])]
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then y ++ [(Nothing, OldForest [])]
                            else [(Nothing, OldForest y)]
            where y = helper (pred i) x

  lookupState (Address a) = helper 31 . getNodes . toOldForest
    where helper _ [] = return ()
          helper n xs = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l
              where (x, OldForest l) = last xs
                    r                = init xs

  collapse = OrdTreeT2 . collapseF . toOldForest
    where collapseF = OldForest . dropWhile empty .
                      map (second collapseF) . getNodes
            where empty (Nothing, OldForest []) = True
                  empty _                       = False

  delSubtree a b = collapse . OrdTreeT2 . OldForest $
                   helper (getNodes . toOldForest $ a) (getNodes . toOldForest $ b)
    where helper [] _   = []
          helper t  []  = t
          helper tx ty  = helper rx ry ++ [(z, OldForest (helper lx ly))]
            where (x, OldForest lx) = last tx
                  rx                = init tx
                  (y, OldForest ly) = last ty
                  ry                = init ty
                  z                 = if x == y then Nothing else x

  bRoot = helper . getNodes . toOldForest
    where helper [] = Nothing
          helper x  = fst . last $ x

  bLeftSubtree = OrdTreeT2 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = getNodes . snd . last $ x

  bRightSubtree = OrdTreeT2 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = init x

  bInsertRoot x ltree rtree =
    OrdTreeT2 . OldForest $
    (getNodes . toOldForest $ rtree) ++ [(x, toOldForest ltree)]


newtype OrdTreeT3 = OrdTreeT3 (OldForest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT3 where
  mempty = OrdTreeT3 $ OldForest []

  tx `mappend` ty = OrdTreeT3 . OldForest $
                    helper (getNodes $ toOldForest tx) (getNodes $ toOldForest ty)
    where helper []                   ys                         = ys
          helper xs                   []                         = xs
          helper ((a, OldForest x) : xs) ((b, OldForest y) : ys) =
            (a <|> b, OldForest (helper x y)) : helper xs ys

instance OrdTree OrdTreeT3 where
  toOldForest (OrdTreeT3 x) = x

  fromEntry (Entry p n) = OrdTreeT3 . OldForest $ helper 31 [(Just n, OldForest [])]
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then [(Nothing, OldForest y)]
                            else (Nothing, OldForest []) : y
            where y = helper (pred i) x

  lookupState (Address a) = helper 31 . getNodes . toOldForest
    where helper _ []                     = return ()
          helper n ((x, OldForest r) : l) = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l

  collapse = OrdTreeT3 . collapseF . toOldForest
    where collapseF x = OldForest $ helper m y
            where y = map (second collapseF) . getNodes $ x
                  m = length . filter nonempty $ y
                  nonempty (Nothing, OldForest []) = False
                  nonempty _                       = True
                  helper _ []     = []
                  helper 0 z      = z
                  helper n (z:zs) = if nonempty z
                                    then helper (pred n) zs
                                    else helper n zs

  delSubtree a b = collapse . OrdTreeT3 . OldForest $
                   helper (getNodes . toOldForest $ a) (getNodes . toOldForest $ b)
    where helper []                    _                           = []
          helper t                     []                          = t
          helper ((x, OldForest rx) : lx) ((y, OldForest ry) : ly) =
            (z, OldForest (helper rx ry)) : helper lx ly
            where z = if x == y then Nothing else x

  bRoot = helper . getNodes . toOldForest
    where helper []           = Nothing
          helper ((x, _) : _) = x

  bLeftSubtree = OrdTreeT3 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = tail x

  bRightSubtree = OrdTreeT3 . OldForest . helper . toOldForest
    where helper (OldForest [])                     = []
          helper (OldForest ((_, OldForest r) : _)) = r

  bInsertRoot x ltree rtree =
    OrdTreeT3 . OldForest $
    (x, toOldForest rtree) : (getNodes . toOldForest $ ltree)


newtype OrdTreeT4 = OrdTreeT4 (OldForest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT4 where
  mempty = OrdTreeT4 $ OldForest []

  tx `mappend` ty = OrdTreeT4 . OldForest $
                    helper (getNodes $ toOldForest tx) (getNodes $ toOldForest ty)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a <|> b, OldForest (helper x y))]
            where (a, OldForest x) = last xs
                  (b, OldForest y) = last ys

instance OrdTree OrdTreeT4 where
  toOldForest (OrdTreeT4 x) = x

  fromEntry (Entry p n) = OrdTreeT4 . OldForest $ helper 31 [(Just n, OldForest [])]
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then [(Nothing, OldForest y)]
                            else y ++ [(Nothing, OldForest [])]
            where y = helper (pred i) x

  collapse = OrdTreeT4 . collapseF . toOldForest
    where collapseF = OldForest . dropWhile empty .
                      map (second collapseF) . getNodes
            where empty (Nothing, OldForest []) = True
                  empty _                       = False

  delSubtree a b = collapse . OrdTreeT4 . OldForest $
                   helper (getNodes . toOldForest $ a) (getNodes . toOldForest $ b)
    where helper [] _   = []
          helper t  []  = t
          helper tx ty  = helper lx ly ++ [(z, OldForest (helper rx ry))]
            where (x, OldForest rx) = last tx
                  lx                = init tx
                  (y, OldForest ry) = last ty
                  ly                = init ty
                  z                 = if x == y then Nothing else x

  lookupState (Address a) = helper 31 . getNodes . toOldForest
    where helper _ [] = return ()
          helper n xs = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l
              where (x, OldForest r) = last xs
                    l                = init xs

  bRoot = helper . getNodes . toOldForest
    where helper [] = Nothing
          helper x  = fst . last $ x

  bLeftSubtree = OrdTreeT4 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = init x

  bRightSubtree = OrdTreeT4 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = getNodes . snd . last $ x

  bInsertRoot x ltree rtree =
    OrdTreeT4 . OldForest $
    (getNodes . toOldForest $ ltree) ++ [(x, toOldForest rtree)]
