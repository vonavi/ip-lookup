{-# LANGUAGE FlexibleInstances,
             UndecidableInstances #-}

module Data.OrdTree
       (
         OrdTree( isEmpty
                , fromEntry
                , collapse
                , delSubtree
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

newtype Forest a = Forest { getNodes :: [(a, Forest a)] } deriving (Eq, Show)

class OrdTree t where
  toForest    :: t       -> Forest (Maybe Int)
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

  isEmpty = null . getNodes . toForest

  size x = pred $ execState (helper . toForest $ x) 0
    where helper :: Forest (Maybe Int) -> State Int ()
          helper (Forest xs) = do
            mapM_ (helper . snd) xs
            modify succ

instance {-# OVERLAPPABLE #-} (Monoid a, OrdTree a) => IpRouter a where
  mkTable = foldr (mappend . fromEntry) mempty

  insEntry e t = t `mappend` fromEntry e

  delEntry e t = t `delSubtree` fromEntry e

  ipLookup a t = execState (lookupState a t) Nothing

  numOfPrefixes t = execState (helper . toForest $ t) 0
    where helper :: Forest (Maybe Int) -> State Int ()
          helper (Forest [])           = return ()
          helper (Forest ((x, l) : r)) = do
            helper l
            helper (Forest r)
            when (isJust x) $ modify succ

forestToBp :: Forest a -> [(a, Paren)]
forestToBp = helper . getNodes
  where helper = concatMap (\(x, Forest l)
                            -> [(x, Open)] ++ helper l ++ [(x, Close)])

ordToBp :: OrdTree a => a -> [(Maybe Int, Paren)]
ordToBp x = [(Nothing, Open)] ++ forestToBp (toForest x) ++ [(Nothing, Close)]

forestToDfuds :: Forest a -> [(a, [Paren])]
forestToDfuds (Forest t) = helper t
  where helper []                  = []
        helper ((x, Forest l) : r) =
          let p = replicate (length l) Open ++ [Close]
          in (x, p) : helper l ++ helper r

ordToDfuds :: OrdTree a => a -> [(Maybe Int, [Paren])]
ordToDfuds x = (Nothing, ps) : forestToDfuds f
  where f  = toForest x
        ps = replicate (length $ getNodes f) Open ++ [Close]


newtype OrdTreeT1 = OrdTreeT1 (Forest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT1 where
  mempty = OrdTreeT1 $ Forest []

  tx `mappend` ty = OrdTreeT1 . Forest $
                    helper (getNodes $ toForest tx) (getNodes $ toForest ty)
    where helper []                   ys                   = ys
          helper xs                   []                   = xs
          helper ((a, Forest x) : xs) ((b, Forest y) : ys) =
            (a <|> b, Forest (helper x y)) : helper xs ys

instance OrdTree OrdTreeT1 where
  toForest (OrdTreeT1 x) = x

  fromEntry (Entry p n) = OrdTreeT1 . Forest $ helper 31 [(Just n, Forest [])]
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then (Nothing, Forest []) : y
                            else [(Nothing, Forest y)]
            where y = helper (pred i) x

  lookupState (Address a) = helper 31 . getNodes . toForest
    where helper _ []                  = return ()
          helper n ((x, Forest l) : r) = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l

  collapse = OrdTreeT1 . collapseF . toForest
    where collapseF x = Forest $ helper m y
            where y = map (second collapseF) . getNodes $ x
                  m = length . filter nonempty $ y
                  nonempty (Nothing, Forest []) = False
                  nonempty _                    = True
                  helper _ []     = []
                  helper 0 _      = []
                  helper n (z:zs) = z : if nonempty z
                                        then helper (pred n) zs
                                        else helper n zs

  delSubtree a b = collapse . OrdTreeT1 . Forest $
                   helper (getNodes . toForest $ a) (getNodes . toForest $ b)
    where helper []                    _                     = []
          helper t                     []                    = t
          helper ((x, Forest lx) : rx) ((y, Forest ly) : ry) =
            (z, Forest (helper lx ly)) : helper rx ry
            where z = if x == y then Nothing else x

  bRoot = helper . getNodes . toForest
    where helper []           = Nothing
          helper ((x, _) : _) = x

  bLeftSubtree = OrdTreeT1 . Forest . helper . toForest
    where helper (Forest [])                  = []
          helper (Forest ((_, Forest l) : _)) = l

  bRightSubtree = OrdTreeT1 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = tail x

  bInsertRoot x ltree rtree =
    OrdTreeT1 . Forest $
    (x, toForest ltree) : (getNodes . toForest $ rtree)


newtype OrdTreeT2 = OrdTreeT2 (Forest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT2 where
  mempty = OrdTreeT2 $ Forest []

  tx `mappend` ty = OrdTreeT2 . Forest $
                    helper (getNodes $ toForest tx) (getNodes $ toForest ty)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a <|> b, Forest (helper x y))]
            where (a, Forest x) = last xs
                  (b, Forest y) = last ys

instance OrdTree OrdTreeT2 where
  toForest (OrdTreeT2 x) = x

  fromEntry (Entry p n) = OrdTreeT2 . Forest $ helper 31 [(Just n, Forest [])]
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then y ++ [(Nothing, Forest [])]
                            else [(Nothing, Forest y)]
            where y = helper (pred i) x

  lookupState (Address a) = helper 31 . getNodes . toForest
    where helper _ [] = return ()
          helper n xs = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l
              where (x, Forest l) = last xs
                    r             = init xs

  collapse = OrdTreeT2 . collapseF . toForest
    where collapseF = Forest . dropWhile empty .
                      map (second collapseF) . getNodes
            where empty (Nothing, Forest []) = True
                  empty _                    = False

  delSubtree a b = collapse . OrdTreeT2 . Forest $
                   helper (getNodes . toForest $ a) (getNodes . toForest $ b)
    where helper [] _   = []
          helper t  []  = t
          helper tx ty  = helper rx ry ++ [(z, Forest (helper lx ly))]
            where (x, Forest lx) = last tx
                  rx             = init tx
                  (y, Forest ly) = last ty
                  ry             = init ty
                  z              = if x == y then Nothing else x

  bRoot = helper . getNodes . toForest
    where helper [] = Nothing
          helper x  = fst . last $ x

  bLeftSubtree = OrdTreeT2 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = getNodes . snd . last $ x

  bRightSubtree = OrdTreeT2 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = init x

  bInsertRoot x ltree rtree =
    OrdTreeT2 . Forest $
    (getNodes . toForest $ rtree) ++ [(x, toForest ltree)]


newtype OrdTreeT3 = OrdTreeT3 (Forest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT3 where
  mempty = OrdTreeT3 $ Forest []

  tx `mappend` ty = OrdTreeT3 . Forest $
                    helper (getNodes $ toForest tx) (getNodes $ toForest ty)
    where helper []                   ys                   = ys
          helper xs                   []                   = xs
          helper ((a, Forest x) : xs) ((b, Forest y) : ys) =
            (a <|> b, Forest (helper x y)) : helper xs ys

instance OrdTree OrdTreeT3 where
  toForest (OrdTreeT3 x) = x

  fromEntry (Entry p n) = OrdTreeT3 . Forest $ helper 31 [(Just n, Forest [])]
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then [(Nothing, Forest y)]
                            else (Nothing, Forest []) : y
            where y = helper (pred i) x

  lookupState (Address a) = helper 31 . getNodes . toForest
    where helper _ []                  = return ()
          helper n ((x, Forest r) : l) = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l

  collapse = OrdTreeT3 . collapseF . toForest
    where collapseF x = Forest $ helper m y
            where y = map (second collapseF) . getNodes $ x
                  m = length . filter nonempty $ y
                  nonempty (Nothing, Forest []) = False
                  nonempty _                    = True
                  helper _ []     = []
                  helper 0 _      = []
                  helper n (z:zs) = z : if nonempty z
                                        then helper (pred n) zs
                                        else helper n zs

  delSubtree a b = collapse . OrdTreeT3 . Forest $
                   helper (getNodes . toForest $ a) (getNodes . toForest $ b)
    where helper []                    _                     = []
          helper t                     []                    = t
          helper ((x, Forest rx) : lx) ((y, Forest ry) : ly) =
            (z, Forest (helper rx ry)) : helper lx ly
            where z = if x == y then Nothing else x

  bRoot = helper . getNodes . toForest
    where helper []           = Nothing
          helper ((x, _) : _) = x

  bLeftSubtree = OrdTreeT3 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = tail x

  bRightSubtree = OrdTreeT3 . Forest . helper . toForest
    where helper (Forest [])                  = []
          helper (Forest ((_, Forest r) : _)) = r

  bInsertRoot x ltree rtree =
    OrdTreeT3 . Forest $
    (x, toForest rtree) : (getNodes . toForest $ ltree)


newtype OrdTreeT4 = OrdTreeT4 (Forest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT4 where
  mempty = OrdTreeT4 $ Forest []

  tx `mappend` ty = OrdTreeT4 . Forest $
                    helper (getNodes $ toForest tx) (getNodes $ toForest ty)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a <|> b, Forest (helper x y))]
            where (a, Forest x) = last xs
                  (b, Forest y) = last ys

instance OrdTree OrdTreeT4 where
  toForest (OrdTreeT4 x) = x

  fromEntry (Entry p n) = OrdTreeT4 . Forest $ helper 31 [(Just n, Forest [])]
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then [(Nothing, Forest y)]
                            else y ++ [(Nothing, Forest [])]
            where y = helper (pred i) x

  collapse = OrdTreeT4 . collapseF . toForest
    where collapseF = Forest . dropWhile empty .
                      map (second collapseF) . getNodes
            where empty (Nothing, Forest []) = True
                  empty _                    = False

  delSubtree a b = collapse . OrdTreeT4 . Forest $
                   helper (getNodes . toForest $ a) (getNodes . toForest $ b)
    where helper [] _   = []
          helper t  []  = t
          helper tx ty  = helper lx ly ++ [(z, Forest (helper rx ry))]
            where (x, Forest rx) = last tx
                  lx             = init tx
                  (y, Forest ry) = last ty
                  ly             = init ty
                  z              = if x == y then Nothing else x

  lookupState (Address a) = helper 31 . getNodes . toForest
    where helper _ [] = return ()
          helper n xs = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l
              where (x, Forest r) = last xs
                    l             = init xs

  bRoot = helper . getNodes . toForest
    where helper [] = Nothing
          helper x  = fst . last $ x

  bLeftSubtree = OrdTreeT4 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = init x

  bRightSubtree = OrdTreeT4 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = getNodes . snd . last $ x

  bInsertRoot x ltree rtree =
    OrdTreeT4 . Forest $
    (getNodes . toForest $ ltree) ++ [(x, toForest rtree)]
