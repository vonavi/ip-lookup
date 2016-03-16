{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             ViewPatterns #-}

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
       , OldTreeT1
       , OldTreeT2
       , OldTreeT3
       , OldTreeT4
       ) where

import qualified Data.Sequence as S
import Data.Sequence ((<|), (|>), ViewL(EmptyL, (:<)), ViewR(EmptyR, (:>)))
import Data.Bits
import Data.Maybe (isJust)
import Control.Applicative ((<|>))
import Control.Monad.State
import Control.Arrow (second)

import Data.IpRouter
import Data.Paren

newtype Forest a = Forest { getSeq :: S.Seq (a, Forest a) } deriving (Eq, Show)

newtype OldForest a = OldForest { getNodes :: [(a, OldForest a)] } deriving (Eq, Show)

class OrdTree t where
  toForest    :: t       -> Forest (Maybe Int)
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

  isEmpty = S.null . getSeq . toForest

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

  numOfPrefixes tree = execState (helper . getSeq . toForest $ tree) 0
    where helper t = case S.viewl t of
                      EmptyL             -> return ()
                      (x, Forest l) :< r -> do helper l
                                               helper r
                                               when (isJust x) $ modify succ

forestToBp :: Forest a -> [(a, Paren)]
forestToBp = concatMap f . getSeq
  where f (x, l) = [(x, Open)] ++ forestToBp l ++ [(x, Close)]

ordToBp :: OrdTree a => a -> [(Maybe Int, Paren)]
ordToBp x = [(Nothing, Open)] ++ forestToBp (toForest x) ++ [(Nothing, Close)]

forestToDfuds :: Forest a -> [(a, [Paren])]
forestToDfuds = helper . getSeq
  where helper t = case S.viewl t of
                    EmptyL             -> []
                    (x, Forest l) :< r -> (x, p) : helper l ++ helper r
                      where p = replicate (S.length l) Open ++ [Close]

ordToDfuds :: OrdTree a => a -> [(Maybe Int, [Paren])]
ordToDfuds x = (Nothing, ps) : forestToDfuds f
  where f  = toForest x
        ps = replicate (length $ getSeq f) Open ++ [Close]


newtype OrdTreeT1 = OrdTreeT1 (Forest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT1 where
  mempty = OrdTreeT1 $ Forest S.empty

  tx `mappend` ty = OrdTreeT1 . Forest $
                    helper (getSeq . toForest $ tx) (getSeq . toForest $ ty)
    where helper xs                   (S.viewl -> EmptyL)  = xs
          helper (S.viewl -> EmptyL)  ys                   = ys
          helper (S.viewl -> x :< xs) (S.viewl -> y :< ys) =
            let (a, Forest fa) = x
                (b, Forest fb) = y
            in (a <|> b, Forest (helper fa fb)) <| helper xs ys

instance OrdTree OrdTreeT1 where
  toForest (OrdTreeT1 x) = x

  fromEntry (Entry p n) = OrdTreeT1 . Forest . helper 31 $
                          S.singleton (Just n, Forest S.empty)
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then (Nothing, Forest S.empty) <| y
                            else S.singleton (Nothing, Forest y)
            where y = helper (pred i) x

  lookupState (Address a) = helper 31 . getSeq . toForest
    where helper _ (S.viewl -> EmptyL)             = return ()
          helper n (S.viewl -> (x, Forest l) :< r) = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l

  collapse = OrdTreeT1 . collapseF . toForest
    where collapseF = Forest . S.dropWhileR empty .
                      fmap (second collapseF) . getSeq
          empty (Nothing, Forest x) = S.null x
          empty _                   = False

  delSubtree a b = collapse . OrdTreeT1 . Forest $
                   helper (getSeq . toForest $ a) (getSeq . toForest $ b)
    where helper (S.viewl -> EmptyL)  _                    = S.empty
          helper t                    (S.viewl -> EmptyL)  = t
          helper (S.viewl -> x :< xs) (S.viewl -> y :< ys) =
            let (a, Forest fa) = x
                (b, Forest fb) = y
                z              = if a == b then Nothing else a
            in (z, Forest (helper fa fb)) <| helper xs ys

  bRoot t = case (S.viewl . getSeq . toForest) t of
             EmptyL      -> Nothing
             (x, _) :< _ -> x

  bLeftSubtree t = OrdTreeT1 . Forest $
                   case (S.viewl . getSeq . toForest) t of
                    EmptyL             -> S.empty
                    (_, Forest l) :< _ -> l

  bRightSubtree t = OrdTreeT1 . Forest $
                    case (S.viewl . getSeq . toForest) t of
                     EmptyL -> S.empty
                     _ :< r -> r

  bInsertRoot x ltree rtree =
    OrdTreeT1 . Forest $
    (x, toForest ltree) <| (getSeq . toForest $ rtree)


newtype OldTreeT1 = OldTreeT1 (OldForest (Maybe Int)) deriving (Eq, Show)

instance Monoid OldTreeT1 where
  mempty = OldTreeT1 $ OldForest []

  tx `mappend` ty = OldTreeT1 . OldForest $
                    helper (getNodes $ toOldForest tx) (getNodes $ toOldForest ty)
    where helper []                   ys                         = ys
          helper xs                   []                         = xs
          helper ((a, OldForest x) : xs) ((b, OldForest y) : ys) =
            (a <|> b, OldForest (helper x y)) : helper xs ys

instance OrdTree OldTreeT1 where
  toOldForest (OldTreeT1 x) = x

  fromEntry (Entry p n) = OldTreeT1 . OldForest $ helper 31 [(Just n, OldForest [])]
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

  collapse = OldTreeT1 . collapseF . toOldForest
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

  delSubtree a b = collapse . OldTreeT1 . OldForest $
                   helper (getNodes . toOldForest $ a) (getNodes . toOldForest $ b)
    where helper []                    _                           = []
          helper t                     []                          = t
          helper ((x, OldForest lx) : rx) ((y, OldForest ly) : ry) =
            (z, OldForest (helper lx ly)) : helper rx ry
            where z = if x == y then Nothing else x

  bRoot = helper . getNodes . toOldForest
    where helper []           = Nothing
          helper ((x, _) : _) = x

  bLeftSubtree = OldTreeT1 . OldForest . helper . toOldForest
    where helper (OldForest [])                     = []
          helper (OldForest ((_, OldForest l) : _)) = l

  bRightSubtree = OldTreeT1 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = tail x

  bInsertRoot x ltree rtree =
    OldTreeT1 . OldForest $
    (x, toOldForest ltree) : (getNodes . toOldForest $ rtree)


newtype OrdTreeT2 = OrdTreeT2 (Forest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT2 where
  mempty = OrdTreeT2 $ Forest S.empty

  tx `mappend` ty = OrdTreeT2 . Forest $
                    helper (getSeq . toForest $ tx) (getSeq . toForest $ ty)
    where helper xs                   (S.viewr -> EmptyR)  = xs
          helper (S.viewr -> EmptyR)  ys                   = ys
          helper (S.viewr -> xs :> x) (S.viewr -> ys :> y) =
            let (a, Forest fa) = x
                (b, Forest fb) = y
            in helper xs ys |> (a <|> b, Forest (helper fa fb))

instance OrdTree OrdTreeT2 where
  toForest (OrdTreeT2 x) = x

  fromEntry (Entry p n) = OrdTreeT2 . Forest . helper 31 $
                          S.singleton (Just n, Forest S.empty)
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then y |> (Nothing, Forest S.empty)
                            else S.singleton (Nothing, Forest y)
            where y = helper (pred i) x

  lookupState (Address a) = helper 31 . getSeq . toForest
    where helper _ (S.viewr -> EmptyR)             = return ()
          helper n (S.viewr -> r :> (x, Forest l)) = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l

  collapse = OrdTreeT2 . collapseF . toForest
    where collapseF = Forest . S.dropWhileL empty .
                      fmap (second collapseF) . getSeq
          empty (Nothing, Forest x) = S.null x
          empty _                   = False

  delSubtree a b = collapse . OrdTreeT2 . Forest $
                   helper (getSeq . toForest $ a) (getSeq . toForest $ b)
    where helper (S.viewr -> EmptyR)  _                    = S.empty
          helper t                    (S.viewr -> EmptyR)  = t
          helper (S.viewr -> xs :> x) (S.viewr -> ys :> y) =
            let (a, Forest fa) = x
                (b, Forest fb) = y
                z              = if a == b then Nothing else a
            in helper xs ys |> (z, Forest (helper fa fb))

  bRoot t = case (S.viewr . getSeq . toForest) t of
             EmptyR      -> Nothing
             _ :> (x, _) -> x

  bLeftSubtree t = OrdTreeT2 . Forest $
                   case (S.viewr . getSeq . toForest) t of
                    EmptyR             -> S.empty
                    _ :> (_, Forest l) -> l

  bRightSubtree t = OrdTreeT2 . Forest $
                    case (S.viewr . getSeq . toForest) t of
                     EmptyR -> S.empty
                     r :> _ -> r

  bInsertRoot x ltree rtree =
    OrdTreeT2 . Forest $
    (getSeq . toForest $ rtree) |> (x, toForest ltree)


newtype OldTreeT2 = OldTreeT2 (OldForest (Maybe Int)) deriving (Eq, Show)

instance Monoid OldTreeT2 where
  mempty = OldTreeT2 $ OldForest []

  tx `mappend` ty = OldTreeT2 . OldForest $
                    helper (getNodes $ toOldForest tx) (getNodes $ toOldForest ty)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a <|> b, OldForest (helper x y))]
            where (a, OldForest x) = last xs
                  (b, OldForest y) = last ys

instance OrdTree OldTreeT2 where
  toOldForest (OldTreeT2 x) = x

  fromEntry (Entry p n) = OldTreeT2 . OldForest $ helper 31 [(Just n, OldForest [])]
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

  collapse = OldTreeT2 . collapseF . toOldForest
    where collapseF = OldForest . dropWhile empty .
                      map (second collapseF) . getNodes
            where empty (Nothing, OldForest []) = True
                  empty _                       = False

  delSubtree a b = collapse . OldTreeT2 . OldForest $
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

  bLeftSubtree = OldTreeT2 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = getNodes . snd . last $ x

  bRightSubtree = OldTreeT2 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = init x

  bInsertRoot x ltree rtree =
    OldTreeT2 . OldForest $
    (getNodes . toOldForest $ rtree) ++ [(x, toOldForest ltree)]


newtype OrdTreeT3 = OrdTreeT3 (Forest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT3 where
  mempty = OrdTreeT3 $ Forest S.empty

  tx `mappend` ty = OrdTreeT3 . Forest $
                    helper (getSeq . toForest $ tx) (getSeq . toForest $ ty)
    where helper xs                   (S.viewl -> EmptyL)  = xs
          helper (S.viewl -> EmptyL)  ys                   = ys
          helper (S.viewl -> x :< xs) (S.viewl -> y :< ys) =
            let (a, Forest fa) = x
                (b, Forest fb) = y
            in (a <|> b, Forest (helper fa fb)) <| helper xs ys

instance OrdTree OrdTreeT3 where
  toForest (OrdTreeT3 x) = x

  fromEntry (Entry p n) = OrdTreeT3 . Forest . helper 31 $
                          S.singleton (Just n, Forest S.empty)
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then S.singleton (Nothing, Forest y)
                            else (Nothing, Forest S.empty) <| y
            where y = helper (pred i) x

  lookupState (Address a) = helper 31 . getSeq . toForest
    where helper _ (S.viewl -> EmptyL)             = return ()
          helper n (S.viewl -> (x, Forest r) :< l) = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l

  collapse = OrdTreeT3 . collapseF . toForest
    where collapseF = Forest . S.dropWhileR empty .
                      fmap (second collapseF) . getSeq
          empty (Nothing, Forest x) = S.null x
          empty _                   = False

  delSubtree a b = collapse . OrdTreeT3 . Forest $
                   helper (getSeq . toForest $ a) (getSeq . toForest $ b)
    where helper (S.viewl -> EmptyL)  _                    = S.empty
          helper t                    (S.viewl -> EmptyL)  = t
          helper (S.viewl -> x :< xs) (S.viewl -> y :< ys) =
            let (a, Forest fa) = x
                (b, Forest fb) = y
                z              = if a == b then Nothing else a
            in (z, Forest (helper fa fb)) <| helper xs ys

  bRoot t = case (S.viewl . getSeq . toForest) t of
             EmptyL      -> Nothing
             (x, _) :< _ -> x

  bLeftSubtree t = OrdTreeT3 . Forest $
                   case (S.viewl . getSeq . toForest) t of
                    EmptyL -> S.empty
                    _ :< l -> l

  bRightSubtree t = OrdTreeT3 . Forest $
                    case (S.viewl . getSeq . toForest) t of
                     EmptyL             -> S.empty
                     (_, Forest r) :< _ -> r

  bInsertRoot x ltree rtree =
    OrdTreeT3 . Forest $
    (x, toForest rtree) <| (getSeq . toForest $ ltree)


newtype OldTreeT3 = OldTreeT3 (OldForest (Maybe Int)) deriving (Eq, Show)

instance Monoid OldTreeT3 where
  mempty = OldTreeT3 $ OldForest []

  tx `mappend` ty = OldTreeT3 . OldForest $
                    helper (getNodes $ toOldForest tx) (getNodes $ toOldForest ty)
    where helper []                   ys                         = ys
          helper xs                   []                         = xs
          helper ((a, OldForest x) : xs) ((b, OldForest y) : ys) =
            (a <|> b, OldForest (helper x y)) : helper xs ys

instance OrdTree OldTreeT3 where
  toOldForest (OldTreeT3 x) = x

  fromEntry (Entry p n) = OldTreeT3 . OldForest $ helper 31 [(Just n, OldForest [])]
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

  collapse = OldTreeT3 . collapseF . toOldForest
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

  delSubtree a b = collapse . OldTreeT3 . OldForest $
                   helper (getNodes . toOldForest $ a) (getNodes . toOldForest $ b)
    where helper []                    _                           = []
          helper t                     []                          = t
          helper ((x, OldForest rx) : lx) ((y, OldForest ry) : ly) =
            (z, OldForest (helper rx ry)) : helper lx ly
            where z = if x == y then Nothing else x

  bRoot = helper . getNodes . toOldForest
    where helper []           = Nothing
          helper ((x, _) : _) = x

  bLeftSubtree = OldTreeT3 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = tail x

  bRightSubtree = OldTreeT3 . OldForest . helper . toOldForest
    where helper (OldForest [])                     = []
          helper (OldForest ((_, OldForest r) : _)) = r

  bInsertRoot x ltree rtree =
    OldTreeT3 . OldForest $
    (x, toOldForest rtree) : (getNodes . toOldForest $ ltree)


newtype OrdTreeT4 = OrdTreeT4 (Forest (Maybe Int)) deriving (Eq, Show)

instance Monoid OrdTreeT4 where
  mempty = OrdTreeT4 $ Forest S.empty

  tx `mappend` ty = OrdTreeT4 . Forest $
                    helper (getSeq . toForest $ tx) (getSeq . toForest $ ty)
    where helper xs                   (S.viewr -> EmptyR)  = xs
          helper (S.viewr -> EmptyR)  ys                   = ys
          helper (S.viewr -> xs :> x) (S.viewr -> ys :> y) =
            let (a, Forest fa) = x
                (b, Forest fb) = y
            in helper xs ys |> (a <|> b, Forest (helper fa fb))

instance OrdTree OrdTreeT4 where
  toForest (OrdTreeT4 x) = x

  fromEntry (Entry p n) = OrdTreeT4 . Forest . helper 31 $
                          S.singleton (Just n, Forest S.empty)
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then S.singleton (Nothing, Forest y)
                            else y |> (Nothing, Forest S.empty)
            where y = helper (pred i) x

  lookupState (Address a) = helper 31 . getSeq . toForest
    where helper _ (S.viewr -> EmptyR)             = return ()
          helper n (S.viewr -> l :> (x, Forest r)) = do
            modify (x <|>)
            helper (pred n) $ if a `testBit` n then r else l

  collapse = OrdTreeT4 . collapseF . toForest
    where collapseF = Forest . S.dropWhileL empty .
                      fmap (second collapseF) . getSeq
          empty (Nothing, Forest x) = S.null x
          empty _                   = False

  delSubtree a b = collapse . OrdTreeT4 . Forest $
                   helper (getSeq . toForest $ a) (getSeq . toForest $ b)
    where helper (S.viewr -> EmptyR)  _                    = S.empty
          helper t                    (S.viewr -> EmptyR)  = t
          helper (S.viewr -> xs :> x) (S.viewr -> ys :> y) =
            let (a, Forest fa) = x
                (b, Forest fb) = y
                z              = if a == b then Nothing else a
            in helper xs ys |> (z, Forest (helper fa fb))

  bRoot t = case (S.viewr . getSeq . toForest) t of
             EmptyR      -> Nothing
             _ :> (x, _) -> x

  bLeftSubtree t = OrdTreeT4 . Forest $
                   case (S.viewr . getSeq . toForest) t of
                    EmptyR -> S.empty
                    l :> _ -> l

  bRightSubtree t = OrdTreeT4 . Forest $
                    case (S.viewr . getSeq . toForest) t of
                     EmptyR             -> S.empty
                     _ :> (_, Forest r) -> r

  bInsertRoot x ltree rtree =
    OrdTreeT4 . Forest $
    (getSeq . toForest $ ltree) |> (x, toForest rtree)


newtype OldTreeT4 = OldTreeT4 (OldForest (Maybe Int)) deriving (Eq, Show)

instance Monoid OldTreeT4 where
  mempty = OldTreeT4 $ OldForest []

  tx `mappend` ty = OldTreeT4 . OldForest $
                    helper (getNodes $ toOldForest tx) (getNodes $ toOldForest ty)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a <|> b, OldForest (helper x y))]
            where (a, OldForest x) = last xs
                  (b, OldForest y) = last ys

instance OrdTree OldTreeT4 where
  toOldForest (OldTreeT4 x) = x

  fromEntry (Entry p n) = OldTreeT4 . OldForest $ helper 31 [(Just n, OldForest [])]
    where Prefix (Address a) (Mask m) = p
          helper i x
            | i == 31 - m = x
            | otherwise   = if a `testBit` i
                            then [(Nothing, OldForest y)]
                            else y ++ [(Nothing, OldForest [])]
            where y = helper (pred i) x

  collapse = OldTreeT4 . collapseF . toOldForest
    where collapseF = OldForest . dropWhile empty .
                      map (second collapseF) . getNodes
            where empty (Nothing, OldForest []) = True
                  empty _                       = False

  delSubtree a b = collapse . OldTreeT4 . OldForest $
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

  bLeftSubtree = OldTreeT4 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = init x

  bRightSubtree = OldTreeT4 . OldForest . helper . toOldForest
    where helper (OldForest []) = []
          helper (OldForest x)  = getNodes . snd . last $ x

  bInsertRoot x ltree rtree =
    OldTreeT4 . OldForest $
    (getNodes . toOldForest $ ltree) ++ [(x, toOldForest rtree)]
