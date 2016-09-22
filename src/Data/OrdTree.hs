{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Data.OrdTree
       (
         OrdTree(..)
       , ordToBp
       , ordToDfuds
       , OrdTreeT1
       , OrdTreeT2
       , OrdTreeT3
       , OrdTreeT4
       ) where

import           Control.Applicative ((<|>))
import           Control.Arrow       (second)
import           Control.Monad.State
import           Data.Bits
import           Data.Maybe          (isJust)
import           Data.Sequence       (ViewL ((:<), EmptyL),
                                      ViewR ((:>), EmptyR), (<|), (|>))
import qualified Data.Sequence       as S

import           Data.IpRouter
import           Data.Paren
import           Data.PrefixTree

newtype Forest a = Forest { getSeq :: S.Seq (a, Forest a) } deriving (Eq, Show)

class OrdTree a where
  toForest    :: a       -> Forest (Maybe Int)
  fromEntry   :: Entry   -> a
  lookupState :: Address -> a -> State (Maybe Int) ()

instance {-# OVERLAPPABLE #-} (OrdTree a, PrefixTree a) => IpRouter a where
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

isOrdEmpty :: OrdTree a => a -> Bool
isOrdEmpty = S.null . getSeq . toForest

ordSize :: OrdTree a => a -> Int
ordSize x = pred $ execState (helper . toForest $ x) 0
  where helper :: Forest (Maybe Int) -> State Int ()
        helper (Forest xs) = do
          mapM_ (helper . snd) xs
          modify succ

delRoot :: Maybe Int -> Maybe Int -> Maybe Int
delRoot x y = if x == y then Nothing else x


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

instance PrefixTree OrdTreeT1 where
  isEmpty = isOrdEmpty

  root t = case (S.viewl . getSeq . toForest) t of
             EmptyL      -> Nothing
             (x, _) :< _ -> x

  leftSubtree t = OrdTreeT1 . Forest $
                  case (S.viewl . getSeq . toForest) t of
                    EmptyL             -> S.empty
                    (_, Forest l) :< _ -> l

  rightSubtree t = OrdTreeT1 . Forest $
                   case (S.viewl . getSeq . toForest) t of
                     EmptyL -> S.empty
                     _ :< r -> r

  singleton x = OrdTreeT1 . Forest . S.singleton $ (x, Forest S.empty)

  merge x ltree rtree = OrdTreeT1 . Forest $
                        (x, toForest ltree) <| (getSeq . toForest $ rtree)

  collapse = OrdTreeT1 . collapseF . toForest
    where collapseF = Forest . S.dropWhileR empty .
                      fmap (second collapseF) . getSeq
          empty (Nothing, Forest x) = S.null x
          empty _                   = False

  delSubtree a b = collapse . OrdTreeT1 . Forest $
                   helper (getSeq . toForest $ a) (getSeq . toForest $ b)
    where helper (S.viewl -> EmptyL)    _                      = S.empty
          helper t                      (S.viewl -> EmptyL)    = t
          helper (S.viewl -> xs :< xss) (S.viewl -> ys :< yss) =
            let (x, Forest fx) = xs
                (y, Forest fy) = ys
            in (x `delRoot` y, Forest (helper fx fy)) <| helper xss yss

  size = ordSize


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

instance PrefixTree OrdTreeT2 where
  isEmpty = isOrdEmpty

  root t = case (S.viewr . getSeq . toForest) t of
             EmptyR      -> Nothing
             _ :> (x, _) -> x

  leftSubtree t = OrdTreeT2 . Forest $
                  case (S.viewr . getSeq . toForest) t of
                    EmptyR             -> S.empty
                    _ :> (_, Forest l) -> l

  rightSubtree t = OrdTreeT2 . Forest $
                   case (S.viewr . getSeq . toForest) t of
                     EmptyR -> S.empty
                     r :> _ -> r

  singleton x = OrdTreeT2 . Forest . S.singleton $ (x, Forest S.empty)

  merge x ltree rtree = OrdTreeT2 . Forest $
                        (getSeq . toForest $ rtree) |> (x, toForest ltree)

  collapse = OrdTreeT2 . collapseF . toForest
    where collapseF = Forest . S.dropWhileL empty .
                      fmap (second collapseF) . getSeq
          empty (Nothing, Forest x) = S.null x
          empty _                   = False

  delSubtree a b = collapse . OrdTreeT2 . Forest $
                   helper (getSeq . toForest $ a) (getSeq . toForest $ b)
    where helper (S.viewr -> EmptyR)    _                      = S.empty
          helper t                      (S.viewr -> EmptyR)    = t
          helper (S.viewr -> xss :> xs) (S.viewr -> yss :> ys) =
            let (x, Forest fx) = xs
                (y, Forest fy) = ys
            in helper xss yss |> (x `delRoot` y, Forest (helper fx fy))

  size = ordSize


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

instance PrefixTree OrdTreeT3 where
  isEmpty = isOrdEmpty

  root t = case (S.viewl . getSeq . toForest) t of
             EmptyL      -> Nothing
             (x, _) :< _ -> x

  leftSubtree t = OrdTreeT3 . Forest $
                  case (S.viewl . getSeq . toForest) t of
                    EmptyL -> S.empty
                    _ :< l -> l

  rightSubtree t = OrdTreeT3 . Forest $
                   case (S.viewl . getSeq . toForest) t of
                     EmptyL             -> S.empty
                     (_, Forest r) :< _ -> r

  singleton x = OrdTreeT3 . Forest . S.singleton $ (x, Forest S.empty)

  merge x ltree rtree = OrdTreeT3 . Forest $
                        (x, toForest rtree) <| (getSeq . toForest $ ltree)

  collapse = OrdTreeT3 . collapseF . toForest
    where collapseF = Forest . S.dropWhileR empty .
                      fmap (second collapseF) . getSeq
          empty (Nothing, Forest x) = S.null x
          empty _                   = False

  delSubtree a b = collapse . OrdTreeT3 . Forest $
                   helper (getSeq . toForest $ a) (getSeq . toForest $ b)
    where helper (S.viewl -> EmptyL)    _                      = S.empty
          helper t                      (S.viewl -> EmptyL)    = t
          helper (S.viewl -> xs :< xss) (S.viewl -> ys :< yss) =
            let (x, Forest fx) = xs
                (y, Forest fy) = ys
            in (x `delRoot` y, Forest (helper fx fy)) <| helper xss yss

  size = ordSize


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

instance PrefixTree OrdTreeT4 where
  isEmpty = isOrdEmpty

  root t = case (S.viewr . getSeq . toForest) t of
             EmptyR      -> Nothing
             _ :> (x, _) -> x

  leftSubtree t = OrdTreeT4 . Forest $
                  case (S.viewr . getSeq . toForest) t of
                    EmptyR -> S.empty
                    l :> _ -> l

  rightSubtree t = OrdTreeT4 . Forest $
                   case (S.viewr . getSeq . toForest) t of
                     EmptyR             -> S.empty
                     _ :> (_, Forest r) -> r

  singleton x = OrdTreeT4 . Forest . S.singleton $ (x, Forest S.empty)

  merge x ltree rtree = OrdTreeT4 . Forest $
                        (getSeq . toForest $ ltree) |> (x, toForest rtree)

  collapse = OrdTreeT4 . collapseF . toForest
    where collapseF = Forest . S.dropWhileL empty .
                      fmap (second collapseF) . getSeq
          empty (Nothing, Forest x) = S.null x
          empty _                   = False

  delSubtree a b = collapse . OrdTreeT4 . Forest $
                   helper (getSeq . toForest $ a) (getSeq . toForest $ b)
    where helper (S.viewr -> EmptyR)    _                      = S.empty
          helper t                      (S.viewr -> EmptyR)    = t
          helper (S.viewr -> xss :> xs) (S.viewr -> yss :> ys) =
            let (x, Forest fx) = xs
                (y, Forest fy) = ys
            in helper xss yss |> (x `delRoot` y, Forest (helper fx fy))

  size = ordSize
