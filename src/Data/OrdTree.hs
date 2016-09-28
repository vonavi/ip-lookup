{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.OrdTree
       (
         OrdTree(..)
       , ordToBp
       , ordToDfuds
       , ordSize
       , OrdTreeT1
       , OrdZipperT1
       , OrdTreeT2
       , OrdZipperT2
       , OrdTreeT3
       , OrdZipperT3
       , OrdTreeT4
       , OrdZipperT4
       ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Bits
import           Data.Maybe          (isJust)
import           Data.Sequence       (ViewL ((:<), EmptyL),
                                      ViewR ((:>), EmptyR), (<|), (|>))
import qualified Data.Sequence       as S

import           Data.IpRouter
import           Data.Paren
import           Data.Zipper

newtype Forest a = Forest { getSeq :: S.Seq (a, Forest a) } deriving (Eq, Show)

class OrdTree a where
  toForest    :: a       -> Forest (Maybe Int)
  fromEntry   :: Entry   -> a
  lookupState :: Address -> a -> State (Maybe Int) ()
  delSubtree  :: a       -> a -> a

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

ordSize :: OrdTree a => a -> Int
ordSize x = pred $ execState (helper . toForest $ x) 0
  where helper :: Forest (Maybe Int) -> State Int ()
        helper (Forest xs) = do
          mapM_ (helper . snd) xs
          modify succ

delRoot :: Maybe Int -> Maybe Int -> Maybe Int
delRoot x y = if x == y then Nothing else x

delEmptyNodeL :: S.Seq (Maybe Int, Forest (Maybe Int))
              -> S.Seq (Maybe Int, Forest (Maybe Int))
delEmptyNodeL t | (Nothing, Forest l) :< r <- S.viewl t
                , S.null l, S.null r
                  = S.empty
                | otherwise
                  = t

delEmptyNodeR :: S.Seq (Maybe Int, Forest (Maybe Int))
              -> S.Seq (Maybe Int, Forest (Maybe Int))
delEmptyNodeR t | l :> (Nothing, Forest r) <- S.viewr t
                , S.null l, S.null r
                  = S.empty
                | otherwise
                  = t


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

  delSubtree tx ty = OrdTreeT1 . Forest $
                     helper (getSeq . toForest $ tx) (getSeq . toForest $ ty)
    where (S.viewl -> EmptyL)   `helper` _                     = S.empty
          t                     `helper` (S.viewl -> EmptyL)   = t
          (S.viewl -> hx :< rx) `helper` (S.viewl -> hy :< ry) =
            delEmptyNodeL $
            (x `delRoot` y, Forest $ lx `helper` ly) <| (rx `helper` ry)
            where (x, Forest lx) = hx
                  (y, Forest ly) = hy


type OrdZipperT1 = (,) OrdTreeT1
                   [Either (Maybe Int, OrdTreeT1) (Maybe Int, OrdTreeT1)]

instance {-# OVERLAPPING #-} Show OrdZipperT1 where
  show (t, _) = show t

instance IpRouter OrdZipperT1 where
  mkTable es           = (mkTable es, [])
  insEntry e (t, _)    = (insEntry e t, [])
  delEntry e (t, _)    = (delEntry e t, [])
  ipLookup e (t, _)    = ipLookup e t
  numOfPrefixes (t, _) = numOfPrefixes t

instance Zipper OrdZipperT1 where
  goLeft (t, es) =
    case S.viewl . getSeq . toForest $ t of
      (x, l) :< r -> (OrdTreeT1 l, Right (x, OrdTreeT1 . Forest $ r) : es)
      EmptyL      -> error "Tried to go left from a leaf"

  goRight (t, es) =
    case S.viewl . getSeq . toForest $ t of
      (x, l) :< r -> (OrdTreeT1 . Forest $ r, Left (x, OrdTreeT1 l) : es)
      EmptyL      -> error "Tried to go right from a leaf"

  goUp z = case z of (OrdTreeT1 l, Right (x, OrdTreeT1 (Forest r)) : es)
                       -> (OrdTreeT1 . Forest $ (x, l) <| r, es)
                     (OrdTreeT1 (Forest r), Left (x, OrdTreeT1 l) : es)
                       -> (OrdTreeT1 . Forest $ (x, l) <| r, es)
                     (_, [])
                       -> error "Tried to go up from the top"

  isLeaf (t, _) = case S.viewl . getSeq . toForest $ t of
                    EmptyL -> True
                    _      -> False

  getLabel (t, _) = case S.viewl . getSeq . toForest $ t of
                      (x, _) :< _ -> x
                      EmptyL      -> Nothing

  setLabel s z@(t, es) = case S.viewl . getSeq . toForest $ t of
                           (_, l) :< r -> (OrdTreeT1 . Forest $ (s, l) <| r, es)
                           EmptyL      -> z

  size (t, _) = ordSize t + 18 * numOfPrefixes t

  insert (t, _) (_, es) = (t, es)

  delete (_, es) = (OrdTreeT1 . Forest $ S.empty, es)


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

  delSubtree tx ty = OrdTreeT2 . Forest $
                     helper (getSeq . toForest $ tx) (getSeq . toForest $ ty)
    where (S.viewr -> EmptyR)   `helper` _                     = S.empty
          t                     `helper` (S.viewr -> EmptyR)   = t
          (S.viewr -> rx :> hx) `helper` (S.viewr -> ry :> hy) =
            delEmptyNodeR $
            rx `helper` ry |> (x `delRoot` y, Forest $ lx `helper` ly)
            where (x, Forest lx) = hx
                  (y, Forest ly) = hy


type OrdZipperT2 = (,) OrdTreeT2
                   [Either (Maybe Int, OrdTreeT2) (Maybe Int, OrdTreeT2)]

instance {-# OVERLAPPING #-} Show OrdZipperT2 where
  show (t, _) = show t

instance IpRouter OrdZipperT2 where
  mkTable es           = (mkTable es, [])
  insEntry e (t, _)    = (insEntry e t, [])
  delEntry e (t, _)    = (delEntry e t, [])
  ipLookup e (t, _)    = ipLookup e t
  numOfPrefixes (t, _) = numOfPrefixes t

instance Zipper OrdZipperT2 where
  goLeft (t, es) =
    case S.viewr . getSeq . toForest $ t of
      r :> (x, l) -> (OrdTreeT2 l, Right (x, OrdTreeT2 . Forest $ r) : es)
      EmptyR      -> error "Tried to go left from a leaf"

  goRight (t, es) =
    case S.viewr . getSeq . toForest $ t of
      r :> (x, l) -> (OrdTreeT2 . Forest $ r, Left (x, OrdTreeT2 l) : es)
      EmptyR      -> error "Tried to go right from a leaf"

  goUp z = case z of (OrdTreeT2 l, Right (x, OrdTreeT2 (Forest r)) : es)
                       -> (OrdTreeT2 . Forest $ r |> (x, l), es)
                     (OrdTreeT2 (Forest r), Left (x, OrdTreeT2 l) : es)
                       -> (OrdTreeT2 . Forest $ r |> (x, l), es)
                     (_, [])
                       -> error "Tried to go up from the top"

  isLeaf (t, _) = case S.viewr . getSeq . toForest $ t of
                    EmptyR -> True
                    _      -> False

  getLabel (t, _) = case S.viewr . getSeq . toForest $ t of
                      _ :> (x, _) -> x
                      EmptyR      -> Nothing

  setLabel s z@(t, es) = case S.viewr . getSeq . toForest $ t of
                           r :> (_, l) -> (OrdTreeT2 . Forest $ r |> (s, l), es)
                           EmptyR      -> z

  size (t, _) = ordSize t + 18 * numOfPrefixes t

  insert (t, _) (_, es) = (t, es)

  delete (_, es) = (OrdTreeT2 . Forest $ S.empty, es)


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

  delSubtree tx ty = OrdTreeT3 . Forest $
                     helper (getSeq . toForest $ tx) (getSeq . toForest $ ty)
    where (S.viewl -> EmptyL)   `helper` _                     = S.empty
          t                     `helper` (S.viewl -> EmptyL)   = t
          (S.viewl -> hx :< lx) `helper` (S.viewl -> hy :< ly) =
            delEmptyNodeL $
            (x `delRoot` y, Forest $ rx `helper` ry) <| lx `helper` ly
            where (x, Forest rx) = hx
                  (y, Forest ry) = hy


type OrdZipperT3 = (,) OrdTreeT3
                   [Either (Maybe Int, OrdTreeT3) (Maybe Int, OrdTreeT3)]

instance {-# OVERLAPPING #-} Show OrdZipperT3 where
  show (t, _) = show t

instance IpRouter OrdZipperT3 where
  mkTable es           = (mkTable es, [])
  insEntry e (t, _)    = (insEntry e t, [])
  delEntry e (t, _)    = (delEntry e t, [])
  ipLookup e (t, _)    = ipLookup e t
  numOfPrefixes (t, _) = numOfPrefixes t

instance Zipper OrdZipperT3 where
  goLeft (t, es) =
    case S.viewl . getSeq . toForest $ t of
      (x, r) :< l -> (OrdTreeT3 . Forest $ l, Right (x, OrdTreeT3 r) : es)
      EmptyL      -> error "Tried to go left from a leaf"

  goRight (t, es) =
    case S.viewl . getSeq . toForest $ t of
      (x, r) :< l -> (OrdTreeT3 r, Left (x, OrdTreeT3 . Forest $ l) : es)
      EmptyL      -> error "Tried to go right from a leaf"

  goUp z = case z of (OrdTreeT3 (Forest l), Right (x, OrdTreeT3 r) : es)
                       -> (OrdTreeT3 . Forest $ (x, r) <| l, es)
                     (OrdTreeT3 r, Left (x, OrdTreeT3 (Forest l)) : es)
                       -> (OrdTreeT3 . Forest $ (x, r) <| l, es)
                     (_, [])
                       -> error "Tried to go up from the top"

  isLeaf (t, _) = case S.viewl . getSeq . toForest $ t of
                    EmptyL -> True
                    _      -> False

  getLabel (t, _) = case S.viewl . getSeq . toForest $ t of
                      (x, _) :< _ -> x
                      EmptyL      -> Nothing

  setLabel s z@(t, es) = case S.viewl . getSeq . toForest $ t of
                           (_, r) :< l -> (OrdTreeT3 . Forest $ (s, r) <| l, es)
                           EmptyL      -> z

  size (t, _) = ordSize t + 18 * numOfPrefixes t

  insert (t, _) (_, es) = (t, es)

  delete (_, es) = (OrdTreeT3 . Forest $ S.empty, es)


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

  delSubtree tx ty = OrdTreeT4 . Forest $
                     helper (getSeq . toForest $ tx) (getSeq . toForest $ ty)
    where (S.viewr -> EmptyR)   `helper` _                     = S.empty
          t                     `helper` (S.viewr -> EmptyR)   = t
          (S.viewr -> lx :> hx) `helper` (S.viewr -> ly :> hy) =
            delEmptyNodeR $
            lx `helper` ly |> (x `delRoot` y, Forest $ rx `helper` ry)
            where (x, Forest rx) = hx
                  (y, Forest ry) = hy


type OrdZipperT4 = (,) OrdTreeT4
                   [Either (Maybe Int, OrdTreeT4) (Maybe Int, OrdTreeT4)]

instance {-# OVERLAPPING #-} Show OrdZipperT4 where
  show (t, _) = show t

instance IpRouter OrdZipperT4 where
  mkTable es           = (mkTable es, [])
  insEntry e (t, _)    = (insEntry e t, [])
  delEntry e (t, _)    = (delEntry e t, [])
  ipLookup e (t, _)    = ipLookup e t
  numOfPrefixes (t, _) = numOfPrefixes t

instance Zipper OrdZipperT4 where
  goLeft (t, es) =
    case S.viewr . getSeq . toForest $ t of
      l :> (x, r) -> (OrdTreeT4 . Forest $ l, Right (x, OrdTreeT4 r) : es)
      EmptyR      -> error "Tried to go left from a leaf"

  goRight (t, es) =
    case S.viewr . getSeq . toForest $ t of
      l :> (x, r) -> (OrdTreeT4 r, Left (x, OrdTreeT4 . Forest $ l) : es)
      EmptyR      -> error "Tried to go right from a leaf"

  goUp z = case z of (OrdTreeT4 (Forest l), Right (x, OrdTreeT4 r) : es)
                       -> (OrdTreeT4 . Forest $ l |> (x, r), es)
                     (OrdTreeT4 r, Left (x, OrdTreeT4 (Forest l)) : es)
                       -> (OrdTreeT4 . Forest $ l |> (x, r), es)
                     (_, [])
                       -> error "Tried to go up from the top"

  isLeaf (t, _) = case S.viewr . getSeq . toForest $ t of
                    EmptyR -> True
                    _      -> False

  getLabel (t, _) = case S.viewr . getSeq . toForest $ t of
                      _ :> (x, _) -> x
                      EmptyR      -> Nothing

  setLabel s z@(t, es) = case S.viewr . getSeq . toForest $ t of
                           l :> (_, r) -> (OrdTreeT4 . Forest $ l |> (s, r), es)
                           EmptyR      -> z

  size (t, _) = ordSize t + 18 * numOfPrefixes t

  insert (t, _) (_, es) = (t, es)

  delete (_, es) = (OrdTreeT4 . Forest $ S.empty, es)
