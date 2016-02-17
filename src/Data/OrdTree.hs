{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Data.OrdTree
       (
         ordToBp
       , ordToDfuds
       , OrdTreeT1
       , OrdTreeT2
       , OrdTreeT3
       , OrdTreeT4
       ) where

import Data.Monoid
import Control.Monad.State

import Data.IpRouter
import Data.Paren

newtype Forest a = Forest { getNodes :: [(a, Forest a)] } deriving Show

class OrdTree t where
  toForest    :: t      -> Forest (Last Int)
  isEmpty     :: t      -> Bool
  fromEntry   :: Entry  -> t
  lookupState :: [Bool] -> t -> State (Last Int) ()

  size          :: t        -> Int
  bRoot         :: t        -> Last Int
  bLeftSubtree  :: t        -> t
  bRightSubtree :: t        -> t
  bInsertRoot   :: Last Int -> t -> t -> t

  isEmpty = null . getNodes . toForest

  size x = pred $ execState (helper . toForest $ x) 0
    where helper :: Forest (Last Int) -> State Int ()
          helper (Forest xs) = do
            mapM_ (helper . snd) xs
            modify succ

instance (Monoid a, OrdTree a) => IpRouter a where
  ipInsert e t = t `mappend` fromEntry e
  ipLookup a t = getLast $
                 execState (lookupState (addrBits a) t) (Last Nothing)

forToBp :: Forest a -> [(a, Paren)]
forToBp = helper . getNodes
  where helper = concatMap (\(x, Forest l)
                            -> [(x, Open)] ++ helper l ++ [(x, Close)])

ordToBp :: OrdTree a => a -> [(Last Int, Paren)]
ordToBp x = [(Last Nothing, Open)] ++ forToBp (toForest x) ++
            [(Last Nothing, Close)]

forToDfuds :: Forest a -> [(a, [Paren])]
forToDfuds (Forest x) = helper x
  where helper []                  = []
        helper ((x, Forest l) : r) =
          let p = replicate (length l) Open ++ [Close]
          in (x, p) : helper l ++ helper r

ordToDfuds :: OrdTree a => a -> [(Last Int, [Paren])]
ordToDfuds x = (Last Nothing, ps) : forToDfuds f
  where f  = toForest x
        ps = replicate (length $ getNodes f) Open ++ [Close]


newtype OrdTreeT1 = OrdTreeT1 (Forest (Last Int)) deriving Show

instance Monoid OrdTreeT1 where
  mempty = OrdTreeT1 $ Forest []

  x `mappend` y = OrdTreeT1 $ Forest $
                  helper (getNodes $ toForest x) (getNodes $ toForest y)
    where helper []                   ys                   = ys
          helper xs                   []                   = xs
          helper ((a, Forest x) : xs) ((b, Forest y) : ys) =
            (a `mappend` b, Forest (helper x y)) : helper xs ys

instance OrdTree OrdTreeT1 where
  toForest (OrdTreeT1 x) = x

  fromEntry (Entry p n) = OrdTreeT1 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Last Int)
          helper []     = Forest [(Last (Just n), Forest [])]
          helper (b:bs) = Forest $ if b
                                   then (Last Nothing, Forest []) :
                                        getNodes (helper bs)
                                   else [(Last Nothing, helper bs)]

  lookupState bs = helper bs . getNodes . toForest
    where helper _      []                  = return ()
          helper []     ((x, _) : _)        = modify (`mappend` x)
          helper (b:bs) ((x, Forest r) : l) = do
            modify (`mappend` x)
            helper bs $ if b then l else r

  bRoot = helper . getNodes . toForest
    where helper []           = Last Nothing
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


newtype OrdTreeT2 = OrdTreeT2 (Forest (Last Int)) deriving Show

instance Monoid OrdTreeT2 where
  mempty = OrdTreeT2 $ Forest []

  x `mappend` y = OrdTreeT2 $ Forest $
                  helper (getNodes $ toForest x) (getNodes $ toForest y)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a `mappend` b, Forest (helper x y))]
            where (a, Forest x) = last xs
                  (b, Forest y) = last ys

instance OrdTree OrdTreeT2 where
  toForest (OrdTreeT2 x) = x

  fromEntry (Entry p n) = OrdTreeT2 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Last Int)
          helper []     = Forest [(Last (Just n), Forest [])]
          helper (b:bs) = Forest $ if b
                                   then getNodes (helper bs) ++
                                        [(Last Nothing, Forest [])]
                                   else [(Last Nothing, helper bs)]

  lookupState bs = helper bs . getNodes . toForest
    where helper _      [] = return ()
          helper []     xs = let x = fst . last $ xs
                             in modify (`mappend` x)
          helper (b:bs) xs = do
            modify (`mappend` x)
            helper bs $ if b then l else r
              where (x, Forest r) = last xs
                    l             = init xs

  bRoot = helper . getNodes . toForest
    where helper [] = Last Nothing
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


newtype OrdTreeT3 = OrdTreeT3 (Forest (Last Int)) deriving Show

instance Monoid OrdTreeT3 where
  mempty = OrdTreeT3 $ Forest []

  x `mappend` y = OrdTreeT3 $ Forest $
                  helper (getNodes $ toForest x) (getNodes $ toForest y)
    where helper []                   ys                   = ys
          helper xs                   []                   = xs
          helper ((a, Forest x) : xs) ((b, Forest y) : ys) =
            (a `mappend` b, Forest (helper x y)) : helper xs ys

instance OrdTree OrdTreeT3 where
  toForest (OrdTreeT3 x) = x

  fromEntry (Entry p n) = OrdTreeT3 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Last Int)
          helper []     = Forest [(Last (Just n), Forest [])]
          helper (b:bs) = Forest $ if b
                                   then [(Last Nothing, helper bs)]
                                   else (Last Nothing, Forest []) :
                                        getNodes (helper bs)

  lookupState bs = helper bs . getNodes . toForest
    where helper _      []                  = return ()
          helper []     ((x, _) : _)        = modify (`mappend` x)
          helper (b:bs) ((x, Forest l) : r) = do
            modify (`mappend` x)
            helper bs $ if b then l else r

  bRoot = helper . getNodes . toForest
    where helper []           = Last Nothing
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


newtype OrdTreeT4 = OrdTreeT4 (Forest (Last Int)) deriving Show

instance Monoid OrdTreeT4 where
  mempty = OrdTreeT4 $ Forest []

  x `mappend` y = OrdTreeT4 $ Forest $
                  helper (getNodes $ toForest x) (getNodes $ toForest y)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a `mappend` b, Forest (helper x y))]
            where (a, Forest x) = last xs
                  (b, Forest y) = last ys

instance OrdTree OrdTreeT4 where
  toForest (OrdTreeT4 x) = x

  fromEntry (Entry p n) = OrdTreeT4 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Last Int)
          helper []     = Forest [(Last (Just n), Forest [])]
          helper (b:bs) = Forest $ if b
                                   then [(Last Nothing, helper bs)]
                                   else getNodes (helper bs) ++
                                        [(Last Nothing, Forest [])]

  lookupState bs = helper bs . getNodes . toForest
    where helper _      [] = return ()
          helper []     xs = let x = fst . last $ xs
                             in modify (`mappend` x)
          helper (b:bs) xs = do
            modify (`mappend` x)
            helper bs $ if b then l else r
              where (x, Forest l) = last xs
                    r             = init xs

  bRoot = helper . getNodes . toForest
    where helper [] = Last Nothing
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
