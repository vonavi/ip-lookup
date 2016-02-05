{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Data.OrdTree
       (
         OrdTreeT1
       , OrdTreeT2
       , OrdTreeT3
       , OrdTreeT4
       ) where

import Data.Monoid
import Control.Monad.State

import Data.IpRouter

class OrdTree t where
  fromEntry   :: Entry  -> t
  lookupState :: [Bool] -> t -> State (Last Int) ()

instance (Monoid a, OrdTree a) => IpRouter a where
  ipInsert e t = t `mappend` fromEntry e
  ipLookup a t = getLast $
                 execState (lookupState (addrBits a) t) (Last Nothing)

newtype Forest a = Forest { getNodes :: [(a, Forest a)] } deriving Show


newtype OrdTreeT1 = OrdTreeT1 (Forest (Last Int)) deriving Show

instance Monoid OrdTreeT1 where
  mempty = OrdTreeT1 $ Forest []

  (OrdTreeT1 x) `mappend` (OrdTreeT1 y) = OrdTreeT1 $ Forest $
                                          helper (getNodes x) (getNodes y)
    where helper []                   ys                   = ys
          helper xs                   []                   = xs
          helper ((a, Forest x) : xs) ((b, Forest y) : ys) =
            (a `mappend` b, Forest (helper x y)) : helper xs ys

instance OrdTree OrdTreeT1 where
  fromEntry (Entry p n) = OrdTreeT1 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Last Int)
          helper []     = Forest [(Last (Just n), Forest [])]
          helper (b:bs) = Forest $ if b
                                   then (Last Nothing, Forest []) :
                                        getNodes (helper bs)
                                   else [(Last Nothing, helper bs)]

  lookupState bs (OrdTreeT1 (Forest a)) = helper bs a
    where helper _      []                  = return ()
          helper []     ((x, _) : _)        = modify (`mappend` x)
          helper (b:bs) ((x, Forest r) : l) = do
            modify (`mappend` x)
            helper bs $ if b then l else r


newtype OrdTreeT2 = OrdTreeT2 (Forest (Last Int)) deriving Show

instance Monoid OrdTreeT2 where
  mempty = OrdTreeT2 $ Forest []

  (OrdTreeT2 x) `mappend` (OrdTreeT2 y) = OrdTreeT2 $ Forest $
                                          helper (getNodes x) (getNodes y)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a `mappend` b, Forest (helper x y))]
            where (a, Forest x) = last xs
                  (b, Forest y) = last ys

instance OrdTree OrdTreeT2 where
  fromEntry (Entry p n) = OrdTreeT2 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Last Int)
          helper []     = Forest [(Last (Just n), Forest [])]
          helper (b:bs) = Forest $ if b
                                   then getNodes (helper bs) ++
                                        [(Last Nothing, Forest [])]
                                   else [(Last Nothing, helper bs)]

  lookupState bs (OrdTreeT2 (Forest a)) = helper bs a
    where helper _      [] = return ()
          helper []     xs = let x = fst . last $ xs
                             in modify (`mappend` x)
          helper (b:bs) xs = do
            modify (`mappend` x)
            helper bs $ if b then l else r
              where (x, Forest r) = last xs
                    l             = init xs


newtype OrdTreeT3 = OrdTreeT3 (Forest (Last Int)) deriving Show

instance Monoid OrdTreeT3 where
  mempty = OrdTreeT3 $ Forest []

  (OrdTreeT3 x) `mappend` (OrdTreeT3 y) = OrdTreeT3 $ Forest $
                                          helper (getNodes x) (getNodes y)
    where helper []                   ys                   = ys
          helper xs                   []                   = xs
          helper ((a, Forest x) : xs) ((b, Forest y) : ys) =
            (a `mappend` b, Forest (helper x y)) : helper xs ys

instance OrdTree OrdTreeT3 where
  fromEntry (Entry p n) = OrdTreeT3 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Last Int)
          helper []     = Forest [(Last (Just n), Forest [])]
          helper (b:bs) = Forest $ if b
                                   then [(Last Nothing, helper bs)]
                                   else (Last Nothing, Forest []) :
                                        getNodes (helper bs)

  lookupState bs (OrdTreeT3 (Forest a)) = helper bs a
    where helper _      []                  = return ()
          helper []     ((x, _) : _)        = modify (`mappend` x)
          helper (b:bs) ((x, Forest l) : r) = do
            modify (`mappend` x)
            helper bs $ if b then l else r


newtype OrdTreeT4 = OrdTreeT4 (Forest (Last Int)) deriving Show

instance Monoid OrdTreeT4 where
  mempty = OrdTreeT4 $ Forest []

  (OrdTreeT4 x) `mappend` (OrdTreeT4 y) = OrdTreeT4 $ Forest $
                                          helper (getNodes x) (getNodes y)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a `mappend` b, Forest (helper x y))]
            where (a, Forest x) = last xs
                  (b, Forest y) = last ys

instance OrdTree OrdTreeT4 where
  fromEntry (Entry p n) = OrdTreeT4 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Last Int)
          helper []     = Forest [(Last (Just n), Forest [])]
          helper (b:bs) = Forest $ if b
                                   then [(Last Nothing, helper bs)]
                                   else getNodes (helper bs) ++
                                        [(Last Nothing, Forest [])]

  lookupState bs (OrdTreeT4 (Forest a)) = helper bs a
    where helper _      [] = return ()
          helper []     xs = let x = fst . last $ xs
                             in modify (`mappend` x)
          helper (b:bs) xs = do
            modify (`mappend` x)
            helper bs $ if b then l else r
              where (x, Forest l) = last xs
                    r             = init xs
