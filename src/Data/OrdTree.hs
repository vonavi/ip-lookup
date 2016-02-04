{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Data.OrdTree
       (
         OrdTreeT1
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
newtype OrdTreeT1 = OrdTreeT1 (Forest (Last Int))

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

  lookupState bs (OrdTreeT1 (Forest x)) = helper bs x
    where helper _      []                  = return ()
          helper []     ((x, _) : _)        = modify (`mappend` x)
          helper (b:bs) ((x, Forest l) : r) = do
            modify (`mappend` x)
            helper bs $ if b then r else l
