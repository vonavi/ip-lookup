{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             OverlappingInstances #-}

module Data.Bp
       (
         parent
       , child
       ) where

import Data.Monoid
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State

import Data.Paren
import Data.OrdTree

class Bp a where
  getList :: a -> [(Last Int, Paren)]

instance Bp a => Show a where
  show = concatMap helper . getList
    where helper (x, l) = if l == Open
                          then show l ++ show (getLast x) ++ " "
                          else show (getLast x) ++ show l ++ " "

toParens :: Bp a => a -> [Paren]
toParens = helper . getList
  where helper []            = []
        helper ((_, x) : xs) = x : helper xs

parent :: Bp a => Int -> a -> Maybe Int
parent n bp
  | isOpen n ps = enclose n ps
  | otherwise   = do n' <- findOpen n ps
                     enclose n' ps
  where ps = toParens bp

childStateT :: Int -> [Paren] -> StateT [Int] Maybe ()
childStateT n ps
  | not (isClose n ps) = return ()
  | otherwise          = do no <- lift $ findOpen n ps
                            modify (no :)
                            childStateT (pred no) ps

child :: Bp a => Int -> Int -> a -> Maybe Int
child n i bp
  | not (isClose n ps) = do nc <- findClose n ps
                            child nc i bp
  | otherwise          = do
      iList <- execStateT (childStateT (pred n) ps) []
      guard $ i >= 0 && i < length iList
      Just $ iList !! i
  where ps = toParens bp
