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
  | not (isOpen n ps) = return ()
  | otherwise         = do n' <- lift $ findClose n ps
                           modify (n :)
                           childStateT (succ n') ps

child :: Bp a => Int -> Int -> a -> Maybe Int
child n i bp
  | not (isOpen n ps) = do n' <- findOpen n ps
                           child n' i bp
  | otherwise         = do
      iList <- execStateT (childStateT (succ n) ps) []
      let len = length iList
      guard $ i >= 0 && i < len
      Just $ iList !! (len - i - 1)
  where ps = toParens bp
