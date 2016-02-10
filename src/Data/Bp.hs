{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             OverlappingInstances #-}

module Data.Bp
       (
         parent
       , firstChild
       , lastChild
       , nextSibling
       , prevSibling
       , subtreeSize
       , degree
       , child
       ) where

import Data.Monoid
import Data.Maybe (fromMaybe)
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

firstChild :: Bp a => Int -> a -> Maybe Int
firstChild n bp
  | not (isOpen n ps)  = do no <- findOpen n ps
                            firstChild no bp
  | not (isOpen nx ps) = Nothing
  | otherwise          = Just nx
  where ps = toParens bp
        nx = succ n

lastChild :: Bp a => Int -> a -> Maybe Int
lastChild n bp
  | not (isClose n ps) = do nc <- findClose n ps
                            lastChild nc bp
  | otherwise          = findOpen (pred n) ps
  where ps = toParens bp

nextSibling :: Bp a => Int -> a -> Maybe Int
nextSibling n bp
  | not (isClose n ps) = do nc <- findClose n ps
                            nextSibling nc bp
  | not (isOpen nx ps) = Nothing
  | otherwise          = Just nx
  where ps = toParens bp
        nx = succ n

prevSibling :: Bp a => Int -> a -> Maybe Int
prevSibling n bp
  | not (isOpen n ps) = do no <- findOpen n ps
                           prevSibling no bp
  | otherwise         = findOpen (pred n) ps
  where ps = toParens bp

subtreeSize :: Bp a => Int -> a -> Int
subtreeSize n bp = fromMaybe 0 . helper $ n
  where ps = toParens bp
        helper n
          | not (isClose n ps) = do nc <- findClose n ps
                                    Just $ (nc - n + 1) `div` 2
          | otherwise          = do no <- findOpen n ps
                                    Just $ (n - no + 1) `div` 2

childStateT :: Int -> [Paren] -> StateT [Int] Maybe ()
childStateT n ps
  | not (isClose n ps) = return ()
  | otherwise          = do no <- lift $ findOpen n ps
                            modify (no :)
                            childStateT (pred no) ps

degree :: Bp a => Int -> a -> Int
degree n bp = length . fromMaybe [] . helper $ n
  where ps = toParens bp
        helper n
          | not (isClose n ps) = do nc <- findClose n ps
                                    helper nc
          | otherwise          = execStateT (childStateT (pred n) ps) []

child :: Bp a => Int -> Int -> a -> Maybe Int
child n i bp
  | not (isClose n ps) = do nc <- findClose n ps
                            child nc i bp
  | otherwise          = do
      iList <- execStateT (childStateT (pred n) ps) []
      guard $ i >= 0 && i < length iList
      Just $ iList !! i
  where ps = toParens bp
