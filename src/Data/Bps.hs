{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Bps
  (
    parent
  , firstChild
  , lastChild
  , nextSibling
  , prevSibling
  , subtreeSize
  , degree
  , child
  , BpsT1
  , ordToBpsT1
  , BpsT2
  , ordToBpsT2
  , BpsT3
  , ordToBpsT3
  , BpsT4
  , ordToBpsT4
  ) where

import           Control.Applicative       ((<|>))
import           Control.Monad             (guard, when)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.State
import           Data.Maybe

import           Data.OrdTree
import           Data.Paren

class Bps a where
  getList      :: a -> [(Maybe Int, Paren)]
  bLeftChild   :: Int -> a -> Maybe Int
  bRightChild  :: Int -> a -> Maybe Int
  bParent      :: Int -> a -> Maybe Int
  bSubtreeSize :: Int -> a -> Int

  bSubtreeSize n bps = execState (bSubtreeSizeState n bps) 0

instance {-# OVERLAPS #-} Bps a => Show a where
  show = tail . concatMap helper . getList
    where helper (x, l) = if l == Open
                          then " " ++ show l ++ show x
                          else " " ++ show x ++ show l

bSubtreeSizeState :: Bps a => Int -> a -> State Int ()
bSubtreeSizeState n bps
  | n < 0 || n >= length (getList bps) = return ()
  | otherwise                          = do
      let l = bLeftChild n bps
          r = bRightChild n bps
      when (isJust l) $ bSubtreeSizeState (fromJust l) bps
      when (isJust r) $ bSubtreeSizeState (fromJust r) bps
      modify succ

toParens :: Bps a => a -> [Paren]
toParens = helper . getList
  where helper []            = []
        helper ((_, x) : xs) = x : helper xs

parent :: Bps a => Int -> a -> Maybe Int
parent n bps
  | isOpen n ps = enclose n ps
  | otherwise   = do n' <- findOpen n ps
                     enclose n' ps
  where ps = toParens bps

firstChild :: Bps a => Int -> a -> Maybe Int
firstChild n bps
  | not (isOpen n ps)  = do no <- findOpen n ps
                            firstChild no bps
  | not (isOpen nx ps) = Nothing
  | otherwise          = Just nx
  where ps = toParens bps
        nx = succ n

lastChild :: Bps a => Int -> a -> Maybe Int
lastChild n bps
  | not (isClose n ps) = do nc <- findClose n ps
                            lastChild nc bps
  | otherwise          = findOpen (pred n) ps
  where ps = toParens bps

nextSibling :: Bps a => Int -> a -> Maybe Int
nextSibling n bps
  | not (isClose n ps) = do nc <- findClose n ps
                            nextSibling nc bps
  | not (isOpen nx ps) = Nothing
  | otherwise          = Just nx
  where ps = toParens bps
        nx = succ n

prevSibling :: Bps a => Int -> a -> Maybe Int
prevSibling n bps
  | not (isOpen n ps) = do no <- findOpen n ps
                           prevSibling no bps
  | otherwise         = findOpen (pred n) ps
  where ps = toParens bps

subtreeSize :: Bps a => Int -> a -> Int
subtreeSize n bps = fromMaybe 0 . helper $ n
  where ps = toParens bps
        helper x
          | not (isClose x ps) = do xc <- findClose x ps
                                    Just $ (xc - x + 1) `div` 2
          | otherwise          = do xo <- findOpen x ps
                                    Just $ (x - xo + 1) `div` 2

childStateT :: Int -> [Paren] -> StateT [Int] Maybe ()
childStateT n ps
  | not (isClose n ps) = return ()
  | otherwise          = do no <- lift $ findOpen n ps
                            modify (no :)
                            childStateT (pred no) ps

degree :: Bps a => Int -> a -> Int
degree n bps = length . fromMaybe [] . helper $ n
  where ps = toParens bps
        helper x
          | not (isClose x ps) = do xc <- findClose x ps
                                    helper xc
          | otherwise          = execStateT (childStateT (pred x) ps) []

child :: Bps a => Int -> Int -> a -> Maybe Int
child n i bps
  | not (isClose n ps) = do nc <- findClose n ps
                            child nc i bps
  | otherwise          = do
      iList <- execStateT (childStateT (pred n) ps) []
      guard $ i >= 0 && i < length iList
      Just $ iList !! i
  where ps = toParens bps


newtype BpsT1 = BpsT1 [(Maybe Int, Paren)]

ordToBpsT1 :: OrdTreeT1 -> BpsT1
ordToBpsT1 = BpsT1 . ordToBps

instance Bps BpsT1 where
  getList (BpsT1 x) = x
  bLeftChild        = firstChild
  bRightChild       = nextSibling
  bParent n bps     = prevSibling n bps <|> parent n bps


newtype BpsT2 = BpsT2 [(Maybe Int, Paren)]

ordToBpsT2 :: OrdTreeT2 -> BpsT2
ordToBpsT2 = BpsT2 . ordToBps

instance Bps BpsT2 where
  getList (BpsT2 x) = x
  bLeftChild        = lastChild
  bRightChild       = prevSibling
  bParent n bps     = nextSibling n bps <|> parent n bps


newtype BpsT3 = BpsT3 [(Maybe Int, Paren)]

ordToBpsT3 :: OrdTreeT3 -> BpsT3
ordToBpsT3 = BpsT3 . ordToBps

instance Bps BpsT3 where
  getList (BpsT3 x) = x
  bLeftChild        = nextSibling
  bRightChild       = firstChild
  bParent n bps     = prevSibling n bps <|> parent n bps


newtype BpsT4 = BpsT4 [(Maybe Int, Paren)]

ordToBpsT4 :: OrdTreeT4 -> BpsT4
ordToBpsT4 = BpsT4 . ordToBps

instance Bps BpsT4 where
  getList (BpsT4 x) = x
  bLeftChild        = prevSibling
  bRightChild       = lastChild
  bParent n bps     = nextSibling n bps <|> parent n bps
