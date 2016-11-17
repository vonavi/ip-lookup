{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Succinct.Dfuds
  (
    parent
  , firstChild
  , lastChild
  , nextSibling
  , prevSibling
  , subtreeSize
  , degree
  , child
  , DfudsT1
  , ordToDfudsT1
  , DfudsT2
  , ordToDfudsT2
  , DfudsT3
  , ordToDfudsT3
  , DfudsT4
  , ordToDfudsT4
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Maybe

import           Data.Succinct.Paren
import           Data.Trees.OrdTree

class Dfuds a where
  getList :: a -> [(Maybe Int, [Paren])]

  bLeftChild   :: Int -> a -> Maybe Int
  bRightChild  :: Int -> a -> Maybe Int
  bParent      :: Int -> a -> Maybe Int
  bSubtreeSize :: Int -> a -> Int

  bSubtreeSize n dfuds = execState (bSubtreeSizeState n dfuds) 0

instance {-# OVERLAPS #-} Dfuds a => Show a where
  show = tail . concatMap ((++) " " . show) . getList

bSubtreeSizeState :: Dfuds a => Int -> a -> State Int ()
bSubtreeSizeState n dfuds
  | n < 0 || n >= length (getList dfuds) = return ()
  | otherwise                            = do
      let l = bLeftChild n dfuds
          r = bRightChild n dfuds
      when (isJust l) $ bSubtreeSizeState (fromJust l) dfuds
      when (isJust r) $ bSubtreeSizeState (fromJust r) dfuds
      modify succ

toParens :: Dfuds a => a -> [Paren]
toParens = concatMap snd . getList

parent :: Dfuds a => Int -> a -> Maybe Int
parent n dfuds = do no <- findOpen (pred n) ps
                    Just . succ $ selectClose (rankClose no ps) ps
  where ps = toParens dfuds

firstChild :: Dfuds a => Int -> a -> Maybe Int
firstChild n = child n 0

lastChild :: Dfuds a => Int -> a -> Maybe Int
lastChild n dfuds = Just succ <*> findClose n (toParens dfuds)

nextSibling :: Dfuds a => Int -> a -> Maybe Int
nextSibling n dfuds = do no <- findOpen (pred n) ps
                         Just succ <*> findClose (pred no) ps
  where ps = toParens dfuds

prevSibling :: Dfuds a => Int -> a -> Maybe Int
prevSibling n dfuds = do no <- findOpen (pred n) ps
                         Just succ <*> findClose (succ no) ps
  where ps = toParens dfuds

subtreeSize :: Dfuds a => Int -> a -> Int
subtreeSize n dfuds
  | n < 0 || n >= length ps = 0
  | isClose n ps            = 1
  | otherwise               = succ $ (nc - n) `div` 2
  where ps = toParens dfuds
        nc = fromMaybe (length ps) $ do ne <- enclose n ps
                                        findClose ne ps

degree :: Dfuds a => Int -> a -> Int
degree n dfuds
  | not (isOpen n ps) = 0
  | otherwise         = nc - n
  where ps = toParens dfuds
        nc = selectClose (succ $ rankClose n ps) ps

child :: Dfuds a => Int -> Int -> a -> Maybe Int
child n i dfuds
  | i >= d    = Nothing
  | otherwise = do no <- findClose (n + d - i - 1) ps
                   Just $ succ no
  where ps = toParens dfuds
        d  = degree n dfuds


newtype DfudsT1 = DfudsT1 [(Maybe Int, [Paren])]

ordToDfudsT1 :: OrdTreeT1 -> DfudsT1
ordToDfudsT1 = DfudsT1 . ordToDfuds

instance Dfuds DfudsT1 where
  getList (DfudsT1 x) = x

  bLeftChild      = firstChild
  bRightChild     = nextSibling
  bParent n dfuds = prevSibling n dfuds <|> parent n dfuds


newtype DfudsT2 = DfudsT2 [(Maybe Int, [Paren])]

ordToDfudsT2 :: OrdTreeT2 -> DfudsT2
ordToDfudsT2 = DfudsT2 . ordToDfuds

instance Dfuds DfudsT2 where
  getList (DfudsT2 x) = x

  bLeftChild      = lastChild
  bRightChild     = prevSibling
  bParent n dfuds = nextSibling n dfuds <|> parent n dfuds


newtype DfudsT3 = DfudsT3 [(Maybe Int, [Paren])]

ordToDfudsT3 :: OrdTreeT3 -> DfudsT3
ordToDfudsT3 = DfudsT3 . ordToDfuds

instance Dfuds DfudsT3 where
  getList (DfudsT3 x) = x

  bLeftChild      = nextSibling
  bRightChild     = firstChild
  bParent n dfuds = prevSibling n dfuds <|> parent n dfuds


newtype DfudsT4 = DfudsT4 [(Maybe Int, [Paren])]

ordToDfudsT4 :: OrdTreeT4 -> DfudsT4
ordToDfudsT4 = DfudsT4 . ordToDfuds

instance Dfuds DfudsT4 where
  getList (DfudsT4 x) = x

  bLeftChild      = prevSibling
  bRightChild     = lastChild
  bParent n dfuds = nextSibling n dfuds <|> parent n dfuds
