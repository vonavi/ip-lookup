{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

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
       , BpT1
       , ordToBpT1
       , BpT2
       , ordToBpT2
       , BpT3
       , ordToBpT3
       , BpT4
       , ordToBpT4
       ) where

import           Control.Applicative       ((<|>))
import           Control.Monad             (guard, when)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.State
import           Data.Maybe

import           Data.OrdTree
import           Data.Paren

class Bp a where
  getList :: a -> [(Maybe Int, Paren)]

  bLeftChild   :: Int -> a -> Maybe Int
  bRightChild  :: Int -> a -> Maybe Int
  bParent      :: Int -> a -> Maybe Int
  bSubtreeSize :: Int -> a -> Int

  bSubtreeSize n bp = execState (bSubtreeSizeState n bp) 0

instance {-# OVERLAPS #-} Bp a => Show a where
  show = tail . concatMap helper . getList
    where helper (x, l) = if l == Open
                          then " " ++ show l ++ show x
                          else " " ++ show x ++ show l

bSubtreeSizeState :: Bp a => Int -> a -> State Int ()
bSubtreeSizeState n bp
  | n < 0 || n >= length (getList bp) = return ()
  | otherwise                         = do
      let l = bLeftChild n bp
          r = bRightChild n bp
      when (isJust l) $ bSubtreeSizeState (fromJust l) bp
      when (isJust r) $ bSubtreeSizeState (fromJust r) bp
      modify succ

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


newtype BpT1 = BpT1 [(Maybe Int, Paren)]

ordToBpT1 :: OrdTreeT1 -> BpT1
ordToBpT1 = BpT1 . ordToBp

instance Bp BpT1 where
  getList (BpT1 x) = x

  bLeftChild   = firstChild
  bRightChild  = nextSibling
  bParent n bp = prevSibling n bp <|> parent n bp


newtype BpT2 = BpT2 [(Maybe Int, Paren)]

ordToBpT2 :: OrdTreeT2 -> BpT2
ordToBpT2 = BpT2 . ordToBp

instance Bp BpT2 where
  getList (BpT2 x) = x

  bLeftChild   = lastChild
  bRightChild  = prevSibling
  bParent n bp = nextSibling n bp <|> parent n bp


newtype BpT3 = BpT3 [(Maybe Int, Paren)]

ordToBpT3 :: OrdTreeT3 -> BpT3
ordToBpT3 = BpT3 . ordToBp

instance Bp BpT3 where
  getList (BpT3 x) = x

  bLeftChild   = nextSibling
  bRightChild  = firstChild
  bParent n bp = prevSibling n bp <|> parent n bp


newtype BpT4 = BpT4 [(Maybe Int, Paren)]

ordToBpT4 :: OrdTreeT4 -> BpT4
ordToBpT4 = BpT4 . ordToBp

instance Bp BpT4 where
  getList (BpT4 x) = x

  bLeftChild   = prevSibling
  bRightChild  = lastChild
  bParent n bp = nextSibling n bp <|> parent n bp
