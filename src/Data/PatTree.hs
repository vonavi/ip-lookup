{-# LANGUAGE FlexibleInstances #-}

module Data.PatTree
       (
         PatTree(..)
       ) where

import Data.Word
import Data.Bits
import Data.Monoid
import Data.Maybe (isJust)
import Control.Applicative ((<|>))

import Data.IpRouter

data Node = Node { stride :: Int
                 , string :: Word32
                 , label  :: Maybe Int
                 } deriving (Show, Eq)

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)

instance Foldable Tree where
  foldMap _ Tip         = mempty
  foldMap f (Bin l x r) = foldMap f l <> f x <> foldMap f r

instance Monoid (Tree Node) where
  mempty = Tip

  tx `mappend` ty
    | tx == Tip                = ty
    | ty == Tip                = tx
    | kmin == kx && kmin == ky = let n = x { label = label x <|> label y }
                                 in Bin (lx <> ly) n (rx <> ry)
    | kmin < kx  && kmin == ky = if xright
                                 then Bin ly y (ry <> tx')
                                 else Bin (ly <> tx') y ry
    | kmin == kx && kmin < ky  = if yright
                                 then Bin lx x (rx <> ty')
                                 else Bin (lx <> ty') x rx
    | xright     && yright     = Bin Tip node (tx' <> ty')
    | not xright && yright     = Bin tx' node ty'
    | xright     && not yright = Bin ty' node tx'
    | otherwise                = Bin (tx' <> ty') node Tip
    where Bin lx x rx = tx
          Bin ly y ry = ty
          kx          = stride x
          ky          = stride y
          vx          = string x
          vy          = string y
          kmin        = minimum [ kx
                                , ky
                                , countLeadingZeros $ vx `xor` vy
                                ]
          node        = Node { stride = kmin
                             , string = vx `shiftL` (kx - kmin)
                             , label  = Nothing
                             }
          xright      = vx `testBit` (31 - kmin)
          yright      = vy `testBit` (31 - kmin)
          x'          = x { stride = kx - kmin - 1
                          , string = vx `shiftL` (kmin + 1)
                          }
          y'          = y { stride = ky - kmin - 1
                          , string = vy `shiftL` (kmin + 1)
                          }
          tx'         = Bin lx x' rx
          ty'         = Bin ly y' ry


newtype PatTree = PatTree { getTree :: Tree Node } deriving (Show, Eq)

instance Monoid PatTree where
  mempty        = PatTree mempty
  x `mappend` y = PatTree $ getTree x `mappend` getTree y

fromEntry :: Entry -> PatTree
fromEntry (Entry p n) = PatTree $ Bin Tip node Tip
  where Prefix (Address a) (Mask m) = p
        node                        = Node { stride = m
                                           , string = a
                                           , label  = Just n
                                           }

instance IpRouter PatTree where
  mkTable = foldr insEntry mempty

  insEntry = mappend . fromEntry

  delEntry = undefined

  ipLookup = undefined

  numOfPrefixes = getSum . foldMap isPrefix . getTree
    where isPrefix x = if (isJust . label) x then Sum 1 else Sum 0
