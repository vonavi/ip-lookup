{-# LANGUAGE FlexibleInstances #-}

module Data.PatTree
       (
         PatNode(..)
       , Tree(..)
       , PatTree(..)
       , gammaSize
       , deltaSize
       ) where

import Data.Word
import Data.Bits
import Data.Monoid
import Data.Maybe (isJust)
import Control.Applicative ((<|>))

import Data.IpRouter

data PatNode = PatNode { stride :: Int
                       , string :: Word32
                       , label  :: Maybe Int
                       } deriving (Show, Eq)

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)

instance Foldable Tree where
  foldMap _ Tip         = mempty
  foldMap f (Bin l x r) = foldMap f l <> f x <> foldMap f r

instance Monoid (Tree PatNode) where
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
          node        = PatNode { stride = kmin
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


newtype PatTree = PatTree { getTree :: Tree PatNode } deriving (Show, Eq)

instance Monoid PatTree where
  mempty        = PatTree mempty
  x `mappend` y = PatTree $ getTree x `mappend` getTree y

fromEntry :: Entry -> PatTree
fromEntry (Entry p n) = PatTree $ Bin Tip node Tip
  where Prefix (Address a) (Mask m) = p
        node                        = PatNode { stride = m
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


gammaSize :: PatTree -> Int
gammaSize = getSum . foldMap nodeSize . getTree
  where nodeSize PatNode { stride = k } =
          {- The node size is built from the following parts:
             parenthesis expression (2 bits), internal prefix (1 bit),
             gamma-code of stride (the stride should be increased by
             one), and node string. -}
          Sum $ 3 + gammaCodeSize (k + 1) + k

gammaCodeSize :: Int -> Int
gammaCodeSize x = 2 * k + 1
  where k = floor . logBase (2 :: Double) . fromIntegral $ x

deltaSize :: PatTree -> Int
deltaSize = getSum . foldMap nodeSize . getTree
  where nodeSize PatNode { stride = k } =
          {- The node size is built from the following parts:
             parenthesis expression (2 bits), internal prefix (1 bit),
             delta-code of stride (the stride should be increased by
             one), and node string. -}
          Sum $ 3 + deltaCodeSize (k + 1) + k

deltaCodeSize :: Int -> Int
deltaCodeSize x = 2 * l + k + 1
  where k = floor . logBase (2 :: Double) . fromIntegral $ x
        l = floor . logBase (2 :: Double) . fromIntegral . succ $ k
