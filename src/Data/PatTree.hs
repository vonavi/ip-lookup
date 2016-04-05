{-# LANGUAGE FlexibleInstances #-}

module Data.PatTree
       (
         PatNode(..)
       , Tree(..)
       , PatTree(..)
       , fromEntry
       , gammaSize
       , deltaSize
       , isEmpty
       , collapse
       , delSubtree
       , bRoot
       , bLeftSubtree
       , bRightSubtree
       , bInsertRoot
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
                       } deriving Show

instance Eq PatNode where
  x == y = kx == ky && n >= kx && label x == label y
    where kx = stride x
          ky = stride y
          n  = countLeadingZeros $ string x `xor` string y


data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap _ Tip         = Tip
  fmap f (Bin l x r) = Bin (fmap f l) (f x) (fmap f r)

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

isEmpty :: PatTree -> Bool
isEmpty (PatTree Tip) = True
isEmpty _             = False

collapse :: PatTree -> PatTree
collapse = undefined

delSubtree :: PatTree -> PatTree -> PatTree
delSubtree = undefined

bRoot :: PatTree -> Maybe Int
bRoot (PatTree Tip) = Nothing
bRoot (PatTree (Bin _ x _))
  | stride x == 0 = label x
  | otherwise     = Nothing

bLeftSubtree :: PatTree -> PatTree
bLeftSubtree (PatTree Tip) = PatTree Tip
bLeftSubtree (PatTree (Bin l x r))
  | k == 0         = PatTree l
  | v `testBit` 31 = PatTree Tip
  | otherwise      = PatTree $ Bin l x' r
  where k  = stride x
        v  = string x
        x' = x { stride = pred k
               , string = v `shiftL` 1
               }

bRightSubtree :: PatTree -> PatTree
bRightSubtree (PatTree Tip) = PatTree Tip
bRightSubtree (PatTree (Bin l x r))
  | k == 0         = PatTree r
  | v `testBit` 31 = PatTree $ Bin l x' r
  | otherwise      = PatTree Tip
  where k  = stride x
        v  = string x
        x' = x { stride = pred k
               , string = v `shiftL` 1
               }

bInsertRoot :: Maybe Int -> PatTree -> PatTree -> PatTree
bInsertRoot x l r
  | isJust x  = PatTree $ Bin Tip node Tip <> lsub <> rsub
  | otherwise = PatTree $ lsub <> rsub
  where node = PatNode { stride = 0
                       , string = 0
                       , label  = x
                       }
        lsub = case getTree l of
                Tip          -> Tip
                Bin ll xl rl ->
                  let xl' = xl { stride = succ $ stride xl
                               , string = (`clearBit` 31) . (`shiftR` 1) $
                                          string xl
                               }
                  in Bin ll xl' rl
        rsub = case getTree r of
                Tip          -> Tip
                Bin lr xr rr ->
                  let xr' = xr { stride = succ $ stride xr
                               , string = (`setBit` 31) . (`shiftR` 1) $
                                          string xr
                               }
                  in Bin lr xr' rr
