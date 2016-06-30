{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.PaCo2TreeM
       (
         PaCo2Node(..)
       , Tree(..)
       , PaCo2Tree(..)
       ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Bits
import           Data.Maybe          (isJust)
import           Data.Monoid
import           Data.Word

import           Data.IpRouter

data PaCo2Node = PaCo2Node { skip   :: Int
                           , string :: Word32
                           , label  :: Maybe Int
                           }
               deriving Show

instance Eq PaCo2Node where
  x == y = kx == ky && n >= kx && label x == label y
    where kx = skip x
          ky = skip y
          n  = countLeadingZeros $ string x `xor` string y


data Tree b a = Leaf (Maybe b) | Bin a (Tree b a) (Tree b a)
              deriving (Show, Eq)

instance Functor (Tree b) where
  fmap _ (Leaf x)    = Leaf x
  fmap f (Bin x l r) = Bin (f x) (fmap f l) (fmap f r)

instance Foldable (Tree b) where
  foldMap _ (Leaf _)    = mempty
  foldMap f (Bin x l r) = f x <> foldMap f l <> foldMap f r

emptyBranch :: Tree b PaCo2Node
emptyBranch = Bin emptyRoot (Leaf Nothing) (Leaf Nothing)
  where emptyRoot = PaCo2Node { skip   = 0
                              , string = 0
                              , label  = Nothing
                              }

balanceRoot :: Tree b PaCo2Node -> Tree b PaCo2Node
balanceRoot t@(Leaf _)                  = t
balanceRoot t@(Bin _ (Leaf _) (Leaf _)) = t
balanceRoot (Bin x (Leaf Nothing) r)    = Bin x emptyBranch r
balanceRoot (Bin _ (Leaf _) _)          = error "Cannot balance the 2-tree"
balanceRoot (Bin x l (Leaf Nothing))    = Bin x l emptyBranch
balanceRoot (Bin _ _ (Leaf _))          = error "Cannot balance the 2-tree"
balanceRoot t                           = t

resizeRoot :: Int -> Tree b PaCo2Node -> Tree b PaCo2Node
resizeRoot k (Bin x l r) | k < kx = tree
  where kx    = skip x
        vx    = string x
        xhead = PaCo2Node { skip   = k
                          , string = vx
                          , label  = Nothing
                          }
        xtail = PaCo2Node { skip   = kx - k - 1
                          , string = vx `shiftL` (k + 1)
                          , label  = label x
                          }
        tree  = if vx `testBit` (31 - k)
                then Bin xhead (Leaf Nothing) (Bin xtail l r)
                else Bin xhead (Bin xtail l r) (Leaf Nothing)
resizeRoot _ tree = tree

instance Monoid (Tree b PaCo2Node) where
  mempty = Leaf Nothing

  (Leaf Nothing) `mappend` t  = t
  t  `mappend` (Leaf Nothing) = t
  (Leaf _) `mappend` (Leaf _) = error "Cannot append two leaves"
  t1 `mappend` t2             = balanceRoot $ t1 `helper` t2
    where tx `helper` ty = Bin node (lx <> ly) (rx <> ry)
            where Bin x _ _    = tx
                  Bin y _ _    = ty
                  kmin         = minimum [ skip x
                                         , skip y
                                         , countLeadingZeros $
                                           string x `xor` string y
                                         ]
                  Bin x' lx rx = resizeRoot kmin tx
                  Bin y' ly ry = resizeRoot kmin ty
                  node         = x' { label = label x' <|> label y' }


fromEntry :: Entry -> Tree b PaCo2Node
fromEntry (Entry p n) = Bin node (Leaf Nothing) (Leaf Nothing)
  where Prefix (Address a) (Mask m) = p
        node                        = PaCo2Node { skip   = m
                                                , string = a
                                                , label  = Just n
                                                }

lookupState :: Address -> Tree b PaCo2Node -> State (Maybe Int) ()
lookupState (Address a) = helper a
  where helper :: Word32 -> Tree b PaCo2Node -> State (Maybe Int) ()
        helper _ (Leaf _)                = return ()
        helper v (Bin x l r) | k < kx    = return ()
                             | otherwise = do
                                 modify (label x <|>)
                                 helper (v `shiftL` (kx + 1)) $
                                   if v `testBit` (31 - kx) then r else l
          where kx = skip x
                k  = countLeadingZeros $ v `xor` string x

instance IpRouter (Tree b PaCo2Node) where
  mkTable = foldr insEntry mempty

  insEntry = mappend . fromEntry

  delEntry = undefined

  ipLookup a t = execState (lookupState a t) Nothing

  numOfPrefixes = getSum . foldMap isPrefix
    where isPrefix x = if (isJust . label) x then Sum 1 else Sum 0


newtype PaCo2Tree b = PaCo2Tree (Tree b PaCo2Node)
                    deriving (Show, Eq, Monoid, IpRouter)
