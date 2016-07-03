{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.PaCo2TreeM
       (
         PaCo2Node(..)
       , Tree(..)
       , PaCo2Tree(..)
       , putPaCo2Tree
       ) where

import           Control.Applicative    ((<|>))
import           Control.Monad.State
import           Data.Bits
import           Data.Maybe             (isJust)
import           Data.Monoid
import           Data.Word

import qualified Data.Bitmap            as BMP
import           Data.Compression.Elias
import           Data.IpRouter
import           Data.PrefixTree

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
balanceRoot t@(Bin _ (Leaf _) (Leaf _)) = t
balanceRoot (Bin x (Leaf Nothing) r)    = Bin x emptyBranch r
balanceRoot (Bin x l (Leaf Nothing))    = Bin x l emptyBranch
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


joinNodes :: PaCo2Node -> Bool -> PaCo2Node -> PaCo2Node
joinNodes xhead b xlast = PaCo2Node { skip   = succ $ skip xhead + skip xlast
                                    , string = str
                                    , label  = label xlast
                                    }
  where w      = succ $ skip xhead
        m      = complement $ (maxBound :: Word32) `shiftR` w
        shead  = string xhead .&. m
        slast  = string xlast `shiftR` w
        setter = if b then setBit else clearBit
        str    = (shead .|. slast) `setter` (32 - w)

uniteRoot :: Tree b PaCo2Node -> Tree b PaCo2Node
uniteRoot t@(Bin x _ _) | isJust (label x)      = t
uniteRoot (Bin _ (Leaf Nothing) (Leaf Nothing)) = Leaf Nothing
uniteRoot (Bin x (Leaf Nothing) (Bin y l r))    = Bin (joinNodes x True y) l r
uniteRoot (Bin x (Bin y l r) (Leaf Nothing))    = Bin (joinNodes x False y) l r
uniteRoot t                                     = t

instance PrefixTree (Tree b PaCo2Node) where
  isEmpty      = undefined
  root         = undefined
  leftSubtree  = undefined
  rightSubtree = undefined
  singleton    = undefined

  merge x l r
    | isJust x  = Bin xroot (Leaf Nothing) (Leaf Nothing) <> lsub <> rsub
    | otherwise = lsub <> rsub
    where xroot = PaCo2Node { skip   = 0
                            , string = 0
                            , label  = x
                            }
          nroot = PaCo2Node { skip   = 0
                            , string = 0
                            , label  = Nothing
                            }
          lsub  = case l of
                    Leaf Nothing -> l
                    Leaf _       -> Bin nroot l (Leaf Nothing)
                    Bin xl ll rl ->
                      let xl' = xl { skip   = succ $ skip xl
                                   , string = (`clearBit` 31) . (`shiftR` 1) $
                                              string xl
                                   }
                      in Bin xl' ll rl
          rsub  = case r of
                    Leaf Nothing -> r
                    Leaf _       -> Bin nroot (Leaf Nothing) r
                    Bin xr lr rr ->
                      let xr' = xr { skip   = succ $ skip xr
                                   , string = (`setBit` 31) . (`shiftR` 1) $
                                              string xr
                                   }
                      in Bin xr' lr rr

  collapse = undefined

  (Leaf Nothing) `delSubtree` _  = Leaf Nothing
  t `delSubtree` (Leaf Nothing)  = uniteRoot t
  (Leaf _) `delSubtree` (Leaf _) = error "Cannot delete a leaf from a leaf"
  t1 `delSubtree` t2             = balanceRoot . uniteRoot $ t1 `helper` t2
    where tx `helper` ty = Bin node (lx `delSubtree` ly) (rx `delSubtree` ry)
            where Bin x _ _    = tx
                  Bin y _ _    = ty
                  kmin         = minimum [ skip x
                                         , skip y
                                         , countLeadingZeros $
                                           string x `xor` string y
                                         ]
                  Bin x' lx rx = resizeRoot kmin tx
                  Bin y' ly ry = resizeRoot kmin ty
                  node         = x' { label = labelDiff (label x') (label y') }
          labelDiff :: Maybe Int -> Maybe Int -> Maybe Int
          labelDiff (Just sx) (Just sy) | sx == sy = Nothing
          labelDiff sx        _                    = sx

  size = eliasGammaSize


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

  delEntry = flip delSubtree . fromEntry

  ipLookup a t = execState (lookupState a t) Nothing

  numOfPrefixes = getSum . foldMap isPrefix
    where isPrefix x = if (isJust . label) x then Sum 1 else Sum 0


eliasGammaSize :: Tree b PaCo2Node -> Int
eliasGammaSize = getSum . foldMap nodeSize
  where nodeSize PaCo2Node { skip = k } =
          {- The node size is built from the following parts:
             open/close parenthesis (1 bit), internal prefix (1 bit),
             Elias gamma code of skip value (the skip value should be
             increased by one), and node string. -}
          Sum $ 2 + (BMP.size . encodeEliasGamma . succ $ k) + k


newtype PaCo2Tree b = PaCo2Tree (Tree b PaCo2Node)
                    deriving (Show, Eq, Monoid, PrefixTree, IpRouter)

putPaCo2Tree :: PaCo2Tree b -> IO ()
putPaCo2Tree (PaCo2Tree t) = do
  putStrLn "Path-compressed 2-tree"
  putStrLn "  Memory usage:"
  putStrLn . (++) "    Elias gamma coding: " . show $ eliasGammaSize t + 18 * n
    where n = numOfPrefixes t
