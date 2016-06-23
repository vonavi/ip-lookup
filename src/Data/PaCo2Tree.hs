{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Data.PaCo2Tree
       (
         PaCo2Node(..)
       , Tree(..)
       , PaCo2Tree(..)
       , gammaSize
       , deltaSize
       , eliasFanoSize
       , huffmanSize
       , putPaCo2Tree
       ) where

import           Control.Applicative            ((<|>))
import           Control.Arrow                  (second)
import           Control.Monad.ST
import           Control.Monad.ST.UnsafePerform
import           Control.Monad.State
import           Data.Bits
import           Data.Maybe                     (isJust)
import           Data.Monoid
import           Data.STRef
import qualified Data.Vector                    as V
import           Data.Word

import qualified Data.Bitmap                    as BMP
import           Data.Compression.Elias
import           Data.Compression.Fibonacci
import           Data.Compression.Huffman
import           Data.IpRouter
import           Data.PrefixTree

data PaCo2Node = PaCo2Node { skip   :: Int
                           , string :: Word32
                           , label  :: Maybe Int
                           } deriving Show

instance Eq PaCo2Node where
  x == y = kx == ky && n >= kx && label x == label y
    where kx = skip x
          ky = skip y
          n  = countLeadingZeros $ string x `xor` string y


data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap _ Tip         = Tip
  fmap f (Bin l x r) = Bin (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap _ Tip         = mempty
  foldMap f (Bin l x r) = foldMap f l <> f x <> foldMap f r

balanceRoot :: Tree PaCo2Node -> Tree PaCo2Node
balanceRoot Tip = Tip
balanceRoot t@(Bin l x r)
  | l == Tip && r == Tip = t
  | l == Tip             = Bin (Bin Tip n Tip) x r
  | r == Tip             = Bin l x (Bin Tip n Tip)
  | otherwise            = t
  where n = PaCo2Node { skip   = 0
                      , string = 0
                      , label  = Nothing
                      }

instance Monoid (Tree PaCo2Node) where
  mempty = Tip

  ttx `mappend` tty = balanceRoot $ ttx `helper` tty
    where tx `helper` ty
            | tx == Tip                = ty
            | ty == Tip                = tx
            | kmin == kx && kmin == ky = let sx = label x
                                             sy = label y
                                             n  = x { label = sx <|> sy }
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
                  kx          = skip x
                  ky          = skip y
                  vx          = string x
                  vy          = string y
                  kmin        = minimum [ kx
                                        , ky
                                        , countLeadingZeros $ vx `xor` vy
                                        ]
                  node        = PaCo2Node { skip   = kmin
                                          , string = vx
                                          , label  = Nothing
                                          }
                  xright      = vx `testBit` (31 - kmin)
                  yright      = vy `testBit` (31 - kmin)
                  x'          = x { skip   = kx - kmin - 1
                                  , string = vx `shiftL` (kmin + 1)
                                  }
                  y'          = y { skip   = ky - kmin - 1
                                  , string = vy `shiftL` (kmin + 1)
                                  }
                  tx'         = Bin lx x' rx
                  ty'         = Bin ly y' ry


appendNode :: PaCo2Node -> Bool -> PaCo2Node -> PaCo2Node
appendNode xhead b xlast = PaCo2Node { skip   = succ $ skip xhead + skip xlast
                                     , string = str
                                     , label  = label xlast
                                     }
  where w      = succ $ skip xhead
        m      = complement $ (maxBound :: Word32) `shiftR` w
        shead  = string xhead .&. m
        slast  = string xlast `shiftR` w
        setter = if b then setBit else clearBit
        str    = (shead .|. slast) `setter` (32 - w)

collapseRoot :: Tree PaCo2Node -> Tree PaCo2Node
collapseRoot Tip             = Tip
collapseRoot t@(Bin _ x _)
  | isJust (label x)         = t
collapseRoot (Bin Tip _ Tip) = Tip
collapseRoot (Bin l x Tip)   = let Bin ll xl rl = l
                               in Bin ll (appendNode x False xl) rl
collapseRoot (Bin Tip x r)   = let Bin lr xr rr = r
                               in Bin lr (appendNode x True xr) rr
collapseRoot t               = t

instance PrefixTree (Tree PaCo2Node) where
  isEmpty Tip = True
  isEmpty _   = False

  root (Bin _ x _) | skip x == 0 = label x
  root _                         = Nothing

  leftSubtree Tip    = Tip
  leftSubtree (Bin l x r)
    | k == 0         = l
    | v `testBit` 31 = Tip
    | otherwise      = Bin l x' r
    where k  = skip x
          v  = string x
          x' = x { skip   = pred k
                 , string = v `shiftL` 1
                 }

  rightSubtree Tip   = Tip
  rightSubtree (Bin l x r)
    | k == 0         = r
    | v `testBit` 31 = Bin l x' r
    | otherwise      = Tip
    where k  = skip x
          v  = string x
          x' = x { skip   = pred k
                 , string = v `shiftL` 1
                 }

  singleton x = Bin Tip node Tip
    where node = PaCo2Node { skip   = 0
                           , string = 0
                           , label  = x
                           }

  merge x l r
    | isJust x  = singleton x <> lsub <> rsub
    | otherwise = lsub <> rsub
    where lsub = case l of
                  Tip          -> Tip
                  Bin ll xl rl ->
                    let xl' = xl { skip   = succ $ skip xl
                                 , string = (`clearBit` 31) . (`shiftR` 1) $
                                            string xl
                                 }
                    in Bin ll xl' rl
          rsub = case r of
                  Tip          -> Tip
                  Bin lr xr rr ->
                    let xr' = xr { skip   = succ $ skip xr
                                 , string = (`setBit` 31) . (`shiftR` 1) $
                                            string xr
                                 }
                    in Bin lr xr' rr

  collapse Tip         = Tip
  collapse (Bin l x r) = balanceRoot . collapseRoot $
                         Bin (collapse l) x (collapse r)

  delSubtree a b = collapse $ helper a b
    where helper Tip _   = Tip
          helper t   Tip = t
          helper t@(Bin lx x rx) (Bin ly y ry)
            | kxy < min kx ky = t
            | kx == ky        = Bin (helper lx ly) ndiff (helper rx ry)
            | kx < ky         = if vy `testBit` (31 - kx)
                                then Bin lx ndiff (helper rx $ Bin ly ylast ry)
                                else Bin (helper lx $ Bin ly ylast ry) ndiff rx
            | otherwise       = if vx `testBit` (31 - ky)
                                then Bin Tip xhead (helper (Bin lx xlast rx) ry)
                                else Bin (helper (Bin lx xlast rx) ly) xhead Tip
            where kx    = skip x
                  ky    = skip y
                  vx    = string x
                  vy    = string y
                  kxy   = countLeadingZeros $ vx `xor` vy
                  ndiff = let labDiff (Just px) (Just py) | px == py = Nothing
                              labDiff l         _                    = l
                          in x { label = labDiff (label x) (label y) }
                  xhead = x { skip  = ky
                            , label = Nothing
                            }
                  xlast = x { skip   = kx - ky - 1
                            , string = vx `shiftL` (ky + 1)
                            }
                  ylast = y { skip   = ky - kx - 1
                            , string = vy `shiftL` (kx + 1)
                            }

  size = gammaSize


fromEntry :: Entry -> Tree PaCo2Node
fromEntry (Entry p n) = Bin Tip node Tip
  where Prefix (Address a) (Mask m) = p
        node                        = PaCo2Node { skip   = m
                                                , string = a
                                                , label  = Just n
                                                }

lookupState :: Address -> Tree PaCo2Node -> State (Maybe Int) ()
lookupState (Address a) = helper a
  where helper :: Word32 -> Tree PaCo2Node -> State (Maybe Int) ()
        helper v t | t == Tip || k < kx = return ()
                   | otherwise          = do
                       modify (label x <|>)
                       helper (v `shiftL` (kx + 1)) $
                         if v `testBit` (31 - kx) then r else l
          where Bin l x r = t
                kx        = skip x
                k         = countLeadingZeros $ v `xor` string x

instance IpRouter (Tree PaCo2Node) where
  mkTable es = runST $ do
    let tree  = foldr insEntry mempty es
        kvec  = foldr accSkipValues (V.replicate 33 0) tree
        hsize = map (second length) . freqToEnc . V.toList . V.indexed $ kvec
    modifySTRef huffmanVecRef (V.// hsize)
    return tree
      where accSkipValues x v = V.accum (+) v [(skip x, 1)]

  insEntry = mappend . fromEntry

  delEntry = flip delSubtree . fromEntry

  ipLookup a t = execState (lookupState a t) Nothing

  numOfPrefixes = getSum . foldMap isPrefix
    where isPrefix x = if (isJust . label) x then Sum 1 else Sum 0


gammaSize :: Tree PaCo2Node -> Int
gammaSize = getSum . foldMap nodeSize
  where nodeSize PaCo2Node { skip = k } =
          {- The node size is built from the following parts:
             open/close parenthesis (1 bit), internal prefix (1 bit),
             Elias gamma code of skip value (the skip value should be
             increased by one), and node string. -}
          Sum $ 2 + (BMP.size . encodeEliasGamma . succ $ k) + k

deltaSize :: Tree PaCo2Node -> Int
deltaSize = getSum . foldMap nodeSize
  where nodeSize PaCo2Node { skip = k } =
          {- The node size is built from the following parts:
             open/close parenthesis (1 bit), internal prefix (1 bit),
             Elias delta code of skip value (the skip value should be
             increased by one), and node string. -}
          Sum $ 2 + (BMP.size . encodeEliasDelta . succ $ k) + k

eliasFanoSize :: Tree PaCo2Node -> Int
eliasFanoSize t = eliasFanoCodeSize t + (getSum . foldMap nodeSize $ t)
{- The node size is built from the following parts: open/close
   parenthesis (1 bit), internal prefix (1 bit), and node string. -}
  where nodeSize x = Sum $ 2 + skip x

eliasFanoCodeSize :: Tree PaCo2Node -> Int
eliasFanoCodeSize t
  | null ks   = 0
  | otherwise = BMP.size $ (encodeUnary . succ . lowSize $ bmp2) <>
                highBits bmp2 <> lowBits bmp2
  where ks   = foldMap ((:[]) . skip) t
        bmp2 = encodeEliasFano . scanl1 (+) $ ks

fibonacciSize :: Tree PaCo2Node -> Int
fibonacciSize = getSum . foldMap nodeSize
  where nodeSize PaCo2Node { skip = k } =
          {- The node size is built from the following parts:
             open/close parenthesis (1 bit), internal prefix (1 bit),
             Fibonacci code of skip value (the skip value should be
             increased by one), and node string. -}
          Sum $ 2 + (BMP.size . encodeFibonacci . succ $ k) + k

{-# NOINLINE huffmanVecRef #-}
huffmanVecRef :: forall s . STRef s (V.Vector Int)
huffmanVecRef = unsafePerformST . newSTRef $ V.replicate 33 0

huffmanSize :: Tree PaCo2Node -> Int
huffmanSize = getSum . foldMap nodeSize
  where nodeSize PaCo2Node { skip = k } =
          {- The node size is built from the following parts:
             open/close parenthesis (1 bit), internal prefix (1 bit),
             Huffman code of skip value (the skip value should be
             increased by one), and node string. -}
          Sum $ runST $ do hsize <- readSTRef huffmanVecRef
                           return $ 2 + (hsize V.! k) + k


newtype PaCo2Tree = PaCo2Tree (Tree PaCo2Node)
                  deriving (Show, Eq, Monoid, PrefixTree, IpRouter)

putPaCo2Tree :: PaCo2Tree -> IO ()
putPaCo2Tree (PaCo2Tree t) = do
  putStrLn "Path-compressed 2-tree"
  putStrLn "  Memory usage:"
  putStrLn . (++) "    Elias gamma coding: " . show $ gammaSize t + 18 * n
  putStrLn . (++) "    Elias delta coding: " . show $ deltaSize t + 18 * n
  putStrLn . (++) "    Elias-Fano coding:  " . show $ eliasFanoSize t + 18 * n
  putStrLn . (++) "    Fibonacci coding:   " . show $ fibonacciSize t + 18 * n
  putStrLn . (++) "    Huffman coding:     " . show $ huffmanSize t + 18 * n
    where n = numOfPrefixes t
