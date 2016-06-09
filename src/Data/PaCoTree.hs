{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Data.PaCoTree
       (
         PaCoNode(..)
       , Tree(..)
       , PaCoTree(..)
       , fromEntry
       , gammaSize
       , deltaSize
       , eliasFanoSize
       , huffmanSize
       , putPaCoTree
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

import           Data.Compression.Huffman
import           Data.IpRouter
import           Data.PrefixTree

data PaCoNode = PaCoNode { skip   :: Int
                         , string :: Word32
                         , label  :: Maybe Int
                         } deriving Show

instance Eq PaCoNode where
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

instance Monoid (Tree PaCoNode) where
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
          kx          = skip x
          ky          = skip y
          vx          = string x
          vy          = string y
          kmin        = minimum [ kx
                                , ky
                                , countLeadingZeros $ vx `xor` vy
                                ]
          node        = PaCoNode { skip   = kmin
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


newtype PaCoTree = PaCoTree { getTree :: Tree PaCoNode } deriving (Show, Eq)

instance Monoid PaCoTree where
  mempty        = PaCoTree mempty
  x `mappend` y = PaCoTree $ getTree x `mappend` getTree y

fromEntry :: Entry -> PaCoTree
fromEntry (Entry p n) = PaCoTree $ Bin Tip node Tip
  where Prefix (Address a) (Mask m) = p
        node                        = PaCoNode { skip   = m
                                               , string = a
                                               , label  = Just n
                                               }

lookupState :: Address -> Tree PaCoNode -> State (Maybe Int) ()
lookupState (Address a) = helper a
  where helper :: Word32 -> Tree PaCoNode -> State (Maybe Int) ()
        helper v t | t == Tip || k < kx = return ()
                   | otherwise          = do
                       modify (label x <|>)
                       helper (v `shiftL` (kx + 1)) $
                         if v `testBit` (31 - kx) then r else l
          where Bin l x r = t
                kx        = skip x
                k         = countLeadingZeros $ v `xor` string x

instance IpRouter PaCoTree where
  mkTable es = PaCoTree $ runST $ do
    let tree  = getTree . foldr insEntry mempty $ es
        kvec  = foldr accSkipValues (V.replicate 32 0) tree
        hsize = map (second length) . freqToEnc . V.toList . V.indexed $ kvec
    modifySTRef huffmanVecRef (V.// hsize)
    return tree
      where accSkipValues x v = V.accum (+) v [(skip x, 1)]

  insEntry = mappend . fromEntry

  delEntry = flip delSubtree . fromEntry

  ipLookup a (PaCoTree t) = execState (lookupState a t) Nothing

  numOfPrefixes = getSum . foldMap isPrefix . getTree
    where isPrefix x = if (isJust . label) x then Sum 1 else Sum 0


gammaSize :: PaCoTree -> Int
gammaSize = getSum . foldMap nodeSize . getTree
  where nodeSize PaCoNode { skip = k } =
          {- The node size is built from the following parts:
             parenthesis expression (2 bits), internal prefix (1 bit),
             gamma-code of skip value (the skip value should be
             increased by one), and node string. -}
          Sum $ 3 + gammaCodeSize (k + 1) + k

gammaCodeSize :: Int -> Int
gammaCodeSize x = 2 * k + 1
  where k = floor . logBase (2 :: Double) . fromIntegral $ x

deltaSize :: PaCoTree -> Int
deltaSize = getSum . foldMap nodeSize . getTree
  where nodeSize PaCoNode { skip = k } =
          {- The node size is built from the following parts:
             parenthesis expression (2 bits), internal prefix (1 bit),
             delta-code of skip value (the skip value should be
             increased by one), and node string. -}
          Sum $ 3 + deltaCodeSize (k + 1) + k

deltaCodeSize :: Int -> Int
deltaCodeSize x = 2 * l + k + 1
  where k = floor . logBase (2 :: Double) . fromIntegral $ x
        l = floor . logBase (2 :: Double) . fromIntegral . succ $ k

eliasFanoSize :: PaCoTree -> Int
eliasFanoSize t = eliasFanoCodeSize t +
                  (getSum . foldMap nodeSize . getTree $ t)
{- The node size is built from the following parts: parenthesis
   expression (2 bits), internal prefix (1 bit), and node string. -}
  where nodeSize x = Sum $ 3 + skip x

eliasFanoCodeSize :: PaCoTree -> Int
eliasFanoCodeSize t
  | null ks   = 0
  | kmax == 0 = 1 + n
  | otherwise = l + 1 + sum ks' + n + n * l
  where ks   = foldMap ((:[]) . skip) . getTree $ t
        ksum = scanl1 (+) ks
        kmax = last ksum
        n    = length ks
        l    = max 0 . floor . logBase (2 :: Double) $
               fromIntegral kmax / fromIntegral n
        ks'  = let ksum' = map (`shiftR` l) ksum
               in zipWith (-) ksum' (0 : init ksum')

{-# NOINLINE huffmanVecRef #-}
huffmanVecRef :: forall s . STRef s (V.Vector Int)
huffmanVecRef = unsafePerformST . newSTRef $ V.replicate 32 0

huffmanSize :: PaCoTree -> Int
huffmanSize = getSum . foldMap nodeSize . getTree
  where nodeSize PaCoNode { skip = k } =
          {- The node size is built from the following parts:
             parenthesis expression (2 bits), internal prefix (1 bit),
             Huffman code of skip value (the skip value should be
             increased by one), and node string. -}
          Sum $ runST $ do hsize <- readSTRef huffmanVecRef
                           return $ 3 + (hsize V.! k) + k

instance PrefixTree PaCoTree where
  isEmpty (PaCoTree Tip) = True
  isEmpty _              = False

  root (PaCoTree (Bin _ x _)) | skip x == 0 = label x
  root _                                    = Nothing

  leftSubtree (PaCoTree Tip) = PaCoTree Tip
  leftSubtree (PaCoTree (Bin l x r))
    | k == 0         = PaCoTree l
    | v `testBit` 31 = PaCoTree Tip
    | otherwise      = PaCoTree $ Bin l x' r
    where k  = skip x
          v  = string x
          x' = x { skip   = pred k
                 , string = v `shiftL` 1
                 }

  rightSubtree (PaCoTree Tip) = PaCoTree Tip
  rightSubtree (PaCoTree (Bin l x r))
    | k == 0         = PaCoTree r
    | v `testBit` 31 = PaCoTree $ Bin l x' r
    | otherwise      = PaCoTree Tip
    where k  = skip x
          v  = string x
          x' = x { skip   = pred k
                 , string = v `shiftL` 1
                 }

  singleton x = PaCoTree $ Bin Tip node Tip
    where node = PaCoNode { skip   = 0
                          , string = 0
                          , label  = x
                          }

  merge x l r
    | isJust x  = singleton x <> PaCoTree lsub <> PaCoTree rsub
    | otherwise = PaCoTree lsub <> PaCoTree rsub
    where lsub = case getTree l of
                  Tip          -> Tip
                  Bin ll xl rl ->
                    let xl' = xl { skip   = succ $ skip xl
                                 , string = (`clearBit` 31) . (`shiftR` 1) $
                                            string xl
                                 }
                    in Bin ll xl' rl
          rsub = case getTree r of
                  Tip          -> Tip
                  Bin lr xr rr ->
                    let xr' = xr { skip   = succ $ skip xr
                                 , string = (`setBit` 31) . (`shiftR` 1) $
                                            string xr
                                 }
                    in Bin lr xr' rr

  collapse = undefined

  delSubtree a b = PaCoTree $ helper (getTree a) (getTree b)
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

putPaCoTree :: PaCoTree -> IO ()
putPaCoTree t = do
  putStrLn "Path-compressed tree"
  putStrLn . (++) "  Size with gamma code: " . show $ gammaSize t + 18 * n
  putStrLn . (++) "  Size with delta code: " . show $ deltaSize t + 18 * n
    where n = numOfPrefixes t
