{-# LANGUAGE FlexibleInstances,
             UndecidableInstances #-}

module Data.OrdTree
       (
         OrdTree( isEmpty
                , size
                , bRoot
                , bLeftSubtree
                , bRightSubtree
                , bInsertRoot
                )
       , ordToBp
       , ordToDfuds
       , OrdTreeT1
       , OrdTreeT2
       , OrdTreeT3
       , OrdTreeT4
       ) where

import Data.Maybe (isJust)
import Control.Applicative ((<|>))
import Control.Monad.State

import Data.IpRouter
import Data.Paren

newtype Forest a = Forest { getNodes :: [(a, Forest a)] } deriving Show

class OrdTree t where
  toForest    :: t      -> Forest (Maybe Int)
  isEmpty     :: t      -> Bool
  fromEntry   :: Entry  -> t
  lookupState :: [Bool] -> t -> State (Maybe Int) ()

  size          :: t         -> Int
  bRoot         :: t         -> Maybe Int
  bLeftSubtree  :: t         -> t
  bRightSubtree :: t         -> t
  bInsertRoot   :: Maybe Int -> t -> t -> t

  isEmpty = null . getNodes . toForest

  size x = pred $ execState (helper . toForest $ x) 0
    where helper :: Forest (Maybe Int) -> State Int ()
          helper (Forest xs) = do
            mapM_ (helper . snd) xs
            modify succ

instance {-# OVERLAPPABLE #-} (Monoid a, OrdTree a) => IpRouter a where
  mkTable = foldr (mappend . fromEntry) mempty

  insEntry = undefined
  delEntry = undefined

  ipLookup a t = execState (lookupState (addrBits a) t) Nothing

  numOfPrefixes t = execState (helper . toForest $ t) 0
    where helper :: Forest (Maybe Int) -> State Int ()
          helper (Forest [])           = return ()
          helper (Forest ((x, l) : r)) = do
            helper l
            helper (Forest r)
            when (isJust x) $ modify succ

forestToBp :: Forest a -> [(a, Paren)]
forestToBp = helper . getNodes
  where helper = concatMap (\(x, Forest l)
                            -> [(x, Open)] ++ helper l ++ [(x, Close)])

ordToBp :: OrdTree a => a -> [(Maybe Int, Paren)]
ordToBp x = [(Nothing, Open)] ++ forestToBp (toForest x) ++ [(Nothing, Close)]

forestToDfuds :: Forest a -> [(a, [Paren])]
forestToDfuds (Forest t) = helper t
  where helper []                  = []
        helper ((x, Forest l) : r) =
          let p = replicate (length l) Open ++ [Close]
          in (x, p) : helper l ++ helper r

ordToDfuds :: OrdTree a => a -> [(Maybe Int, [Paren])]
ordToDfuds x = (Nothing, ps) : forestToDfuds f
  where f  = toForest x
        ps = replicate (length $ getNodes f) Open ++ [Close]


newtype OrdTreeT1 = OrdTreeT1 (Forest (Maybe Int)) deriving Show

instance Monoid OrdTreeT1 where
  mempty = OrdTreeT1 $ Forest []

  tx `mappend` ty = OrdTreeT1 $ Forest $
                    helper (getNodes $ toForest tx) (getNodes $ toForest ty)
    where helper []                   ys                   = ys
          helper xs                   []                   = xs
          helper ((a, Forest x) : xs) ((b, Forest y) : ys) =
            (a <|> b, Forest (helper x y)) : helper xs ys

instance OrdTree OrdTreeT1 where
  toForest (OrdTreeT1 x) = x

  fromEntry (Entry p n) = OrdTreeT1 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Maybe Int)
          helper []     = Forest [(Just n, Forest [])]
          helper (b:bs) = Forest $ if b
                                   then (Nothing, Forest []) :
                                        getNodes (helper bs)
                                   else [(Nothing, helper bs)]

  lookupState bits = helper bits . getNodes . toForest
    where helper _      []                  = return ()
          helper []     ((x, _) : _)        = modify (x <|>)
          helper (b:bs) ((x, Forest r) : l) = do
            modify (x <|>)
            helper bs $ if b then l else r

  bRoot = helper . getNodes . toForest
    where helper []           = Nothing
          helper ((x, _) : _) = x

  bLeftSubtree = OrdTreeT1 . Forest . helper . toForest
    where helper (Forest [])                  = []
          helper (Forest ((_, Forest l) : _)) = l

  bRightSubtree = OrdTreeT1 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = tail x

  bInsertRoot x ltree rtree =
    OrdTreeT1 . Forest $
    (x, toForest ltree) : (getNodes . toForest $ rtree)


newtype OrdTreeT2 = OrdTreeT2 (Forest (Maybe Int)) deriving Show

instance Monoid OrdTreeT2 where
  mempty = OrdTreeT2 $ Forest []

  tx `mappend` ty = OrdTreeT2 $ Forest $
                    helper (getNodes $ toForest tx) (getNodes $ toForest ty)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a <|> b, Forest (helper x y))]
            where (a, Forest x) = last xs
                  (b, Forest y) = last ys

instance OrdTree OrdTreeT2 where
  toForest (OrdTreeT2 x) = x

  fromEntry (Entry p n) = OrdTreeT2 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Maybe Int)
          helper []     = Forest [(Just n, Forest [])]
          helper (b:bs) = Forest $ if b
                                   then getNodes (helper bs) ++
                                        [(Nothing, Forest [])]
                                   else [(Nothing, helper bs)]

  lookupState bits = helper bits . getNodes . toForest
    where helper _      [] = return ()
          helper []     xs = let x = fst . last $ xs
                             in modify (x <|>)
          helper (b:bs) xs = do
            modify (x <|>)
            helper bs $ if b then l else r
              where (x, Forest r) = last xs
                    l             = init xs

  bRoot = helper . getNodes . toForest
    where helper [] = Nothing
          helper x  = fst . last $ x

  bLeftSubtree = OrdTreeT2 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = getNodes . snd . last $ x

  bRightSubtree = OrdTreeT2 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = init x

  bInsertRoot x ltree rtree =
    OrdTreeT2 . Forest $
    (getNodes . toForest $ rtree) ++ [(x, toForest ltree)]


newtype OrdTreeT3 = OrdTreeT3 (Forest (Maybe Int)) deriving Show

instance Monoid OrdTreeT3 where
  mempty = OrdTreeT3 $ Forest []

  tx `mappend` ty = OrdTreeT3 $ Forest $
                    helper (getNodes $ toForest tx) (getNodes $ toForest ty)
    where helper []                   ys                   = ys
          helper xs                   []                   = xs
          helper ((a, Forest x) : xs) ((b, Forest y) : ys) =
            (a <|> b, Forest (helper x y)) : helper xs ys

instance OrdTree OrdTreeT3 where
  toForest (OrdTreeT3 x) = x

  fromEntry (Entry p n) = OrdTreeT3 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Maybe Int)
          helper []     = Forest [(Just n, Forest [])]
          helper (b:bs) = Forest $ if b
                                   then [(Nothing, helper bs)]
                                   else (Nothing, Forest []) :
                                        getNodes (helper bs)

  lookupState bits = helper bits . getNodes . toForest
    where helper _      []                  = return ()
          helper []     ((x, _) : _)        = modify (x <|>)
          helper (b:bs) ((x, Forest l) : r) = do
            modify (x <|>)
            helper bs $ if b then l else r

  bRoot = helper . getNodes . toForest
    where helper []           = Nothing
          helper ((x, _) : _) = x

  bLeftSubtree = OrdTreeT3 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = tail x

  bRightSubtree = OrdTreeT3 . Forest . helper . toForest
    where helper (Forest [])                  = []
          helper (Forest ((_, Forest r) : _)) = r

  bInsertRoot x ltree rtree =
    OrdTreeT3 . Forest $
    (x, toForest rtree) : (getNodes . toForest $ ltree)


newtype OrdTreeT4 = OrdTreeT4 (Forest (Maybe Int)) deriving Show

instance Monoid OrdTreeT4 where
  mempty = OrdTreeT4 $ Forest []

  tx `mappend` ty = OrdTreeT4 $ Forest $
                    helper (getNodes $ toForest tx) (getNodes $ toForest ty)
    where helper [] ys = ys
          helper xs [] = xs
          helper xs ys = helper (init xs) (init ys) ++
                         [(a <|> b, Forest (helper x y))]
            where (a, Forest x) = last xs
                  (b, Forest y) = last ys

instance OrdTree OrdTreeT4 where
  toForest (OrdTreeT4 x) = x

  fromEntry (Entry p n) = OrdTreeT4 $ helper . prefixBits $ p
    where helper :: [Bool] -> Forest (Maybe Int)
          helper []     = Forest [(Just n, Forest [])]
          helper (b:bs) = Forest $ if b
                                   then [(Nothing, helper bs)]
                                   else getNodes (helper bs) ++
                                        [(Nothing, Forest [])]

  lookupState bits = helper bits . getNodes . toForest
    where helper _      [] = return ()
          helper []     xs = let x = fst . last $ xs
                             in modify (x <|>)
          helper (b:bs) xs = do
            modify (x <|>)
            helper bs $ if b then l else r
              where (x, Forest l) = last xs
                    r             = init xs

  bRoot = helper . getNodes . toForest
    where helper [] = Nothing
          helper x  = fst . last $ x

  bLeftSubtree = OrdTreeT4 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = init x

  bRightSubtree = OrdTreeT4 . Forest . helper . toForest
    where helper (Forest []) = []
          helper (Forest x)  = getNodes . snd . last $ x

  bInsertRoot x ltree rtree =
    OrdTreeT4 . Forest $
    (getNodes . toForest $ ltree) ++ [(x, toForest rtree)]
