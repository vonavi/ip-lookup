{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Partition
  (
    MemTree
  , putPartition
  , Partible(..)
  , minHeightMerge
  , minSizeMerge
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Bits
import           Data.Function       (on)
import           Data.Maybe          (fromMaybe, isNothing)
import           Data.Monoid

import           Data.IpRouter
import           Data.Zipper

data Node a = Node { zipper :: a
                   , height :: Int
                   }
            deriving (Show, Eq)
data Tree a = Tip (Maybe a)
            | Bin (Maybe a) (Tree a) (Tree a)
            deriving Eq
type MemTree a = Tree (Node a)

instance Show a => Show (Tree a) where
  show = head . nodeList
    where nodeList (Tip _)            = []
          nodeList (Bin Nothing l r)  = nodeList l ++ nodeList r
          nodeList (Bin (Just x) l r) = ["Branch (" ++ show x ++ ", [" ++
                                         accStr (nodeList l ++ nodeList r) ++
                                         "])"]
            where accStr [] = ""
                  accStr xs = drop 2 . concatMap (", " ++) $ xs

instance Foldable Tree where
  foldMap _ (Tip _)     = mempty
  foldMap f (Bin x l r) = fromMaybe mempty (f <$> x) <>
                          foldMap f l <> foldMap f r


minPageSize :: Int
minPageSize = 128

maxPageSize :: Int
maxPageSize = 6 * minPageSize

-- | 18 bits are reserved for the 'plpm' folder.
pageSize :: Zipper a => Node a -> Int
pageSize x = size (zipper x) + 18

isFitted :: Zipper a => Node a -> Bool
isFitted = (<= maxPageSize) . pageSize


fromRoot :: (Node a -> b) -> MemTree a -> b
fromRoot f t = case t of Bin x _ _ -> apply f x
                         Tip x     -> apply f x
  where apply g = fromMaybe (error "No pointer") . (g <$>)

updatePointer :: Node a -> MemTree a -> MemTree a
updatePointer x (Tip y)     = Tip (y <|> Just x { height = 0 })
updatePointer x (Bin y l r) = Bin (y <|> Just x) l r

removePointer :: MemTree a -> MemTree a
removePointer (Tip _)     = Tip Nothing
removePointer (Bin _ l r) = Bin Nothing l r


prtnBuild :: Partible a => a -> MemTree a
prtnBuild z | isLeaf z  = Tip . Just $ Node { zipper = z
                                            , height = 0
                                            }
            | otherwise = memTreeMerge z' l r
  where zl  = goLeft z
        l   = prtnBuild zl
        zl' = insert (fromRoot zipper l) zl
        zr  = goRight . goUp $ zl'
        r   = prtnBuild zr
        zr' = insert (fromRoot zipper r) zr
        z'  = goUp zr'

prtnInsert :: Partible a => a -> MemTree a -> MemTree a
prtnInsert z (Tip _)            = prtnBuild z
prtnInsert z t | isLeaf z       = t
prtnInsert z (Bin (Just x) l r) = memTreeMerge (setLabel s z') l' r'
  where h   = height x
        zl  = goLeft z
        l'  = prtnInsert zl $
              updatePointer Node { zipper = goLeft . zipper $ x, height = h } l
        zl' = insert (fromRoot zipper l') zl
        zr  = goRight . goUp $ zl'
        r'  = prtnInsert zr $
              updatePointer Node { zipper = goRight . zipper $ x, height = h } r
        zr' = insert (fromRoot zipper r') zr
        z'  = goUp zr'
        s   = getLabel z' <|> getLabel (zipper x)
prtnInsert _ (Bin Nothing _ _)  = error "No pointer"

delEmptyPage :: Zipper a => MemTree a -> MemTree a
delEmptyPage t
  | (Bin (Just x) Tip{} Tip{}) <- t
  , isNothing . getLabel . zipper $ x
    = Tip . Just $ Node { zipper = delete . zipper $ x
                        , height = 0
                        }
  | otherwise
    = t

delLabel :: Zipper a => a -> a -> Maybe Int
delLabel z1 z2
  | Just s1 <- getLabel z1
  , Just s2 <- getLabel z2
  , s1 == s2               = Nothing
  | otherwise              = getLabel z1

prtnDelete :: Partible a => a -> MemTree a -> MemTree a
prtnDelete _ t@(Tip _)          = t
prtnDelete z t | isLeaf z       = t
prtnDelete z (Bin (Just x) l r) = delEmptyPage $
                                  memTreeMerge (setLabel s z') l' r'
  where h   = height x
        zl  = goLeft z
        l'  = prtnDelete zl $
              updatePointer Node { zipper = goLeft . zipper $ x, height = h } l
        zl' = insert (fromRoot zipper l') zl
        zr  = goRight . goUp $ zl'
        r'  = prtnDelete zr $
              updatePointer Node { zipper = goRight . zipper $ x, height = h } r
        zr' = insert (fromRoot zipper r') zr
        z'  = goUp zr'
        s   = delLabel (zipper x) z'
prtnDelete _ (Bin Nothing _ _)  = error "No pointer"

lookupState :: Zipper a => Address -> MemTree a -> State (Maybe Int) ()
lookupState (Address a) = helper 31
  where helper _ (Tip _)            = return ()
        helper n (Bin (Just x) l r) = do
          modify (getLabel z <|>)
          if a `testBit` n
            then helper (pred n) $
                 updatePointer Node { zipper = goRight z, height = h } r
            else helper (pred n) $
                 updatePointer Node { zipper = goLeft z, height = h } l
            where z = zipper x
                  h = height x
        helper _ (Bin Nothing _ _)  = error "No pointer"

instance (IpRouter a, Partible a) => IpRouter (MemTree a) where
  mkTable       = prtnBuild . (mkTable :: IpRouter a => [Entry] -> a)
  insEntry e    = prtnInsert (mkTable [e] :: IpRouter a => a)
  delEntry e    = prtnDelete (mkTable [e] :: IpRouter a => a)
  ipLookup a t  = execState (lookupState a t) Nothing
  numOfPrefixes = getSum . foldMap (Sum . numOfPrefixes . zipper)


numOfPages :: MemTree a -> Int
numOfPages = getSum . foldMap (Sum . const 1)

memUsage :: Zipper a => MemTree a -> Int
memUsage = getSum . foldMap (Sum . fitToMinPage . pageSize)
  where fitToMinPage s = let k = (s + minPageSize - 1) `div` minPageSize
                         in k * minPageSize

fillSize :: Zipper a => MemTree a -> Int
fillSize = getSum . foldMap (Sum . pageSize)

putPartition :: (IpRouter a, Partible a) => MemTree a -> IO ()
putPartition t = do
  putStrLn . (++) "  Number of prefixes " . show . numOfPrefixes $ t
  putStrLn . (++) "  Height             " . show . fromRoot height $ t
  putStrLn . (++) "  Number of pages    " . show . numOfPages $ t
  putStrLn . (++) "  Memory usage       " . show . memUsage $ t
  putStrLn . (++) "  Memory utilization " . show $ memUtil
  putStrLn . (++) "  Fill size          " . show . fillSize $ t
  putStrLn . (++) "  Fill ratio         " . show $ fillRatio
    where memUtil = (\x -> 12 * x `div` 10) . memUsage $ t
          fillRatio :: Double
          fillRatio = fromIntegral (fillSize t) / fromIntegral (memUsage t)


class Zipper a => Partible a where
  memTreeMerge :: a -> MemTree a -> MemTree a -> MemTree a

mergeBoth :: a -> MemTree a -> MemTree a -> MemTree a
mergeBoth z l r = Bin (Just x) (removePointer l) (removePointer r)
  where x = Node { zipper = z
                 , height = maximum [1, fromRoot height l, fromRoot height r]
                 }

pruneZipper :: Zipper a => MemTree a -> a -> a
pruneZipper (Tip _) = id
pruneZipper _       = delete

mergeLeft :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
mergeLeft z l r = Bin (Just x) (removePointer l) r
  where z' = goUp . pruneZipper r . goRight $ z
        x  = Node { zipper = z'
                  , height = max (fromRoot height l) (succ $ fromRoot height r)
                  }

mergeRight :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
mergeRight z l r = Bin (Just x) l (removePointer r)
  where z' = goUp . pruneZipper l . goLeft $ z
        x  = Node { zipper = z'
                  , height = max (succ $ fromRoot height l) (fromRoot height r)
                  }

pruneTree :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
pruneTree z l r = Bin (Just x) l r
  where z'  = goUp . pruneZipper l . goLeft $ z
        z'' = goUp . pruneZipper r . goRight $ z'
        x   = Node { zipper = z''
                   , height = succ $ max (fromRoot height l) (fromRoot height r)
                   }

minHeightMerge :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
minHeightMerge z l r
  | fromRoot isFitted t = t
  | otherwise           = pruneTree z l r
  where t   = mergeTree z l r
        lht = fromRoot height l
        rht = fromRoot height r
        mergeTree | lht == rht = mergeBoth
                  | lht > rht  = mergeLeft
                  | otherwise  = mergeRight

minSizeMerge :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
minSizeMerge z l r
  | fromRoot isFitted large  = large
  | fromRoot isFitted middle = middle
  | otherwise                = small
  where large  = mergeBoth z l r
        middle = if ((<) `on` fromRoot pageSize) l r
                 then mergeLeft z l r
                 else mergeRight z l r
        small  = pruneTree z l r
