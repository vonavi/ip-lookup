{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE Rank2Types          #-}

module Data.PaCo2.Partition
  (
    prtnBuild
  , prtnInsert
  , putPrtn
  ) where

import           Control.Applicative ((<|>))
import           Data.Maybe          (fromMaybe)
import           Data.Monoid

import           Data.PaCo2.Zipper

data Node a where
  Node :: { zipper :: Zipper a => a, height :: Int } -> Node a
data Tree a = Leaf (Maybe a)
            | Bin (Maybe a) (Tree a) (Tree a)
            deriving Eq
type MemTree a = Tree (Node a)

instance Foldable Tree where
  foldMap _ (Leaf _)    = mempty
  foldMap f (Bin x l r) = fromMaybe mempty (f <$> x) <>
                          foldMap f l <> foldMap f r

instance Zipper a => Show (MemTree a) where
  show t = case t of
             Leaf x    -> "Leaf " ++ nodeToStr x
             Bin x l r -> "Bin " ++ nodeToStr x ++
                          " (" ++ show l ++ ") (" ++ show r ++ ")"
    where nodeToStr (Just x) = "(Just Node {zipper = " ++ show (zipper x) ++
                               ", height = " ++ show (height x) ++ "})"
          nodeToStr Nothing  = "Nothing"


rootZipper :: Zipper a => MemTree a -> a
rootZipper t | Leaf x    <- t = toZipper x
             | Bin x _ _ <- t = toZipper x
  where toZipper = fromMaybe (error "No zipper found") . (zipper <$>)

rootHeight :: MemTree a -> Int
rootHeight (Leaf _)    = 0
rootHeight (Bin x _ _) = fromMaybe 0 $ height <$> x

updatePointer :: Node a -> MemTree a -> MemTree a
updatePointer x (Leaf y)    = Leaf (y <|> Just x)
updatePointer x (Bin y l r) = Bin (y <|> Just x) l r

removePointer :: MemTree a -> MemTree a
removePointer (Leaf _)    = Leaf Nothing
removePointer (Bin _ l r) = Bin Nothing l r

minPageSize :: Int
minPageSize = 128

maxPageSize :: Int
maxPageSize = 6 * minPageSize

-- | 18 bits are reserved for the 'plpm' folder.
pageSize :: Zipper a => Node a -> Int
pageSize x = size (zipper x) + 18


pruneZipper :: Zipper a => MemTree a -> a -> a
pruneZipper (Leaf _) = id
pruneZipper _        = delete

delEmptyPage :: Zipper a => MemTree a -> MemTree a
delEmptyPage (Bin (Just x) Leaf{} Leaf{})
  | Nothing <- getLabel . zipper $ x = Leaf Nothing
delEmptyPage t                       = t

pruneTree :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
pruneTree z l r = Bin (Just x) (delEmptyPage l) (delEmptyPage r)
  where z'  = goUp . pruneZipper l . goLeft $ z
        z'' = goUp . pruneZipper r . goRight $ z'
        x   = Node { zipper = z''
                   , height = succ $ max (rootHeight l) (rootHeight r)
                   }

separateRoot :: Zipper a => a -> MemTree a -> MemTree a
separateRoot z (Leaf _)           = Bin (Just x) (Leaf Nothing) (Leaf Nothing)
  where x = Node { zipper = z
                 , height = 1
                 }
separateRoot z (Bin (Just x) l r) = pruneTree z l' r'
  where h  = height x
        l' = updatePointer Node { zipper = goLeft z, height = h } l
        r' = updatePointer Node { zipper = goRight z, height = h } r

mergeBoth :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
mergeBoth z l r = Bin (Just x) (removePointer l) (removePointer r)
  where x = Node { zipper = z
                 , height = maximum [1, rootHeight l, rootHeight r]
                 }

mergeLeft :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
mergeLeft z l r = if isNodeFull z'
                  then Bin (Just x) (removePointer l) r
                  else mergeBoth (goUp . rootZipper $ r') l r'
  where z' = goUp . pruneZipper r . goRight $ z
        x  = Node { zipper = z'
                  , height = max (rootHeight l) (succ . rootHeight $ r)
                  }
        r' = separateRoot (goRight . mkNodeFull $ z) r

mergeRight :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
mergeRight z l r = if isNodeFull z'
                   then Bin (Just x) l (removePointer r)
                   else mergeBoth (goUp . rootZipper $ l') l' r
  where z' = goUp . pruneZipper l . goLeft $ z
        x  = Node { zipper = z'
                  , height = max (succ . rootHeight $ l) (rootHeight r)
                  }
        l' = separateRoot (goLeft . mkNodeFull $ z) l

minHeightMerge :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
minHeightMerge z l r
  | not . isNodeFull . rootZipper $ t = error "Unbalanced tree root"
  | pageSize x <= maxPageSize         = t
  | otherwise                         = pruneTree z l r
  where t                = mergeTree z l r
        Bin (Just x) _ _ = t
        lht              = rootHeight l
        rht              = rootHeight r
        mergeTree | lht == rht = mergeBoth
                  | lht > rht  = mergeLeft
                  | otherwise  = mergeRight

prtnBuild :: Zipper a => a -> MemTree a
prtnBuild z | isLeaf z  = Leaf . Just $ Node { zipper = z
                                             , height = 0
                                             }
            | otherwise = minHeightMerge z' l r
  where zl  = goLeft z
        l   = prtnBuild zl
        zl' = insert (rootZipper l) zl
        zr  = goRight . goUp $ zl'
        r   = prtnBuild zr
        zr' = insert (rootZipper r) zr
        z'  = goUp zr'

undoSeparateRoot :: Zipper a => MemTree a -> MemTree a
undoSeparateRoot t@(Bin (Just x) l r)
  | Bin (Just xl) _ _ <- l, Bin (Just xr) _ _ <- r = merge xl xr
  | Leaf (Just xl)    <- l, Bin (Just xr) _ _ <- r = merge xl xr
  | Bin (Just xl) _ _ <- l, Leaf (Just xr)    <- r = merge xl xr
  | otherwise                                      = t
  where merge yl yr = minHeightMerge (goUp zr) l r
          where zl = insert (zipper yl) . goLeft . zipper $ x
                zr = insert (zipper yr) . goRight . goUp $ zl

prtnInsert :: Zipper a => a -> MemTree a -> MemTree a
prtnInsert z (Leaf _)           = prtnBuild z
prtnInsert z t | isLeaf z       = undoSeparateRoot t
prtnInsert z (Bin (Just x) l r) = minHeightMerge (setLabel s z') l' r'
  where h   = height x
        zl  = goLeft z
        l'  = prtnInsert zl $
              updatePointer Node { zipper = goLeft . zipper $ x, height = h } l
        zl' = insert (rootZipper l') zl
        zr  = goRight . goUp $ zl'
        r'  = prtnInsert zr $
              updatePointer Node { zipper = goRight . zipper $ x, height = h } r
        zr' = insert (rootZipper r') zr
        z'  = goUp zr'
        s   = getLabel z' <|> getLabel (zipper x)


numOfPages :: MemTree a -> Int
numOfPages = getSum . foldMap (Sum . const 1)

memUsage :: Zipper a => MemTree a -> Int
memUsage = getSum . foldMap (Sum . fitToMinPage . pageSize)
  where fitToMinPage s = let k = (s + minPageSize - 1) `div` minPageSize
                         in k * minPageSize

fillSize :: Zipper a => MemTree a -> Int
fillSize = getSum . foldMap (Sum . pageSize)

putPrtn :: Zipper a => MemTree a -> IO ()
putPrtn t = do
  putStrLn "Partition of path-compressed 2-tree"
  putStrLn . (++) "  Height:             " . show . rootHeight $ t
  putStrLn . (++) "  Number of pages:    " . show . numOfPages $ t
  putStrLn . (++) "  Memory usage:       " . show . memUsage $ t
  putStrLn . (++) "  Memory utilization: " . show $ memUtil
  putStrLn . (++) "  Fill size:          " . show . fillSize $ t
  putStrLn . (++) "  Fill ratio:         " . show $ fillRatio
    where memUtil = (\x -> 12 * x `div` 10) . memUsage $ t
          fillRatio :: Double
          fillRatio = fromIntegral (fillSize t) / fromIntegral (memUsage t)
