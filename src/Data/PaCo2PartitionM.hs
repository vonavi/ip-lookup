{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE Rank2Types          #-}

module Data.PaCo2PartitionM
  (
    prtnBuild
  , putPrtn
  ) where

import           Data.Monoid

import           Data.Zipper

data Node a where
  Node :: { zipper :: Zipper a => a, height :: Int } -> Node a
data Tree a = Leaf a | Bin a (Tree a) (Tree a) deriving Eq
type MemTree a = Tree (Node a)

instance Foldable Tree where
  foldMap f (Leaf x)    = f x
  foldMap f (Bin x l r) = f x <> foldMap f l <> foldMap f r

instance Zipper a => Show (MemTree a) where
  show t = case t of
             Leaf x    -> "Leaf " ++ nodeToStr x
             Bin x l r -> "Bin " ++ nodeToStr x ++
                          " (" ++ show l ++ ") (" ++ show r ++ ")"
    where nodeToStr x = "(Node {zipper = " ++ show (zipper x) ++
                        ", height = " ++ show (height x) ++ "})"


rootZipper :: Zipper a => MemTree a -> a
rootZipper (Leaf x)    = zipper x
rootZipper (Bin x _ _) = zipper x

rootHeight :: MemTree a -> Int
rootHeight (Leaf _)    = 0
rootHeight (Bin x _ _) = height x

setRootHeight :: Int -> MemTree a -> MemTree a
setRootHeight _ t@(Leaf _)  = t
setRootHeight h (Bin x l r) = Bin x { height = h } l r

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

separateRoot :: Zipper a => a -> MemTree a -> MemTree a
separateRoot _ t@(Leaf _)  = t
separateRoot z (Bin x l r) = Bin x' l' r'
  where ht  = height x
        l'  = if rootHeight l /= 0 then l else setRootHeight ht l
        r'  = if rootHeight r /= 0 then r else setRootHeight ht r
        z'  = goUp . pruneZipper l . goLeft $ z
        z'' = goUp . pruneZipper r . goRight $ z'
        x'  = Node { zipper = z''
                   , height = succ $ max (rootHeight l') (rootHeight r')
                   }

mergeBoth :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
mergeBoth z l r = Bin x (setRootHeight 0 l) (setRootHeight 0 r)
  where x = Node { zipper = z
                 , height = maximum [1, rootHeight l, rootHeight r]
                 }

mergeLeft :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
mergeLeft z l r = if isNodeFull z'
                  then Bin x (setRootHeight 0 l) r
                  else mergeBoth (goUp . rootZipper $ r') l r'
  where z' = goUp . pruneZipper r . goRight $ z
        x  = Node { zipper = z'
                  , height = max (rootHeight l) (succ . rootHeight $ r)
                  }
        r' = separateRoot (goRight z) r

mergeRight :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
mergeRight z l r = if isNodeFull z'
                   then Bin x l (setRootHeight 0 r)
                   else mergeBoth (goUp . rootZipper $ l') l' r
  where z' = goUp . pruneZipper l . goLeft $ z
        x  = Node { zipper = z'
                  , height = max (succ . rootHeight $ l) (rootHeight r)
                  }
        l' = separateRoot (goLeft z) l

pruneTree :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
pruneTree z l r = Bin x l r
  where z'  = goUp . pruneZipper l . goLeft $ z
        z'' = goUp . pruneZipper r . goRight $ z'
        x   = Node { zipper = z''
                   , height = succ $ max (rootHeight l) (rootHeight r)
                   }

minHeightMerge :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
minHeightMerge z l r
  | not . isNodeFull . rootZipper $ t = error "Unbalanced tree root"
  | pageSize x <= maxPageSize         = t
  | otherwise                         = pruneTree z l r
  where t         = mergeTree z l r
        Bin x _ _ = t
        lht       = rootHeight l
        rht       = rootHeight r
        mergeTree | lht == rht = mergeBoth
                  | lht > rht  = mergeLeft
                  | otherwise  = mergeRight

prtnBuild :: Zipper a => a -> MemTree a
prtnBuild z | isLeaf z  = Leaf Node { zipper = z
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


numOfPages :: MemTree a -> Int
numOfPages = getSum . foldMap (Sum . addPage)
  where addPage x = if height x /= 0 then 1 else 0

memUsage :: Zipper a => MemTree a -> Int
memUsage = getSum . foldMap (Sum . nodeMemUsage)
  where nodeMemUsage x = if height x /= 0
                         then fitToMinPage . pageSize $ x
                         else 0
        fitToMinPage s = let k = (s + minPageSize - 1) `div` minPageSize
                         in k * minPageSize

fillSize :: Zipper a => MemTree a -> Int
fillSize = getSum . foldMap (Sum . nodeFillSize)
  where nodeFillSize x = if height x /= 0 then pageSize x else 0

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
