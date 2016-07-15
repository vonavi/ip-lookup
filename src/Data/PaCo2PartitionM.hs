module Data.PaCo2PartitionM
  (
    prtnBuild
  , putPrtn
  ) where

import           Data.Maybe  (isJust)
import           Data.Monoid

import           Data.Zipper

data Node = Node { size   :: Int
                 , height :: Int
                 }
          deriving (Show, Eq)
data Tree a = Tip | Bin a (Tree a) (Tree a) deriving (Show, Eq)
type MemTree = Tree Node

instance Foldable Tree where
  foldMap _ Tip         = mempty
  foldMap f (Bin x l r) = f x <> foldMap f l <> foldMap f r


rootHeight :: MemTree -> Int
rootHeight (Bin x _ _) = height x
rootHeight Tip         = 0

setRootHeight :: Int -> MemTree -> MemTree
setRootHeight h (Bin x l r) = Bin x { height = h } l r
setRootHeight _ Tip         = Tip

minPageSize :: Int
minPageSize = 128

maxPageSize :: Int
maxPageSize = 6 * minPageSize

pageSize :: MemTree -> Int
pageSize (Bin x _ _) = size x
pageSize Tip         = 0

-- | 18 bits are reserved for the 'plpm' folder.
isFitted :: MemTree -> Bool
isFitted = (maxPageSize - 18 >=) . pageSize

-- | For each routing prefix, an RE index (18 bits) is added to the
--   node size.
totalNodeSize :: Zipper a => a -> Int
totalNodeSize z = nodeSize z + if isJust . getLabel $ z then 18 else 0


separateRoot :: Zipper a => a -> MemTree -> (a, MemTree)
separateRoot z (Bin x l r) = (z'', t)
  where z'  = goUp . delete . goLeft $ z
        z'' = goUp . delete . goRight $ z'
        ht  = height x
        t   = Bin x' (setRootHeight ht l) (setRootHeight ht r)
        x'  = Node { size   = totalNodeSize z''
                   , height = succ ht
                   }
separateRoot z Tip         = (z, Tip)

isBalancedRoot :: MemTree -> Bool
isBalancedRoot (Bin Node { height = ht } l r)
  | ht == lht && ht == rht = True
  | ht > lht && ht > rht   = True
  | otherwise              = False
  where lht = rootHeight l
        rht = rootHeight r
isBalancedRoot Tip         = True

mergeBoth :: Zipper a => a -> MemTree -> MemTree -> (a, MemTree)
mergeBoth z l r = (z, t)
  where t = Bin x (setRootHeight 0 l) (setRootHeight 0 r)
        x = Node { size   = totalNodeSize z + pageSize l + pageSize r
                 , height = maximum [1, rootHeight l, rootHeight r]
                 }

mergeLeft :: Zipper a => a -> MemTree -> MemTree -> (a, MemTree)
mergeLeft z l r = if isBalancedRoot t
                  then (z', t)
                  else mergeBoth (goUp zr) l r'
  where z'       = goUp . delete . goRight $ z
        (zr, r') = separateRoot (goRight z) r
        t        = Bin x (setRootHeight 0 l) r
        x        = Node { size   = totalNodeSize z' + pageSize l
                        , height = max (rootHeight l) (succ . rootHeight $ r)
                        }

mergeRight :: Zipper a => a -> MemTree -> MemTree -> (a, MemTree)
mergeRight z l r = if isBalancedRoot t
                   then (z', t)
                   else mergeBoth (goUp zl) l' r
  where z'       = goUp . delete . goLeft $ z
        (zl, l') = separateRoot (goLeft z) l
        t        = Bin x l (setRootHeight 0 r)
        x        = Node { size   = totalNodeSize z' + pageSize r
                        , height = max (succ . rootHeight $ l) (rootHeight r)
                        }

pruneTree :: Zipper a => a -> MemTree -> MemTree -> (a, MemTree)
pruneTree z l r = (z'', Bin x l r)
  where z'  = goUp . delete . goLeft $ z
        z'' = goUp . delete . goRight $ z'
        x   = Node { size   = totalNodeSize z''
                   , height = succ $ max (rootHeight l) (rootHeight r)
                   }

minHeightMerge :: Zipper a => a -> MemTree -> MemTree -> (a, MemTree)
minHeightMerge z l r
  | not . isBalancedRoot $ t = error "Unbalanced tree root"
  | isFitted t               = (z', t)
  | otherwise                = pruneTree z l r
  where (z', t) = mergeTree z l r
        lht     = rootHeight l
        rht     = rootHeight r
        mergeTree | lht == rht = mergeBoth
                  | lht > rht  = mergeLeft
                  | otherwise  = mergeRight

prtnBuild :: Zipper a => a -> (a, MemTree)
prtnBuild z | isLeaf z  = (z, Tip)
            | otherwise = minHeightMerge z'' l r
  where (zl, l) = prtnBuild . goLeft $ z
        z'      = goUp zl
        (zr, r) = prtnBuild . goRight $ z'
        z''     = goUp zr


numOfPages :: MemTree -> Int
numOfPages = getSum . foldMap (Sum . addPage)
  where addPage x = if height x /= 0 then 1 else 0

-- | 18 bits of memory bandwidth are already utilized by the 'plpm'
--   folder.
memUsage :: MemTree -> Int
memUsage = getSum . foldMap (Sum . addMemSize)
  where addMemSize x = if height x /= 0
                       then fitToMem $ 18 + size x
                       else 0
        fitToMem s   = let k = (s + minPageSize - 1) `div` minPageSize
                       in k * minPageSize

-- | 18 bits of memory bandwidth are already utilized by the 'plpm'
--   folder.
fillSize :: MemTree -> Int
fillSize = getSum . foldMap (Sum . addFillSize)
  where addFillSize x = if height x /= 0
                        then 18 + size x
                        else 0

putPrtn :: MemTree -> IO ()
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
