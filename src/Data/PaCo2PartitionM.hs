module Data.PaCo2PartitionM
  (
    prtnBuild
  ) where

import           Data.Zipper

data Node = Node { size   :: Int
                 , height :: Int
                 }
          deriving (Show, Eq)
data Tree a = Tip | Bin a (Tree a) (Tree a) deriving (Show, Eq)
type MemTree = Tree Node

getRootHeight :: MemTree -> Int
getRootHeight (Bin x _ _) = height x
getRootHeight Tip         = 0

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

-- | The presence of routing prefix is indicated by a prefix bit; if
--   the bit is set then, additionally, an RE index (18 bits) is added
--   to the node size.
totalNodeSize :: Zipper a => a -> Int
totalNodeSize z = nodeSize z + 1 + if isPrefix z then 18 else 0


separateRoot :: MemTree -> MemTree
separateRoot (Bin x l r) = Bin x' (setRootHeight ht l) (setRootHeight ht r)
  where ht = height x
        x' = Node { size   = size x - pageSize l - pageSize r
                  , height = succ ht
                  }
separateRoot Tip         = Tip

isBalancedRoot :: MemTree -> Bool
isBalancedRoot (Bin Node { height = ht } l r)
  | ht == lht && ht == rht = True
  | ht > lht && ht > rht   = True
  | otherwise              = False
  where lht = getRootHeight l
        rht = getRootHeight r
isBalancedRoot Tip         = True

mergeBoth :: Int -> MemTree -> MemTree -> MemTree
mergeBoth s l r = Bin x (setRootHeight 0 l) (setRootHeight 0 r)
  where x = Node { size   = s + pageSize l + pageSize r
                 , height = maximum [1, getRootHeight l, getRootHeight r]
                 }

mergeLeft :: Int -> MemTree -> MemTree -> MemTree
mergeLeft s l r = if isBalancedRoot t
                  then t
                  else mergeBoth s l (separateRoot r)
  where t = Bin x (setRootHeight 0 l) r
        x = Node { size   = s + pageSize l
                 , height = max (getRootHeight l) (succ . getRootHeight $ r)
                 }

mergeRight :: Int -> MemTree -> MemTree -> MemTree
mergeRight s l r = if isBalancedRoot t
                   then t
                   else mergeBoth s (separateRoot l) r
  where t = Bin x l (setRootHeight 0 r)
        x = Node { size   = s + pageSize r
                 , height = max (succ . getRootHeight $ l) (getRootHeight r)
                 }

pruneTree :: Int -> MemTree -> MemTree -> MemTree
pruneTree s l r = Bin x l r
  where x = Node { size   = s
                 , height = succ $ max (getRootHeight l) (getRootHeight r)
                 }

minHeightMerge :: Int -> MemTree -> MemTree -> MemTree
minHeightMerge s l r
  | not . isBalancedRoot $ t = error "Unbalanced tree root"
  | isFitted t               = t
  | otherwise                = pruneTree s l r
  where t   = mergeTree s l r
        lht = getRootHeight l
        rht = getRootHeight r
        mergeTree | lht == rht = mergeBoth
                  | lht > rht  = mergeLeft
                  | otherwise  = mergeRight

prtnBuild :: Zipper a => Maybe a -> MemTree
prtnBuild Nothing  = Tip
prtnBuild (Just z) = minHeightMerge (totalNodeSize z) l r
  where l = prtnBuild . goLeft $ z
        r = prtnBuild . goRight $ z
