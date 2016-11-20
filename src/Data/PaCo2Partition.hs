{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Partitions a path-compressed 2-tree into a tree of memory pages.
module Data.PaCo2Partition
  (
    MemTree
  , PaCo2MinHeight(..)
  , showPaCo2MinHeight
  , PaCo2MinSize(..)
  , showPaCo2MinSize
  ) where

import           Control.Applicative  ((<|>))
import           Control.Monad.State
import           Data.Function        (on)
import           Data.Maybe           (fromMaybe, isNothing)
import           Data.Monoid

import           Config
import           Data.IpRouter
import           Data.Prefix
import           Data.Trees.PaCo2Tree (PaCo2Zipper, Zipper (..))

-- | Memory-tree page
data Node a = Node { zipper :: a   -- ^ 'Data.Zipper.Zipper' of page
                   , height :: Int -- ^ height of rooted subtree
                   }
            deriving (Show, Eq)
-- | Structure of memory tree
data Tree a = Tip (Maybe a)
            | Bin (Maybe a) (Tree a) (Tree a)
            deriving Eq
-- | Memory tree
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


-- | Returns the size of page.
pageSize :: Zipper a => Node a -> Int
pageSize x = size (zipper x)
             + nextHopSize config * fromEnum (prevNextHop config)

-- | Checks if page is fitted into the maximum page size.
isFitted :: Zipper a => Node a -> Bool
isFitted = (<= maxPageSize config) . pageSize


-- | Applies a function to the page at the root of memory tree.
fromRoot :: (Node a -> b) -> MemTree a -> b
fromRoot f t = case t of Bin x _ _ -> apply f x
                         Tip x     -> apply f x
  where apply g = fromMaybe (error "No pointer") . (g <$>)

-- | Stores page information at the root of memory tree.
updatePointer :: Node a -> MemTree a -> MemTree a
updatePointer x (Tip y)     = Tip (y <|> Just x { height = 0 })
updatePointer x (Bin y l r) = Bin (y <|> Just x) l r

-- | Removes page information from the root of memory tree.
removePointer :: MemTree a -> MemTree a
removePointer (Tip _)     = Tip Nothing
removePointer (Bin _ l r) = Bin Nothing l r


-- | Builds a memory tree of data structure.
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

-- | Undoes the separation of the root of memory tree into a single
--   page.
undoSeparateRoot :: Partible a => MemTree a -> MemTree a
undoSeparateRoot t@(Bin (Just x) l r)
  | Bin (Just xl) _ _ <- l, Bin (Just xr) _ _ <- r = merge xl xr
  | Tip (Just xl)     <- l, Bin (Just xr) _ _ <- r = merge xl xr
  | Bin (Just xl) _ _ <- l, Tip (Just xr)     <- r = merge xl xr
  | otherwise                                      = t
  where merge yl yr = memTreeMerge (goUp zr) l r
          where zl = insert (zipper yl) . goLeft . zipper $ x
                zr = insert (zipper yr) . goRight . goUp $ zl
undoSeparateRoot (Bin Nothing _ _)                 = error "No pointer"
undoSeparateRoot (Tip _)                           = error "Nothing to separate"

-- | Inserts routing-table entry into memory tree.
prtnInsert :: Partible a => a -> MemTree a -> MemTree a
prtnInsert z (Tip _)            = prtnBuild z
prtnInsert z t | isLeaf z       = undoSeparateRoot t
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

-- | Deletes the empty page.
delEmptyPage :: Zipper a => MemTree a -> MemTree a
delEmptyPage t
  | (Bin (Just x) Tip{} Tip{}) <- t
  , isNothing . getLabel . zipper $ x
    = Tip . Just $ Node { zipper = delete . zipper $ x
                        , height = 0
                        }
  | otherwise
    = t

-- | Deletes one label from another.
delLabel :: Zipper a => a -> a -> Maybe Int
delLabel z1 z2
  | Just s1 <- getLabel z1
  , Just s2 <- getLabel z2
  , s1 == s2               = Nothing
  | otherwise              = getLabel z1

-- | Deletes routing-table entry from memory tree.
prtnDelete :: Partible a => a -> MemTree a -> MemTree a
prtnDelete _ t@(Tip _)          = t
prtnDelete z t | isLeaf z       = undoSeparateRoot t
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

-- | Makes the longest-prefix match (LPM).
lookupState :: Zipper a => Prefix -> MemTree a -> State (Maybe Int) ()
lookupState _ (Tip _)            = return ()
lookupState v (Bin (Just x) l r) = do
  modify (getLabel z <|>)
  case uncons v of
    Nothing      -> return ()
    Just (b, v') -> if b
                    then lookupState v' $
                         updatePointer Node { zipper = goRight z, height = h } r
                    else lookupState v' $
                         updatePointer Node { zipper = goLeft z, height = h } l
    where z = zipper x
          h = height x
lookupState _ (Bin Nothing _ _)  = error "No pointer"

instance (IpRouter a, Partible a) => IpRouter (MemTree a) where
  mkTable       = prtnBuild . (mkTable :: IpRouter a => [Entry] -> a)
  insEntry e    = prtnInsert (mkTable [e] :: IpRouter a => a)
  delEntry e    = prtnDelete (mkTable [e] :: IpRouter a => a)
  ipLookup a t  = execState (lookupState a t) Nothing
  numOfPrefixes = getSum . foldMap (Sum . numOfPrefixes . zipper)


-- | Returns the number of memory-tree pages.
numOfPages :: MemTree a -> Int
numOfPages = getSum . foldMap (Sum . const 1)

-- | Returns the memory usage of memory tree.
memUsage :: Zipper a => MemTree a -> Int
memUsage = getSum . foldMap (Sum . granularity . pageSize)
  where granularity s = ((s + smin - 1) `div` smin) * smin
        smin          = minPageSize config

-- | Returns the total size of filled parts of memory tree.
fillSize :: Zipper a => MemTree a -> Int
fillSize = getSum . foldMap (Sum . pageSize)

-- | Shows characteristics of memory tree.
showPartition :: (IpRouter a, Partible a) => MemTree a -> String
showPartition t =
  "  Number of prefixes " ++ show (numOfPrefixes t)   ++ "\n" ++
  "  Height             " ++ show (fromRoot height t) ++ "\n" ++
  "  Number of pages    " ++ show (numOfPages t)      ++ "\n" ++
  "  Memory usage       " ++ show (memUsage t)        ++ "\n" ++
  "  Fill size          " ++ show (fillSize t)        ++ "\n" ++
  "  Fill ratio         " ++ show fillRatio           ++ "\n"
  where fillRatio :: Double
        fillRatio = ((/) `on` fromIntegral) (fillSize t) (memUsage t)


-- | Specifies the partition strategy.
class Zipper a => Partible a where
  memTreeMerge :: a -> MemTree a -> MemTree a -> MemTree a

-- | Merges both memory subtrees into the current page.
mergeBoth :: a -> MemTree a -> MemTree a -> MemTree a
mergeBoth z l r = Bin (Just x) (removePointer l) (removePointer r)
  where x = Node { zipper = z
                 , height = maximum [1, fromRoot height l, fromRoot height r]
                 }

-- | Cuts off the current page.
pruneZipper :: Zipper a => MemTree a -> a -> a
pruneZipper (Tip _) = id
pruneZipper _       = delete

-- | Prunes the page at the root of memory tree.
pruneTree :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
pruneTree z l r = Bin (Just x) (delEmptyPage l) (delEmptyPage r)
  where z'  = goUp . pruneZipper l . goLeft $ z
        z'' = goUp . pruneZipper r . goRight $ z'
        x   = Node { zipper = z''
                   , height = succ $ max (fromRoot height l) (fromRoot height r)
                   }

-- | Separates the root of memory tree into a single page.
separateRoot :: Zipper a => a -> MemTree a -> MemTree a
separateRoot z (Tip _)            = Bin (Just x) (Tip Nothing) (Tip Nothing)
  where x = Node { zipper = z
                 , height = 1
                 }
separateRoot z (Bin (Just x) l r) = pruneTree z l' r'
  where h  = height x
        l' = updatePointer Node { zipper = goLeft z, height = h } l
        r' = updatePointer Node { zipper = goRight z, height = h } r
separateRoot _ (Bin Nothing _ _)  = error "No pointer"

-- | Merges the left memory subtree into the current page.
mergeLeft :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
mergeLeft z l r = if isNodeFull z'
                  then Bin (Just x) (removePointer l) r
                  else mergeBoth (goUp . fromRoot zipper $ r') l r'
  where z' = goUp . pruneZipper r . goRight $ z
        x  = Node { zipper = z'
                  , height = max (fromRoot height l) (succ $ fromRoot height r)
                  }
        r' = separateRoot (goRight . mkNodeFull $ z) r

-- | Merges the right memory subtree into the current page.
mergeRight :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
mergeRight z l r = if isNodeFull z'
                   then Bin (Just x) l (removePointer r)
                   else mergeBoth (goUp . fromRoot zipper $ l') l' r
  where z' = goUp . pruneZipper l . goLeft $ z
        x  = Node { zipper = z'
                  , height = max (succ $ fromRoot height l) (fromRoot height r)
                  }
        l' = separateRoot (goLeft . mkNodeFull $ z) l

-- | Merges memory subtrees, keeping the minimum-height property.
minHeightMerge :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
minHeightMerge z l r
  | not . isNodeFull . fromRoot zipper $ t = error "Unbalanced tree root"
  | fromRoot isFitted t                    = t
  | otherwise                              = pruneTree z l r
  where t   = mergeTree z l r
        lht = fromRoot height l
        rht = fromRoot height r
        mergeTree | lht == rht = mergeBoth
                  | lht > rht  = mergeLeft
                  | otherwise  = mergeRight

-- | Merges memory subtrees, keeping the minimum-size property.
minSizeMerge :: Zipper a => a -> MemTree a -> MemTree a -> MemTree a
minSizeMerge z l r
  | not . isNodeFull . fromRoot zipper $ large  = error "Unbalanced tree root"
  | fromRoot isFitted large                     = large
  | not . isNodeFull . fromRoot zipper $ middle = error "Unbalanced tree root"
  | fromRoot isFitted middle                    = middle
  | otherwise                                   = small
  where large  = mergeBoth z l r
        middle = if ((<) `on` fromRoot pageSize) l r
                 then mergeLeft z l r
                 else mergeRight z l r
        small  = pruneTree z l r


-- | Minimum-height partition of path-compressed 2-tree
newtype PaCo2MinHeight = PaCo2MinHeight PaCo2Zipper
                       deriving (Eq, Show, IpRouter, Zipper)
instance Partible PaCo2MinHeight where
  memTreeMerge = minHeightMerge

-- | Shows the minimum-height partition of path-compressed 2-tree.
showPaCo2MinHeight :: MemTree PaCo2MinHeight -> String
showPaCo2MinHeight = ("Min-height partition of path-compressed 2-tree\n" ++)
                     . showPartition

-- | Minimum-size partition of path-compressed 2-tree
newtype PaCo2MinSize = PaCo2MinSize PaCo2Zipper
                     deriving (Eq, Show, IpRouter, Zipper)
instance Partible PaCo2MinSize where
  memTreeMerge = minSizeMerge

-- | Shows the minimum-size partition of path-compressed 2-tree.
showPaCo2MinSize :: MemTree PaCo2MinSize -> String
showPaCo2MinSize = ("Min-size partition of path-compressed 2-tree\n" ++)
                   . showPartition
