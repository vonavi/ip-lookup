{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.PaCo2.Partition
  (
    MemTree
  , putPrtn
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Bits
import           Data.Maybe          (fromMaybe, isNothing)
import           Data.Monoid

import           Data.IpRouter
import           Data.PaCo2.Zipper

data Node a = Node { zipper :: a
                   , height :: Int
                   }
            deriving (Show, Eq)
data Tree a = Leaf (Maybe a)
            | Bin (Maybe a) (Tree a) (Tree a)
            deriving Eq
type MemTree a = Tree (Node a)

instance Show a => Show (Tree a) where
  show = head . nodeList
    where nodeList (Leaf _)           = []
          nodeList (Bin Nothing l r)  = nodeList l ++ nodeList r
          nodeList (Bin (Just x) l r) = ["Branch (" ++ show x ++ ", [" ++
                                         accStr (nodeList l ++ nodeList r) ++
                                         "])"]
            where accStr [] = ""
                  accStr xs = drop 2 . concatMap (", " ++) $ xs

instance Foldable Tree where
  foldMap _ (Leaf _)    = mempty
  foldMap f (Bin x l r) = fromMaybe mempty (f <$> x) <>
                          foldMap f l <> foldMap f r


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
delEmptyPage t
  | (Bin (Just x) Leaf{} Leaf{}) <- t
  , isNothing . getLabel . zipper $ x
    = Leaf . Just $ Node { zipper = delete . zipper $ x
                         , height = 0
                         }
  | otherwise
    = t

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

delLabel :: Zipper a => a -> a -> Maybe Int
delLabel z1 z2
  | Just s1 <- getLabel z1
  , Just s2 <- getLabel z2
  , s1 == s2               = Nothing
  | otherwise              = getLabel z1

prtnDelete :: Zipper a => a -> MemTree a -> MemTree a
prtnDelete _ t@(Leaf _)         = t
prtnDelete z t | isLeaf z       = undoSeparateRoot t
prtnDelete z (Bin (Just x) l r) = delEmptyPage $
                                  minHeightMerge (setLabel s z') l' r'
  where h   = height x
        zl  = goLeft z
        l'  = prtnDelete zl $
              updatePointer Node { zipper = goLeft . zipper $ x, height = h } l
        zl' = insert (rootZipper l') zl
        zr  = goRight . goUp $ zl'
        r'  = prtnDelete zr $
              updatePointer Node { zipper = goRight . zipper $ x, height = h } r
        zr' = insert (rootZipper r') zr
        z'  = goUp zr'
        s   = delLabel (zipper x) z'

lookupState :: Zipper a => Address -> MemTree a -> State (Maybe Int) ()
lookupState (Address a) = helper 31
  where helper _ (Leaf _)           = return ()
        helper n (Bin (Just x) l r) = do
          modify (getLabel z <|>)
          if a `testBit` n
            then helper (pred n) $
                 updatePointer Node { zipper = goRight z, height = h } r
            else helper (pred n) $
                 updatePointer Node { zipper = goLeft z, height = h } l
            where z = zipper x
                  h = height x

instance (IpRouter a, Zipper a) => IpRouter (MemTree a) where
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

putPrtn :: (IpRouter a, Zipper a) => MemTree a -> IO ()
putPrtn t = do
  putStrLn "Partition of path-compressed 2-tree"
  putStrLn . (++) "  Number of prefixes " . show . numOfPrefixes $ t
  putStrLn . (++) "  Height             " . show . rootHeight $ t
  putStrLn . (++) "  Number of pages    " . show . numOfPages $ t
  putStrLn . (++) "  Memory usage       " . show . memUsage $ t
  putStrLn . (++) "  Memory utilization " . show $ memUtil
  putStrLn . (++) "  Fill size          " . show . fillSize $ t
  putStrLn . (++) "  Fill ratio         " . show $ fillRatio
    where memUtil = (\x -> 12 * x `div` 10) . memUsage $ t
          fillRatio :: Double
          fillRatio = fromIntegral (fillSize t) / fromIntegral (memUsage t)
