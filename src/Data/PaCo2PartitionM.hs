{-# LANGUAGE FlexibleInstances #-}

module Data.PaCo2PartitionM
       (
         Page
       , putPaCo2Prtn
       ) where

import           Data.Maybe      (fromJust)
import           Data.Monoid

import           Data.IpRouter
import           Data.PaCo2TreeM
import qualified Data.PrefixTree as PT

data Page = Page { tree  :: Tree Page PaCo2Node
                 , depth :: Int
                 }
          deriving (Eq, Show)

minPageSize :: Int
minPageSize = 128

maxPageSize :: Int
maxPageSize = 6 * minPageSize

pageDepth :: Maybe Page -> Int
pageDepth Nothing  = 0
pageDepth (Just x) = depth x

pageTree :: Maybe Page -> Tree Page PaCo2Node
pageTree Nothing  = mempty
pageTree (Just x) = tree x

pageSize :: Maybe Page -> Int
{- The page size is build from RE indexes (18 bits for each) and the
   size of path-compressed tree. -}
pageSize Nothing  = 0
pageSize (Just x) = let t = tree x in PT.size t + 18 * numOfPrefixes t

isFitted :: Maybe Page -> Bool
{- Withing the maximal page size, some place is reserved for 'plpm'
   folder (18 bits) and ordinal-tree root (its size can be reduced
   from 2 to 1, because the position of its open parenthesis is
   well-known). -}
isFitted = (maxPageSize - 18 - 1 >=) . pageSize

pageMerge :: Maybe Int -> Maybe Page -> Maybe Page -> Maybe Page
pageMerge x lp rp = Just Page { tree  = PT.merge x (pageTree lp) (pageTree rp)
                              , depth = maximum [1, pageDepth lp, pageDepth rp]
                              }

pagePrune :: Maybe Int -> Maybe Page -> Maybe Page -> Maybe Page
pagePrune x lp rp = Just Page { tree  = PT.merge x (Leaf lp) (Leaf rp)
                              , depth = succ $ max (pageDepth lp) (pageDepth rp)
                              }

minHeightMerge :: Maybe Int -> Maybe Page -> Maybe Page -> Maybe Page
minHeightMerge x lp rp = if isFitted updPage
                         then updPage
                         else newPage
  where lht = pageDepth lp
        rht = pageDepth rp
        updPage | lht == rht = pageMerge x lp rp
                | lht > rht  = pageMerge x lp (pagePrune Nothing rp Nothing)
                | otherwise  = pageMerge x (pagePrune Nothing lp Nothing) rp
        newPage = pagePrune x lp rp

prtnBuild :: Tree Page PaCo2Node -> Maybe Page
prtnBuild t = if PT.isEmpty t
              then Nothing
              else minHeightMerge (PT.root t) lpage rpage
  where lpage = prtnBuild . PT.leftSubtree $ t
        rpage = prtnBuild . PT.rightSubtree $ t

instance IpRouter (Maybe Page) where
  mkTable       = prtnBuild . (mkTable :: [Entry] -> Tree Page PaCo2Node)
  insEntry      = undefined
  delEntry      = undefined
  ipLookup      = undefined
  numOfPrefixes = getSum . foldPages (Sum . numOfPrefixes . pageTree)


class Partition a where
  height     :: a -> Int
  numOfPages :: a -> Int
  memUsage   :: a -> Int
  fillSize   :: a -> Int

foldPages :: (Monoid m, Show m) => (Maybe Page -> m) -> Maybe Page -> m
foldPages _ Nothing  = mempty
foldPages f x        = (f x <>) . helper . tree . fromJust $ x
  where helper (Leaf p)    = foldPages f p
        helper (Bin _ l r) = helper l <> helper r

instance Partition (Maybe Page) where
  height = pageDepth

  numOfPages = getSum . foldPages (const (Sum 1))

  {- Withing the maximal page size, some place is already used for
     'plpm' folder (18 bits) and ordinal-tree root (its size can be
     reduced from 2 to 1, because the position of its open parenthesis
     is well-known). -}
  memUsage = getSum . foldPages (\x -> Sum $ fitToPage (18 + 1 + pageSize x))
    where fitToPage s = let k = (s + minPageSize - 1) `div` minPageSize
                        in k * minPageSize
  fillSize = getSum . foldPages (\x -> Sum (18 + 1 + pageSize x))

putPaCo2Prtn :: Maybe Page -> IO ()
putPaCo2Prtn t = do
  putStrLn "Partition of path-compressed 2-tree"
  putStrLn . (++) "  Number of prefixes: " . show . numOfPrefixes $ t
  putStrLn . (++) "  Height:             " . show . height $ t
  putStrLn . (++) "  Number of pages:    " . show . numOfPages $ t
  putStrLn . (++) "  Memory usage:       " . show . memUsage $ t
  putStrLn . (++) "  Memory utilization: " . show $ memUtil
  putStrLn . (++) "  Fill size:          " . show . fillSize $ t
  putStrLn . (++) "  Fill ratio:         " . show $ fillRatio
    where memUtil = (\x -> 12 * x `div` 10) . memUsage $ t
          fillRatio :: Double
          fillRatio = fromIntegral (fillSize t) / fromIntegral (memUsage t)
