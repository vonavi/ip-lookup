module OrdTreePartition where

import Data.Monoid

import Data.OrdTree

data Pages a = Empty | Page (a, [Pages a]) deriving Show

maxPageSize :: Int
maxPageSize = 3

pageSize :: OrdTree a => Pages a -> Int
pageSize Empty    = 0
pageSize (Page x) = size . fst $ x

pageDepth :: OrdTree a => Pages a -> Int
pageDepth Empty          = 0
pageDepth (Page (_, xs))
  | null xs   = 1
  | otherwise = succ . maximum . map pageDepth $ xs


pageMergeBoth :: (OrdTree a, Monoid a) => Last Int
                 -> Pages a -> Pages a -> Pages a
pageMergeBoth x Empty Empty = Page (bInsertRoot x mempty mempty, [])
pageMergeBoth x (Page (l, lps)) Empty = Page (bInsertRoot x l mempty, lps)
pageMergeBoth x Empty (Page (r, rps)) = Page (bInsertRoot x mempty r, rps)
pageMergeBoth x (Page (l, lps)) (Page (r, rps)) =
  Page (bInsertRoot x l r, lps ++ rps)

pageMergeLeft :: (OrdTree a, Monoid a) => Last Int
                 -> Pages a -> Pages a -> Pages a
pageMergeLeft x Empty Empty = Page (bInsertRoot x mempty mempty, [])
pageMergeLeft x (Page (l, lps)) Empty = Page (bInsertRoot x l mempty, lps)
pageMergeLeft x Empty rpage = Page (bInsertRoot x mempty mempty, [rpage])
pageMergeLeft x (Page (l, lps)) rpage =
  Page (bInsertRoot x l mempty, lps ++ [rpage])

pageMergeRight :: (OrdTree a, Monoid a) => Last Int
                  -> Pages a -> Pages a -> Pages a
pageMergeRight x Empty Empty = Page (bInsertRoot x mempty mempty, [])
pageMergeRight x lpage Empty = Page (bInsertRoot x mempty mempty, [lpage])
pageMergeRight x Empty (Page (r, rps)) = Page (bInsertRoot x mempty r, rps)
pageMergeRight x lpage (Page (r, rps)) =
  Page (bInsertRoot x mempty r, lpage : rps)

ordTreePartition :: (OrdTree a, Monoid a) => a -> Pages a
ordTreePartition t
  | isEmpty t  = Empty
  | lht == rht =
      if pageSize lpages + size xt + pageSize rpages <= maxPageSize
      then pageMergeBoth x lpages rpages
      else Page (xt, [lpages, rpages])
  | lht > rht  =
      if size xt + pageSize lpages <= maxPageSize
      then pageMergeLeft x lpages rpages
      else Page (xt, [lpages, rpages])
  | otherwise  =
      if size xt + pageSize rpages <= maxPageSize
      then pageMergeRight x lpages rpages
      else Page (xt, [lpages, rpages])
  where x      = bRoot t
        xt     = bInsertRoot x mempty mempty
        lpages = ordTreePartition . bLeftSubtree $ t
        rpages = ordTreePartition . bRightSubtree $ t
        lht    = pageDepth lpages
        rht    = pageDepth rpages
