module OrdTreePartition where

import Data.Monoid

import Data.OrdTree

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

data Pages a = Empty
             | Page { iTree :: a
                    , depth :: Int
                    , oTree :: Tree (Pages a)
                    }
             deriving Show

maxPageSize :: Int
maxPageSize = 3

pageSize :: OrdTree a => Pages a -> Int
pageSize Empty = 0
pageSize x     = size . iTree $ x

pageDepth :: OrdTree a => Pages a -> Int
pageDepth Empty              = 0
pageDepth Page { depth = d } = d


pageMergeBoth :: (OrdTree a, Monoid a) => Last Int
                 -> Pages a -> Pages a -> Pages a
pageMergeBoth x Empty Empty = Page { iTree = bInsertRoot x mempty mempty
                                   , depth = 1
                                   , oTree = Leaf Empty
                                   }
pageMergeBoth x lp Empty = Page { iTree = bInsertRoot x (iTree lp) mempty
                                , depth = depth lp
                                , oTree = Node (oTree lp) (Leaf Empty)
                                }
pageMergeBoth x Empty rp = Page { iTree = bInsertRoot x mempty (iTree rp)
                                , depth = depth rp
                                , oTree = Node (Leaf Empty) (oTree rp)
                                }
pageMergeBoth x lp rp = Page { iTree = bInsertRoot x (iTree lp) (iTree rp)
                             , depth = max (depth lp) (depth rp)
                             , oTree = Node (oTree lp) (oTree rp)
                             }

pageMergeLeft :: (OrdTree a, Monoid a) => Last Int
                 -> Pages a -> Pages a -> Pages a
pageMergeLeft x Empty Empty = Page { iTree = bInsertRoot x mempty mempty
                                   , depth = 1
                                   , oTree = Leaf Empty
                                   }
pageMergeLeft x lp Empty = Page { iTree = bInsertRoot x (iTree lp) mempty
                                , depth = depth lp
                                , oTree = Node (oTree lp) (Leaf Empty)
                                }
pageMergeLeft x Empty rp = Page { iTree = bInsertRoot x mempty mempty
                                , depth = succ . depth $ rp
                                , oTree = Node (Leaf Empty) (Leaf rp)
                                }
pageMergeLeft x lp rp = Page { iTree = bInsertRoot x (iTree lp) mempty
                             , depth = max (depth lp) (succ . depth $ rp)
                             , oTree = Node (oTree lp) (Leaf rp)
                             }

pageMergeRight :: (OrdTree a, Monoid a) => Last Int
                  -> Pages a -> Pages a -> Pages a
pageMergeRight x Empty Empty = Page { iTree = bInsertRoot x mempty mempty
                                    , depth = 1
                                    , oTree = Leaf Empty
                                    }
pageMergeRight x lp Empty = Page { iTree = bInsertRoot x mempty mempty
                                 , depth = succ . depth $ lp
                                 , oTree = Node (Leaf lp) (Leaf Empty)
                                 }
pageMergeRight x Empty rp = Page { iTree = bInsertRoot x mempty (iTree rp)
                                 , depth = depth rp
                                 , oTree = Node (Leaf Empty) (oTree rp)
                                 }
pageMergeRight x lp rp = Page { iTree = bInsertRoot x mempty (iTree rp)
                              , depth = max (succ . depth $ lp) (depth rp)
                              , oTree = Node (Leaf lp) (oTree rp)
                              }

ordTreePartition :: (OrdTree a, Monoid a) => a -> Pages a
ordTreePartition t
  | isEmpty t  = Empty
  | lht == rht =
      if pageSize lpage + size xt + pageSize rpage <= maxPageSize
      then pageMergeBoth x lpage rpage
      else npage
  | lht > rht  =
      if size xt + pageSize lpage <= maxPageSize
      then pageMergeLeft x lpage rpage
      else npage
  | otherwise  =
      if size xt + pageSize rpage <= maxPageSize
      then pageMergeRight x lpage rpage
      else npage
  where x     = bRoot t
        xt    = bInsertRoot x mempty mempty
        lpage = ordTreePartition . bLeftSubtree $ t
        rpage = ordTreePartition . bRightSubtree $ t
        lht   = pageDepth lpage
        rht   = pageDepth rpage
        npage = Page { iTree = xt
                     , depth = succ $ max (depth lpage) (depth rpage)
                     , oTree = Node (Leaf lpage) (Leaf rpage)
                     }
