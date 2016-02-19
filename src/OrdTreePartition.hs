module OrdTreePartition where

import Data.Monoid

import Data.OrdTree

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

data Pages a = Empty
             | Page { tree  :: a
                    , depth :: Int
                    , pages :: Tree (Pages a)
                    }
             deriving Show

maxPageSize :: Int
maxPageSize = 3

pageSize :: OrdTree a => Pages a -> Int
pageSize Empty = 0
pageSize x     = size . tree $ x

pageDepth :: OrdTree a => Pages a -> Int
pageDepth Empty              = 0
pageDepth Page { depth = d } = d


pageMergeBoth :: (OrdTree a, Monoid a) => Last Int
                 -> Pages a -> Pages a -> Pages a
pageMergeBoth x Empty Empty = Page { tree  = bInsertRoot x mempty mempty
                                   , depth = 1
                                   , pages = Leaf Empty
                                   }
pageMergeBoth x lp Empty = Page { tree  = bInsertRoot x (tree lp) mempty
                                , depth = depth lp
                                , pages = Node (pages lp) (Leaf Empty)
                                }
pageMergeBoth x Empty rp = Page { tree  = bInsertRoot x mempty (tree rp)
                                , depth = depth rp
                                , pages = Node (Leaf Empty) (pages rp)
                                }
pageMergeBoth x lp rp = Page { tree  = bInsertRoot x (tree lp) (tree rp)
                             , depth = max (depth lp) (depth rp)
                             , pages = Node (pages lp) (pages rp)
                             }

pageMergeLeft :: (OrdTree a, Monoid a) => Last Int
                 -> Pages a -> Pages a -> Pages a
pageMergeLeft x Empty Empty = Page { tree  = bInsertRoot x mempty mempty
                                   , depth = 1
                                   , pages = Leaf Empty
                                   }
pageMergeLeft x lp Empty = Page { tree  = bInsertRoot x (tree lp) mempty
                                , depth = depth lp
                                , pages = Node (pages lp) (Leaf Empty)
                                }
pageMergeLeft x Empty rp = Page { tree  = bInsertRoot x mempty mempty
                                , depth = succ . depth $ rp
                                , pages = Node (Leaf Empty) (Leaf rp)
                                }
pageMergeLeft x lp rp = Page { tree  = bInsertRoot x (tree lp) mempty
                             , depth = max (depth lp) (succ . depth $ rp)
                             , pages = Node (pages lp) (Leaf rp)
                             }

pageMergeRight :: (OrdTree a, Monoid a) => Last Int
                  -> Pages a -> Pages a -> Pages a
pageMergeRight x Empty Empty = Page { tree  = bInsertRoot x mempty mempty
                                    , depth = 1
                                    , pages = Leaf Empty
                                    }
pageMergeRight x lp Empty = Page { tree  = bInsertRoot x mempty mempty
                                 , depth = succ . depth $ lp
                                 , pages = Node (Leaf lp) (Leaf Empty)
                                 }
pageMergeRight x Empty rp = Page { tree  = bInsertRoot x mempty (tree rp)
                                 , depth = depth rp
                                 , pages = Node (Leaf Empty) (pages rp)
                                 }
pageMergeRight x lp rp = Page { tree  = bInsertRoot x mempty (tree rp)
                              , depth = max (succ . depth $ lp) (depth rp)
                              , pages = Node (Leaf lp) (pages rp)
                              }

ordTreePartition :: (OrdTree a, Monoid a) => a -> Pages a
ordTreePartition t
  | isEmpty t  = Empty
  | lht == rht =
      if pageSize lpages + size xt + pageSize rpages <= maxPageSize
      then pageMergeBoth x lpages rpages
      else npage
  | lht > rht  =
      if size xt + pageSize lpages <= maxPageSize
      then pageMergeLeft x lpages rpages
      else npage
  | otherwise  =
      if size xt + pageSize rpages <= maxPageSize
      then pageMergeRight x lpages rpages
      else npage
  where x      = bRoot t
        xt     = bInsertRoot x mempty mempty
        lpages = ordTreePartition . bLeftSubtree $ t
        rpages = ordTreePartition . bRightSubtree $ t
        lht    = pageDepth lpages
        rht    = pageDepth rpages
        npage  = Page { tree  = xt
                      , depth = succ $ max (depth lpages) (depth rpages)
                      , pages = Node (Leaf lpages) (Leaf rpages)
                      }
