module OrdTreePartition where

import Data.Monoid

import Data.OrdTree

data Pages a = Empty
             | Page { tree  :: a
                    , pages :: [Pages a]
                    }
             deriving Show

maxPageSize :: Int
maxPageSize = 3

pageSize :: OrdTree a => Pages a -> Int
pageSize Empty = 0
pageSize x     = size . tree $ x

pageDepth :: OrdTree a => Pages a -> Int
pageDepth Empty = 0
pageDepth x
  | null ps   = 1
  | otherwise = succ . maximum . map pageDepth $ ps
  where ps = pages x


pageMergeBoth :: (OrdTree a, Monoid a) => Last Int
                 -> Pages a -> Pages a -> Pages a
pageMergeBoth x Empty Empty = Page { tree  = bInsertRoot x mempty mempty
                                   , pages = []
                                   }
pageMergeBoth x lp Empty = Page { tree  = bInsertRoot x (tree lp) mempty
                                , pages = pages lp
                                }
pageMergeBoth x Empty rp = Page { tree  = bInsertRoot x mempty (tree rp)
                                , pages = pages rp
                                }
pageMergeBoth x lp rp = Page { tree  = bInsertRoot x (tree lp) (tree rp)
                             , pages = pages lp ++ pages rp
                             }

pageMergeLeft :: (OrdTree a, Monoid a) => Last Int
                 -> Pages a -> Pages a -> Pages a
pageMergeLeft x Empty Empty = Page { tree  = bInsertRoot x mempty mempty
                                   , pages = []
                                   }
pageMergeLeft x lp Empty = Page { tree  = bInsertRoot x (tree lp) mempty
                                , pages = pages lp
                                }
pageMergeLeft x Empty rp = Page { tree  = bInsertRoot x mempty mempty
                                , pages = [rp]
                                }
pageMergeLeft x lp rp = Page { tree  = bInsertRoot x (tree lp) mempty
                             , pages = pages lp ++ [rp]
                             }

pageMergeRight :: (OrdTree a, Monoid a) => Last Int
                  -> Pages a -> Pages a -> Pages a
pageMergeRight x Empty Empty = Page { tree  = bInsertRoot x mempty mempty
                                    , pages = []
                                    }
pageMergeRight x lp Empty = Page { tree  = bInsertRoot x mempty mempty
                                 , pages = [lp]
                                 }
pageMergeRight x Empty rp = Page { tree  = bInsertRoot x mempty (tree rp)
                                 , pages = pages rp
                                 }
pageMergeRight x lp rp = Page { tree  = bInsertRoot x mempty (tree rp)
                              , pages = lp : pages rp
                              }

ordTreePartition :: (OrdTree a, Monoid a) => a -> Pages a
ordTreePartition t
  | isEmpty t  = Empty
  | lht == rht =
      if pageSize lpages + size xt + pageSize rpages <= maxPageSize
      then pageMergeBoth x lpages rpages
      else Page { tree = xt, pages = [lpages, rpages] }
  | lht > rht  =
      if size xt + pageSize lpages <= maxPageSize
      then pageMergeLeft x lpages rpages
      else Page { tree = xt, pages = [lpages, rpages] }
  | otherwise  =
      if size xt + pageSize rpages <= maxPageSize
      then pageMergeRight x lpages rpages
      else Page { tree = xt, pages = [lpages, rpages] }
  where x      = bRoot t
        xt     = bInsertRoot x mempty mempty
        lpages = ordTreePartition . bLeftSubtree $ t
        rpages = ordTreePartition . bRightSubtree $ t
        lht    = pageDepth lpages
        rht    = pageDepth rpages
