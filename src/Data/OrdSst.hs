module Data.OrdSst where

import Data.Monoid
import Control.Monad.State

import Data.IpRouter
import Data.OrdTree

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

data OrdSst a = Empty
              | Page { iTree :: a
                     , depth :: Int
                     , oTree :: Tree (OrdSst a)
                     }
            deriving Show

maxPageSize :: Int
maxPageSize = 3

pageSize :: OrdTree a => OrdSst a -> Int
pageSize Empty = 0
pageSize x     = size . iTree $ x

pageDepth :: OrdTree a => OrdSst a -> Int
pageDepth Empty              = 0
pageDepth Page { depth = d } = d


pageMergeBoth :: (OrdTree a, Monoid a) => Last Int
                 -> OrdSst a -> OrdSst a -> OrdSst a
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
                 -> OrdSst a -> OrdSst a -> OrdSst a
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
                  -> OrdSst a -> OrdSst a -> OrdSst a
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

minHeightOrdSst :: (OrdTree a, Monoid a) => a -> OrdSst a
minHeightOrdSst t
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
        lpage = minHeightOrdSst . bLeftSubtree $ t
        rpage = minHeightOrdSst . bRightSubtree $ t
        lht   = pageDepth lpage
        rht   = pageDepth rpage
        npage = Page { iTree = xt
                     , depth = succ $ max (pageDepth lpage) (pageDepth rpage)
                     , oTree = Node (Leaf lpage) (Leaf rpage)
                     }

ordSstLookup :: OrdTree a => Address -> OrdSst a -> Maybe Int
ordSstLookup addr t =
  getLast $ execState (lookupState (addrBits addr) t) (Last Nothing)

lookupState :: OrdTree a => [Bool] -> OrdSst a -> State (Last Int) ()
lookupState _           Empty = return ()
lookupState []          page  = modify (`mappend` (bRoot . iTree $ page))
lookupState bits@(b:bs) page
  | isEmpty t = do let Leaf p = oTree page
                   lookupState bits p
  | otherwise = do modify (`mappend` bRoot t)
                   if b
                     then do let Node _ r = oTree page
                             lookupState bs Page { iTree = bRightSubtree t
                                                 , depth = depth page
                                                 , oTree = r
                                                 }
                     else do let Node l _ = oTree page
                             lookupState bs Page { iTree = bLeftSubtree t
                                                 , depth = depth page
                                                 , oTree = l
                                                 }
  where t = iTree page

newtype MhOrdSstT1 = MhOrdSstT1 (OrdSst OrdTreeT1) deriving Show

instance IpRouter MhOrdSstT1 where
  ipBuild = MhOrdSstT1 . minHeightOrdSst . (ipBuild :: [Entry] -> OrdTreeT1)
  ipLookup addr (MhOrdSstT1 t) = ordSstLookup addr t


newtype MhOrdSstT2 = MhOrdSstT2 (OrdSst OrdTreeT2) deriving Show

instance IpRouter MhOrdSstT2 where
  ipBuild = MhOrdSstT2 . minHeightOrdSst . (ipBuild :: [Entry] -> OrdTreeT2)
  ipLookup addr (MhOrdSstT2 t) = ordSstLookup addr t


newtype MhOrdSstT3 = MhOrdSstT3 (OrdSst OrdTreeT3) deriving Show

instance IpRouter MhOrdSstT3 where
  ipBuild = MhOrdSstT3 . minHeightOrdSst . (ipBuild :: [Entry] -> OrdTreeT3)
  ipLookup addr (MhOrdSstT3 t) = ordSstLookup addr t


newtype MhOrdSstT4 = MhOrdSstT4 (OrdSst OrdTreeT4) deriving Show

instance IpRouter MhOrdSstT4 where
  ipBuild = MhOrdSstT4 . minHeightOrdSst . (ipBuild :: [Entry] -> OrdTreeT4)
  ipLookup addr (MhOrdSstT4 t) = ordSstLookup addr t
