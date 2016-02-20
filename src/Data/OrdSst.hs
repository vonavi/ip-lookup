module Data.OrdSst
       (
         MhOrdSstT1
       , MhOrdSstT2
       , MhOrdSstT3
       , MhOrdSstT4
       ) where

import Data.Monoid
import Control.Monad.State

import Data.IpRouter
import Data.OrdTree

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

data Page a = Empty
            | Page { iTree :: a
                   , depth :: Int
                   , oTree :: Tree (Page a)
                   }
            deriving Show

maxPageSize :: Int
maxPageSize = 256

pageSize :: OrdTree a => Page a -> Int
pageSize Empty = 0
pageSize x     = 18 * numOfPrefixes t + 3 * size t + 1
  where t = iTree x

isFitted :: OrdTree a => [Page a] -> Bool
isFitted = (<= maxPageSize) . sum . map pageSize

pageDepth :: OrdTree a => Page a -> Int
pageDepth Empty              = 0
pageDepth Page { depth = d } = d


pageMergeBoth :: (OrdTree a, Monoid a) => Last Int
                 -> Page a -> Page a -> Page a
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
                 -> Page a -> Page a -> Page a
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
                  -> Page a -> Page a -> Page a
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

minHeightOrdSst :: (OrdTree a, Monoid a) => a -> Page a
minHeightOrdSst t
  | isEmpty t  = Empty
  | lht == rht =
      if isFitted [npage, lpage, rpage]
      then pageMergeBoth x lpage rpage
      else npage
  | lht > rht  =
      if isFitted [npage, lpage]
      then pageMergeLeft x lpage rpage
      else npage
  | otherwise  =
      if isFitted [npage, rpage]
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

ordSstLookup :: OrdTree a => Address -> Page a -> Maybe Int
ordSstLookup addr t =
  getLast $ execState (lookupState (addrBits addr) t) (Last Nothing)

lookupState :: OrdTree a => [Bool] -> Page a -> State (Last Int) ()
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


newtype MhOrdSstT1 = MhOrdSstT1 (Page OrdTreeT1) deriving Show

instance IpRouter MhOrdSstT1 where
  ipBuild = MhOrdSstT1 . minHeightOrdSst . (ipBuild :: [Entry] -> OrdTreeT1)
  ipLookup addr (MhOrdSstT1 t) = ordSstLookup addr t


newtype MhOrdSstT2 = MhOrdSstT2 (Page OrdTreeT2) deriving Show

instance IpRouter MhOrdSstT2 where
  ipBuild = MhOrdSstT2 . minHeightOrdSst . (ipBuild :: [Entry] -> OrdTreeT2)
  ipLookup addr (MhOrdSstT2 t) = ordSstLookup addr t


newtype MhOrdSstT3 = MhOrdSstT3 (Page OrdTreeT3) deriving Show

instance IpRouter MhOrdSstT3 where
  ipBuild = MhOrdSstT3 . minHeightOrdSst . (ipBuild :: [Entry] -> OrdTreeT3)
  ipLookup addr (MhOrdSstT3 t) = ordSstLookup addr t


newtype MhOrdSstT4 = MhOrdSstT4 (Page OrdTreeT4) deriving Show

instance IpRouter MhOrdSstT4 where
  ipBuild = MhOrdSstT4 . minHeightOrdSst . (ipBuild :: [Entry] -> OrdTreeT4)
  ipLookup addr (MhOrdSstT4 t) = ordSstLookup addr t
