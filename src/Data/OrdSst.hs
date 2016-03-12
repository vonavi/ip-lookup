{-# LANGUAGE FlexibleContexts,
             IncoherentInstances #-}

module Data.OrdSst
       (
         OrdSst(..)
       , MhOrdSstT1
       , MhOrdSstT2
       , MhOrdSstT3
       , MhOrdSstT4
       ) where

import Data.Bits
import Control.Applicative ((<|>))
import Control.Monad.State

import Data.IpRouter
import Data.OrdTree

data Tree a = Leaf !a | Node !(Tree a) !(Tree a) deriving Show

data Page a = Empty
            | Page { iTree :: a
                   , depth :: {-# UNPACK #-} !Int
                   , oTree :: Tree (Page a)
                   }
            deriving Show

class OrdSst a where
  height     :: a -> Int
  numOfPages :: a -> Int
  fillSize   :: a -> Int
  checkPages :: a -> Bool


maxPageSize :: Int
maxPageSize = 256

isPageEmpty :: Page a -> Bool
isPageEmpty Empty = True
isPageEmpty _     = False

pageSize :: (OrdTree a, Monoid a) => Page a -> Int
pageSize Empty = 0
pageSize x     = 18 * numOfPrefixes t + 3 * size t + 1
  where t = iTree x

isFitted :: (OrdTree a, Monoid a) => [Page a] -> Bool
isFitted = (<= maxPageSize) . sum . map pageSize

pageDepth :: OrdTree a => Page a -> Int
pageDepth Empty              = 0
pageDepth Page { depth = d } = d


pageMergeBoth :: (OrdTree a, Monoid a) => Maybe Int
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

pageMergeLeft :: (OrdTree a, Monoid a) => Maybe Int
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

pageMergeRight :: (OrdTree a, Monoid a) => Maybe Int
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

mhInsertRoot :: (OrdTree a, Monoid a) => Maybe Int
                -> Page a -> Page a -> Page a
mhInsertRoot x lpage rpage
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
  where xt    = bInsertRoot x mempty mempty
        lht   = pageDepth lpage
        rht   = pageDepth rpage
        npage = Page { iTree = xt
                     , depth = succ $ max (pageDepth lpage) (pageDepth rpage)
                     , oTree = Node (Leaf lpage) (Leaf rpage)
                     }

minHeightOrdSst :: (OrdTree a, Monoid a) => a -> Page a
minHeightOrdSst t
  | isEmpty t  = Empty
  | otherwise  = mhInsertRoot (bRoot t) lpage rpage
  where lpage = minHeightOrdSst . bLeftSubtree $ t
        rpage = minHeightOrdSst . bRightSubtree $ t

ordSstLookup :: OrdTree a => Address -> Page a -> Maybe Int
ordSstLookup a t = execState (lookupState a t) Nothing

lookupState :: OrdTree a => Address -> Page a -> State (Maybe Int) ()
lookupState (Address a) = helper 31
  where helper n page
          | isPageEmpty page = return ()
          | isEmpty t        = do let Leaf p = oTree page
                                  helper n p
          | otherwise        = do
              modify (bRoot t <|>)
              when (n >= 0) $
                if a `testBit` n
                then helper (pred n) page { iTree = bRightSubtree t
                                          , oTree = r
                                          }
                else helper (pred n) page { iTree = bLeftSubtree t
                                          , oTree = l
                                          }
          where t        = iTree page
                Node l r = oTree page

foldOrdSst :: (Page a -> Int -> Int) -> Page a -> Int
foldOrdSst f p = execState (foldState f p) (f p 0)

foldState :: (Page a -> Int -> Int) -> Page a -> State Int ()
foldState _ Empty = return ()
foldState f page  = case oTree page of
                     Leaf p   -> case p of
                                  Empty -> return ()
                                  _     -> do modify (f p)
                                              foldState f p
                     Node l r -> do foldState f $ page { oTree = l }
                                    foldState f $ page { oTree = r }

numOfPrefixes' :: (OrdTree a, Monoid a) => Page a -> Int
numOfPrefixes' = foldOrdSst $ (+) . numOfPrefixes . iTree

numOfPages' :: OrdTree a => Page a -> Int
numOfPages' = foldOrdSst $ const succ

fillSize' :: (OrdTree a, Monoid a) => Page a -> Int
fillSize' = foldOrdSst $ (+) . pageSize

minHeightInsert :: (OrdTree a, Monoid a) => Entry -> Page a -> Page a
minHeightInsert = helper . fromEntry
  where helper :: (OrdTree a, Monoid a) => a -> Page a -> Page a
        helper tree page
          | isPageEmpty page = minHeightOrdSst tree
          | isEmpty tree     = page
          | isEmpty t        = let Leaf p = oTree page
                               in helper tree p
          | otherwise        =
              mhInsertRoot (bRoot tree <|> bRoot t) lpage rpage
          where t        = iTree page
                Node l r = oTree page
                lpage    = helper (bLeftSubtree tree)
                           page { iTree = bLeftSubtree t
                                , oTree = l
                                }
                rpage    = helper (bRightSubtree tree)
                           page { iTree = bRightSubtree t
                                , oTree = r
                                }

minHeightDelete :: OrdTree a => Entry -> Page a -> Page a
minHeightDelete = undefined

checkPage :: OrdTree a => Page a -> Bool
checkPage Empty                              = True
checkPage Page { iTree = t, oTree = Leaf _ } = isEmpty t
checkPage Page { iTree = t }                 = not . isEmpty $ t

checkPagesS :: OrdTree a => Page a -> State Bool ()
checkPagesS Empty = return ()
checkPagesS page  = case oTree page of
                     Leaf p   -> case p of
                                  Empty -> return ()
                                  _     -> do modify (&& checkPage p)
                                              checkPagesS p
                     Node l r -> do checkPagesS page { oTree = l }
                                    checkPagesS page { oTree = r }

checkPages' :: OrdTree a => Page a -> Bool
checkPages' p = execState (checkPagesS p) $ checkPage p


newtype MhOrdSstT1 = MhOrdSstT1 (Page OldTreeT1) deriving Show

instance IpRouter MhOrdSstT1 where
  mkTable                      =
    MhOrdSstT1 . minHeightOrdSst . (mkTable :: [Entry] -> OldTreeT1)
  insEntry e (MhOrdSstT1 t)    = MhOrdSstT1 $ minHeightInsert e t
  delEntry e (MhOrdSstT1 t)    = MhOrdSstT1 $ minHeightDelete e t
  ipLookup addr (MhOrdSstT1 t) = ordSstLookup addr t
  numOfPrefixes (MhOrdSstT1 t) = numOfPrefixes' t

instance OrdSst MhOrdSstT1 where
  height (MhOrdSstT1 t)     = pageDepth t
  numOfPages (MhOrdSstT1 t) = numOfPages' t
  fillSize (MhOrdSstT1 t)   = fillSize' t
  checkPages (MhOrdSstT1 t) = checkPages' t


newtype MhOrdSstT2 = MhOrdSstT2 (Page OldTreeT2) deriving Show

instance IpRouter MhOrdSstT2 where
  mkTable                      =
    MhOrdSstT2 . minHeightOrdSst . (mkTable :: [Entry] -> OldTreeT2)
  insEntry e (MhOrdSstT2 t)    = MhOrdSstT2 $ minHeightInsert e t
  delEntry e (MhOrdSstT2 t)    = MhOrdSstT2 $ minHeightDelete e t
  ipLookup addr (MhOrdSstT2 t) = ordSstLookup addr t
  numOfPrefixes (MhOrdSstT2 t) = numOfPrefixes' t

instance OrdSst MhOrdSstT2 where
  height (MhOrdSstT2 t)     = pageDepth t
  numOfPages (MhOrdSstT2 t) = numOfPages' t
  fillSize (MhOrdSstT2 t)   = fillSize' t
  checkPages (MhOrdSstT2 t) = checkPages' t


newtype MhOrdSstT3 = MhOrdSstT3 (Page OldTreeT3) deriving Show

instance IpRouter MhOrdSstT3 where
  mkTable                      =
    MhOrdSstT3 . minHeightOrdSst . (mkTable :: [Entry] -> OldTreeT3)
  insEntry e (MhOrdSstT3 t)    = MhOrdSstT3 $ minHeightInsert e t
  delEntry e (MhOrdSstT3 t)    = MhOrdSstT3 $ minHeightDelete e t
  ipLookup addr (MhOrdSstT3 t) = ordSstLookup addr t
  numOfPrefixes (MhOrdSstT3 t) = numOfPrefixes' t

instance OrdSst MhOrdSstT3 where
  height (MhOrdSstT3 t)     = pageDepth t
  numOfPages (MhOrdSstT3 t) = numOfPages' t
  fillSize (MhOrdSstT3 t)   = fillSize' t
  checkPages (MhOrdSstT3 t) = checkPages' t


newtype MhOrdSstT4 = MhOrdSstT4 (Page OldTreeT4) deriving Show

instance IpRouter MhOrdSstT4 where
  mkTable                      =
    MhOrdSstT4 . minHeightOrdSst . (mkTable :: [Entry] -> OldTreeT4)
  insEntry e (MhOrdSstT4 t)    = MhOrdSstT4 $ minHeightInsert e t
  delEntry e (MhOrdSstT4 t)    = MhOrdSstT4 $ minHeightDelete e t
  ipLookup addr (MhOrdSstT4 t) = ordSstLookup addr t
  numOfPrefixes (MhOrdSstT4 t) = numOfPrefixes' t

instance OrdSst MhOrdSstT4 where
  height (MhOrdSstT4 t)     = pageDepth t
  numOfPages (MhOrdSstT4 t) = numOfPages' t
  fillSize (MhOrdSstT4 t)   = fillSize' t
  checkPages (MhOrdSstT4 t) = checkPages' t
