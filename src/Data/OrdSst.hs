{-# LANGUAGE FlexibleContexts,
             IncoherentInstances #-}

module Data.OrdSst
       (
         OrdSst(..)
       , MhOrdSstT1
       , MhOrdSstT2
       , MhOrdSstT3
       , MhOrdSstT4
       , MsOrdSstT1
       , MsOrdSstT2
       , MsOrdSstT3
       , MsOrdSstT4
       ) where

import Data.Bits
import Control.Applicative ((<|>))
import Control.Monad.State

import Data.IpRouter
import Data.OrdTree

data Tree a = Leaf !a | Node !(Tree a) !(Tree a) deriving (Eq, Show)

data Page a = Empty
            | Page { iTree :: a
                   , depth :: {-# UNPACK #-} !Int
                   , oTree :: Tree (Page a)
                   }
            deriving (Eq, Show)

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

isPageLast :: Page a -> Bool
isPageLast Empty                       = True
isPageLast Page { oTree = Leaf Empty } = True
isPageLast _                           = False

pageDepth :: Page a -> Int
pageDepth Empty              = 0
pageDepth Page { depth = d } = d

treeDepth :: Tree (Page a) -> Int
treeDepth (Leaf x)   = pageDepth x
treeDepth (Node l r) = max (treeDepth l) (treeDepth r)

pageSize :: (OrdTree a, Monoid a) => Page a -> Int
pageSize Empty = 0
pageSize x     = 18 * numOfPrefixes t + 3 * size t + 1
  where t = iTree x

isFitted :: (OrdTree a, Monoid a) => [Page a] -> Bool
isFitted = (<= maxPageSize) . sum . map pageSize


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

ordSstBuild :: (OrdTree a, Monoid a)
               => (Maybe Int -> Page a -> Page a -> Page a)
               -> a -> Page a
ordSstBuild merge t = if isEmpty t
                      then Empty
                      else merge (bRoot t) lpage rpage
  where lpage = ordSstBuild merge . bLeftSubtree $ t
        rpage = ordSstBuild merge . bRightSubtree $ t

ordSstInsert :: (OrdTree a, Monoid a)
                => (Maybe Int -> Page a -> Page a -> Page a)
                -> Entry -> Page a -> Page a
ordSstInsert m = flip (helper m) . fromEntry
  where helper :: (OrdTree a, Monoid a)
                  => (Maybe Int -> Page a -> Page a -> Page a)
                  -> Page a -> a -> Page a
        helper merge page tree
          | isEmpty tree     = page
          | isPageEmpty page = ordSstBuild merge tree
          | isEmpty itree    = let Leaf p = otree in helper merge p tree
          | otherwise        =
              let lpage  = case otree of
                            Node l _   -> page { iTree = bLeftSubtree itree
                                               , depth = succ $ treeDepth l
                                               , oTree = l
                                               }
                            Leaf Empty -> page { iTree = bLeftSubtree itree
                                               , depth = 1
                                               , oTree = Leaf Empty
                                               }
                            Leaf _     -> error "Not linked page"
                  lpage' = helper merge lpage (bLeftSubtree tree)

                  rpage  = case otree of
                            Node _ r   -> page { iTree = bRightSubtree itree
                                               , depth = succ $ treeDepth r
                                               , oTree = r
                                               }
                            Leaf Empty -> page { iTree = bRightSubtree itree
                                               , depth = 1
                                               , oTree = Leaf Empty
                                               }
                            Leaf _     -> error "Not linked page"
                  rpage' = helper merge rpage (bRightSubtree tree)
              in merge (bRoot tree <|> bRoot itree) lpage' rpage'
          where itree = iTree page
                otree = oTree page

collapseLast :: OrdTree a => Page a -> Page a
collapseLast page
  | isPageEmpty page                 = Empty
  | isPageLast page && isEmpty ntree = Empty
  | isPageLast page                  = page { iTree = ntree }
  | otherwise                        = page
  where ntree = collapse . iTree $ page

collapsePage :: (OrdTree a, Monoid a)
                => (Maybe Int -> Page a -> Page a -> Page a)
                -> Page a -> Page a
collapsePage merge page
  | isPageEmpty page                 = Empty
  | isPageLast page && isEmpty ntree = Empty
  | isPageLast page                  = page { iTree = ntree }
  | otherwise                        =
      collapseLast $ case oTree page of
                      Leaf p   -> page { oTree = Leaf $ collapsePage merge p }
                      Node l r -> merge (bRoot itree) lpage rpage
                        where itree = iTree page
                              lpage = collapsePage merge $
                                      page { iTree = bLeftSubtree itree
                                           , oTree = l
                                           }
                              rpage = collapsePage merge $
                                      page { iTree = bRightSubtree itree
                                           , oTree = r
                                           }
  where ntree = collapse . iTree $ page

ordSstDelete :: (OrdTree a, Monoid a)
                => (Maybe Int -> Page a -> Page a -> Page a)
                -> Entry -> Page a -> Page a
ordSstDelete m = flip (helper m) . fromEntry
  where helper :: (OrdTree a, Monoid a)
                  => (Maybe Int -> Page a -> Page a -> Page a)
                  -> Page a -> a -> Page a
        helper merge page tree
          | isPageEmpty page = Empty
          | isEmpty tree     = page
          | isPageLast page  =
              collapseLast $ page { iTree = delSubtree itree tree
                                  , oTree = Leaf Empty
                                  }
          | otherwise        =
              collapsePage merge $
              case oTree page of
               Leaf p   -> page { oTree = Leaf $ helper merge p tree }
               Node l r -> merge z lpage' rpage'
                 where troot  = bRoot itree
                       z      = if bRoot tree == troot then Nothing else troot
                       lpage  = page { iTree = bLeftSubtree itree
                                     , oTree = l
                                     }
                       lpage' = helper merge lpage (bLeftSubtree tree)

                       rpage  = page { iTree = bRightSubtree itree
                                     , oTree = r
                                     }
                       rpage' = helper merge rpage (bRightSubtree tree)
          where itree = iTree page

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

checkPage :: OrdTree a => Page a -> Bool
checkPage page
  | isPageEmpty page              = True
  | isPageLast page               = dpt == 1
  | dpt /= succ (treeDepth otree) = False
  | otherwise                     = case otree of
                                     Leaf _   -> isEmpty itree
                                     Node _ _ -> not . isEmpty $ itree
  where itree = iTree page
        dpt   = depth page
        otree = oTree page

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


mhInsertRoot :: (OrdTree a, Monoid a)
                => Maybe Int -> Page a -> Page a -> Page a
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

newtype MhOrdSstT1 = MhOrdSstT1 (Page OrdTreeT1) deriving (Eq, Show)

instance IpRouter MhOrdSstT1 where
  mkTable                      = MhOrdSstT1 . ordSstBuild mhInsertRoot .
                                 (mkTable :: [Entry] -> OrdTreeT1)
  insEntry e (MhOrdSstT1 t)    = MhOrdSstT1 $ ordSstInsert mhInsertRoot e t
  delEntry e (MhOrdSstT1 t)    = MhOrdSstT1 $ ordSstDelete mhInsertRoot e t
  ipLookup addr (MhOrdSstT1 t) = ordSstLookup addr t
  numOfPrefixes (MhOrdSstT1 t) = numOfPrefixes' t

instance OrdSst MhOrdSstT1 where
  height (MhOrdSstT1 t)     = pageDepth t
  numOfPages (MhOrdSstT1 t) = numOfPages' t
  fillSize (MhOrdSstT1 t)   = fillSize' t
  checkPages (MhOrdSstT1 t) = checkPages' t


newtype MhOrdSstT2 = MhOrdSstT2 (Page OrdTreeT2) deriving (Eq, Show)

instance IpRouter MhOrdSstT2 where
  mkTable                      = MhOrdSstT2 . ordSstBuild mhInsertRoot .
                                 (mkTable :: [Entry] -> OrdTreeT2)
  insEntry e (MhOrdSstT2 t)    = MhOrdSstT2 $ ordSstInsert mhInsertRoot e t
  delEntry e (MhOrdSstT2 t)    = MhOrdSstT2 $ ordSstDelete mhInsertRoot e t
  ipLookup addr (MhOrdSstT2 t) = ordSstLookup addr t
  numOfPrefixes (MhOrdSstT2 t) = numOfPrefixes' t

instance OrdSst MhOrdSstT2 where
  height (MhOrdSstT2 t)     = pageDepth t
  numOfPages (MhOrdSstT2 t) = numOfPages' t
  fillSize (MhOrdSstT2 t)   = fillSize' t
  checkPages (MhOrdSstT2 t) = checkPages' t


newtype MhOrdSstT3 = MhOrdSstT3 (Page OrdTreeT3) deriving (Eq, Show)

instance IpRouter MhOrdSstT3 where
  mkTable                      = MhOrdSstT3 . ordSstBuild mhInsertRoot .
                                 (mkTable :: [Entry] -> OrdTreeT3)
  insEntry e (MhOrdSstT3 t)    = MhOrdSstT3 $ ordSstInsert mhInsertRoot e t
  delEntry e (MhOrdSstT3 t)    = MhOrdSstT3 $ ordSstDelete mhInsertRoot e t
  ipLookup addr (MhOrdSstT3 t) = ordSstLookup addr t
  numOfPrefixes (MhOrdSstT3 t) = numOfPrefixes' t

instance OrdSst MhOrdSstT3 where
  height (MhOrdSstT3 t)     = pageDepth t
  numOfPages (MhOrdSstT3 t) = numOfPages' t
  fillSize (MhOrdSstT3 t)   = fillSize' t
  checkPages (MhOrdSstT3 t) = checkPages' t


newtype MhOrdSstT4 = MhOrdSstT4 (Page OrdTreeT4) deriving (Eq, Show)

instance IpRouter MhOrdSstT4 where
  mkTable                      = MhOrdSstT4 . ordSstBuild mhInsertRoot .
                                 (mkTable :: [Entry] -> OrdTreeT4)
  insEntry e (MhOrdSstT4 t)    = MhOrdSstT4 $ ordSstInsert mhInsertRoot e t
  delEntry e (MhOrdSstT4 t)    = MhOrdSstT4 $ ordSstDelete mhInsertRoot e t
  ipLookup addr (MhOrdSstT4 t) = ordSstLookup addr t
  numOfPrefixes (MhOrdSstT4 t) = numOfPrefixes' t

instance OrdSst MhOrdSstT4 where
  height (MhOrdSstT4 t)     = pageDepth t
  numOfPages (MhOrdSstT4 t) = numOfPages' t
  fillSize (MhOrdSstT4 t)   = fillSize' t
  checkPages (MhOrdSstT4 t) = checkPages' t


msInsertRoot :: (OrdTree a, Monoid a)
                => Maybe Int -> Page a -> Page a -> Page a
msInsertRoot x lpage rpage
  | isFitted [npage, lpage, rpage]  = pageMergeBoth x lpage rpage
  | pageSize lpage < pageSize rpage =
      if isFitted [npage, lpage]
      then pageMergeLeft x lpage rpage
      else npage
  | otherwise                       =
      if isFitted [npage, rpage]
      then pageMergeRight x lpage rpage
      else npage
  where npage = Page { iTree = bInsertRoot x mempty mempty
                     , depth = succ $ max (pageDepth lpage) (pageDepth rpage)
                     , oTree = Node (Leaf lpage) (Leaf rpage)
                     }

newtype MsOrdSstT1 = MsOrdSstT1 (Page OrdTreeT1) deriving (Eq, Show)

instance IpRouter MsOrdSstT1 where
  mkTable                      = MsOrdSstT1 . ordSstBuild msInsertRoot .
                                 (mkTable :: [Entry] -> OrdTreeT1)
  insEntry e (MsOrdSstT1 t)    = MsOrdSstT1 $ ordSstInsert msInsertRoot e t
  delEntry e (MsOrdSstT1 t)    = MsOrdSstT1 $ ordSstDelete msInsertRoot e t
  ipLookup addr (MsOrdSstT1 t) = ordSstLookup addr t
  numOfPrefixes (MsOrdSstT1 t) = numOfPrefixes' t

instance OrdSst MsOrdSstT1 where
  height (MsOrdSstT1 t)     = pageDepth t
  numOfPages (MsOrdSstT1 t) = numOfPages' t
  fillSize (MsOrdSstT1 t)   = fillSize' t
  checkPages (MsOrdSstT1 t) = checkPages' t


newtype MsOrdSstT2 = MsOrdSstT2 (Page OrdTreeT2) deriving (Eq, Show)

instance IpRouter MsOrdSstT2 where
  mkTable                      = MsOrdSstT2 . ordSstBuild msInsertRoot .
                                 (mkTable :: [Entry] -> OrdTreeT2)
  insEntry e (MsOrdSstT2 t)    = MsOrdSstT2 $ ordSstInsert msInsertRoot e t
  delEntry e (MsOrdSstT2 t)    = MsOrdSstT2 $ ordSstDelete msInsertRoot e t
  ipLookup addr (MsOrdSstT2 t) = ordSstLookup addr t
  numOfPrefixes (MsOrdSstT2 t) = numOfPrefixes' t

instance OrdSst MsOrdSstT2 where
  height (MsOrdSstT2 t)     = pageDepth t
  numOfPages (MsOrdSstT2 t) = numOfPages' t
  fillSize (MsOrdSstT2 t)   = fillSize' t
  checkPages (MsOrdSstT2 t) = checkPages' t


newtype MsOrdSstT3 = MsOrdSstT3 (Page OrdTreeT3) deriving (Eq, Show)

instance IpRouter MsOrdSstT3 where
  mkTable                      = MsOrdSstT3 . ordSstBuild msInsertRoot .
                                 (mkTable :: [Entry] -> OrdTreeT3)
  insEntry e (MsOrdSstT3 t)    = MsOrdSstT3 $ ordSstInsert msInsertRoot e t
  delEntry e (MsOrdSstT3 t)    = MsOrdSstT3 $ ordSstDelete msInsertRoot e t
  ipLookup addr (MsOrdSstT3 t) = ordSstLookup addr t
  numOfPrefixes (MsOrdSstT3 t) = numOfPrefixes' t

instance OrdSst MsOrdSstT3 where
  height (MsOrdSstT3 t)     = pageDepth t
  numOfPages (MsOrdSstT3 t) = numOfPages' t
  fillSize (MsOrdSstT3 t)   = fillSize' t
  checkPages (MsOrdSstT3 t) = checkPages' t


newtype MsOrdSstT4 = MsOrdSstT4 (Page OrdTreeT4) deriving (Eq, Show)

instance IpRouter MsOrdSstT4 where
  mkTable                      = MsOrdSstT4 . ordSstBuild msInsertRoot .
                                 (mkTable :: [Entry] -> OrdTreeT4)
  insEntry e (MsOrdSstT4 t)    = MsOrdSstT4 $ ordSstInsert msInsertRoot e t
  delEntry e (MsOrdSstT4 t)    = MsOrdSstT4 $ ordSstDelete msInsertRoot e t
  ipLookup addr (MsOrdSstT4 t) = ordSstLookup addr t
  numOfPrefixes (MsOrdSstT4 t) = numOfPrefixes' t

instance OrdSst MsOrdSstT4 where
  height (MsOrdSstT4 t)     = pageDepth t
  numOfPages (MsOrdSstT4 t) = numOfPages' t
  fillSize (MsOrdSstT4 t)   = fillSize' t
  checkPages (MsOrdSstT4 t) = checkPages' t
