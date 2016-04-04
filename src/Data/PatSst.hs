{-# LANGUAGE FlexibleContexts,
             IncoherentInstances #-}

module Data.PatSst
       (
         PatSst(..)
       , MhPatSst
       , MsPatSst
       ) where

import Data.Bits
import Control.Applicative ((<|>))
import Control.Monad.State

import Data.IpRouter
import Data.PatTree hiding (Tree)

data Tree a = Leaf !a | Node !(Tree a) !(Tree a) deriving (Eq, Show)

data Page a = Empty
            | Page { iTree :: a
                   , depth :: {-# UNPACK #-} !Int
                   , oTree :: Tree (Page a)
                   }
            deriving (Eq, Show)

class PatSst a where
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

pageSize :: Page PatTree -> Int
pageSize Empty = 0
pageSize x     =
  {- The page size is build from the following parts: 'plpm' folder
     (18 bits), RE indexes (18 bits for each), ordinal-tree root (its
     size can be reduced from 2 to 1, because the position of its open
     parenthesis is well-known), PATRICIA-trie size. -}
  let t = iTree x in 18 + 18 * numOfPrefixes t + 1 + gammaSize t

isFitted :: [Page PatTree] -> Bool
isFitted = (<= maxPageSize) . sum . map pageSize


pageMergeBoth :: Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree
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

pageMergeLeft :: Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree
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

pageMergeRight :: Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree
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

patSstBuild :: (Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree)
               -> PatTree -> Page PatTree
patSstBuild merge t = if isEmpty t
                      then Empty
                      else merge (bRoot t) lpage rpage
  where lpage = patSstBuild merge . bLeftSubtree $ t
        rpage = patSstBuild merge . bRightSubtree $ t

patSstInsert :: (Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree)
                -> Entry -> Page PatTree -> Page PatTree
patSstInsert m = flip (helper m) . fromEntry
  where helper :: (Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree)
                  -> Page PatTree -> PatTree -> Page PatTree
        helper merge page tree
          | isEmpty tree     = page
          | isPageEmpty page = patSstBuild merge tree
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

collapseLast :: Page PatTree -> Page PatTree
collapseLast page
  | isPageEmpty page                 = Empty
  | isPageLast page && isEmpty ntree = Empty
  | isPageLast page                  = page { iTree = ntree }
  | otherwise                        = page
  where ntree = collapse . iTree $ page

collapsePage :: (Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree)
                -> Page PatTree -> Page PatTree
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

patSstDelete :: (Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree)
                -> Entry -> Page PatTree -> Page PatTree
patSstDelete m = flip (helper m) . fromEntry
  where helper :: (Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree)
                  -> Page PatTree -> PatTree -> Page PatTree
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

patSstLookup :: Address -> Page PatTree -> Maybe Int
patSstLookup a t = execState (lookupState a t) Nothing

lookupState :: Address -> Page PatTree -> State (Maybe Int) ()
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

foldPatSst :: (Page a -> Int -> Int) -> Page a -> Int
foldPatSst f p = execState (foldState f p) (f p 0)

foldState :: (Page a -> Int -> Int) -> Page a -> State Int ()
foldState _ Empty = return ()
foldState f page  = case oTree page of
                     Leaf p   -> case p of
                                  Empty -> return ()
                                  _     -> do modify (f p)
                                              foldState f p
                     Node l r -> do foldState f $ page { oTree = l }
                                    foldState f $ page { oTree = r }

numOfPrefixes' :: Page PatTree -> Int
numOfPrefixes' = foldPatSst $ (+) . numOfPrefixes . iTree

numOfPages' :: Page PatTree -> Int
numOfPages' = foldPatSst $ const succ

fillSize' :: Page PatTree -> Int
fillSize' = foldPatSst $ (+) . pageSize

checkPage :: Page PatTree -> Bool
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

checkPagesS :: Page PatTree -> State Bool ()
checkPagesS Empty = return ()
checkPagesS page  = case oTree page of
                     Leaf p   -> case p of
                                  Empty -> return ()
                                  _     -> do modify (&& checkPage p)
                                              checkPagesS p
                     Node l r -> do checkPagesS page { oTree = l }
                                    checkPagesS page { oTree = r }

checkPages' :: Page PatTree -> Bool
checkPages' p = execState (checkPagesS p) $ checkPage p


mhInsertRoot :: Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree
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

newtype MhPatSst = MhPatSst (Page PatTree) deriving (Eq, Show)

instance IpRouter MhPatSst where
  mkTable                    = MhPatSst . patSstBuild mhInsertRoot .
                               (mkTable :: [Entry] -> PatTree)
  insEntry e (MhPatSst t)    = MhPatSst $ patSstInsert mhInsertRoot e t
  delEntry e (MhPatSst t)    = MhPatSst $ patSstDelete mhInsertRoot e t
  ipLookup addr (MhPatSst t) = patSstLookup addr t
  numOfPrefixes (MhPatSst t) = numOfPrefixes' t

instance PatSst MhPatSst where
  height (MhPatSst t)     = pageDepth t
  numOfPages (MhPatSst t) = numOfPages' t
  fillSize (MhPatSst t)   = fillSize' t
  checkPages (MhPatSst t) = checkPages' t


msInsertRoot :: Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree
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

newtype MsPatSst = MsPatSst (Page PatTree) deriving (Eq, Show)

instance IpRouter MsPatSst where
  mkTable                    = MsPatSst . patSstBuild msInsertRoot .
                               (mkTable :: [Entry] -> PatTree)
  insEntry e (MsPatSst t)    = MsPatSst $ patSstInsert msInsertRoot e t
  delEntry e (MsPatSst t)    = MsPatSst $ patSstDelete msInsertRoot e t
  ipLookup addr (MsPatSst t) = patSstLookup addr t
  numOfPrefixes (MsPatSst t) = numOfPrefixes' t

instance PatSst MsPatSst where
  height (MsPatSst t)     = pageDepth t
  numOfPages (MsPatSst t) = numOfPages' t
  fillSize (MsPatSst t)   = fillSize' t
  checkPages (MsPatSst t) = checkPages' t
