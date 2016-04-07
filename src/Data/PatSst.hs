{-# LANGUAGE FlexibleContexts,
             IncoherentInstances #-}

module Data.PatSst
       (
         PatSst(..)
       , MhPatSst
       , MhPatSstM
       , MsPatSst
       , MsPatSstM
       ) where

import Data.Bits
import Data.Monoid
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


instance Foldable Page where
  foldMap _ Empty = mempty
  foldMap f p     = f (iTree p) <> helper f (oTree p)
    where helper g (Leaf x)   = foldMap g x
          helper g (Node l r) = helper g l <> helper g r

maxPageSize :: Int
{- Withing the maximal page size, some place is reserved for 'plpm'
   folder (18 bits) and ordinal-tree root (its size can be reduced
   from 2 to 1, because the position of its open parenthesis is
   well-known). -}
maxPageSize = 256 - 18 - 1

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
pageSize x     = treeSize $ iTree x

treeSize :: PatTree -> Int
{- The page size is build from RE indexes (18 bits for each) and
   PATRICIA-trie size. -}
treeSize t = 18 * numOfPrefixes t + gammaSize t

isFitted :: [Page PatTree] -> Bool
isFitted = (<= maxPageSize) . sum . map pageSize


pageMergeBoth :: Maybe Int -> Page PatTree -> Page PatTree -> Page PatTree
pageMergeBoth x Empty Empty = Page { iTree = bMerge x mempty mempty
                                   , depth = 1
                                   , oTree = Leaf Empty
                                   }
pageMergeBoth x lp Empty = Page { iTree = bMerge x (iTree lp) mempty
                                , depth = depth lp
                                , oTree = Node (oTree lp) (Leaf Empty)
                                }
pageMergeBoth x Empty rp = Page { iTree = bMerge x mempty (iTree rp)
                                , depth = depth rp
                                , oTree = Node (Leaf Empty) (oTree rp)
                                }
pageMergeBoth x lp rp = Page { iTree = bSingleton x <>
                                       bMerge Nothing (iTree lp) (iTree rp)
                             , depth = max (depth lp) (depth rp)
                             , oTree = Node (oTree lp) (oTree rp)
                             }

pageMergeLeft :: Bool -> Maybe Int -> Page PatTree -> Page PatTree
                 -> Page PatTree
pageMergeLeft _ x Empty Empty = Page { iTree = bMerge x mempty mempty
                                     , depth = 1
                                     , oTree = Leaf Empty
                                     }
pageMergeLeft _ x lp Empty = Page { iTree = bMerge x (iTree lp) mempty
                                  , depth = depth lp
                                  , oTree = Node (oTree lp) (Leaf Empty)
                                  }
pageMergeLeft c x Empty rp = let rp' = if c then pagePress rp else rp
                             in Page { iTree = bMerge x mempty mempty
                                     , depth = succ . depth $ rp'
                                     , oTree = Node (Leaf Empty) (Leaf rp')
                                     }
pageMergeLeft c x lp rp = let rp' = if c then pagePress rp else rp
                          in Page { iTree = bSingleton x <>
                                            bMerge Nothing (iTree lp) mempty
                                  , depth = max (depth lp) (succ . depth $ rp')
                                  , oTree = Node (oTree lp) (Leaf rp')
                                  }

pageMergeRight :: Bool -> Maybe Int -> Page PatTree -> Page PatTree
                  -> Page PatTree
pageMergeRight _ x Empty Empty = Page { iTree = bMerge x mempty mempty
                                      , depth = 1
                                      , oTree = Leaf Empty
                                      }
pageMergeRight c x lp Empty = let lp' = if c then pagePress lp else lp
                              in Page { iTree = bMerge x mempty mempty
                                      , depth = succ . depth $ lp'
                                      , oTree = Node (Leaf lp') (Leaf Empty)
                                      }
pageMergeRight _ x Empty rp = Page { iTree = bMerge x mempty (iTree rp)
                                   , depth = depth rp
                                   , oTree = Node (Leaf Empty) (oTree rp)
                                   }
pageMergeRight c x lp rp = let lp' = if c then pagePress lp else lp
                           in Page { iTree = bSingleton x <>
                                             bMerge Nothing mempty (iTree rp)
                                   , depth = max (succ . depth $ lp') (depth rp)
                                   , oTree = Node (Leaf lp') (oTree rp)
                                   }

pageInsertNew :: Bool -> Maybe Int -> Page PatTree -> Page PatTree
                 -> Page PatTree
pageInsertNew c x lp rp
  | lp == Empty || rp == Empty = npage
  | otherwise                  = npage { iTree = bSingleton x }
  where npage = Page { iTree = bMerge x mempty mempty
                     , depth = succ $ max (pageDepth lp') (pageDepth rp')
                     , oTree = Node (Leaf lp') (Leaf rp')
                     }
        lp'   = if c then pagePress lp else lp
        rp'   = if c then pagePress rp else rp

patSstBuild :: Bool -> (Bool -> Maybe Int -> Page PatTree -> Page PatTree
                        -> Page PatTree)
               -> PatTree -> Page PatTree
patSstBuild c merge t = if isEmpty t
                        then Empty
                        else merge c (bRoot t) lpage rpage
  where lpage = patSstBuild c merge . bLeftSubtree $ t
        rpage = patSstBuild c merge . bRightSubtree $ t

patSstInsert :: Bool -> (Bool -> Maybe Int -> Page PatTree -> Page PatTree
                         -> Page PatTree)
                -> Entry -> Page PatTree -> Page PatTree
patSstInsert c m = flip (helper c m) . fromEntry
  where helper :: Bool -> (Bool -> Maybe Int -> Page PatTree -> Page PatTree
                           -> Page PatTree)
                  -> Page PatTree -> PatTree -> Page PatTree
        helper c' merge page tree
          | isEmpty tree     = page
          | isPageEmpty page = patSstBuild c' merge tree
          | isEmpty itree    = let Leaf p = otree in helper c' merge p tree
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
                  lpage' = helper c' merge lpage (bLeftSubtree tree)

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
                  rpage' = helper c' merge rpage (bRightSubtree tree)
              in merge c' (bRoot tree <|> bRoot itree) lpage' rpage'
          where itree = iTree page
                otree = oTree page

pagePress :: Page PatTree -> Page PatTree
pagePress page
  | page == Empty || npage == Empty = page
  | isFitted [updPage]              = pagePress updPage
  | otherwise                       = page
  where updPage = npage { iTree = iTree page <> iTree npage }
        npage   = helper . oTree $ page
        helper :: Tree (Page PatTree) -> Page PatTree
        helper (Leaf Empty)            = Empty
        helper (Leaf p)                = Page { iTree = iTree p
                                              , depth = succ $ depth p
                                              , oTree = Leaf Empty
                                              }
        helper (Node l r)
          | lp == Empty && rp == Empty = Empty
          | rp == Empty                = lpMerged
          | lp == Empty                = rpMerged
          | pageSize lp < pageSize rp  = lpMerged
          | otherwise                  = rpMerged
          where lp       = helper l
                rp       = helper r
                lpMerged = lp { iTree = bMerge Nothing (iTree lp) mempty
                              , oTree = Node (oTree lp) r
                              }
                rpMerged = rp { iTree = bMerge Nothing mempty (iTree rp)
                              , oTree = Node l (oTree rp)
                              }

collapseLast :: Page PatTree -> Page PatTree
collapseLast page
  | isPageEmpty page                 = Empty
  | isPageLast page && isEmpty ntree = Empty
  | isPageLast page                  = page { iTree = ntree }
  | otherwise                        = page
  where ntree = collapse . iTree $ page

collapsePage :: Bool -> (Bool -> Maybe Int -> Page PatTree -> Page PatTree
                         -> Page PatTree)
                -> Page PatTree -> Page PatTree
collapsePage c merge page
  | isPageEmpty page                 = Empty
  | isPageLast page && isEmpty ntree = Empty
  | isPageLast page                  = page { iTree = ntree }
  | otherwise                        =
      collapseLast $ case oTree page of
                      Leaf p   -> page { oTree = Leaf $ collapsePage c merge p }
                      Node l r -> merge c (bRoot itree) lpage rpage
                        where itree = iTree page
                              lpage = collapsePage c merge $
                                      page { iTree = bLeftSubtree itree
                                           , oTree = l
                                           }
                              rpage = collapsePage c merge $
                                      page { iTree = bRightSubtree itree
                                           , oTree = r
                                           }
  where ntree = collapse . iTree $ page

patSstDelete :: Bool -> (Bool -> Maybe Int -> Page PatTree -> Page PatTree
                         -> Page PatTree)
                -> Entry -> Page PatTree -> Page PatTree
patSstDelete c m = flip (helper c m) . fromEntry
  where helper :: Bool -> (Bool -> Maybe Int -> Page PatTree -> Page PatTree
                           -> Page PatTree)
                  -> Page PatTree -> PatTree -> Page PatTree
        helper c' merge page tree
          | isPageEmpty page = Empty
          | isEmpty tree     = page
          | isPageLast page  =
              collapseLast $ page { iTree = delSubtree itree tree
                                  , oTree = Leaf Empty
                                  }
          | otherwise        =
              collapsePage c' merge $
              case oTree page of
               Leaf p   -> page { oTree = Leaf $ helper c' merge p tree }
               Node l r -> merge c' z lpage' rpage'
                 where troot  = bRoot itree
                       z      = if bRoot tree == troot then Nothing else troot
                       lpage  = page { iTree = bLeftSubtree itree
                                     , oTree = l
                                     }
                       lpage' = helper c' merge lpage (bLeftSubtree tree)

                       rpage  = page { iTree = bRightSubtree itree
                                     , oTree = r
                                     }
                       rpage' = helper c' merge rpage (bRightSubtree tree)
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

numOfPrefixes' :: Page PatTree -> Int
numOfPrefixes' = getSum . foldMap (Sum . numOfPrefixes)

numOfPages' :: Page PatTree -> Int
numOfPages' = getSum . foldMap (const (Sum 1))

fillSize' :: Page PatTree -> Int
{- Withing the maximal page size, some place is already used for
   'plpm' folder (18 bits) and ordinal-tree root (its size can be
   reduced from 2 to 1, because the position of its open parenthesis
   is well-known). -}
fillSize' = getSum . foldMap (\x -> Sum (18 + 1 + treeSize x))

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


mhMerge :: Bool -> Maybe Int -> Page PatTree -> Page PatTree
           -> Page PatTree
mhMerge c x lpage rpage
  | lht == rht =
      if isFitted [npage, lpage, rpage]
      then pageMergeBoth x lpage rpage
      else npage
  | lht > rht  =
      if isFitted [npage, lpage]
      then pageMergeLeft c x lpage rpage
      else npage
  | otherwise  =
      if isFitted [npage, rpage]
      then pageMergeRight c x lpage rpage
      else npage
  where lht   = pageDepth lpage
        rht   = pageDepth rpage
        npage = pageInsertNew c x lpage rpage

newtype MhPatSst = MhPatSst (Page PatTree) deriving (Eq, Show)

instance IpRouter MhPatSst where
  mkTable                    = MhPatSst . patSstBuild False mhMerge .
                               (mkTable :: [Entry] -> PatTree)
  insEntry e (MhPatSst t)    = MhPatSst $ patSstInsert False mhMerge e t
  delEntry e (MhPatSst t)    = MhPatSst $ patSstDelete False mhMerge e t
  ipLookup addr (MhPatSst t) = patSstLookup addr t
  numOfPrefixes (MhPatSst t) = numOfPrefixes' t

instance PatSst MhPatSst where
  height (MhPatSst t)     = pageDepth t
  numOfPages (MhPatSst t) = numOfPages' t
  fillSize (MhPatSst t)   = fillSize' t
  checkPages (MhPatSst t) = checkPages' t

newtype MhPatSstM = MhPatSstM (Page PatTree) deriving (Eq, Show)

instance IpRouter MhPatSstM where
  mkTable                     = MhPatSstM . patSstBuild True mhMerge .
                                (mkTable :: [Entry] -> PatTree)
  insEntry e (MhPatSstM t)    = MhPatSstM $ patSstInsert True mhMerge e t
  delEntry e (MhPatSstM t)    = MhPatSstM $ patSstDelete True mhMerge e t
  ipLookup addr (MhPatSstM t) = patSstLookup addr t
  numOfPrefixes (MhPatSstM t) = numOfPrefixes' t

instance PatSst MhPatSstM where
  height (MhPatSstM t)     = pageDepth t
  numOfPages (MhPatSstM t) = numOfPages' t
  fillSize (MhPatSstM t)   = fillSize' t
  checkPages (MhPatSstM t) = checkPages' t


msMerge :: Bool -> Maybe Int -> Page PatTree -> Page PatTree
           -> Page PatTree
msMerge c x lpage rpage
  | isFitted [npage, lpage, rpage]  = pageMergeBoth x lpage rpage
  | pageSize lpage < pageSize rpage =
      if isFitted [npage, lpage]
      then pageMergeLeft c x lpage rpage
      else npage
  | otherwise                       =
      if isFitted [npage, rpage]
      then pageMergeRight c x lpage rpage
      else npage
  where npage = pageInsertNew c x lpage rpage

newtype MsPatSst = MsPatSst (Page PatTree) deriving (Eq, Show)

instance IpRouter MsPatSst where
  mkTable                    = MsPatSst . patSstBuild False msMerge .
                               (mkTable :: [Entry] -> PatTree)
  insEntry e (MsPatSst t)    = MsPatSst $ patSstInsert False msMerge e t
  delEntry e (MsPatSst t)    = MsPatSst $ patSstDelete False msMerge e t
  ipLookup addr (MsPatSst t) = patSstLookup addr t
  numOfPrefixes (MsPatSst t) = numOfPrefixes' t

instance PatSst MsPatSst where
  height (MsPatSst t)     = pageDepth t
  numOfPages (MsPatSst t) = numOfPages' t
  fillSize (MsPatSst t)   = fillSize' t
  checkPages (MsPatSst t) = checkPages' t

newtype MsPatSstM = MsPatSstM (Page PatTree) deriving (Eq, Show)

instance IpRouter MsPatSstM where
  mkTable                     = MsPatSstM . patSstBuild True msMerge .
                                (mkTable :: [Entry] -> PatTree)
  insEntry e (MsPatSstM t)    = MsPatSstM $ patSstInsert True msMerge e t
  delEntry e (MsPatSstM t)    = MsPatSstM $ patSstDelete True msMerge e t
  ipLookup addr (MsPatSstM t) = patSstLookup addr t
  numOfPrefixes (MsPatSstM t) = numOfPrefixes' t

instance PatSst MsPatSstM where
  height (MsPatSstM t)     = pageDepth t
  numOfPages (MsPatSstM t) = numOfPages' t
  fillSize (MsPatSstM t)   = fillSize' t
  checkPages (MsPatSstM t) = checkPages' t
