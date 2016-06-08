{-# LANGUAGE FlexibleContexts #-}

module Data.Partition
       (
         Partition(..)
       , Partible(..)
       , Page
       , minHeightMerge
       , minSizeMerge
       ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Bits
import           Data.Monoid

import           Data.IpRouter
import           Data.PaCoTree       hiding (Tree)
import           Data.PrefixTree

class Partition a where
  height     :: a -> Int
  numOfPages :: a -> Int
  memUsage   :: a -> Int
  fillSize   :: a -> Int
  checkPages :: a -> Bool

  fillRatio :: a -> Double
  fillRatio t = fromIntegral (fillSize t) / fromIntegral (memUsage t)

class Partible a where
  pageMerge :: Maybe Int -> Page a -> Page a -> Page a

data Tree a = Leaf !a | Node !(Tree a) !(Tree a) deriving (Eq, Show)

data Page a = Empty
            | Page { iTree :: a
                   , depth :: {-# UNPACK #-} !Int
                   , oTree :: Tree (Page a)
                   }
            deriving (Eq, Show)

instance Foldable Page where
  foldMap _ Empty = mempty
  foldMap f p     = f (iTree p) <> helper f (oTree p)
    where helper g (Leaf x)   = foldMap g x
          helper g (Node l r) = helper g l <> helper g r

minPageSize :: Int
minPageSize = 128

maxPageSize :: Int
maxPageSize = 6 * minPageSize

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

pageSize :: PrefixTree a => Page a -> Int
pageSize Empty = 0
pageSize x     = treeSize $ iTree x

treeSize :: PrefixTree a => a -> Int
{- The page size is build from RE indexes (18 bits for each) and
   the size of path-compressed tree. -}
treeSize t = 18 * numOfPrefixes t + size t

checkPage :: PrefixTree a => Page a -> Bool
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

checkPagesS :: PrefixTree a => Page a -> State Bool ()
checkPagesS Empty = return ()
checkPagesS page  = case oTree page of
                     Leaf p   -> case p of
                                  Empty -> return ()
                                  _     -> do modify (&& checkPage p)
                                              checkPagesS p
                     Node l r -> do checkPagesS page { oTree = l }
                                    checkPagesS page { oTree = r }

instance (PrefixTree a, Partible a) => Partition (Page a) where
  height = pageDepth

  numOfPages = getSum . foldMap (const (Sum 1))

  memUsage = getSum . foldMap (Sum . fitPage . treeSize)
    where fitPage s = let k = ceiling $ fromIntegral s /
                              (fromIntegral minPageSize :: Double)
                      in k * minPageSize

  {- Withing the maximal page size, some place is already used for
     'plpm' folder (18 bits) and ordinal-tree root (its size can be
     reduced from 2 to 1, because the position of its open parenthesis
     is well-known). -}
  fillSize = getSum . foldMap (\x -> Sum (18 + 1 + treeSize x))

  checkPages p = execState (checkPagesS p) $ checkPage p


isFitted :: PrefixTree a => [Page a] -> Bool
{- Withing the maximal page size, some place is reserved for 'plpm'
   folder (18 bits) and ordinal-tree root (its size can be reduced
   from 2 to 1, because the position of its open parenthesis is
   well-known). -}
isFitted = (maxPageSize - 18 - 1 >=) . sum . map pageSize

pageMergeBoth :: PrefixTree a
                 => Maybe Int -> Page a -> Page a -> Page a
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

pageMergeLeft :: PrefixTree a
                 => Bool -> Maybe Int -> Page a -> Page a -> Page a
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

pageMergeRight :: PrefixTree a
                  => Bool -> Maybe Int -> Page a -> Page a -> Page a
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

pageInsertNew :: PrefixTree a
                 => Bool -> Maybe Int -> Page a -> Page a -> Page a
pageInsertNew c x lp rp
  | isPageEmpty lp || isPageEmpty rp = npage
  | otherwise                        = npage { iTree = bSingleton x }
  where npage = Page { iTree = bMerge x mempty mempty
                     , depth = succ $ max (pageDepth lp') (pageDepth rp')
                     , oTree = Node (Leaf lp') (Leaf rp')
                     }
        lp'   = if c then pagePress lp else lp
        rp'   = if c then pagePress rp else rp

patSstBuild :: (PrefixTree a, Partible a) => a -> Page a
patSstBuild t = if isEmpty t
                then Empty
                else pageMerge (bRoot t) lpage rpage
  where lpage = patSstBuild . bLeftSubtree $ t
        rpage = patSstBuild . bRightSubtree $ t

patSstInsert :: (PrefixTree a, Partible a) => Entry -> Page a -> Page a
patSstInsert = flip helper . mkTable . (:[])
  where helper :: (PrefixTree a, Partible a) => Page a -> a -> Page a
        helper page tree
          | isEmpty tree     = page
          | isPageEmpty page = patSstBuild tree
          | isEmpty itree    = let Leaf p = otree in helper p tree
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
                  lpage' = helper lpage (bLeftSubtree tree)

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
                  rpage' = helper rpage (bRightSubtree tree)
              in pageMerge (bRoot tree <|> bRoot itree) lpage' rpage'
          where itree = iTree page
                otree = oTree page

pagePress :: PrefixTree a => Page a -> Page a
pagePress page
  | isPageEmpty page || isPageEmpty npage = page
  | isFitted [updPage]                    = pagePress updPage
  | otherwise                             = page
  where updPage = npage { iTree = iTree page <> iTree npage }
        npage   = helper . oTree $ page
        helper :: PrefixTree a => Tree (Page a) -> Page a
        helper (Leaf Empty)            = Empty
        helper (Leaf p)                = Page { iTree = iTree p
                                              , depth = succ $ depth p
                                              , oTree = Leaf Empty
                                              }
        helper (Node l r)
          | isPageEmpty lp && isPageEmpty rp = Empty
          | isPageEmpty rp                   = lpMerged
          | isPageEmpty lp                   = rpMerged
          | pageSize lp < pageSize rp        = lpMerged
          | otherwise                        = rpMerged
          where lp       = helper l
                rp       = helper r
                lpMerged = lp { iTree = bMerge Nothing (iTree lp) mempty
                              , oTree = Node (oTree lp) r
                              }
                rpMerged = rp { iTree = bMerge Nothing mempty (iTree rp)
                              , oTree = Node l (oTree rp)
                              }

collapseLast :: PrefixTree a => Page a -> Page a
collapseLast page
  | isPageEmpty page                 = Empty
  | isPageLast page && isEmpty ntree = Empty
  | isPageLast page                  = page { iTree = ntree }
  | otherwise                        = page
  where ntree = collapse . iTree $ page

collapsePage :: (PrefixTree a, Partible a) => Page a -> Page a
collapsePage page
  | isPageEmpty page                 = Empty
  | isPageLast page && isEmpty ntree = Empty
  | isPageLast page                  = page { iTree = ntree }
  | otherwise                        =
      collapseLast $ case oTree page of
                      Leaf p   -> page { oTree = Leaf . collapsePage $ p }
                      Node l r -> pageMerge (bRoot itree) lpage rpage
                        where itree = iTree page
                              lpage = collapsePage $
                                      page { iTree = bLeftSubtree itree
                                           , oTree = l
                                           }
                              rpage = collapsePage $
                                      page { iTree = bRightSubtree itree
                                           , oTree = r
                                           }
  where ntree = collapse . iTree $ page

patSstDelete :: (PrefixTree a, Partible a) => Entry -> Page a -> Page a
patSstDelete = flip helper . mkTable . (:[])
  where helper :: (PrefixTree a, Partible a) => Page a -> a -> Page a
        helper page tree
          | isPageEmpty page = Empty
          | isEmpty tree     = page
          | isPageLast page  =
              collapseLast $ page { iTree = delSubtree itree tree
                                  , oTree = Leaf Empty
                                  }
          | otherwise        =
              collapsePage $
              case oTree page of
               Leaf p   -> page { oTree = Leaf $ helper p tree }
               Node l r -> pageMerge z lpage' rpage'
                 where troot  = bRoot itree
                       z      = if bRoot tree == troot then Nothing else troot
                       lpage  = page { iTree = bLeftSubtree itree
                                     , oTree = l
                                     }
                       lpage' = helper lpage (bLeftSubtree tree)

                       rpage  = page { iTree = bRightSubtree itree
                                     , oTree = r
                                     }
                       rpage' = helper rpage (bRightSubtree tree)
          where itree = iTree page

patSstLookup :: PrefixTree a => Address -> Page a -> Maybe Int
patSstLookup a t = execState (lookupState a t) Nothing

lookupState :: PrefixTree a => Address -> Page a -> State (Maybe Int) ()
lookupState (Address a) = helper 31
  where helper n page
          | isPageEmpty page = return ()
          | isEmpty t        = let Leaf p = oTree page
                               in helper n p
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

instance (IpRouter a, PrefixTree a, Partible a) => IpRouter (Page a) where
  mkTable       = patSstBuild . (mkTable :: IpRouter a => [Entry] -> a)
  insEntry      = patSstInsert
  delEntry      = patSstDelete
  ipLookup      = patSstLookup
  numOfPrefixes = getSum . foldMap (Sum . numOfPrefixes)


minHeightMerge :: PrefixTree a
                  => Bool -> Maybe Int -> Page a -> Page a -> Page a
minHeightMerge c x lpage rpage
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

minSizeMerge :: PrefixTree a
                => Bool -> Maybe Int -> Page a -> Page a -> Page a
minSizeMerge c x lpage rpage
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
