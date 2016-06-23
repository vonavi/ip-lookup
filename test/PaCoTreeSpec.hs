module PaCoTreeSpec
       (
         fromPaCoTree
       , paCoTreeSpec
       , paCoIpRouterSpec
       ) where

import           Data.Bits
import           Data.Word
import           Test.Hspec

import           Data.IpRouter
import           Data.PaCoTree
import           Data.PrefixTree
import           RandomPrefixes
import           TestIpRouter

data Node = Node { list :: [Bool]
                 , pref :: Maybe Int
                 } deriving (Show, Eq)

fromPaCoTree :: PaCoTree -> Tree Node
fromPaCoTree (PaCoTree t) = fmap f t
  where f PaCoNode { skip = k, string = v, label = p } =
          Node { list = l, pref = p }
          where l = map (\x -> v `testBit` (31 - x)) [0 .. pred k]

toPaCoTree :: Tree Node -> PaCoTree
toPaCoTree = PaCoTree . fmap f
  where f Node { list = l, pref = p } =
          PaCoNode { skip = k, string = v, label = p }
          where k = length l
                v = foldr helper (0 :: Word32) l
                helper b = if b
                           then (`setBit` 31) . (`shiftR` 1)
                           else (`shiftR` 1)

testPaCoTree :: PaCoTree
testPaCoTree = mkTable . map toEntry $ l
  where toEntry (s, m, h) = Entry p h
          where p = Prefix (strToAddr s) (strToMask m)
        l = [ ("0.0.0.0",   "/1", 0)
            , ("127.0.0.0", "/2", 1)
            , ("111.0.0.0", "/4", 2)
            , ("255.0.0.0", "/1", 3)
            , ("223.0.0.0", "/4", 4)
            , ("223.0.0.0", "/5", 5)
            ]

refTree :: Tree Node
refTree = Bin (Bin Tip
                   Node { list = []
                        , pref = Just 0
                        }
                   (Bin Tip
                        Node { list = []
                             , pref = Just 1
                             }
                        (Bin Tip
                             Node { list = [False]
                                  , pref = Just 2
                                  }
                             Tip)))
              Node { list = []
                   , pref = Nothing
                   }
              (Bin Tip
                   Node { list = []
                        , pref = Just 3
                        }
                   (Bin Tip
                        Node { list = [False, True]
                             , pref = Just 4
                             }
                        (Bin Tip
                             Node { list = []
                                  , pref = Just 5
                                  }
                             Tip)))

refPaCoTree :: PaCoTree
refPaCoTree = toPaCoTree refTree

paCoTreeSpec :: Spec
paCoTreeSpec = do
  describe "Simple path-compressed tree" $ do
    it "Building" $ do
      testPaCoTree `shouldBe` refPaCoTree

    it "Merging" $ do
      merge (root t) l' r' `shouldBe` refPaCoTree
        where t   = testPaCoTree
              l   = leftSubtree t
              r   = rightSubtree t
              ll  = leftSubtree l
              lr  = rightSubtree l
              rl  = leftSubtree r
              rr  = rightSubtree r
              lll = leftSubtree ll
              llr = rightSubtree ll
              lrl = leftSubtree lr
              lrr = rightSubtree lr
              rll = leftSubtree rl
              rlr = rightSubtree rl
              rrl = leftSubtree rr
              rrr = rightSubtree rr

              l'   = merge (root l) ll' lr'
              r'   = merge (root r) rl' rr'
              ll'  = merge (root ll) lll' llr'
              lr'  = merge (root lr) lrl' lrr'
              rl'  = merge (root rl) rll' rlr'
              rr'  = merge (root rr) rrl' rrr'
              lll' = merge (root lll) (leftSubtree lll) (rightSubtree lll)
              llr' = merge (root llr) (leftSubtree llr) (rightSubtree llr)
              lrl' = merge (root lrl) (leftSubtree lrl) (rightSubtree lrl)
              lrr' = merge (root lrr) (leftSubtree lrr) (rightSubtree lrr)
              rll' = merge (root rll) (leftSubtree rll) (rightSubtree rll)
              rlr' = merge (root rlr) (leftSubtree rlr) (rightSubtree rlr)
              rrl' = merge (root rrl) (leftSubtree rrl) (rightSubtree rrl)
              rrr' = merge (root rrr) (leftSubtree rrr) (rightSubtree rrr)

paCoIpRouterSpec :: Spec
paCoIpRouterSpec = do
  describe "Path-compressed tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: PaCoTree) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: PaCoTree) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: PaCoTree) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: PaCoTree) e `shouldBe` (mkTable [] :: PaCoTree)
