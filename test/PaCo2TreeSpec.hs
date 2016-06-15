module PaCo2TreeSpec
       (
         paCo2TreeSpec
       , paCo2IpRouterSpec
       ) where

import           Data.Bits
import           Data.Word
import           Test.Hspec

import           Data.IpRouter
import           Data.PaCo2Tree
import           Data.PrefixTree
import           RandomPrefixes
import           TestIpRouter

data Node = Node { list :: [Bool]
                 , pref :: Maybe Int
                 } deriving (Show, Eq)

fromPaCo2Tree :: PaCo2Tree -> Tree Node
fromPaCo2Tree (PaCo2Tree t) = fmap f t
  where f PaCo2Node { skip = k, string = v, label = p } =
          Node { list = l, pref = p }
          where l = map (\x -> v `testBit` (31 - x)) [0 .. pred k]

toPaCo2Tree :: Tree Node -> PaCo2Tree
toPaCo2Tree = PaCo2Tree . fmap f
  where f Node { list = l, pref = p } =
          PaCo2Node { skip = k, string = v, label = p }
          where k = length l
                v = foldr helper (0 :: Word32) l
                helper b = if b
                           then (`setBit` 31) . (`shiftR` 1)
                           else (`shiftR` 1)

testPaCo2Tree :: PaCo2Tree
testPaCo2Tree = mkTable . map toEntry $ l
  where toEntry (s, m, h) = Entry p h
          where p = Prefix (strToAddr s) (strToMask m)
        l = [ ("0.0.0.0",   "/1", 0)
            , ("127.0.0.0", "/2", 1)
            , ("111.0.0.0", "/4", 2)
            , ("255.0.0.0", "/1", 3)
            , ("223.0.0.0", "/4", 4)
            , ("223.0.0.0", "/5", 5)
            ]

ref2Tree :: Tree Node
ref2Tree = Bin (Bin (Bin Tip
                         Node { list = []
                              , pref = Nothing
                              }
                         Tip)
                    Node { list = []
                         , pref = Just 0
                         }
                    (Bin (Bin Tip
                              Node { list = []
                                   , pref = Nothing
                                   }
                              Tip)
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
               (Bin (Bin Tip
                         Node { list = []
                              , pref = Nothing
                              }
                         Tip)
                    Node { list = []
                         , pref = Just 3
                         }
                    (Bin (Bin Tip
                              Node { list = []
                                   , pref = Nothing
                                   }
                              Tip)
                         Node { list = [False, True]
                              , pref = Just 4
                              }
                         (Bin Tip
                              Node { list = []
                                   , pref = Just 5
                                   }
                              Tip)))

refPaCo2Tree :: PaCo2Tree
refPaCo2Tree = toPaCo2Tree ref2Tree

paCo2TreeSpec :: Spec
paCo2TreeSpec = do
  describe "Simple path-compressed 2-tree" $ do
    it "Building" $ do
      testPaCo2Tree `shouldBe` refPaCo2Tree

    it "Merging" $ do
      merge (root t) l' r' `shouldBe` refPaCo2Tree
        where t   = testPaCo2Tree
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

paCo2IpRouterSpec :: Spec
paCo2IpRouterSpec = do
  describe "Path-compressed 2-tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: PaCo2Tree) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: PaCo2Tree) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: PaCo2Tree) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: PaCo2Tree) e `shouldBe` (mkTable [] :: PaCo2Tree)
