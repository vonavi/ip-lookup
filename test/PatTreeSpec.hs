module PatTreeSpec
       (
         patTreeSpec
       ) where

import Data.Bits
import Data.Word
import Test.Hspec

import Data.IpRouter
import Data.PatTree

data Node = Node { list :: [Bool]
                 , pref :: Maybe Int
                 } deriving (Show, Eq)

fromPatTree :: PatTree -> Tree Node
fromPatTree = fmap f . getTree
  where f PatNode { stride = k, string = v, label = p } =
          Node { list = l, pref = p }
          where l = map (\x -> v `testBit` (31 - x)) [0 .. pred k]

toPatTree :: Tree Node -> PatTree
toPatTree = PatTree . fmap f
  where f Node { list = l, pref = p } =
          PatNode { stride = k, string = v, label = p }
          where k = length l
                v = foldr helper (0 :: Word32) l
                helper b = if b
                           then (`setBit` 31) . (`shiftR` 1)
                           else (`shiftR` 1)

testIpRouter :: IpRouter a => a
testIpRouter = mkTable . map toEntry $ l
  where toEntry (s, m, h) = Entry p h
          where p = Prefix (strToAddr s) (strToMask m)
        l = [ ("0.0.0.0",   "/1", 0)
            , ("127.0.0.0", "/2", 1)
            , ("111.0.0.0", "/4", 2)
            , ("255.0.0.0", "/1", 3)
            , ("223.0.0.0", "/4", 4)
            , ("223.0.0.0", "/5", 5)
            ]

testTree :: Tree Node
testTree = Bin (Bin Tip
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

testPatTree :: PatTree
testPatTree = toPatTree testTree

patTreeSpec :: Spec
patTreeSpec = do
  describe "Simple PATRICIA tree" $ do
    it "Check building" $ do
      (testIpRouter :: PatTree) `shouldBe` testPatTree

    it "Check merging" $ do
      bInsertRoot (bRoot t) l' r' `shouldBe` testPatTree
        where t   = testIpRouter :: PatTree
              l   = bLeftSubtree t
              r   = bRightSubtree t
              ll  = bLeftSubtree l
              lr  = bRightSubtree l
              rl  = bLeftSubtree r
              rr  = bRightSubtree r
              lll = bLeftSubtree ll
              llr = bRightSubtree ll
              lrl = bLeftSubtree lr
              lrr = bRightSubtree lr
              rll = bLeftSubtree rl
              rlr = bRightSubtree rl
              rrl = bLeftSubtree rr
              rrr = bRightSubtree rr

              l'   = bInsertRoot (bRoot l) ll' lr'
              r'   = bInsertRoot (bRoot r) rl' rr'
              ll'  = bInsertRoot (bRoot ll) lll' llr'
              lr'  = bInsertRoot (bRoot lr) lrl' lrr'
              rl'  = bInsertRoot (bRoot rl) rll' rlr'
              rr'  = bInsertRoot (bRoot rr) rrl' rrr'
              lll' = bInsertRoot (bRoot lll)
                     (bLeftSubtree lll) (bRightSubtree lll)
              llr' = bInsertRoot (bRoot llr)
                     (bLeftSubtree llr) (bRightSubtree llr)
              lrl' = bInsertRoot (bRoot lrl)
                     (bLeftSubtree lrl) (bRightSubtree lrl)
              lrr' = bInsertRoot (bRoot lrr)
                     (bLeftSubtree lrr) (bRightSubtree lrr)
              rll' = bInsertRoot (bRoot rll)
                     (bLeftSubtree rll) (bRightSubtree rll)
              rlr' = bInsertRoot (bRoot rlr)
                     (bLeftSubtree rlr) (bRightSubtree rlr)
              rrl' = bInsertRoot (bRoot rrl)
                     (bLeftSubtree rrl) (bRightSubtree rrl)
              rrr' = bInsertRoot (bRoot rrr)
                     (bLeftSubtree rrr) (bRightSubtree rrr)
