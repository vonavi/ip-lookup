module PaCoTreeSpec
       (
         paCoTreeSpec
       ) where

import           Data.Bits
import           Data.Word
import           Test.Hspec

import           Data.IpRouter
import           Data.PaCoTree

data Node = Node { list :: [Bool]
                 , pref :: Maybe Int
                 } deriving (Show, Eq)

fromPaCoTree :: PaCoTree -> Tree Node
fromPaCoTree = fmap f . getTree
  where f PaCoNode { stride = k, string = v, label = p } =
          Node { list = l, pref = p }
          where l = map (\x -> v `testBit` (31 - x)) [0 .. pred k]

toPaCoTree :: Tree Node -> PaCoTree
toPaCoTree = PaCoTree . fmap f
  where f Node { list = l, pref = p } =
          PaCoNode { stride = k, string = v, label = p }
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

testPaCoTree :: PaCoTree
testPaCoTree = toPaCoTree testTree

paCoTreeSpec :: Spec
paCoTreeSpec = do
  describe "Simple path-compressed tree" $ do
    it "Check building" $ do
      (testIpRouter :: PaCoTree) `shouldBe` testPaCoTree

    it "Check merging" $ do
      bMerge (bRoot t) l' r' `shouldBe` testPaCoTree
        where t   = testIpRouter :: PaCoTree
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

              l'   = bMerge (bRoot l) ll' lr'
              r'   = bMerge (bRoot r) rl' rr'
              ll'  = bMerge (bRoot ll) lll' llr'
              lr'  = bMerge (bRoot lr) lrl' lrr'
              rl'  = bMerge (bRoot rl) rll' rlr'
              rr'  = bMerge (bRoot rr) rrl' rrr'
              lll' = bMerge (bRoot lll) (bLeftSubtree lll) (bRightSubtree lll)
              llr' = bMerge (bRoot llr) (bLeftSubtree llr) (bRightSubtree llr)
              lrl' = bMerge (bRoot lrl) (bLeftSubtree lrl) (bRightSubtree lrl)
              lrr' = bMerge (bRoot lrr) (bLeftSubtree lrr) (bRightSubtree lrr)
              rll' = bMerge (bRoot rll) (bLeftSubtree rll) (bRightSubtree rll)
              rlr' = bMerge (bRoot rlr) (bLeftSubtree rlr) (bRightSubtree rlr)
              rrl' = bMerge (bRoot rrl) (bLeftSubtree rrl) (bRightSubtree rrl)
              rrr' = bMerge (bRoot rrr) (bLeftSubtree rrr) (bRightSubtree rrr)
