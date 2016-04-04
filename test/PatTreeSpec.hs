module PatTreeSpec
       (
         patTreeSpec
       ) where

import Data.Bits
import Test.Hspec

import Data.IpRouter
import Data.PatTree

data SimpleNode = SimpleNode { list :: [Bool]
                             , pref :: Maybe Int
                             } deriving (Show, Eq)

simplifyNodes :: PatTree -> Tree SimpleNode
simplifyNodes = fmap f . getTree
  where f PatNode { stride = k, string = v, label = p } =
          SimpleNode { list = l, pref = p }
          where l = map (\x -> v `testBit` (31 - x)) [0 .. pred k]

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

testTree :: Tree SimpleNode
testTree = Bin (Bin Tip
                    SimpleNode { list = []
                               , pref = Just 0
                               }
                    (Bin Tip
                         SimpleNode { list = []
                                    , pref = Just 1
                                    }
                         (Bin Tip
                              SimpleNode { list = [False]
                                         , pref = Just 2
                                         }
                              Tip)))
               SimpleNode { list = []
                          , pref = Nothing
                          }
               (Bin Tip
                    SimpleNode { list = []
                               , pref = Just 3
                               }
                    (Bin Tip
                         SimpleNode { list = [False, True]
                                    , pref = Just 4
                                    }
                         (Bin Tip
                              SimpleNode { list = []
                                         , pref = Just 5
                                         }
                              Tip)))

patTreeSpec :: Spec
patTreeSpec = do
  describe "Simple PATRICIA tree" $ do
    it "Check building" $ do
      simplifyNodes (testIpRouter :: PatTree) `shouldBe` testTree

    it "Check merging" $ do
      simplifyNodes (bInsertRoot x l r) `shouldBe` testTree
        where t = testIpRouter :: PatTree
              x = bRoot t
              l = bLeftSubtree t
              r = bRightSubtree t
