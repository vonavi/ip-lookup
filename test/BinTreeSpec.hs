module BinTreeSpec
  (
    binIpRouterSpec
  ) where

import           Test.Hspec

import           Data.BinTree
import           Data.IpRouter
import           RandomEntries
import           TestIpRouter

binIpRouterSpec :: Spec
binIpRouterSpec = do
  describe "Binary tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: BinTree) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: BinTree) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: BinTree) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: BinTree) e `shouldBe` (mkTable [] :: BinTree)
