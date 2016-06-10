module BinTreeSpec
       (
         binIpRouterSpec
       ) where

import           Test.Hspec

import           Data.BinTree
import           Data.IpRouter
import           RandomPrefixes
import           TestIpRouter

binIpRouterSpec :: Spec
binIpRouterSpec = do
  describe "Simple IP lookups" $ do
    it "Check binary tree" $ do
      testIpLookup (testIpRouter :: BinTree) `shouldBe` True

  describe "Number of random prefixes" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check binary tree" $ do
      numOfPrefixes (mkTable e :: BinTree) `shouldBe` n

  describe "Insertion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check binary tree" $ do
      numOfPrefixes (insEntries (mkTable [] :: BinTree) e) `shouldBe` n

  describe "Deletion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check binary tree" $ do
      delEntries (mkTable e :: BinTree) e `shouldBe` (mkTable [] :: BinTree)
