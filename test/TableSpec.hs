module TableSpec
       (
         tableIpRouterSpec
       ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.Table
import           RandomPrefixes
import           TestIpRouter

tableIpRouterSpec :: Spec
tableIpRouterSpec = do
  describe "Simple IP lookups" $ do
    it "Check table" $ do
      testIpLookup (testIpRouter :: Table) `shouldBe` True

  describe "Number of random prefixes" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check table" $ do
      numOfPrefixes (mkTable e :: Table) `shouldBe` n

  describe "Insertion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check table" $ do
      numOfPrefixes (insEntries (mkTable [] :: Table) e) `shouldBe` n

  describe "Deletion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check table" $ do
      delEntries (mkTable e :: Table) e `shouldBe` (mkTable [] :: Table)
