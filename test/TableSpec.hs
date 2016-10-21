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
  describe "Table" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: Table) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: Table) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: Table) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: Table) e `shouldBe` (mkTable [] :: Table)
