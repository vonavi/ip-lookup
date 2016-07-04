module PaCo2PartitionSpec
       (
         paCo2PrtnIpRouterSpec
       ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.PaCo2Partition
import           RandomPrefixes
import           TestIpRouter

paCo2PrtnIpRouterSpec :: Spec
paCo2PrtnIpRouterSpec = do
  describe "Min-height partition of path-compressed 2-tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: Maybe Page) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: Maybe Page) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: Maybe Page) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: Maybe Page) e `shouldBe`
        (mkTable [] :: Maybe Page)
