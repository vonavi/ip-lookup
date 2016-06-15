module PaCo2PartitionSpec
       (
         paCo2PrtnIpRouterSpec
       , paCo2PrtnCheckSpec
       ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.PaCo2Partition
import           Data.Partition
import           RandomPrefixes
import           TestIpRouter

paCo2PrtnIpRouterSpec :: Spec
paCo2PrtnIpRouterSpec = do
  describe "Min-height partition of path-compressed 2-tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MhPaCo2Prtn) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MhPaCo2Prtn) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MhPaCo2Prtn) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MhPaCo2Prtn) e
        `shouldBe` (mkTable [] :: MhPaCo2Prtn)

paCo2PrtnCheckSpec :: Spec
paCo2PrtnCheckSpec = do
  describe "Min-height partition of path-compressed 2-tree" $ do
    it "Pages built by 'mkTable'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (mkTable e :: MhPaCo2Prtn) `shouldBe` True

    it "Pages built by 'insEntries'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (insEntries (mkTable [] :: MhPaCo2Prtn) e) `shouldBe` True
