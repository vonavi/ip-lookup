module PaCo2PartitionSpec
  (
    paCo2PrtnIpRouterSpec
  ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.PaCo2Partition
import           RandomEntries
import           TestIpRouter

paCo2PrtnIpRouterSpec :: Spec
paCo2PrtnIpRouterSpec = do paCo2MinHeightSpec
                           paCo2MinSizeSpec

paCo2MinHeightSpec :: Spec
paCo2MinHeightSpec = do
  describe "Min-height partition of path-compressed 2-tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MemTree PaCo2MinHeight) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MemTree PaCo2MinHeight) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MemTree PaCo2MinHeight) e)
        `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MemTree PaCo2MinHeight) e `shouldBe`
        (mkTable [] :: MemTree PaCo2MinHeight)

paCo2MinSizeSpec :: Spec
paCo2MinSizeSpec = do
  describe "Min-size partition of path-compressed 2-tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MemTree PaCo2MinSize) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MemTree PaCo2MinSize) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MemTree PaCo2MinSize) e)
        `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MemTree PaCo2MinSize) e `shouldBe`
        (mkTable [] :: MemTree PaCo2MinSize)
