module BinPartitionSpec
  (
    binPrtnIpRouterSpec
  ) where

import           Test.Hspec

import           Data.BinPartition
import           Data.IpRouter
import           Data.Partition    (MemTree)
import           RandomPrefixes
import           TestIpRouter

binPrtnIpRouterSpec :: Spec
binPrtnIpRouterSpec = do binMinHeightSpec
                         binMinSizeSpec

binMinHeightSpec :: Spec
binMinHeightSpec = do
  describe "Min-height partition of binary tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MemTree BinMinHeight) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MemTree BinMinHeight) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MemTree BinMinHeight) e)
        `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MemTree BinMinHeight) e
        `shouldBe` (mkTable [] :: MemTree BinMinHeight)

binMinSizeSpec :: Spec
binMinSizeSpec = do
  describe "Min-size partition of binary tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MemTree BinMinSize) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MemTree BinMinSize) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MemTree BinMinSize) e)
        `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MemTree BinMinSize) e
        `shouldBe` (mkTable [] :: MemTree BinMinSize)
