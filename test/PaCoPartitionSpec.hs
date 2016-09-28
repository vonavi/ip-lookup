module PaCoPartitionSpec
       (
         paCoPrtnIpRouterSpec
       ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.PaCoPartition
import           Data.Partition     (MemTree)
import           RandomPrefixes
import           TestIpRouter

paCoPrtnIpRouterSpec :: Spec
paCoPrtnIpRouterSpec = do paCoMinHeightSpec
                          paCoMinSizeSpec

paCoMinHeightSpec :: Spec
paCoMinHeightSpec = do
  describe "Min-height partition of path-compressed tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MemTree PaCoMinHeight) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MemTree PaCoMinHeight) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MemTree PaCoMinHeight) e)
        `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MemTree PaCoMinHeight) e
        `shouldBe` (mkTable [] :: MemTree PaCoMinHeight)

paCoMinSizeSpec :: Spec
paCoMinSizeSpec = do
  describe "Min-size partition of path-compressed tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MemTree PaCoMinSize) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MemTree PaCoMinSize) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MemTree PaCoMinSize) e)
        `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MemTree PaCoMinSize) e
        `shouldBe` (mkTable [] :: MemTree PaCoMinSize)
