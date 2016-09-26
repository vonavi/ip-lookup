module PaCo2PartitionSpec
       (
         paCo2PrtnIpRouterSpec
       ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.PaCo2.Partition (MemTree)
import           Data.PaCo2.Tree      (PaCo2Zipper)
import           RandomPrefixes
import           TestIpRouter

type PaCo2Partition = MemTree PaCo2Zipper

paCo2PrtnIpRouterSpec :: Spec
paCo2PrtnIpRouterSpec = do
  describe "Min-height partition of path-compressed 2-tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: PaCo2Partition) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: PaCo2Partition) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: PaCo2Partition) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: PaCo2Partition) e `shouldBe`
        (mkTable [] :: PaCo2Partition)
