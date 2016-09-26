module PaCoPartitionSpec
       (
         paCoPrtnIpRouterSpec
       ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.PaCoTreeM  (PaCoZipper)
import           Data.PartitionM (MemTree)
import           RandomPrefixes
import           TestIpRouter

type PaCoPartition = MemTree PaCoZipper

paCoPrtnIpRouterSpec :: Spec
paCoPrtnIpRouterSpec = do
  describe "Partition of path-compressed tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: PaCoPartition) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: PaCoPartition) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: PaCoPartition) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: PaCoPartition) e
        `shouldBe` (mkTable [] :: PaCoPartition)
