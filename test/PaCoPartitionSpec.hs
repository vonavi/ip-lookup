module PaCoPartitionSpec
       (
         paCoPrtnIpRouterSpec
       , paCoPrtnCheckSpec
       ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.PaCoPartition
import           Data.Partition
import           RandomPrefixes
import           TestIpRouter

paCoPrtnIpRouterSpec :: Spec
paCoPrtnIpRouterSpec = do
  describe "Min-height partition of path-compressed tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MhPaCoPrtn) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MhPaCoPrtn) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MhPaCoPrtn) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MhPaCoPrtn) e
        `shouldBe` (mkTable [] :: MhPaCoPrtn)

paCoPrtnCheckSpec :: Spec
paCoPrtnCheckSpec = do
  describe "Min-height partition of path-compressed tree" $ do
    it "Pages built by 'mkTable'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (mkTable e :: MhPaCoPrtn) `shouldBe` True

    it "Pages built by 'insEntries'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (insEntries (mkTable [] :: MhPaCoPrtn) e) `shouldBe` True
