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
  describe "Simple IP lookups" $ do
    it "Check a min-height partition of path-compressed tree" $ do
      testIpLookup (testIpRouter :: MhPaCoPrtn) `shouldBe` True

  describe "Number of random prefixes" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check a min-height partition of path-compressed tree" $ do
      numOfPrefixes (mkTable e :: MhPaCoPrtn) `shouldBe` n

  describe "Insertion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check a min-height partition of path-compressed tree" $ do
      numOfPrefixes (insEntries (mkTable [] :: MhPaCoPrtn) e) `shouldBe` n

paCoPrtnCheckSpec :: Spec
paCoPrtnCheckSpec = do
  let n = 1000
      e = genRandomEntries n

  describe "Check pages built by 'mkTable'" $ do
    it "Check a min-height partition of path-compressed tree" $ do
      checkPages (mkTable e :: MhPaCoPrtn) `shouldBe` True

  describe "Check pages built by 'insEntries'" $ do
    it "Check a min-height partition of path-compressed tree" $ do
      checkPages (insEntries (mkTable [] :: MhPaCoPrtn) e) `shouldBe` True
