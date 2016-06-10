module IpRouterSpec
       (
         ipLookupSpec
       , numOfPrefixesSpec
       , insEntriesSpec
       , delEntriesSpec
       , paCoPrtnCheckSpec
       , ordSstCheckSpec
       ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.OrdSst        (MhOrdSstT1, MhOrdSstT2, MhOrdSstT3,
                                     MhOrdSstT4)
import qualified Data.OrdSst        as OS
import           Data.PaCoPartition (MhPaCoPrtn)
import           Data.PaCoTree
import qualified Data.Partition     as Prtn
import           Data.Table
import           RandomPrefixes
import           TestIpRouter

ipLookupSpec :: Spec
ipLookupSpec = do
  describe "Simple IP lookups" $ do
    it "Check table" $ do
      testIpLookup (testIpRouter :: Table) `shouldBe` True

    it "Check path-compressed tree" $ do
      testIpLookup (testIpRouter :: PaCoTree) `shouldBe` True

    it "Check a min-height partition of path-compressed tree" $ do
      testIpLookup (testIpRouter :: MhPaCoPrtn) `shouldBe` True

    it "Check min-height SST for ordinal tree T1" $ do
      testIpLookup (testIpRouter :: MhOrdSstT1) `shouldBe` True

    it "Check min-height SST for ordinal tree T2" $ do
      testIpLookup (testIpRouter :: MhOrdSstT2) `shouldBe` True

    it "Check min-height SST for ordinal tree T3" $ do
      testIpLookup (testIpRouter :: MhOrdSstT3) `shouldBe` True

    it "Check min-height SST for ordinal tree T4" $ do
      testIpLookup (testIpRouter :: MhOrdSstT4) `shouldBe` True

numOfPrefixesSpec :: Spec
numOfPrefixesSpec = do
  describe "Number of random prefixes" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check table" $ do
      numOfPrefixes (mkTable e :: Table) `shouldBe` n

    it "Check path-compressed tree" $ do
      numOfPrefixes (mkTable e :: PaCoTree) `shouldBe` n

    it "Check a min-height partition of path-compressed tree" $ do
      numOfPrefixes (mkTable e :: MhPaCoPrtn) `shouldBe` n

    it "Check min-height SST for ordinal tree T1" $ do
      numOfPrefixes (mkTable e :: MhOrdSstT1) `shouldBe` n

    it "Check min-height SST for ordinal tree T2" $ do
      numOfPrefixes (mkTable e :: MhOrdSstT2) `shouldBe` n

    it "Check min-height SST for ordinal tree T3" $ do
      numOfPrefixes (mkTable e :: MhOrdSstT3) `shouldBe` n

    it "Check min-height SST for ordinal tree T4" $ do
      numOfPrefixes (mkTable e :: MhOrdSstT4) `shouldBe` n

insEntriesSpec :: Spec
insEntriesSpec = do
  describe "Insertion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check table" $ do
      numOfPrefixes (insEntries (mkTable [] :: Table) e) `shouldBe` n

    it "Check path-compressed tree" $ do
      numOfPrefixes (insEntries (mkTable [] :: PaCoTree) e) `shouldBe` n

    it "Check a min-height partition of path-compressed tree" $ do
      numOfPrefixes (insEntries (mkTable [] :: MhPaCoPrtn) e) `shouldBe` n

    it "Check min-height SST for ordinal tree T1" $ do
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT1) e) `shouldBe` n

    it "Check min-height SST for ordinal tree T2" $ do
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT2) e) `shouldBe` n

    it "Check min-height SST for ordinal tree T3" $ do
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT3) e) `shouldBe` n

    it "Check min-height SST for ordinal tree T4" $ do
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT4) e) `shouldBe` n

delEntriesSpec :: Spec
delEntriesSpec = do
  describe "Deletion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check table" $ do
      delEntries (mkTable e :: Table) e `shouldBe` (mkTable [] :: Table)

    it "Check path-compressed tree" $ do
      delEntries (mkTable e :: PaCoTree) e `shouldBe` (mkTable [] :: PaCoTree)

    it "Check min-height SST for ordinal tree T1" $ do
      delEntries (mkTable e :: MhOrdSstT1) e
        `shouldBe` (mkTable [] :: MhOrdSstT1)

    it "Check min-height SST for ordinal tree T2" $ do
      delEntries (mkTable e :: MhOrdSstT2) e
        `shouldBe` (mkTable [] :: MhOrdSstT2)

    it "Check min-height SST for ordinal tree T3" $ do
      delEntries (mkTable e :: MhOrdSstT3) e
        `shouldBe` (mkTable [] :: MhOrdSstT3)

    it "Check min-height SST for ordinal tree T4" $ do
      delEntries (mkTable e :: MhOrdSstT4) e
        `shouldBe` (mkTable [] :: MhOrdSstT4)

paCoPrtnCheckSpec :: Spec
paCoPrtnCheckSpec = do
  let n = 1000
      e = genRandomEntries n

  describe "Check pages built by 'mkTable'" $ do
    it "Check a min-height partition of path-compressed tree" $ do
      Prtn.checkPages (mkTable e :: MhPaCoPrtn) `shouldBe` True

  describe "Check pages built by 'insEntries'" $ do
    it "Check a min-height partition of path-compressed tree" $ do
      Prtn.checkPages (insEntries (mkTable [] :: MhPaCoPrtn) e) `shouldBe` True

ordSstCheckSpec :: Spec
ordSstCheckSpec = do
  let n = 1000
      e = genRandomEntries n

  describe "Check pages built by 'mkTable'" $ do
    it "Check min-height SST for ordinal tree T1" $ do
      OS.checkPages (mkTable e :: MhOrdSstT1) `shouldBe` True

    it "Check min-height SST for ordinal tree T2" $ do
      OS.checkPages (mkTable e :: MhOrdSstT2) `shouldBe` True

    it "Check min-height SST for ordinal tree T3" $ do
      OS.checkPages (mkTable e :: MhOrdSstT3) `shouldBe` True

    it "Check min-height SST for ordinal tree T4" $ do
      OS.checkPages (mkTable e :: MhOrdSstT4) `shouldBe` True

  describe "Check pages built by 'insEntries'" $ do
    it "Check min-height SST for ordinal tree T1" $ do
      OS.checkPages (insEntries (mkTable [] :: MhOrdSstT1) e) `shouldBe` True

    it "Check min-height SST for ordinal tree T2" $ do
      OS.checkPages (insEntries (mkTable [] :: MhOrdSstT2) e) `shouldBe` True

    it "Check min-height SST for ordinal tree T3" $ do
      OS.checkPages (insEntries (mkTable [] :: MhOrdSstT3) e) `shouldBe` True

    it "Check min-height SST for ordinal tree T4" $ do
      OS.checkPages (insEntries (mkTable [] :: MhOrdSstT4) e) `shouldBe` True
