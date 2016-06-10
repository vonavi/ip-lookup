module OrdSstSpec
       (
         ordSstIpRouterSpec
       , ordSstCheckSpec
       )
       where

import           Test.Hspec

import           Data.IpRouter
import           Data.OrdSst
import           RandomPrefixes
import           TestIpRouter

ordSstIpRouterSpec :: Spec
ordSstIpRouterSpec = do
  describe "Simple IP lookups" $ do
    it "Check min-height SST for ordinal tree T1" $ do
      testIpLookup (testIpRouter :: MhOrdSstT1) `shouldBe` True

    it "Check min-height SST for ordinal tree T2" $ do
      testIpLookup (testIpRouter :: MhOrdSstT2) `shouldBe` True

    it "Check min-height SST for ordinal tree T3" $ do
      testIpLookup (testIpRouter :: MhOrdSstT3) `shouldBe` True

    it "Check min-height SST for ordinal tree T4" $ do
      testIpLookup (testIpRouter :: MhOrdSstT4) `shouldBe` True

  describe "Number of random prefixes" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check min-height SST for ordinal tree T1" $ do
      numOfPrefixes (mkTable e :: MhOrdSstT1) `shouldBe` n

    it "Check min-height SST for ordinal tree T2" $ do
      numOfPrefixes (mkTable e :: MhOrdSstT2) `shouldBe` n

    it "Check min-height SST for ordinal tree T3" $ do
      numOfPrefixes (mkTable e :: MhOrdSstT3) `shouldBe` n

    it "Check min-height SST for ordinal tree T4" $ do
      numOfPrefixes (mkTable e :: MhOrdSstT4) `shouldBe` n

  describe "Insertion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check min-height SST for ordinal tree T1" $ do
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT1) e) `shouldBe` n

    it "Check min-height SST for ordinal tree T2" $ do
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT2) e) `shouldBe` n

    it "Check min-height SST for ordinal tree T3" $ do
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT3) e) `shouldBe` n

    it "Check min-height SST for ordinal tree T4" $ do
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT4) e) `shouldBe` n

  describe "Deletion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
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

ordSstCheckSpec :: Spec
ordSstCheckSpec = do
  let n = 1000
      e = genRandomEntries n

  describe "Check pages built by 'mkTable'" $ do
    it "Check min-height SST for ordinal tree T1" $ do
      checkPages (mkTable e :: MhOrdSstT1) `shouldBe` True

    it "Check min-height SST for ordinal tree T2" $ do
      checkPages (mkTable e :: MhOrdSstT2) `shouldBe` True

    it "Check min-height SST for ordinal tree T3" $ do
      checkPages (mkTable e :: MhOrdSstT3) `shouldBe` True

    it "Check min-height SST for ordinal tree T4" $ do
      checkPages (mkTable e :: MhOrdSstT4) `shouldBe` True

  describe "Check pages built by 'insEntries'" $ do
    it "Check min-height SST for ordinal tree T1" $ do
      checkPages (insEntries (mkTable [] :: MhOrdSstT1) e) `shouldBe` True

    it "Check min-height SST for ordinal tree T2" $ do
      checkPages (insEntries (mkTable [] :: MhOrdSstT2) e) `shouldBe` True

    it "Check min-height SST for ordinal tree T3" $ do
      checkPages (insEntries (mkTable [] :: MhOrdSstT3) e) `shouldBe` True

    it "Check min-height SST for ordinal tree T4" $ do
      checkPages (insEntries (mkTable [] :: MhOrdSstT4) e) `shouldBe` True
