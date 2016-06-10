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
  describe "Min-height SST for ordinal tree T1" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MhOrdSstT1) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MhOrdSstT1) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT1) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MhOrdSstT1) e
        `shouldBe` (mkTable [] :: MhOrdSstT1)

  describe "Min-height SST for ordinal tree T2" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MhOrdSstT2) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MhOrdSstT2) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT2) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MhOrdSstT2) e
        `shouldBe` (mkTable [] :: MhOrdSstT2)

  describe "Min-height SST for ordinal tree T3" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MhOrdSstT3) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MhOrdSstT3) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT3) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MhOrdSstT3) e
        `shouldBe` (mkTable [] :: MhOrdSstT3)

  describe "Min-height SST for ordinal tree T4" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MhOrdSstT4) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MhOrdSstT4) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MhOrdSstT4) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MhOrdSstT4) e
        `shouldBe` (mkTable [] :: MhOrdSstT4)

ordSstCheckSpec :: Spec
ordSstCheckSpec = do
  describe "Min-height SST for ordinal tree T1" $ do
    it "Pages built by 'mkTable'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (mkTable e :: MhOrdSstT1) `shouldBe` True

    it "Pages built by 'insEntries'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (insEntries (mkTable [] :: MhOrdSstT1) e) `shouldBe` True

  describe "Min-height SST for ordinal tree T2" $ do
    it "Pages built by 'mkTable'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (mkTable e :: MhOrdSstT2) `shouldBe` True

    it "Pages built by 'insEntries'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (insEntries (mkTable [] :: MhOrdSstT2) e) `shouldBe` True

  describe "Min-height SST for ordinal tree T3" $ do
    it "Pages built by 'mkTable'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (mkTable e :: MhOrdSstT3) `shouldBe` True

    it "Pages built by 'insEntries'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (insEntries (mkTable [] :: MhOrdSstT3) e) `shouldBe` True

  describe "Min-height SST for ordinal tree T4" $ do
    it "Pages built by 'mkTable'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (mkTable e :: MhOrdSstT4) `shouldBe` True

    it "Pages built by 'insEntries'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (insEntries (mkTable [] :: MhOrdSstT4) e) `shouldBe` True
