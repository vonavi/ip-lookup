module OrdPartitionSpec
       (
         ordPrtnIpRouterSpec
       , ordPrtnCheckSpec
       )
       where

import           Test.Hspec

import           Data.IpRouter
import           Data.OrdPartition
import           Data.Partition
import           RandomPrefixes
import           TestIpRouter

ordPrtnIpRouterSpec :: Spec
ordPrtnIpRouterSpec = do
  describe "Min-height partition of ordinal tree T1" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MhOrdPrtnT1) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MhOrdPrtnT1) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MhOrdPrtnT1) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MhOrdPrtnT1) e
        `shouldBe` (mkTable [] :: MhOrdPrtnT1)

  describe "Min-height partition of ordinal tree T2" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MhOrdPrtnT2) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MhOrdPrtnT2) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MhOrdPrtnT2) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MhOrdPrtnT2) e
        `shouldBe` (mkTable [] :: MhOrdPrtnT2)

  describe "Min-height partition of ordinal tree T3" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MhOrdPrtnT3) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MhOrdPrtnT3) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MhOrdPrtnT3) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MhOrdPrtnT3) e
        `shouldBe` (mkTable [] :: MhOrdPrtnT3)

  describe "Min-height partition of ordinal tree T4" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: MhOrdPrtnT4) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: MhOrdPrtnT4) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: MhOrdPrtnT4) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: MhOrdPrtnT4) e
        `shouldBe` (mkTable [] :: MhOrdPrtnT4)

ordPrtnCheckSpec :: Spec
ordPrtnCheckSpec = do
  describe "Min-height partition of ordinal tree T1" $ do
    it "Pages built by 'mkTable'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (mkTable e :: MhOrdPrtnT1) `shouldBe` True

    it "Pages built by 'insEntries'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (insEntries (mkTable [] :: MhOrdPrtnT1) e) `shouldBe` True

  describe "Min-height partition of ordinal tree T2" $ do
    it "Pages built by 'mkTable'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (mkTable e :: MhOrdPrtnT2) `shouldBe` True

    it "Pages built by 'insEntries'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (insEntries (mkTable [] :: MhOrdPrtnT2) e) `shouldBe` True

  describe "Min-height partition of ordinal tree T3" $ do
    it "Pages built by 'mkTable'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (mkTable e :: MhOrdPrtnT3) `shouldBe` True

    it "Pages built by 'insEntries'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (insEntries (mkTable [] :: MhOrdPrtnT3) e) `shouldBe` True

  describe "Min-height partition of ordinal tree T4" $ do
    it "Pages built by 'mkTable'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (mkTable e :: MhOrdPrtnT4) `shouldBe` True

    it "Pages built by 'insEntries'" $ do
      let n = 1000
          e = genRandomEntries n
      checkPages (insEntries (mkTable [] :: MhOrdPrtnT4) e) `shouldBe` True
