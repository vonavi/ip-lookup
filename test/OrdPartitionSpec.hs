module OrdPartitionSpec
  (
    ordPrtnIpRouterSpec
  ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.OrdPartition
import           RandomEntries
import           TestIpRouter

ordPrtnIpRouterSpec :: Spec
ordPrtnIpRouterSpec = do ordMinHeightSpecT1
                         ordMinSizeSpecT1
                         ordMinHeightSpecT2
                         ordMinSizeSpecT2
                         ordMinHeightSpecT3
                         ordMinSizeSpecT3
                         ordMinHeightSpecT4
                         ordMinSizeSpecT4


ordMinHeightSpecT1 :: Spec
ordMinHeightSpecT1 = do
  describe "Min-height partition of ordinal tree T1" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdMinHeightT1) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdMinHeightT1) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdMinHeightT1) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdMinHeightT1) e
        `shouldBe` (mkTable [] :: OrdMinHeightT1)

ordMinSizeSpecT1 :: Spec
ordMinSizeSpecT1 = do
  describe "Min-size partition of ordinal tree T1" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdMinSizeT1) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdMinSizeT1) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdMinSizeT1) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdMinSizeT1) e
        `shouldBe` (mkTable [] :: OrdMinSizeT1)


ordMinHeightSpecT2 :: Spec
ordMinHeightSpecT2 = do
  describe "Min-height partition of ordinal tree T2" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdMinHeightT2) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdMinHeightT2) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdMinHeightT2) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdMinHeightT2) e
        `shouldBe` (mkTable [] :: OrdMinHeightT2)

ordMinSizeSpecT2 :: Spec
ordMinSizeSpecT2 = do
  describe "Min-size partition of ordinal tree T2" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdMinSizeT2) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdMinSizeT2) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdMinSizeT2) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdMinSizeT2) e
        `shouldBe` (mkTable [] :: OrdMinSizeT2)


ordMinHeightSpecT3 :: Spec
ordMinHeightSpecT3 = do
  describe "Min-height partition of ordinal tree T3" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdMinHeightT3) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdMinHeightT3) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdMinHeightT3) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdMinHeightT3) e
        `shouldBe` (mkTable [] :: OrdMinHeightT3)

ordMinSizeSpecT3 :: Spec
ordMinSizeSpecT3 = do
  describe "Min-size partition of ordinal tree T3" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdMinSizeT3) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdMinSizeT3) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdMinSizeT3) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdMinSizeT3) e
        `shouldBe` (mkTable [] :: OrdMinSizeT3)


ordMinHeightSpecT4 :: Spec
ordMinHeightSpecT4 = do
  describe "Min-height partition of ordinal tree T4" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdMinHeightT4) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdMinHeightT4) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdMinHeightT4) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdMinHeightT4) e
        `shouldBe` (mkTable [] :: OrdMinHeightT4)

ordMinSizeSpecT4 :: Spec
ordMinSizeSpecT4 = do
  describe "Min-size partition of ordinal tree T4" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdMinSizeT4) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdMinSizeT4) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdMinSizeT4) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdMinSizeT4) e
        `shouldBe` (mkTable [] :: OrdMinSizeT4)
