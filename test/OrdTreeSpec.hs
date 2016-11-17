module OrdTreeSpec
  (
    ordBpsSpec
  , ordDfudsSpec
  , ordIpRouterSpec
  ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.Prefix
import           Data.Succinct.Paren
import           Data.Trees.OrdTree
import           RandomEntries
import           TestIpRouter

testOrdTree :: IpRouter a => a
testOrdTree = mkTable . map toEntry $ l
  where toEntry (v, m, n) = Entry { network = mkPrefix (read v :: Address) m
                                  , nextHop = n
                                  }
        l = [ ("63.0.0.0",  2, 1)
            , ("63.0.0.0",  3, 2)
            , ("63.0.0.0",  1, 3)
            , ("63.0.0.0",  0, 4)
            , ("128.0.0.0", 2, 5)
            , ("128.0.0.0", 1, 6)
            , ("192.0.0.0", 3, 7)
            , ("192.0.0.0", 2, 8)
            ]

ordBpsSpec :: Spec
ordBpsSpec = do
  describe "Balanced-parentheses sequence (BPS)" $ do
    it "Ordinal tree T1" $ do
      ordToBps (testOrdTree :: OrdTreeT1) `shouldBe`
        [ (Nothing, Open), (Just 4, Open), (Just 3, Open), (Just 1, Open)
        , (Just 1, Close), (Just 2, Open), (Just 2, Close), (Just 3, Close)
        , (Just 4, Close), (Just 6, Open), (Just 5, Open), (Just 5, Close)
        , (Just 6, Close), (Just 8, Open), (Just 7, Open), (Just 7, Close)
        , (Just 8, Close), (Nothing, Close)
        ]

    it "Ordinal tree T2" $ do
      ordToBps (testOrdTree :: OrdTreeT2) `shouldBe`
        [ (Nothing, Open), (Just 8, Open), (Just 7, Open), (Just 7, Close)
        , (Just 8, Close), (Just 6, Open), (Just 5, Open), (Just 5, Close)
        , (Just 6, Close), (Just 4, Open), (Just 3, Open), (Just 2, Open)
        , (Just 2, Close), (Just 1, Open), (Just 1, Close), (Just 3, Close)
        , (Just 4, Close), (Nothing, Close)
        ]

    it "Ordinal tree T3" $ do
      ordToBps (testOrdTree :: OrdTreeT3) `shouldBe`
        [ (Nothing, Open), (Just 4, Open), (Just 6, Open), (Just 8, Open)
        , (Just 8, Close), (Just 7, Open), (Just 7, Close), (Just 6, Close)
        , (Just 5, Open), (Just 5, Close), (Just 4, Close), (Just 3, Open)
        , (Just 3, Close), (Just 1, Open), (Just 2, Open), (Just 2, Close)
        , (Just 1, Close), (Nothing, Close)
        ]

    it "Ordinal tree T4" $ do
      ordToBps (testOrdTree :: OrdTreeT4) `shouldBe`
        [ (Nothing, Open), (Just 1, Open), (Just 2, Open), (Just 2, Close)
        , (Just 1, Close), (Just 3, Open), (Just 3, Close), (Just 4, Open)
        , (Just 5, Open), (Just 5, Close), (Just 6, Open), (Just 7, Open)
        , (Just 7, Close), (Just 8, Open), (Just 8, Close), (Just 6, Close)
        , (Just 4, Close), (Nothing, Close)
        ]

ordDfudsSpec :: Spec
ordDfudsSpec = do
  describe "DFUDS representation" $ do
    it "Ordinal tree T1" $ do
      ordToDfuds (testOrdTree :: OrdTreeT1) `shouldBe`
        [ (Nothing, [Open, Open, Open, Close]), (Just 4, [Open, Close])
        , (Just 3, [Open, Open, Close]), (Just 1, [Close]), (Just 2, [Close])
        , (Just 6, [Open, Close]), (Just 5, [Close]), (Just 8, [Open, Close])
        , (Just 7, [Close])
        ]

    it "Ordinal tree T2" $ do
      ordToDfuds (testOrdTree :: OrdTreeT2) `shouldBe`
        [ (Nothing, [Open, Open, Open, Close]), (Just 8, [Open, Close])
        , (Just 7, [Close]), (Just 6, [Open, Close]), (Just 5, [Close])
        , (Just 4, [Open, Close]), (Just 3, [Open, Open, Close])
        , (Just 2, [Close]), (Just 1, [Close])
        ]

    it "Ordinal tree T3" $ do
      ordToDfuds (testOrdTree :: OrdTreeT3) `shouldBe`
        [ (Nothing, [Open, Open, Open, Close]), (Just 4, [Open, Open, Close])
        , (Just 6, [Open, Open, Close]), (Just 8, [Close]), (Just 7, [Close])
        , (Just 5, [Close]), (Just 3, [Close]), (Just 1, [Open, Close])
        , (Just 2, [Close])
        ]

    it "Ordinal tree T4" $ do
      ordToDfuds (testOrdTree :: OrdTreeT4) `shouldBe`
        [ (Nothing, [Open, Open, Open, Close]), (Just 1, [Open, Close])
        , (Just 2, [Close]), (Just 3, [Close]), (Just 4, [Open, Open, Close])
        , (Just 5, [Close]), (Just 6, [Open, Open, Close]), (Just 7, [Close])
        , (Just 8, [Close])
        ]


ordIpRouterSpec :: Spec
ordIpRouterSpec = do
  describe "Ordinal tree T1" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdTreeT1) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdTreeT1) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT1) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdTreeT1) e `shouldBe` (mkTable [] :: OrdTreeT1)

  describe "Ordinal tree T2" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdTreeT2) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdTreeT2) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT2) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdTreeT2) e `shouldBe` (mkTable [] :: OrdTreeT2)

  describe "Ordinal tree T3" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdTreeT3) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdTreeT3) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT3) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdTreeT3) e `shouldBe` (mkTable [] :: OrdTreeT3)

  describe "Ordinal tree T4" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: OrdTreeT4) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: OrdTreeT4) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT4) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: OrdTreeT4) e `shouldBe` (mkTable [] :: OrdTreeT4)
