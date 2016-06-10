module OrdTreeSpec
       (
         ordSizeSpec
       , ordBpSpec
       , ordDfudsSpec
       , ordIpRouterSpec
       ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.OrdTree
import           Data.Paren
import           RandomPrefixes
import           TestIpRouter

testOrdTree :: (Monoid a, OrdTree a) => a
testOrdTree = mkTable . map toEntry $ l
  where toEntry (s, m, h) = let p = Prefix (strToAddr s) (strToMask m)
                            in Entry p h
        l = [ ("63.0.0.0",  "/2", 1)
            , ("63.0.0.0",  "/3", 2)
            , ("63.0.0.0",  "/1", 3)
            , ("63.0.0.0",  "/0", 4)
            , ("128.0.0.0", "/2", 5)
            , ("128.0.0.0", "/1", 6)
            , ("192.0.0.0", "/3", 7)
            , ("192.0.0.0", "/2", 8)
            ]

ordSizeSpec :: Spec
ordSizeSpec = do
  describe "Size of example ordinal tree" $ do
    it "Check ordinal tree T1" $ do
      size (testOrdTree :: OrdTreeT1) `shouldBe` 8

    it "Check ordinal tree T2" $ do
      size (testOrdTree :: OrdTreeT2) `shouldBe` 8

    it "Check ordinal tree T3" $ do
      size (testOrdTree :: OrdTreeT3) `shouldBe` 8

    it "Check ordinal tree T4" $ do
      size (testOrdTree :: OrdTreeT4) `shouldBe` 8

ordBpSpec :: Spec
ordBpSpec = do
  describe "Balanced-parentheses (BP) representation" $ do
    it "Check ordinal tree T1" $ do
      ordToBp (testOrdTree :: OrdTreeT1) `shouldBe`
        [ (Nothing, Open), (Just 4, Open), (Just 3, Open), (Just 1, Open)
        , (Just 1, Close), (Just 2, Open), (Just 2, Close), (Just 3, Close)
        , (Just 4, Close), (Just 6, Open), (Just 5, Open), (Just 5, Close)
        , (Just 6, Close), (Just 8, Open), (Just 7, Open), (Just 7, Close)
        , (Just 8, Close), (Nothing, Close)
        ]

    it "Check ordinal tree T2" $ do
      ordToBp (testOrdTree :: OrdTreeT2) `shouldBe`
        [ (Nothing, Open), (Just 8, Open), (Just 7, Open), (Just 7, Close)
        , (Just 8, Close), (Just 6, Open), (Just 5, Open), (Just 5, Close)
        , (Just 6, Close), (Just 4, Open), (Just 3, Open), (Just 2, Open)
        , (Just 2, Close), (Just 1, Open), (Just 1, Close), (Just 3, Close)
        , (Just 4, Close), (Nothing, Close)
        ]

    it "Check ordinal tree T3" $ do
      ordToBp (testOrdTree :: OrdTreeT3) `shouldBe`
        [ (Nothing, Open), (Just 4, Open), (Just 6, Open), (Just 8, Open)
        , (Just 8, Close), (Just 7, Open), (Just 7, Close), (Just 6, Close)
        , (Just 5, Open), (Just 5, Close), (Just 4, Close), (Just 3, Open)
        , (Just 3, Close), (Just 1, Open), (Just 2, Open), (Just 2, Close)
        , (Just 1, Close), (Nothing, Close)
        ]

    it "Check ordinal tree T4" $ do
      ordToBp (testOrdTree :: OrdTreeT4) `shouldBe`
        [ (Nothing, Open), (Just 1, Open), (Just 2, Open), (Just 2, Close)
        , (Just 1, Close), (Just 3, Open), (Just 3, Close), (Just 4, Open)
        , (Just 5, Open), (Just 5, Close), (Just 6, Open), (Just 7, Open)
        , (Just 7, Close), (Just 8, Open), (Just 8, Close), (Just 6, Close)
        , (Just 4, Close), (Nothing, Close)
        ]

ordDfudsSpec :: Spec
ordDfudsSpec = do
  describe "DFUDS representation" $ do
    it "Check ordinal tree T1" $ do
      ordToDfuds (testOrdTree :: OrdTreeT1) `shouldBe`
        [ (Nothing, [Open, Open, Open, Close]), (Just 4, [Open, Close])
        , (Just 3, [Open, Open, Close]), (Just 1, [Close]), (Just 2, [Close])
        , (Just 6, [Open, Close]), (Just 5, [Close]), (Just 8, [Open, Close])
        , (Just 7, [Close])
        ]

    it "Check ordinal tree T2" $ do
      ordToDfuds (testOrdTree :: OrdTreeT2) `shouldBe`
        [ (Nothing, [Open, Open, Open, Close]), (Just 8, [Open, Close])
        , (Just 7, [Close]), (Just 6, [Open, Close]), (Just 5, [Close])
        , (Just 4, [Open, Close]), (Just 3, [Open, Open, Close])
        , (Just 2, [Close]), (Just 1, [Close])
        ]

    it "Check ordinal tree T3" $ do
      ordToDfuds (testOrdTree :: OrdTreeT3) `shouldBe`
        [ (Nothing, [Open, Open, Open, Close]), (Just 4, [Open, Open, Close])
        , (Just 6, [Open, Open, Close]), (Just 8, [Close]), (Just 7, [Close])
        , (Just 5, [Close]), (Just 3, [Close]), (Just 1, [Open, Close])
        , (Just 2, [Close])
        ]

    it "Check ordinal tree T4" $ do
      ordToDfuds (testOrdTree :: OrdTreeT4) `shouldBe`
        [ (Nothing, [Open, Open, Open, Close]), (Just 1, [Open, Close])
        , (Just 2, [Close]), (Just 3, [Close]), (Just 4, [Open, Open, Close])
        , (Just 5, [Close]), (Just 6, [Open, Open, Close]), (Just 7, [Close])
        , (Just 8, [Close])
        ]


ordIpRouterSpec :: Spec
ordIpRouterSpec = do
  describe "Simple IP lookups" $ do
    it "Check ordinal tree T1" $ do
      testIpLookup (testIpRouter :: OrdTreeT1) `shouldBe` True

    it "Check ordinal tree T2" $ do
      testIpLookup (testIpRouter :: OrdTreeT2) `shouldBe` True

    it "Check ordinal tree T3" $ do
      testIpLookup (testIpRouter :: OrdTreeT3) `shouldBe` True

    it "Check ordinal tree T4" $ do
      testIpLookup (testIpRouter :: OrdTreeT4) `shouldBe` True

  describe "Number of random prefixes" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check ordinal tree T1" $ do
      numOfPrefixes (mkTable e :: OrdTreeT1) `shouldBe` n

    it "Check ordinal tree T2" $ do
      numOfPrefixes (mkTable e :: OrdTreeT2) `shouldBe` n

    it "Check ordinal tree T3" $ do
      numOfPrefixes (mkTable e :: OrdTreeT3) `shouldBe` n

    it "Check ordinal tree T4" $ do
      numOfPrefixes (mkTable e :: OrdTreeT4) `shouldBe` n

  describe "Insertion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check ordinal tree T1" $ do
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT1) e) `shouldBe` n

    it "Check ordinal tree T2" $ do
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT2) e) `shouldBe` n

    it "Check ordinal tree T3" $ do
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT3) e) `shouldBe` n

    it "Check ordinal tree T4" $ do
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT4) e) `shouldBe` n

  describe "Deletion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check ordinal tree T1" $ do
      delEntries (mkTable e :: OrdTreeT1) e `shouldBe` (mkTable [] :: OrdTreeT1)

    it "Check ordinal tree T2" $ do
      delEntries (mkTable e :: OrdTreeT2) e `shouldBe` (mkTable [] :: OrdTreeT2)

    it "Check ordinal tree T3" $ do
      delEntries (mkTable e :: OrdTreeT3) e `shouldBe` (mkTable [] :: OrdTreeT3)

    it "Check ordinal tree T4" $ do
      delEntries (mkTable e :: OrdTreeT4) e `shouldBe` (mkTable [] :: OrdTreeT4)
