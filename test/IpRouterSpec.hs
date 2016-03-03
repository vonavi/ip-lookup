module IpRouterSpec
       (
         ipLookupSpec
       , numOfPrefixesSpec
       , insEntriesSpec
       , delEntriesSpec
       ) where

import Test.Hspec

import Data.IpRouter
import RandomPrefixes
import Data.Table
import Data.BinTree
import Data.OrdTree
import Data.OrdSst

testIpRouter :: IpRouter a => a
testIpRouter = mkTable . map toEntry $ l
  where toEntry (s, m, h) = Entry p h
          where p = Prefix (strToAddr s) (strToMask m)
        l = [ ("0.0.0.0",       "/0",  0)
            , ("192.168.0.0",   "/24", 1)
            , ("192.168.0.1",   "/22", 2)
            , ("192.168.0.255", "/32", 3)
            , ("192.168.1.0",   "/28", 4)
            , ("192.168.1.2",   "/32", 5)
            ]

testIpLookup :: IpRouter a => a -> Bool
testIpLookup router = and $ zipWith (==) testHopList nextHopList
  where
    testHopList = map (\x -> ipLookup (strToAddr x) router) addrList
    addrList    = [ "0.0.0.0"
                  , "192.168.0.0"
                  , "192.168.0.1"
                  , "192.168.0.255"
                  , "192.168.1.2"
                  , "192.168.1.0"
                  , "192.168.3.17"
                  , "192.168.4.17"
                  ]
    nextHopList = [ Just 0
                  , Just 1
                  , Just 1
                  , Just 3
                  , Just 5
                  , Just 4
                  , Just 2
                  , Just 0
                  ]

genRandomEntries :: Int -> [Entry]
genRandomEntries n = Entry (Prefix (Address 0) (Mask 0)) 0 :
                     randomEntries (32, 32) [1 .. pred n]

insEntries :: IpRouter a => a -> [Entry] -> a
insEntries = foldr insEntry

delEntries :: IpRouter a => a -> [Entry] -> a
delEntries = foldr delEntry


ipLookupSpec :: Spec
ipLookupSpec = do
  describe "Simple IP lookups" $ do
    it "Check table" $ do
      testIpLookup (testIpRouter :: Table) `shouldBe` True

    it "Check binary tree" $ do
      testIpLookup (testIpRouter :: BinTree) `shouldBe` True

    it "Check ordinal tree T1" $ do
      testIpLookup (testIpRouter :: OrdTreeT1) `shouldBe` True

    it "Check ordinal tree T2" $ do
      testIpLookup (testIpRouter :: OrdTreeT2) `shouldBe` True

    it "Check ordinal tree T3" $ do
      testIpLookup (testIpRouter :: OrdTreeT3) `shouldBe` True

    it "Check ordinal tree T4" $ do
      testIpLookup (testIpRouter :: OrdTreeT4) `shouldBe` True

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

    it "Check binary tree" $ do
      numOfPrefixes (mkTable e :: BinTree) `shouldBe` n

    it "Check ordinal tree T1" $ do
      numOfPrefixes (mkTable e :: OrdTreeT1) `shouldBe` n

    it "Check ordinal tree T2" $ do
      numOfPrefixes (mkTable e :: OrdTreeT2) `shouldBe` n

    it "Check ordinal tree T3" $ do
      numOfPrefixes (mkTable e :: OrdTreeT3) `shouldBe` n

    it "Check ordinal tree T4" $ do
      numOfPrefixes (mkTable e :: OrdTreeT4) `shouldBe` n

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

    it "Check binary tree" $ do
      numOfPrefixes (insEntries (mkTable [] :: BinTree) e) `shouldBe` n

delEntriesSpec :: Spec
delEntriesSpec = do
  describe "Deletion of random entries" $ do
    let n = 1000
        e = genRandomEntries n
    it "Check table" $ do
      delEntries (mkTable e :: Table) e `shouldBe` (mkTable [] :: Table)

    it "Check binary tree" $ do
      delEntries (mkTable e :: BinTree) e `shouldBe` (mkTable [] :: BinTree)
