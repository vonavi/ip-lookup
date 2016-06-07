module IpRouterSpec
       (
         ipLookupSpec
       , numOfPrefixesSpec
       , insEntriesSpec
       , delEntriesSpec
       , patSstCheckSpec
       , ordSstCheckSpec
       ) where

import           Test.Hspec

import           Data.BinTree
import           Data.IpRouter
import           Data.OrdSst        (MhOrdSstT1, MhOrdSstT2, MhOrdSstT3,
                                     MhOrdSstT4)
import qualified Data.OrdSst        as OS
import           Data.OrdTree
import           Data.PaCoPartition (MhPatSst)
import           Data.PaCoTree
import           Data.Table
import qualified Partible           as Par
import           RandomPrefixes

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
genRandomEntries n = randomEntries (32, 32) [1 .. n]

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

    it "Check path-compressed tree" $ do
      testIpLookup (testIpRouter :: PaCoTree) `shouldBe` True

    it "Check ordinal tree T1" $ do
      testIpLookup (testIpRouter :: OrdTreeT1) `shouldBe` True

    it "Check ordinal tree T2" $ do
      testIpLookup (testIpRouter :: OrdTreeT2) `shouldBe` True

    it "Check ordinal tree T3" $ do
      testIpLookup (testIpRouter :: OrdTreeT3) `shouldBe` True

    it "Check ordinal tree T4" $ do
      testIpLookup (testIpRouter :: OrdTreeT4) `shouldBe` True

    it "Check min-height SST for path-compressed tree" $ do
      testIpLookup (testIpRouter :: MhPatSst) `shouldBe` True

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

    it "Check path-compressed tree" $ do
      numOfPrefixes (mkTable e :: PaCoTree) `shouldBe` n

    it "Check ordinal tree T1" $ do
      numOfPrefixes (mkTable e :: OrdTreeT1) `shouldBe` n

    it "Check ordinal tree T2" $ do
      numOfPrefixes (mkTable e :: OrdTreeT2) `shouldBe` n

    it "Check ordinal tree T3" $ do
      numOfPrefixes (mkTable e :: OrdTreeT3) `shouldBe` n

    it "Check ordinal tree T4" $ do
      numOfPrefixes (mkTable e :: OrdTreeT4) `shouldBe` n

    it "Check min-height SST for path-compressed tree" $ do
      numOfPrefixes (mkTable e :: MhPatSst) `shouldBe` n

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

    it "Check path-compressed tree" $ do
      numOfPrefixes (insEntries (mkTable [] :: PaCoTree) e) `shouldBe` n

    it "Check ordinal tree T1" $ do
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT1) e) `shouldBe` n

    it "Check ordinal tree T2" $ do
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT2) e) `shouldBe` n

    it "Check ordinal tree T3" $ do
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT3) e) `shouldBe` n

    it "Check ordinal tree T4" $ do
      numOfPrefixes (insEntries (mkTable [] :: OrdTreeT4) e) `shouldBe` n

    it "Check min-height SST for path-compressed tree" $ do
      numOfPrefixes (insEntries (mkTable [] :: MhPatSst) e) `shouldBe` n

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

    it "Check binary tree" $ do
      delEntries (mkTable e :: BinTree) e `shouldBe` (mkTable [] :: BinTree)

    it "Check ordinal tree T1" $ do
      delEntries (mkTable e :: OrdTreeT1) e `shouldBe` (mkTable [] :: OrdTreeT1)

    it "Check ordinal tree T2" $ do
      delEntries (mkTable e :: OrdTreeT2) e `shouldBe` (mkTable [] :: OrdTreeT2)

    it "Check ordinal tree T3" $ do
      delEntries (mkTable e :: OrdTreeT3) e `shouldBe` (mkTable [] :: OrdTreeT3)

    it "Check ordinal tree T4" $ do
      delEntries (mkTable e :: OrdTreeT4) e `shouldBe` (mkTable [] :: OrdTreeT4)

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

patSstCheckSpec :: Spec
patSstCheckSpec = do
  let n = 1000
      e = genRandomEntries n

  describe "Check pages built by 'mkTable'" $ do
    it "Check min-height SST for path-compressed tree" $ do
      Par.checkPages (mkTable e :: MhPatSst) `shouldBe` True

  describe "Check pages built by 'insEntries'" $ do
    it "Check min-height SST for path-compressed tree" $ do
      Par.checkPages (insEntries (mkTable [] :: MhPatSst) e) `shouldBe` True

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
