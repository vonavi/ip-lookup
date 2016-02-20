module Main where

import Test.Hspec
import Data.Monoid

import Data.IpRouter
import Data.Table
import Data.BinTree
import Data.OrdTree
import Data.OrdSst

testIpRouter :: IpRouter a => a
testIpRouter = ipBuild . map toEntry $ l
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

main :: IO ()
main = hspec $ do
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
