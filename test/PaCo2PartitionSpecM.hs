module PaCo2PartitionSpecM
       (
         paCo2PrtnIpRouterSpec
       ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.PaCo2PartitionM
import           RandomPrefixes
import           TestIpRouter

paCo2PrtnIpRouterSpec :: Spec
paCo2PrtnIpRouterSpec = do
  describe "Min-height partition of path-compressed 2-tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: Maybe Page) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: Maybe Page) `shouldBe` n
