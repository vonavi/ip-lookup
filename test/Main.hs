module Main where

import           Test.Hspec

import           IpRouterSpec
import           OrdTreeSpec
import           PatTreeSpec

main :: IO ()
main = do
  hspec ipLookupSpec
  hspec numOfPrefixesSpec
  hspec insEntriesSpec
  hspec delEntriesSpec
  hspec patSstCheckSpec
  hspec ordSstCheckSpec

  hspec patTreeSpec

  hspec ordSizeSpec
  hspec ordBpSpec
  hspec ordDfudsSpec
