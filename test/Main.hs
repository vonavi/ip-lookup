module Main where

import           Test.Hspec

import           IpRouterSpec
import           OrdTreeSpec
import           PaCoTreeSpec

main :: IO ()
main = do
  hspec ipLookupSpec
  hspec numOfPrefixesSpec
  hspec insEntriesSpec
  hspec delEntriesSpec
  hspec patSstCheckSpec
  hspec ordSstCheckSpec

  hspec paCoTreeSpec

  hspec ordSizeSpec
  hspec ordBpSpec
  hspec ordDfudsSpec
