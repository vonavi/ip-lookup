module Main where

import Test.Hspec

import IpRouterSpec
import OrdTreeSpec

main :: IO ()
main = do
  hspec ipLookupSpec
  hspec numOfPrefixesSpec
  hspec insEntriesSpec
  hspec delEntriesSpec

  hspec ordSizeSpec
  hspec ordBpSpec
  hspec ordDfudsSpec
