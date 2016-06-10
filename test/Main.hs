module Main where

import           Test.Hspec

import           BinTreeSpec
import           IpRouterSpec
import           OrdTreeSpec
import           PaCoTreeSpec
import           TableSpec

main :: IO ()
main = do
  hspec ipLookupSpec
  hspec numOfPrefixesSpec
  hspec insEntriesSpec
  hspec delEntriesSpec
  hspec paCoPrtnCheckSpec
  hspec ordSstCheckSpec

  hspec tableIpRouterSpec
  hspec binIpRouterSpec

  hspec paCoTreeSpec
  hspec paCoIpRouterSpec

  hspec ordSizeSpec
  hspec ordBpSpec
  hspec ordDfudsSpec
  hspec ordIpRouterSpec
