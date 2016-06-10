module Main where

import           Test.Hspec

import           BinTreeSpec
import           IpRouterSpec
import           OrdTreeSpec
import           PaCoTreeSpec

main :: IO ()
main = do
  hspec ipLookupSpec
  hspec numOfPrefixesSpec
  hspec insEntriesSpec
  hspec delEntriesSpec
  hspec paCoPrtnCheckSpec
  hspec ordSstCheckSpec

  hspec paCoTreeSpec

  hspec binIpRouterSpec

  hspec ordSizeSpec
  hspec ordBpSpec
  hspec ordDfudsSpec
  hspec ordIpRouterSpec
