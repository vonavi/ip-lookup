module Main where

import           Test.Hspec

import           BinTreeSpec
import           IpRouterSpec
import           OrdSstSpec
import           OrdTreeSpec
import           PaCoTreeSpec
import           TableSpec

main :: IO ()
main = do
  hspec ipLookupSpec
  hspec numOfPrefixesSpec
  hspec insEntriesSpec
  hspec paCoPrtnCheckSpec

  hspec tableIpRouterSpec
  hspec binIpRouterSpec

  hspec paCoTreeSpec
  hspec paCoIpRouterSpec

  hspec ordSizeSpec
  hspec ordBpSpec
  hspec ordDfudsSpec
  hspec ordIpRouterSpec

  hspec ordSstIpRouterSpec
  hspec ordSstCheckSpec
