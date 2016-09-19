module Main where

import           Test.Hspec

import           BinTreeSpec
import           OrdPartitionSpec
import           OrdTreeSpec
import           PaCo2PartitionSpec
import           PaCo2TreeSpec
import           PaCoPartitionSpec
import           PaCoTreeSpec
import           TableSpec

main :: IO ()
main = do
  hspec tableIpRouterSpec
  hspec binIpRouterSpec

  hspec paCoTreeSpec
  hspec paCoIpRouterSpec
  hspec paCoPrtnIpRouterSpec

  hspec ordSizeSpec
  hspec ordBpSpec
  hspec ordDfudsSpec
  hspec ordIpRouterSpec

  hspec ordPrtnIpRouterSpec
  hspec ordPrtnCheckSpec

  hspec paCo2TreeSpec
  hspec paCo2IpRouterSpec
  hspec paCo2PrtnIpRouterSpec
