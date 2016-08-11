module Main where

import           Test.Hspec

import           BinTreeSpec
import           OrdPartitionSpec
import           OrdTreeSpec
import           PaCo2PartitionSpec
import           PaCo2TreeSpecM
import           PaCoPartitionSpec
import           PaCoTreeSpec
import           TableSpec

main :: IO ()
main = do
  hspec tableIpRouterSpec
  hspec binIpRouterSpec

  hspec paCo2TreeSpec
  hspec paCo2IpRouterSpec

  hspec paCoTreeSpec
  hspec paCoIpRouterSpec

  hspec ordSizeSpec
  hspec ordBpSpec
  hspec ordDfudsSpec
  hspec ordIpRouterSpec

  hspec paCo2PrtnIpRouterSpec

  hspec paCoPrtnIpRouterSpec

  hspec ordPrtnIpRouterSpec
  hspec ordPrtnCheckSpec
