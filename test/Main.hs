module Main where

import           Test.Hspec

import           BinTreeSpec
import           OrdPartitionSpec
import           OrdTreeSpec
import           PaCo2TreeSpec
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

  hspec paCoPrtnIpRouterSpec
  hspec paCoPrtnCheckSpec

  hspec ordPrtnIpRouterSpec
  hspec ordPrtnCheckSpec
