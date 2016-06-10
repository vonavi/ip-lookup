module Main where

import           Test.Hspec

import           BinTreeSpec
import           OrdSstSpec
import           OrdTreeSpec
import           PaCoPartitionSpec
import           PaCoTreeSpec
import           TableSpec

main :: IO ()
main = do
  hspec tableIpRouterSpec
  hspec binIpRouterSpec

  hspec paCoTreeSpec
  hspec paCoIpRouterSpec

  hspec ordSizeSpec
  hspec ordBpSpec
  hspec ordDfudsSpec
  hspec ordIpRouterSpec

  hspec paCoPrtnIpRouterSpec
  hspec paCoPrtnCheckSpec

  hspec ordSstIpRouterSpec
  hspec ordSstCheckSpec
