module Main where

import           Test.Hspec

-- import           BinPartitionSpec
-- import           BinTreeSpec
-- import           OrdPartitionSpec
-- import           OrdTreeSpec
-- import           PaCo2PartitionSpec
-- import           PaCo2TreeSpec
import           PaCoPartitionSpec
import           PaCoTreeSpec
import           PrefixSpec

main :: IO ()
main = do
  hspec paCoTreeSpec
  hspec paCoIpRouterSpec
  hspec paCoPrtnIpRouterSpec

  -- hspec ordBpsSpec
  -- hspec ordDfudsSpec
  -- hspec ordIpRouterSpec
  -- hspec ordPrtnIpRouterSpec

  -- hspec binIpRouterSpec
  -- hspec binPrtnIpRouterSpec

  -- hspec paCo2TreeSpec
  -- hspec paCo2IpRouterSpec
  -- hspec paCo2PrtnIpRouterSpec

  hspec addressSpec
  hspec prefixSpec
