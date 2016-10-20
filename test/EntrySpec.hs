module EntrySpec
  (
    addressSpec
  ) where

import           Test.Hspec

import           Data.Entry

addressSpec :: Spec
addressSpec = do ipv4AddressSpec
                 ipv6AddressSpec

ipv4AddressSpec :: Spec
ipv4AddressSpec = do
  describe "IPv4 address" $ do
    it "Reading" $ do
      show (read "192.168.0.255" :: Address) `shouldBe` "192.168.0.255"

ipv6AddressSpec :: Spec
ipv6AddressSpec = do
  describe "IPv6 address" $ do
    it "Reading" $ do
      show (read "::" :: Address) `shouldBe` "::"
      show (read "2001:db8::" :: Address) `shouldBe` "2001:db8::"
      show (read "::ffff:0:0" :: Address) `shouldBe` "::ffff:0:0"
      show (read "2001:db8:0:0:0:0:2:1" :: Address) `shouldBe` "2001:db8::2:1"
      show (read "2001:db8:0:1:1:1:1:1" :: Address)
        `shouldBe` "2001:db8:0:1:1:1:1:1"
      show (read "2001:0:0:1:0:0:0:1" :: Address) `shouldBe` "2001:0:0:1::1"
      show (read "2001:db8:0:0:1:0:0:1" :: Address)
        `shouldBe` "2001:db8::1:0:0:1"
