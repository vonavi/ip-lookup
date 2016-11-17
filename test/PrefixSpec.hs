module PrefixSpec
  (
    addressSpec
  , prefixSpec
  ) where

import           Data.List   (unfoldr)
import           Test.Hspec

import qualified Data.Prefix as P

addressSpec :: Spec
addressSpec = do ipv4AddressSpec
                 ipv6AddressSpec

ipv4AddressSpec :: Spec
ipv4AddressSpec = do
  describe "IPv4 address" $ do
    it "Reading" $ do
      show (read "192.168.0.255" :: P.Address) `shouldBe` "192.168.0.255"

ipv6AddressSpec :: Spec
ipv6AddressSpec = do
  describe "IPv6 address" $ do
    it "Reading" $ do
      show (read "::" :: P.Address) `shouldBe` "::"
      show (read "2001:db8::" :: P.Address) `shouldBe` "2001:db8::"
      show (read "::ffff:0:0" :: P.Address) `shouldBe` "::ffff:0:0"
      show (read "2001:db8:0:0:0:0:2:1" :: P.Address) `shouldBe` "2001:db8::2:1"
      show (read "2001:db8:0:1:1:1:1:1" :: P.Address)
        `shouldBe` "2001:db8:0:1:1:1:1:1"
      show (read "2001:0:0:1:0:0:0:1" :: P.Address) `shouldBe` "2001:0:0:1::1"
      show (read "2001:db8:0:0:1:0:0:1" :: P.Address)
        `shouldBe` "2001:db8::1:0:0:1"


prefixSpec :: Spec
prefixSpec = do testBitSpec
                commonPrefixesSpec

significantBits :: P.Prefix -> [Bool]
significantBits = unfoldr P.uncons

testBitSpec :: Spec
testBitSpec = do
  describe "Test prefix bits" $ do
    it "IPv4 prefix" $ do
      significantBits (P.mkPrefix (read "192.168.0.1") 16)
        `shouldBe` [ True, True, False, False, False, False, False, False
                   , True, False, True, False, True, False, False, False
                   ]

    it "IPv6 prefix" $ do
      significantBits (P.mkPrefix (read "2001:db8::") 32)
        `shouldBe` [ False, False, True, False, False, False, False, False
                   , False, False, False, False, False, False, False, True
                   , False, False, False, False, True, True, False, True
                   , True, False, True, True, True, False, False, False
                   ]

commonPrefixesSpec :: Spec
commonPrefixesSpec = do
  describe "Test common prefixes" $ do
    it "IPv4 prefix" $ do
      let p1  = P.mkPrefix (read "192.168.0.0") 24
          p2  = P.mkPrefix (read "192.168.127.0") 28
          p3  = P.mkPrefix (read "192.168.0.0") 17
          p1' = P.mkPrefix (read "0.0.0.0") 7
          p2' = P.mkPrefix (read "254.0.0.0") 11
      P.commonPrefixes p1 p2 `shouldBe` (p3, p1', p2')
      P.splitAt 17 p1 `shouldBe` (p3, p1')
      P.splitAt 17 p2 `shouldBe` (p3, p2')
      P.append p3 p1' `shouldBe` p1
      P.append p3 p2' `shouldBe` p2

    it "IPv6 prefix" $ do
      let p1  = P.mkPrefix (read "2001:db8::") 64
          p2  = P.mkPrefix (read "2001:dc8::") 48
          p3  = P.mkPrefix (read "2001:db8::") 25
          p1' = P.mkPrefix (read "7000::") 39
          p2' = P.mkPrefix (read "9000::") 23
      P.commonPrefixes p1 p2 `shouldBe` (p3, p1', p2')
      P.splitAt 25 p1 `shouldBe` (p3, p1')
      P.splitAt 25 p2 `shouldBe` (p3, p2')
      P.append p3 p1' `shouldBe` p1
      P.append p3 p2' `shouldBe` p2
