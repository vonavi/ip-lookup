module PrefixSpec
  (
    addressSpec
  , prefixSpec
  ) where

import           Data.List   (unfoldr)
import           Test.Hspec

import           Data.Prefix

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


prefixSpec :: Spec
prefixSpec = do testBitSpec
                commonPrefixesSpec

significantBits :: Prefix -> [Bool]
significantBits = unfoldr uncons

testBitSpec :: Spec
testBitSpec = do
  describe "Test prefix bits" $ do
    let p1 = mkPrefix (read "192.168.0.1") 16
        p2 = setVpn (read "db8") p1
        p3 = setVpn (read "2001") p2
    it "IPv4 prefix" $ do
      significantBits p1
        `shouldBe` [ True, True, False, False, False, False, False, False
                   , True, False, True, False, True, False, False, False
                   ]
    it "VPNv4 prefix" $ do
      significantBits p2
        `shouldBe` [ False, False, False, False, True, True, False, True
                   , True, False, True, True, True, False, False, False
                   , True, True, False, False, False, False, False, False
                   , True, False, True, False, True, False, False, False
                   ]
    it "VPNv4 prefix (changed)" $ do
      significantBits p3
        `shouldBe` [ False, False, True, False, False, False, False, False
                   , False, False, False, False, False, False, False, True
                   , True, True, False, False, False, False, False, False
                   , True, False, True, False, True, False, False, False
                   ]

    let p4 = mkPrefix (read "2001:db8::") 32
        p5 = setVpn (read "ac10") p4
        p6 = setVpn (read "fe01") p5
    it "IPv6 prefix" $ do
      significantBits p4
        `shouldBe` [ False, False, True, False, False, False, False, False
                   , False, False, False, False, False, False, False, True
                   , False, False, False, False, True, True, False, True
                   , True, False, True, True, True, False, False, False
                   ]
    it "VPNv6 prefix" $ do
      significantBits p5
        `shouldBe` [ True, False, True, False, True, True, False, False
                   , False, False, False, True, False, False, False, False
                   , False, False, True, False, False, False, False, False
                   , False, False, False, False, False, False, False, True
                   , False, False, False, False, True, True, False, True
                   , True, False, True, True, True, False, False, False
                   ]
    it "VPNv6 prefix (changed)" $ do
      significantBits p6
        `shouldBe` [ True, True, True, True, True, True, True, False
                   , False, False, False, False, False, False, False, True
                   , False, False, True, False, False, False, False, False
                   , False, False, False, False, False, False, False, True
                   , False, False, False, False, True, True, False, True
                   , True, False, True, True, True, False, False, False
                   ]

commonPrefixesSpec :: Spec
commonPrefixesSpec = do
  describe "Test common prefixes" $ do
    it "IPv4 prefix" $ do
      let p1  = mkPrefix (read "192.168.0.0") 24
          p2  = mkPrefix (read "192.168.127.0") 28
          p3  = mkPrefix (read "192.168.0.0") 17
          p1' = mkPrefix (read "0.0.0.0") 7
          p2' = mkPrefix (read "254.0.0.0") 11
      commonPrefixes p1 p2 `shouldBe` (p3, p1', p2')
      append p3 p1' `shouldBe` p1
      append p3 p2' `shouldBe` p2

    it "VPNv4 prefix" $ do
      let p1  = setVpn (read "64") $ mkPrefix (read "192.168.255.0") 32
          p2  = setVpn (read "64") $ mkPrefix (read "192.168.127.0") 32
          p3  = setVpn (read "64") $ mkPrefix (read "192.168.0.0") 16
          p1' = setVpn (read "ff00") $ mkPrefix (read "0.0.0.0") 0
          p2' = setVpn (read "7f00") $ mkPrefix (read "0.0.0.0") 0
      commonPrefixes p1 p2 `shouldBe` (p3, p1', p2')
      append p3 p1' `shouldBe` p1
      append p3 p2' `shouldBe` p2

    it "IPv6 prefix" $ do
      let p1  = mkPrefix (read "2001:db8::") 64
          p2  = mkPrefix (read "2001:dc8::") 48
          p3  = mkPrefix (read "2001:db8::") 25
          p1' = mkPrefix (read "7000::") 39
          p2' = mkPrefix (read "9000::") 23
      commonPrefixes p1 p2 `shouldBe` (p3, p1', p2')
      append p3 p1' `shouldBe` p1
      append p3 p2' `shouldBe` p2

    it "VPNv6 prefix" $ do
      let p1  = setVpn (read "ac10") $ mkPrefix (read "2001:db8::") 64
          p2  = setVpn (read "ac10") $ mkPrefix (read "2001:dc8::") 48
          p3  = setVpn (read "ac10") $ mkPrefix (read "2001:db8::") 25
          p1' = setVpn (read "7000") $ mkPrefix (read "::") 23
          p2' = setVpn (read "9000") $ mkPrefix (read "::") 7
      commonPrefixes p1 p2 `shouldBe` (p3, p1', p2')
      append p3 p1' `shouldBe` p1
      append p3 p2' `shouldBe` p2
