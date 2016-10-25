module EntrySpec
  (
    addressSpec
  , prefixSpec
  ) where

import           Data.Function (on)
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


prefixSpec :: Spec
prefixSpec = do testPrefixBitSpec
                commonPrefixSpec

getPrefixBits :: Entry -> [Bool]
getPrefixBits e = map (e `testPrefixBit`) . take (maskLength e) $ [0 ..]

testPrefixBitSpec :: Spec
testPrefixBitSpec = do
  describe "Test prefix bits" $ do
    let p1 = mkPrefix (read "192.168.0.1" :: Address) (read "16" :: Mask)
        p2 = setVpn (read "db8" :: Vpn) p1
        p3 = setVpn (read "2001" :: Vpn) p2
    it "IPv4 prefix" $ do
      getPrefixBits (mkEntry 0 p1)
        `shouldBe` [ True, True, False, False, False, False, False, False
                   , True, False, True, False, True, False, False, False
                   ]
    it "VPNv4 prefix" $ do
      getPrefixBits (mkEntry 255 p2)
        `shouldBe` [ False, False, False, False, True, True, False, True
                   , True, False, True, True, True, False, False, False
                   , True, True, False, False, False, False, False, False
                   , True, False, True, False, True, False, False, False
                   ]
    it "VPNv4 prefix (changed)" $ do
      getPrefixBits (mkEntry 1 p3)
        `shouldBe` [ False, False, True, False, False, False, False, False
                   , False, False, False, False, False, False, False, True
                   , True, True, False, False, False, False, False, False
                   , True, False, True, False, True, False, False, False
                   ]

    let p4 = mkPrefix (read "2001:db8::" :: Address) (read "32" :: Mask)
        p5 = setVpn (read "ac10" :: Vpn) p4
        p6 = setVpn (read "fe01" :: Vpn) p5
    it "IPv6 prefix" $ do
      getPrefixBits (mkEntry 10 p4)
        `shouldBe` [ False, False, True, False, False, False, False, False
                   , False, False, False, False, False, False, False, True
                   , False, False, False, False, True, True, False, True
                   , True, False, True, True, True, False, False, False
                   ]
    it "VPNv6 prefix" $ do
      getPrefixBits (mkEntry 128 p5)
        `shouldBe` [ True, False, True, False, True, True, False, False
                   , False, False, False, True, False, False, False, False
                   , False, False, True, False, False, False, False, False
                   , False, False, False, False, False, False, False, True
                   , False, False, False, False, True, True, False, True
                   , True, False, True, True, True, False, False, False
                   ]
    it "VPNv6 prefix (changed)" $ do
      getPrefixBits (mkEntry 7 p6)
        `shouldBe` [ True, True, True, True, True, True, True, False
                   , False, False, False, False, False, False, False, True
                   , False, False, True, False, False, False, False, False
                   , False, False, False, False, False, False, False, True
                   , False, False, False, False, True, True, False, True
                   , True, False, True, True, True, False, False, False
                   ]

commonPrefixSpec :: Spec
commonPrefixSpec = do
  describe "Test common prefix" $ do
    let p1 = mkPrefix (read "192.168.0.0" :: Address) (read "24" :: Mask)
        p2 = mkPrefix (read "192.168.127.0" :: Address) (read "28" :: Mask)
        p3 = mkPrefix (read "192.168.0.0" :: Address) (read "17" :: Mask)
    it "IPv4 prefix" $ do
      commonPrefix p1 p2 `shouldBe` p3
    it "VPNv4 prefix" $ do
      (commonPrefix `on` setVpn (read "64" :: Vpn)) p1 p2
        `shouldBe` setVpn (read "64" :: Vpn) p3

    let p4 = mkPrefix (read "2001:db8::" :: Address) (read "64" :: Mask)
        p5 = mkPrefix (read "2001:dc8::" :: Address) (read "48" :: Mask)
        p6 = mkPrefix (read "2001:db8::" :: Address) (read "25" :: Mask)
    it "IPv6 prefix" $ do
      commonPrefix p4 p5 `shouldBe` p6
    it "VPNv6 prefix" $ do
      (commonPrefix `on` setVpn (read "ac10" :: Vpn)) p4 p5
        `shouldBe` setVpn (read "ac10" :: Vpn) p6
