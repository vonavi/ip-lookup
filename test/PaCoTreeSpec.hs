module PaCoTreeSpec
  (
    paCoTreeSpec
  , paCoIpRouterSpec
  ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.PaCoTree
import           Data.Prefix
import           RandomEntries
import           TestIpRouter

testPaCoTree :: PaCoTree
testPaCoTree = mkTable . map toEntry $ l
  where toEntry (v, m, n) = Entry { network = mkPrefix (read v :: Address) m
                                  , nextHop = n
                                  }
        l = [ ("0.0.0.0",   1, 0)
            , ("127.0.0.0", 2, 1)
            , ("111.0.0.0", 4, 2)
            , ("255.0.0.0", 1, 3)
            , ("223.0.0.0", 4, 4)
            , ("223.0.0.0", 5, 5)
            ]

refPaCoTree :: PaCoTree
refPaCoTree = Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                       , label  = Nothing
                       }
                  (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                            , label  = Just 0
                            }
                       Tip
                       (Bin Node { prefix = mkPrefix (ipv4Address 4227858432) 0
                                 , label  = Just 1
                                 }
                            Tip
                            (Bin Node { prefix =
                                          mkPrefix (ipv4Address 2013265920) 1
                                      , label  = Just 2
                                      }
                                 Tip
                                 Tip)))
                  (Bin Node { prefix = mkPrefix (ipv4Address 4261412864) 0
                            , label  = Just 3
                            }
                       Tip
                       (Bin Node { prefix = mkPrefix (ipv4Address 2080374784) 2
                                 , label  = Just 4
                                 }
                            Tip
                            (Bin Node { prefix =
                                          mkPrefix (ipv4Address 3758096384) 0
                                      , label  = Just 5
                                      }
                                 Tip
                                 Tip)))

paCoTreeSpec :: Spec
paCoTreeSpec = do
  describe "Simple path-compressed tree" $ do
    it "Building" $ do
      testPaCoTree `shouldBe` refPaCoTree

paCoIpRouterSpec :: Spec
paCoIpRouterSpec = do
  describe "Path-compressed tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: PaCoTree) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: PaCoTree) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: PaCoTree) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: PaCoTree) e `shouldBe` (mkTable [] :: PaCoTree)
