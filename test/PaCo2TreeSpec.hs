module PaCo2TreeSpec
  (
    paCo2TreeSpec
  , paCo2IpRouterSpec
  ) where

import           Control.Monad        (zipWithM_)
import           Test.Hspec

import           Data.IpRouter
import           Data.Prefix
import           Data.Trees.PaCo2Tree
import           RandomEntries
import           TestIpRouter

testEntries :: [Entry]
testEntries = map toEntry l
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

testPaCo2Trees :: [PaCo2Tree]
testPaCo2Trees = scanr insEntry (mkTable []) . reverse $ testEntries

testPaCo2Tree5 :: PaCo2Tree
testPaCo2Tree5 = head testPaCo2Trees

refPaCo2Tree0 :: PaCo2Tree
refPaCo2Tree0 = Bin Node { prefix = mkPrefix (ipv4Address 0) 1
                         , label  = Just 0
                         }
                    Tip
                    Tip

refPaCo2Tree1 :: PaCo2Tree
refPaCo2Tree1 = Bin Node { prefix = mkPrefix (ipv4Address 2130706432) 1
                         , label  = Just 0
                         }
                    (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                              , label  = Nothing
                              }
                         Tip
                         Tip)
                    (Bin Node { prefix = mkPrefix (ipv4Address 4227858432) 0
                              , label  = Just 1
                              }
                         Tip
                         Tip)

refPaCo2Tree2 :: PaCo2Tree
refPaCo2Tree2 = Bin Node { prefix = mkPrefix (ipv4Address 1862270976) 1
                         , label  = Just 0
                         }
                    (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                              , label  = Nothing
                              }
                         Tip
                         Tip)
                    (Bin Node { prefix = mkPrefix (ipv4Address 3154116608) 0
                              , label  = Just 1
                              }
                         (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                                   , label  = Nothing
                                   }
                              Tip
                              Tip)
                         (Bin Node { prefix = mkPrefix (ipv4Address 2013265920) 1
                                   , label  = Just 2
                                   }
                              Tip
                              Tip))

refPaCo2Tree3 :: PaCo2Tree
refPaCo2Tree3 = Bin Node { prefix = mkPrefix (ipv4Address 4278190080) 0
                         , label  = Nothing
                         }
                    (Bin Node { prefix = mkPrefix (ipv4Address 3724541952) 0
                              , label  = Just 0
                              }
                         (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                                   , label  = Nothing
                                   }
                              Tip
                              Tip)
                         (Bin Node { prefix =
                                       mkPrefix (ipv4Address 3154116608) 0
                                   , label  = Just 1
                                   }
                              (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                                        , label  = Nothing
                                        }
                                   Tip
                                   Tip)
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
                         Tip)

refPaCo2Tree4 :: PaCo2Tree
refPaCo2Tree4 = Bin Node { prefix = mkPrefix (ipv4Address 3741319168) 0
                         , label  = Nothing
                         }
                    (Bin Node { prefix = mkPrefix (ipv4Address 3724541952) 0
                              , label  = Just 0
                              }
                         (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                                   , label  = Nothing
                                   }
                              Tip
                              Tip)
                         (Bin Node { prefix =
                                       mkPrefix (ipv4Address 3154116608) 0
                                   , label  = Just 1
                                   }
                              (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                                        , label  = Nothing
                                        }
                                   Tip
                                   Tip)
                              (Bin Node { prefix =
                                            mkPrefix (ipv4Address 2013265920) 1
                                        , label  = Just 2
                                        }
                                   Tip
                                   Tip)))
                    (Bin Node { prefix = mkPrefix (ipv4Address 3187671040) 0
                              , label  = Just 3
                              }
                         (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                                   , label  = Nothing
                                   }
                              Tip
                              Tip)
                         (Bin Node { prefix =
                                       mkPrefix (ipv4Address 2080374784) 2
                                   , label  = Just 4
                                   }
                              Tip
                              Tip))

refPaCo2Tree5 :: PaCo2Tree
refPaCo2Tree5 = Bin Node { prefix = mkPrefix (ipv4Address 3741319168) 0
                         , label  = Nothing
                         }
                    (Bin Node { prefix = mkPrefix (ipv4Address 3724541952) 0
                              , label  = Just 0
                              }
                         (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                                   , label  = Nothing
                                   }
                              Tip
                              Tip)
                         (Bin Node { prefix =
                                       mkPrefix (ipv4Address 3154116608) 0
                                   , label  = Just 1
                                   }
                              (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                                        , label  = Nothing
                                        }
                                   Tip
                                   Tip)
                              (Bin Node { prefix =
                                            mkPrefix (ipv4Address 2013265920) 1
                                        , label  = Just 2
                                        }
                                   Tip
                                   Tip)))
                    (Bin Node { prefix = mkPrefix (ipv4Address 3187671040) 0
                              , label  = Just 3
                              }
                         (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                                   , label  = Nothing
                                   }
                              Tip
                              Tip)
                         (Bin Node { prefix =
                                       mkPrefix (ipv4Address 2080374784) 2
                                   , label  = Just 4
                                   }
                              (Bin Node { prefix = mkPrefix (ipv4Address 0) 0
                                        , label  = Nothing
                                        }
                                   Tip
                                   Tip)
                              (Bin Node { prefix =
                                            mkPrefix (ipv4Address 3758096384) 0
                                        , label  = Just 5
                                        }
                                   Tip
                                   Tip)))


paCo2TreeSpec :: Spec
paCo2TreeSpec = do
  describe "Simple path-compressed 2-tree" $ do
    it "Building" $ do
      zipWithM_ shouldBe testPaCo2Trees
        [ refPaCo2Tree5, refPaCo2Tree4, refPaCo2Tree3, refPaCo2Tree2
        , refPaCo2Tree1, refPaCo2Tree0, Tip
        ]

    it "Deletion" $ do
      let ts = scanr delEntry testPaCo2Tree5 testEntries
      zipWithM_ shouldBe ts
        [ Tip, refPaCo2Tree0, refPaCo2Tree1, refPaCo2Tree2
        , refPaCo2Tree3, refPaCo2Tree4, refPaCo2Tree5
        ]

paCo2IpRouterSpec :: Spec
paCo2IpRouterSpec = do
  describe "Path-compressed 2-tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: PaCo2Tree) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: PaCo2Tree) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: PaCo2Tree) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: PaCo2Tree) e `shouldBe` (mkTable [] :: PaCo2Tree)
