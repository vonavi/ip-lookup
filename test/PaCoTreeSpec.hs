module PaCoTreeSpec
  (
    paCoTreeSpec
  , paCoIpRouterSpec
  ) where

import           Test.Hspec

import           Data.IpRouter
import           Data.PaCoTree
import           RandomPrefixes
import           TestIpRouter

testPaCoTree :: PaCoTree
testPaCoTree = mkTable . map toEntry $ l
  where toEntry (s, m, h) = Entry (read (s ++ m) :: Prefix) h
        l = [ ("0.0.0.0",   "/1", 0)
            , ("127.0.0.0", "/2", 1)
            , ("111.0.0.0", "/4", 2)
            , ("255.0.0.0", "/1", 3)
            , ("223.0.0.0", "/4", 4)
            , ("223.0.0.0", "/5", 5)
            ]

refPaCoTree :: PaCoTree
refPaCoTree = Bin Node { skip   = 0
                       , string = 0
                       , label  = Nothing
                       }
                  (Bin Node { skip   = 0
                            , string = 0
                            , label  = Just 0
                            }
                       Tip
                       (Bin Node { skip   = 0
                                 , string = 4227858432
                                 , label  = Just 1
                                 }
                            Tip
                            (Bin Node { skip   = 1
                                      , string = 2013265920
                                      , label  = Just 2
                                      }
                                 Tip
                                 Tip)))
                  (Bin Node { skip   = 0
                            , string = 4261412864
                            , label  = Just 3
                            }
                       Tip
                       (Bin Node { skip   = 2
                                 , string = 2080374784
                                 , label  = Just 4
                                 }
                            Tip
                            (Bin Node { skip   = 0
                                      , string = 3758096384
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
