module PaCo2TreeSpecM
       (
         paCo2TreeSpec
       , paCo2IpRouterSpec
       ) where

import           Control.Monad   (zipWithM_)
import           Data.Bits
import           Test.Hspec

import           Data.IpRouter
import           Data.PaCo2TreeM hiding (Bin, Leaf, Tree)
import qualified Data.PaCo2TreeM as PC
import           RandomPrefixes
import           TestIpRouter

data Node = Node { list :: [Bool]
                 , pref :: Maybe Int
                 } deriving (Show, Eq)

data Tree a = Tip | Bin a (Tree a) (Tree a) deriving (Show, Eq)

fromPaCo2Tree :: PaCo2Tree b -> Tree Node
fromPaCo2Tree (PaCo2Tree t) = helper t
  where helper (PC.Leaf _)    = Tip
        helper (PC.Bin x l r) = Bin x' (helper l) (helper r)
          where PaCo2Node { skip = k, string = v, label = p } = x
                bs = map (\n -> v `testBit` (31 - n)) [0 .. pred k]
                x' = Node { list = bs
                          , pref = p
                          }

testEntries :: [Entry]
testEntries = map toEntry l
  where toEntry (s, m, h) = let p = Prefix (strToAddr s) (strToMask m)
                            in Entry p h
        l = [ ("0.0.0.0",   "/1", 0)
            , ("127.0.0.0", "/2", 1)
            , ("111.0.0.0", "/4", 2)
            , ("255.0.0.0", "/1", 3)
            , ("223.0.0.0", "/4", 4)
            , ("223.0.0.0", "/5", 5)
            ]

testPaCo2Trees :: [PaCo2Tree ()]
testPaCo2Trees = scanr insEntry (mkTable []) . reverse $ testEntries

testPaCo2Tree5 :: PaCo2Tree ()
testPaCo2Tree5 = head testPaCo2Trees

refTree0 :: Tree Node
refTree0 = Bin Node { list = [False]
                    , pref = Just 0
                    }
               Tip
               Tip

refTree1 :: Tree Node
refTree1 = Bin Node { list = [False]
                    , pref = Just 0
                    }
               (Bin Node { list = []
                         , pref = Nothing
                         }
                    Tip
                    Tip)
               (Bin Node { list = []
                         , pref = Just 1
                         }
                    Tip
                    Tip)

refTree2 :: Tree Node
refTree2 = Bin Node { list = [False]
                    , pref = Just 0
                    }
               (Bin Node { list = []
                         , pref = Nothing
                         }
                    Tip
                    Tip)
               (Bin Node { list = []
                         , pref = Just 1
                         }
                    (Bin Node { list = []
                              , pref = Nothing
                              }
                         Tip
                         Tip)
                    (Bin Node { list = [False]
                              , pref = Just 2
                              }
                         Tip
                         Tip))

refTree3 :: Tree Node
refTree3 = Bin Node { list = []
                    , pref = Nothing
                    }
               (Bin Node { list = []
                         , pref = Just 0
                         }
                    (Bin Node { list = []
                              , pref = Nothing
                              }
                         Tip
                         Tip)
                    (Bin Node { list = []
                              , pref = Just 1
                              }
                         (Bin Node { list = []
                                   , pref = Nothing
                                   }
                              Tip
                              Tip)
                         (Bin Node { list = [False]
                                   , pref = Just 2
                                   }
                              Tip
                              Tip)))
               (Bin Node { list = []
                         , pref = Just 3
                         }
                    Tip
                    Tip)

refTree4 :: Tree Node
refTree4 = Bin Node { list = []
                    , pref = Nothing
                    }
               (Bin Node { list = []
                         , pref = Just 0
                         }
                    (Bin Node { list = []
                              , pref = Nothing
                              }
                         Tip
                         Tip)
                    (Bin Node { list = []
                              , pref = Just 1
                              }
                         (Bin Node { list = []
                                   , pref = Nothing
                                   }
                              Tip
                              Tip)
                         (Bin Node { list = [False]
                                   , pref = Just 2
                                   }
                              Tip
                              Tip)))
               (Bin Node { list = []
                         , pref = Just 3
                         }
                    (Bin Node { list = []
                              , pref = Nothing
                              }
                         Tip
                         Tip)
                    (Bin Node { list = [False, True]
                              , pref = Just 4
                              }
                         Tip
                         Tip))

refTree5 :: Tree Node
refTree5 = Bin Node { list = []
                    , pref = Nothing
                    }
               (Bin Node { list = []
                         , pref = Just 0
                         }
                    (Bin Node { list = []
                              , pref = Nothing
                              }
                         Tip
                         Tip)
                    (Bin Node { list = []
                              , pref = Just 1
                              }
                         (Bin Node { list = []
                                   , pref = Nothing
                                   }
                              Tip
                              Tip)
                         (Bin Node { list = [False]
                                   , pref = Just 2
                                   }
                              Tip
                              Tip)))
               (Bin Node { list = []
                         , pref = Just 3
                         }
                    (Bin Node { list = []
                              , pref = Nothing
                              }
                         Tip
                         Tip)
                    (Bin Node { list = [False, True]
                              , pref = Just 4
                              }
                         (Bin Node { list = []
                                   , pref = Nothing
                                   }
                              Tip
                              Tip)
                         (Bin Node { list = []
                                   , pref = Just 5
                                   }
                              Tip
                              Tip)))


paCo2TreeSpec :: Spec
paCo2TreeSpec = do
  describe "Simple path-compressed 2-tree" $ do
    it "Building" $ do
      zipWithM_ shouldBe (map fromPaCo2Tree testPaCo2Trees)
        [ refTree5, refTree4, refTree3, refTree2, refTree1, refTree0, Tip ]

    it "Deletion" $ do
      let ts = scanr delEntry testPaCo2Tree5 testEntries
      zipWithM_ shouldBe (map fromPaCo2Tree ts)
        [ Tip, refTree0, refTree1, refTree2, refTree3, refTree4, refTree5 ]

paCo2IpRouterSpec :: Spec
paCo2IpRouterSpec = do
  describe "Path-compressed 2-tree" $ do
    it "Simple IP lookup" $ do
      testIpLookup (testIpRouter :: PaCo2Tree ()) `shouldBe` True

    it "Number of random prefixes" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (mkTable e :: PaCo2Tree ()) `shouldBe` n

    it "Insertion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      numOfPrefixes (insEntries (mkTable [] :: PaCo2Tree ()) e) `shouldBe` n

    it "Deletion of random entries" $ do
      let n = 1000
          e = genRandomEntries n
      delEntries (mkTable e :: PaCo2Tree ()) e `shouldBe`
        (mkTable [] :: PaCo2Tree ())
