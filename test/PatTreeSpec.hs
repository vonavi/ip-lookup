module PatTreeSpec
       (
         patTreeSpec
       ) where

import Test.Hspec

import Data.IpRouter
import Data.PatTree

testIpRouter :: IpRouter a => a
testIpRouter = mkTable . map toEntry $ l
  where toEntry (s, m, h) = Entry p h
          where p = Prefix (strToAddr s) (strToMask m)
        l = [ ("0.0.0.0",   "/1", 0)
            , ("127.0.0.0", "/2", 1)
            , ("111.0.0.0", "/4", 2)
            , ("255.0.0.0", "/1", 3)
            , ("223.0.0.0", "/4", 4)
            , ("223.0.0.0", "/5", 5)
            ]

patTreeSpec :: Spec
patTreeSpec = do
  describe "Simple PATRICIA tree" $ do
    it "Check building" $ do
      (testIpRouter :: PatTree) `shouldBe`
        PatTree (Bin (Bin Tip
                          PatNode { stride = 0
                                  , string = 0
                                  , label  = Just 0
                                  }
                          (Bin Tip
                               PatNode { stride = 0
                                       , string = 4227858432
                                       , label  = Just 1
                                       }
                               (Bin Tip
                                    PatNode { stride = 1
                                            , string = 2013265920
                                            , label  = Just 2
                                            }
                                    Tip)))
                     PatNode { stride = 0
                             , string = 4026531840
                             , label  = Nothing
                             }
                     (Bin Tip
                          PatNode { stride = 0
                                  , string = 4261412864
                                  , label  = Just 3
                                  }
                          (Bin Tip
                               PatNode { stride = 2
                                       , string = 2080374784
                                       , label  = Just 4
                                       }
                               (Bin Tip
                                    PatNode { stride = 0
                                            , string = 3758096384
                                            , label  = Just 5
                                            }
                                    Tip))))
