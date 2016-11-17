module TestIpRouter
  (
    testIpRouter
  , testIpLookup
  ) where

import           Data.IpRouter
import           Data.Prefix

testIpRouter :: IpRouter a => a
testIpRouter = mkTable . map toEntry $ l
  where toEntry (v, m, n) = Entry { network = mkPrefix (read v :: Address) m
                                  , nextHop = n
                                  }
        l = [ ("0.0.0.0",       0,  0)
            , ("192.168.0.0",   24, 1)
            , ("192.168.0.1",   22, 2)
            , ("192.168.0.255", 32, 3)
            , ("192.168.1.0",   28, 4)
            , ("192.168.1.2",   32, 5)
            ]

testIpLookup :: IpRouter a => a -> Bool
testIpLookup router = and . zipWith (==) nextHopList . map getNextHop $ addrList
  where getNextHop x = ipLookup (mkPrefix (read x :: Address) 32) router
        addrList     = [ "0.0.0.0"
                       , "192.168.0.0"
                       , "192.168.0.1"
                       , "192.168.0.255"
                       , "192.168.1.2"
                       , "192.168.1.0"
                       , "192.168.3.17"
                       , "192.168.4.17"
                       ]
        nextHopList  = [ Just 0
                       , Just 1
                       , Just 1
                       , Just 3
                       , Just 5
                       , Just 4
                       , Just 2
                       , Just 0
                       ]
