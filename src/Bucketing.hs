module Bucketing
       (
         putBuckets
       ) where

import           Data.Bits
import           Data.List      (sortBy, unfoldr)
import           Data.Ord       (comparing)
import           Data.Word

import           Data.IpRouter
import           Data.PatSst    (MhPatSst, PatSst)
import qualified Data.PatSst    as PS
import           RandomPrefixes


splitEntryAt :: Int -> Entry -> (Word32, Entry)
splitEntryAt i (Entry p n) = (a `shiftR` (32 - i), Entry p' n)
  where Address a = address p
        Mask m    = mask p
        a'        = a `shiftL` i
        m'        = m - i
        p'        = Prefix (Address a') (Mask m')

groupEntries :: Int -> [Entry] -> [[(Word32, Entry)]]
groupEntries i es = unfoldr mkGroups gs
  where gs = sortBy (comparing fst) . map (splitEntryAt i) $ es
        mkGroups []              = Nothing
        mkGroups xs@((w, _) : _) = Just (span p xs)
          where p (w', _) = w' == w

data PatSstProps = PatSstProps { height    :: Int
                               , memUsage  :: Int
                               , fillRatio :: Double
                               }
                 deriving Show

getPatSstProps :: PatSst a => a -> PatSstProps
getPatSstProps t = PatSstProps { height    = PS.height t
                               , memUsage  = PS.memUsage t
                               , fillRatio = PS.fillRatio t
                               }

accPatSstProps :: [PatSstProps] -> PatSstProps
accPatSstProps ps = PatSstProps { height    = maximum . map height $ ps
                                , memUsage  = sum . map memUsage $ ps
                                , fillRatio = (sum . map fillRatio $ ps) /
                                              fromIntegral (length ps)
                                }

putBuckets :: Int -> IO ()
putBuckets b = do
  putStrLn "SST for PATRICIA tree with bucketing"
  putStrLn . (++) "  Bucket size:  " . show $ b
  putStrLn . (++) "  Height:       " . show . height $ prop
  putStrLn . (++) "  Memory usage: " . show . memUsage $ prop
  putStrLn . (++) "  Fill ratio:   " . show . fillRatio $ prop
  where n    = 1000000
        ess  = map (map snd) . groupEntries b $ randomEntries (32, 32) [1 .. n]
        prop = accPatSstProps .
               map (\es -> getPatSstProps (mkTable es :: MhPatSst)) $ ess
