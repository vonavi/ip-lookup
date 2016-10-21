module Data.Compression.Huffman
  (
    freqToEnc
  ) where

import           Control.Arrow (second)
import           Data.List     (sortBy)
import           Data.Ord      (comparing)

data Tree a = Leaf (a, Int) | Branch (Tree a) Int (Tree a) deriving Show

getFreq :: Tree a -> Int
getFreq (Leaf x)       = snd x
getFreq (Branch _ x _) = x

freqToTree :: [(a, Int)] -> Tree a
freqToTree [] = error "Empty list"
freqToTree xs = head . fromLeaves . map Leaf . sortBy (comparing snd) $ xs
  where fromLeaves :: [Tree a] -> [Tree a]
        fromLeaves ts@[_]     = ts
        fromLeaves (t1:t2:ts) = fromLeaves . sortBy (comparing getFreq) $
                                Branch t1 f t2 : ts
          where f = getFreq t1 + getFreq t2
        fromLeaves []         = error "Empty list"

freqToEnc :: [(a, Int)] -> [(a, [Bool])]
freqToEnc = helper . freqToTree
  where helper (Leaf x)       = [(fst x, [])]
        helper (Branch l _ r) = map (second (False :)) (helper l) ++
                                map (second (True :)) (helper r)
