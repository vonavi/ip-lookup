module Data.Tree
       (
         Tree
       ) where

import Data.Monoid
import Control.Monad.State

import Data.IpRouter

data BTree a = Tip | Bin (BTree a) a (BTree a) deriving Show

instance Monoid a => Monoid (BTree a) where
  mempty = Tip
  Tip `mappend` x = x
  x `mappend` Tip = x
  (Bin ll x lr) `mappend` (Bin rl y rr) =
    Bin (ll `mappend` rl) (x `mappend` y) (lr `mappend` rr)

newtype Tree = Tree { getBTree :: BTree (Last Int) } deriving Show

instance Monoid Tree where
  mempty        = Tree mempty
  x `mappend` y = Tree $ getBTree x `mappend` getBTree y

fromEntry :: Entry -> Tree
fromEntry (Entry p n) = Tree $ helper . prefixBits $ p
  where helper :: [Bool] -> BTree (Last Int)
        helper []     = Bin Tip (Last (Just n)) Tip
        helper (b:bs) = if b
                        then Bin Tip (Last Nothing) (helper bs)
                        else Bin (helper bs) (Last Nothing) Tip

lookupState :: [Bool] -> BTree (Last Int) -> State (Last Int) ()
lookupState _      Tip         = return ()
lookupState []     (Bin _ x _) = modify (`mappend` x)
lookupState (b:bs) (Bin l x r) = do modify (`mappend` x)
                                    lookupState bs $ if b then r else l

instance IpRouter Tree where
  ipInsert e t        = t `mappend` fromEntry e
  ipLookup a (Tree t) = getLast $
                        execState (lookupState (addrBits a) t) (Last Nothing)
