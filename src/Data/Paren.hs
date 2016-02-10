module Data.Paren
       (
         Paren(Open, Close)
       , isOpen
       , isClose
       , findOpen
       , findClose
       , enclose
       , rankOpen
       , rankClose
       , selectOpen
       , selectClose
       ) where

data Paren = Open | Close deriving Eq

instance Show Paren where
  show Open  = "("
  show Close = ")"

isOpen :: Int -> [Paren] -> Bool
isOpen n ps
  | n < 0 || n >= length ps = False
  | otherwise               = (ps !! n) == Open

isClose :: Int -> [Paren] -> Bool
isClose n ps
  | n < 0 || n >= length ps = False
  | otherwise               = (ps !! n) == Close

findOpen :: Int -> [Paren] -> Maybe Int
findOpen n ps
  | not (isClose n ps) = Nothing
  | otherwise          = helper 1 n $ take n ps
  where helper d n x
          | d == 0    = Just n
          | null x    = Nothing
          | otherwise = if last x == Open
                        then helper (pred d) (pred n) (init x)
                        else helper (succ d) (pred n) (init x)

findClose :: Int -> [Paren] -> Maybe Int
findClose n ps
  | not (isOpen n ps) = Nothing
  | otherwise         = helper 1 n $ drop (succ n) ps
  where helper d n x
          | d == 0    = Just n
          | null x    = Nothing
          | otherwise = if head x == Close
                        then helper (pred d) (succ n) (tail x)
                        else helper (succ d) (succ n) (tail x)

enclose :: Int -> [Paren] -> Maybe Int
enclose n ps = do cl <- findClose n ps
                  nx <- helper 1 cl . drop (succ cl) $ ps
                  findOpen nx ps
  where helper d n x
          | d == 0    = Just n
          | null x    = Nothing
          | otherwise = if head x == Close
                        then helper (pred d) (succ n) (tail x)
                        else helper (succ d) (succ n) (tail x)

rank :: Int -> (Paren -> Bool) -> [Paren] -> Int
rank n p = length . filter p . take (succ n)

rankOpen :: Int -> [Paren] -> Int
rankOpen n = rank n (== Open)

rankClose :: Int -> [Paren] -> Int
rankClose n = rank n (== Close)

select :: Int -> (Paren -> Bool) -> [Paren] -> Int
select n p ps
  | n == 0    = -1
  | otherwise = fst $ op !! pred n
  where op = filter (p . snd) . zip [0 ..] $ ps

selectOpen :: Int -> [Paren] -> Int
selectOpen n = select n (== Open)

selectClose :: Int -> [Paren] -> Int
selectClose n = select n (== Close)
