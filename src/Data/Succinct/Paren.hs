module Data.Succinct.Paren
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
  | otherwise          = helper (1 :: Int) n $ take n ps
  where helper depth no x
          | depth == 0 = Just no
          | null x     = Nothing
          | otherwise  = if last x == Open
                         then helper (pred depth) (pred no) (init x)
                         else helper (succ depth) (pred no) (init x)

findClose :: Int -> [Paren] -> Maybe Int
findClose n ps
  | not (isOpen n ps) = Nothing
  | otherwise         = helper (1 :: Int) n $ drop (succ n) ps
  where helper depth nc x
          | depth == 0 = Just nc
          | null x     = Nothing
          | otherwise  = if head x == Close
                         then helper (pred depth) (succ nc) (tail x)
                         else helper (succ depth) (succ nc) (tail x)

enclose :: Int -> [Paren] -> Maybe Int
enclose n ps = do nc <- findClose n ps
                  nx <- helper (1 :: Int) nc . drop (succ nc) $ ps
                  findOpen nx ps
  where helper depth nc' x
          | depth == 0 = Just nc'
          | null x     = Nothing
          | otherwise  = if head x == Close
                         then helper (pred depth) (succ nc') (tail x)
                         else helper (succ depth) (succ nc') (tail x)

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
