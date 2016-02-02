module Data.Bp
       (
         Paren(Open, Close)
       , Bp(Bp)
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

newtype Bp = Bp { toList :: [Paren] } deriving Show

findOpen :: Int -> Bp -> Maybe Int
findOpen n bp
  | n < 0 || n >= length x = Nothing
  | (x !! n) /= Close      = Nothing
  | otherwise              = helper 1 n $ take n x
  where x = toList bp
        helper d n x
          | d == 0    = Just n
          | null x    = Nothing
          | otherwise = if last x == Open
                        then helper (pred d) (pred n) (init x)
                        else helper (succ d) (pred n) (init x)

findClose :: Int -> Bp -> Maybe Int
findClose n bp
  | n < 0 || n >= length x = Nothing
  | (x !! n) /= Open       = Nothing
  | otherwise              = helper 1 n $ drop (succ n) x
  where x = toList bp
        helper d n x
          | d == 0    = Just n
          | null x    = Nothing
          | otherwise = if head x == Close
                        then helper (pred d) (succ n) (tail x)
                        else helper (succ d) (succ n) (tail x)

enclose :: Int -> Bp -> Maybe Int
enclose n bp = do cl <- findClose n bp
                  nx <- helper 1 cl . drop (succ cl) . toList $ bp
                  findOpen nx bp
  where helper d n x
          | d == 0    = Just n
          | null x    = Nothing
          | otherwise = if head x == Close
                        then helper (pred d) (succ n) (tail x)
                        else helper (succ d) (succ n) (tail x)

rank :: Int -> (Paren -> Bool) -> Bp -> Int
rank n p = helper n p . toList
  where helper n p = length . filter p . take (succ n)

rankOpen :: Int -> Bp -> Int
rankOpen n = rank n (== Open)

rankClose :: Int -> Bp -> Int
rankClose n = rank n (== Close)

select :: Int -> (Paren -> Bool) -> Bp -> Maybe Int
select n p bp
  | n <= 0 || n > length op = Nothing
  | otherwise               = Just . fst $ op !! pred n
  where op = filter (p . snd) . zip [0 ..] . toList $ bp

selectOpen :: Int -> Bp -> Maybe Int
selectOpen n = select n (== Open)

selectClose :: Int -> Bp -> Maybe Int
selectClose n = select n (== Close)
