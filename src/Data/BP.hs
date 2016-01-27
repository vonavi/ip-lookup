module Data.BP
       (
         Paren(Open, Close)
       , BP(BP)
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

newtype BP = BP { toList :: [Paren] } deriving Show

findOpen :: Int -> BP -> Maybe Int
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

findClose :: Int -> BP -> Maybe Int
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

enclose :: Int -> BP -> Maybe Int
enclose n bp = do cl <- findClose n bp
                  nx <- helper 1 cl . drop (succ cl) . toList $ bp
                  findOpen nx bp
  where helper d n x
          | d == 0    = Just n
          | null x    = Nothing
          | otherwise = if head x == Close
                        then helper (pred d) (succ n) (tail x)
                        else helper (succ d) (succ n) (tail x)

rank :: Int -> (Paren -> Bool) -> BP -> Int
rank n p = helper n p . toList
  where helper n p = length . filter p . take (succ n)

rankOpen :: Int -> BP -> Int
rankOpen n = rank n (== Open)

rankClose :: Int -> BP -> Int
rankClose n = rank n (== Close)

select :: Int -> (Paren -> Bool) -> BP -> Maybe Int
select n p bp
  | n <= 0 || n > length op = Nothing
  | otherwise               = Just . fst $ op !! pred n
  where op = filter (p . snd) . zip [0 ..] . toList $ bp

selectOpen :: Int -> BP -> Maybe Int
selectOpen n = select n (== Open)

selectClose :: Int -> BP -> Maybe Int
selectClose n = select n (== Close)
