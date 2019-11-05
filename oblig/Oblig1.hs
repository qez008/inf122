module Oblig1 where
-- Morten Bergmann 

-- A(a) -------------------------------------------------------------------------------------
fjern :: String -> Int -> String
fjern str x = take x str ++ drop (x+1) str

-- A(b) -------------------------------------------------------------------------------------
fjernc :: String -> Char -> String
fjernc str c = [x | x <- str, x /= c]

-- A(c) -------------------------------------------------------------------------------------
tegnpos :: String -> Char -> [Int]
tegnpos str c = tegnpos' str c 0 []

tegnpos' :: String -> Char -> Int -> [Int] -> [Int]
tegnpos' str c i r
  | i == length str = r
  | c == str !! i   = tegnpos' str c (i+1) (r ++ [i])
  | otherwise       = tegnpos' str c (i+1) r

tegnp :: String -> Char -> [Int]
tegnp xs c =[i | (x,i) <- zip xs [0..], c == x]


-- B(a) -------------------------------------------------------------------------------------
ord :: String -> [String]
ord str = ord' str [] 

ord' :: String -> [String] -> [String]
ord' [] a = a
ord' (' ':str) a = ord' str a
ord' str a = ord' res (a++[wrd])
  where wrd = takeWhile (/= ' ') str
        res = drop (length wrd) str

-- B(b) -------------------------------------------------------------------------------------
------- IMPROVED SOLUTION -------------------------------------------------------------------
tokenize :: String -> String -> String -> [String]
tokenize str imp rem = let fragmented = fragment str (imp ++ rem) [] 
                        in clean fragmented (sstring rem)

fragment :: String -> String -> [String] -> [String]
fragment [] imp a = a
fragment str imp a
  | null res  = a ++ [wrd]
  | null wrd  = fragment (tail res) imp (a ++ [[head res]])
  | null a    = fragment (tail res) imp ([wrd] ++ [[head res]])
  | otherwise = fragment (tail res) imp (a ++ [wrd] ++ [[head res]])
  where wrd = takeWhile (`notElem` imp) str 
        len = length wrd
        res = drop len str

clean :: [String] -> [String] -> [String]
clean strs rem = [x | x <- strs , x `notElem` rem]

sstring :: String -> [String]
sstring str = [[x] | x <- str]

-- C ----------------------------------------------------------------------------------------
eqli :: Eq t => [t] -> [t] -> Bool
eqli xs ys = xs `contains` ys && ys `contains` xs

contains :: Eq t => [t] -> [t] -> Bool
contains [] _ = True
contains (x:xs) ys 
  | x `elem` ys = contains xs ys
  | otherwise = False

-- D ----------------------------------------------------------------------------------------
sjekk :: String -> String
sjekk str = if sjekk' str [] then "Korrekt" else "Feil"

sjekk' :: String -> String -> Bool
sjekk' [] [] = True
sjekk' [] _ = False
sjekk' (x:xs) ps
  | x `elem` closing && null ps = False
  -- if a closing parenthesis appear then the previous parentehsis
  -- must be an opening parenthesis of the same type.
  | x `elem` closing = if openClose (head ps) x then sjekk' xs (tail ps) else False
  | x `elem` opening = sjekk' xs (x:ps)
  | otherwise =  sjekk' xs ps
  where opening = ['(','{','['] 
        closing = [')','}',']'] 

openClose :: Char -> Char -> Bool
openClose '{' '}' = True
openClose '[' ']' = True
openClose '(' ')' = True
openClose _ _ = False










------- OLD SOLUTION B(b) -------------------------------------------------------------------
tokenize' :: String -> String -> String -> [String]
tokenize' str imp rem = let filtered = fst $ filterAll ([str], (imp ++ rem))
                        in fst $ removeAll (filtered, rem)
-- tokenize str imp rem = fst $ removeAll (fst $ filterAll ([str], (imp ++ rem)), rem)

remove :: [String] -> String -> [String] -> [String]
remove [] rem result = result
remove (x:xs) rem result 
  | x == rem  = remove xs rem result
  | otherwise = remove xs rem (result ++ [x]) 

removeAll :: ([String], String) -> ([String],String)
removeAll (strs, []) = (strs, [])
removeAll (strs, (x:xs)) = removeAll (remove strs [x] [], xs)

split :: String -> Char -> String -> [String]
split [] _ [] = []
split [] _ tmp = [tmp]
split (x:xs) c tmp 
  | x == c    = if null tmp then [x]:(split xs c []) else tmp:[x]:(split xs c [])
  | otherwise = split xs c (tmp ++ [x]) 

filter' :: [String] -> Char -> [String]
filter' [] _ = []
filter' (x:xs) c = (split x c []) ++ (filter' xs c)

filterAll :: ([String], String) -> ([String], String)
filterAll (strs, []) = (strs, [])
filterAll (strs, (x:xs)) = filterAll (filter' strs x, xs)

