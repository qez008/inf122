


-- 6    Recursive Funcs
-- 2:
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3:
pow :: Int -> Int -> Int
pow _ 0 = 1
pow x p = x * pow x (p-1)
-- | pow 2 3 = 2 * (pow 2 2) = 2 * 2 * (pow 2 1) = 2 * 2 * 2 * pow 2 0 = 2 * 2 * 2 * 1 = 8 
-- 4:
euclid :: Int -> Int -> Int
euclid a b = euclid' a b (min a b)
  where min a b | a < b = a 
                | otherwise = b

euclid' :: Int -> Int -> Int -> Int
euclid' a b r | comdiv a b r = r
              | otherwise = euclid' a b (r-1)

comdiv :: Int -> Int -> Int -> Bool
comdiv a b r = (a `mod` r == 0) && (b `mod`r == 0)


-- 6:
-- a.
and' :: [Bool] -> Bool
and' xs | null xs   = True
        | head xs   = and' $ tail xs
        | otherwise = False

-- b.
cat :: [[a]] -> [a]
cat [[]] = []
cat (l:ls) = l ++ cat ls


-- Forelesning

-- Enkel rekursiv funksjon
append :: [a] -> [a] -> [a]
append [] bs = bs
append (a:as) bs = a:append as bs
-- Bedre, men ikke så enkle hale rekurvsiv funksjon
app :: [a] -> [a] -> [a]
app as bs = apph (reverse as) bs
apph :: [a] -> [a] -> [a]
apph [] bs = bs
apph (x:xs) bs = apph xs (x:bs)

-- Bedre implementasjon av fibonacci med bruk av halerekursjon
fib :: Int -> (Integer, Integer)
fib n = fibh 1 n (1,1)
  where fibh x n (a,b) 
          | x == n+1 = (a,b)
          | x == 1 = fibh 2 n (a,b)
          | otherwise = fibh (x+1) n (a+b,a)

fibb :: Int -> Integer
fibb n = if n <= 1 then 1 else fibb (n-1) + fibb (n-2)

bs x [] = -1
bs x [(y,z)] | x == y = z
             | otherwise = -1
bs x l | n == x    = v
       | n < x     = bs x (drop m l)
       | otherwise = bs x (take m l) 
       where m = length l `div` 2
             (n,v) = l !! m
