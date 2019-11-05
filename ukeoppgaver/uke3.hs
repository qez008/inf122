import Data.Char

-- 4.8 Exercises
-- 1:
halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
  where half = length xs `div` 2

-- 2:
third :: [a] -> a
third xs = head(tail (tail xs))
third' :: [a] -> a
third' xs = xs !! 2
third'' :: [a] -> a
third'' (_:_:x:xs) = x

-- 3:
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs 

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

-- 4:
or1 :: Bool -> Bool -> Bool
or1 False False = False
or1 _ _ = True

or2 :: Bool -> Bool -> Bool
or2 True _ = True
or2 _ True = True
or2 _ _ = False

or3 :: Bool -> Bool -> Bool
or3 b False = b
or3 _ _ = True

or4 :: Bool -> Bool -> Bool
or4 b c | b == c = b
        | otherwise = True

--or4 True True = True
--or4 True False = True
--or4 False True = True
--or4 False False = False
--or4 True True = True
--or4 True False = True
--or4 False True = True
--or4 False False = False

-- 5:
and' :: Bool -> Bool -> Bool
and' x y = if x then (if y then True else False) else False

-- 6:
and'' :: Bool -> Bool -> Bool
and'' x y = if x then y else False

--7:
mult' :: Int -> Int -> Int -> Int
mult' = \a -> \b -> \c -> a*b*c

-- 8:
lunhD :: Int -> Int
lunhD x = if double > 9 then double-9 else double
  where double = x*2

lunh :: Int -> Int -> Int -> Int -> Bool
lunh a b c d = (result `mod` 10) == 0
  where result = lunhD a + b + lunhD c + d 

-- B:
f :: Int -> Int
f x = if x == 0 then 0 else x * x + f (x-1)

f' :: Int -> Int
f' x = ff' ([0..x]) 
  where ff' [] = 0
        ff' (x:xs) = x * x + ff' xs

f'' :: Int -> Int
f'' = \x -> if x == 0 then 0 else x * x + f'' (x-1)

f''' :: Int -> Int
f''' x = sum [x^2 | x <- [1..x]]

-- C:
toList :: Int -> [Int]
toList x = map digitToInt $ show x
-- or use this helper method
toList' :: [Char] -> [Int]
toList' [] = []
toList' (c:cs) = digitToInt c:toList' cs
-- or this function
listInt :: Int -> [Int]
listInt 0 = []
listInt x = listInt (x `div` 10) ++ [x `mod` 10]

-- D:
ab :: String -> Bool
ab s = solve s (0,0)

solve :: String -> (Int, Int) -> Bool
solve s (as,bs) | as < bs = False
                | s == [] = True
                | otherwise = solve (tail s) $ test (head s) (as,bs)

test :: Char -> (Int,Int) -> (Int,Int)
test c (as,bs) | c == 'a' = (as+1, bs)
               | c == 'b' = (as, bs+1)
               | otherwise = (as, bs)

-- Alternativ fra forelesning:
aab st = ook st []
ook [] as = True
ook ('a':xs) as = ook xs ('a':as)
ook ('b':xs) [] = False
ook ('b':xs) (a:as) = ook xs as
okk (x:xs) as = okk xs as

-- 5.7 Exercises
-- 2:
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(a,b) | a <- [0..x], b <- [0..y]]

-- 3:
square :: Int -> [(Int,Int)]
square a = [x | x <- grid a a, fst x /= snd x]

-- 4:
replicate' :: Int -> a -> [a]
replicate' i a = [a | _ <- [0..(i-1)]]

-- 5:
pyths :: Int -> [(Int, Int, Int)]
pyths n = pyths' [1..n]
  where pyths' xs = [(x,y,z) | x <- xs, y <- xs, z <- xs, x^2 + y^2 == z^2]

-- 6:
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]

isPerfect :: Int -> Bool
isPerfect n = n == sum(factors n)

factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

-- 8:
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: a -> [(a,b)] -> [Int]
positions k xs = []

-- 9:
scalarP :: [Int] -> [Int] -> Int
scalarP a b = sum $ [x*y |(x,y) <- zip a b]
