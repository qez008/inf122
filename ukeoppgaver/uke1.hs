-- A. From the book ------------------------------------------------------------------
-- 1.3
prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * (prod xs)

-- 1.4
-- quick sort:
qs [] = []
qs (x:xs) = qs s ++ [x] ++ qs l
  where
    s = [a | a <- xs, a <= x]
    l = [b | b <- xs, b > x] 

-- reversed quick sort:
rqs [] = []
rqs (x:xs) = rqs l ++ [x] ++ rqs s
  where
    s = [a | a <- xs, a <= x]
    l = [b | b <- xs, b > x] 

-- 1.5 
-- if we replace '<=' with '<' elements that are equal to x would be placed in neither
-- smaller or larger.said elements would be lost

-- 2.2
n = a `div`length xs
  where
    a = 10
    xs = [1,2,3,4,5]

-- B. Not from the book --------------------------------------------------------------
-- 1.
plu :: [Int] -> Int -> [Int]
plu [] _ = []
plu (e:k) n = (e+n):(plu k n)

-- 2.
pali :: String -> Bool
pali [x] = False
pali xs = (take half xs) == (take half (reverse xs))
  where half = (length xs) `div`2

pali2 :: String -> Bool
pali2 x = x == reverse x

-- C. Also not from the book --------------------------------------------------------
revlists :: [[a]] -> [[a]]
revlists [] = []
revlists (x:xs) = (reverse x):(revlists xs)

llist :: [[a]] -> [[a]]
llist xs = [reverse x | x <- xs]
