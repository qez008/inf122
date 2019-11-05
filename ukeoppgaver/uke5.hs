module Uke5 where -- higher order funcs (kap 7)

twice :: (f -> f) -> f -> f
twice = \f -> f . f

odd' :: Int -> Bool
odd' = not . even

tw :: (t -> t) -> t -> t
tw = \f -> \x -> f (f x)

--f [] = []
--f (h:t) = (... h) : f t

-- n-te read av matrisen m
rad :: [[a]] -> Int -> [a]
rad m n = (!!n) m
-- k-te kolonnne
col :: [[a]] -> Int -> [a]
col m n = map (!!n) m


-- 7.1
one f p xs = map f $ filter p xs
one' f p xs = [f x | x <- xs, p x]
filterMap f p ls = (map f . filter p) ls

-- 7.2
-- a:
all' :: (a -> Bool) -> [a] -> Bool
all' p [x] = p x
all' p (x:xs) = p x && all' p xs

all2 p ls = and (map p ls)

all3 :: Foldable t => (a -> Bool) -> t a -> Bool
all3 p ls = foldl (\a x -> a && p x) True ls
-- b:
any' :: (a -> Bool) -> [a] -> Bool
any' p [x] = p x
any' p (x:xs) = p x || any' p xs
any2 :: (a -> Bool) -> [a] -> Bool
any2 p ls = or (map p ls)

-- c:
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) = if p x then x:takeWhile' p xs else []
-- tail rec
sevenC :: (a -> Bool) -> [a] -> [a] -> [a]
sevenC _ [] a     = a
sevenC p (x:xs) a = if p x then sevenC p xs (a++[x]) else a
-- d:
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs else (x:xs)

-- 7.4
dec2int :: [Int] -> Int -- dec2int [2,3,4,5] -> 2345
dec2int = foldl (\a x -> a * 10 + x) 0

--7.5
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a, b) = f a b

-- 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs
-- 7.6
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x  
  | p x = []
  | otherwise = h x : unfold p h t (t x)

chop8 :: [Int] -> [[Int]]
chop8 ls = unfold null (take 8) (drop 8) ls

map' :: (a -> b) -> [a] -> [b]
map' f = unfold (null) (f . head) (tail)

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f 

-- 7.10
lunh :: [Int] -> Bool
lunh xs = let lunh' xs = sum $ altMap (\x -> if x*2 > 9 then x*2-9 else x*2) id xs
           in lunh' xs `mod` 10 == 0

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f v []     = v
-- foldr f v (x:xs) = f x (foldr f v xs)

-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl f v []     = v
-- foldl f v (x:xs) = foldl f (f v x) xs

-- 7.3
mapR :: (a -> b) -> [a] -> [b]
mapR f ls = foldr (\x xs -> (f x):xs) [] ls

filterR :: (a -> Bool) -> [a] -> [a]
filterR p ls = foldr (\x xs -> if p x then x:xs else xs) [] ls

sumL :: [Int] -> Int
sumL = foldl (+) 0

sumR :: [Int] -> Int
sumR = foldr (+) 0


catR l1 l2 = foldr (:) l2 l1

catR2 :: [a] -> [a] -> [a]
catR2 = flip $ foldr (:)
