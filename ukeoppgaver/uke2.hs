-- 2.7 Exercises ------------------------------
-- 2.4
last' :: [a] -> a
last' xs = head (reverse xs)
last'' :: [a] -> a
last'' [x] = x
last'' (x:xs) = last xs


-- 2.5
init1 :: [a] -> [a]
init1 xs = take (length xs - 1) xs
init2 :: [a] -> [a]
init2 xs = reverse (drop 1 (reverse xs))
init3 :: [a] -> [a]
init3 xs = reverse  (tail (reverse xs))

-- Chapter 3 -------------------------------------------------------------------
-- 3.1
-- ['a','b','c'] :: [Char]
-- ('a','b','c') :: (Char, Char, Char]
-- [(False, '0'), (True, '1')] :: [(Bool, Char)]-
-- ([False, '0'], [True, '1']) :: type error - cant have lists of False and Char
-- [tail, init, reverse] :: [[a] -> [a]] - [list]

-- 3.2
bools :: [Bool]
bools = [True, False]
nums :: [[Int]]
nums = [[1],[2],[3]]
add :: Int -> Int -> Int -> Int
add x y z = x + y + z
copy :: a -> (a,a)
copy x = (x,x)
apply :: (a -> b) -> a -> b
apply f x = f x

-- 3.3
second :: [a] -> a
second xs = head (tail xs)
swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)
pair :: a -> b -> (a,b)
pair x y = (x,y)
double :: Num a => a -> a
double x = x*2
palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs
twice :: (t -> t) -> t -> t
twice f x = f (f x)

{- 3.5
	Halting problem
-}

ones = [1,1..]

del :: [a] -> ([a],[a])
del [] = ([],[])
del xs = (take half xs, drop half xs)
  where half = (length xs) `div` 2

-- Flettesortering -------------------------------------------------------------
flett :: Ord a => [a] -> [a] -> [a]
flett [] ys = ys
flett xs [] = xs
flett (x:xs) (y:ys) = if x < y then x:(flett xs (y:ys))
                      else y:(flett (x:xs) ys)
fs :: Ord a => [a] -> [a]
fs [] = []
fs [x] = [x]
fs xs = let (aa, bb) = del xs
  in flett (fs aa) (fs bb)

fs' :: Ord a => [a] -> [a]
fs' [] = []
fs' [x] = [x]
fs' xs = flett (fs' a) (fs' b)
  where (a,b) = del xs

-- uke oppgaver ----------------------------------------------------------------
{- B:
 - False :: Bool
 - 5 + 8 :: Num a => a
 - (+) 2 :: Num a => Num a -> Num a
 - (+2) :: Num a => Num a -> Num a
 - (2+) :: Num a => Num a -> Num a
 - (["foo", "bar"], 'a') :: ([String], Char)
 - [(True,[]), (False, [['a']])] :: [(Bool, [String])]
 - \x y -> y !! x :: Int a -> [a] -> a
 - [take, drop, \x y -> [y !! x]] :: [f]
 -}
-- C:
e1 :: [Bool]
e1 = [False, True, False]
e2 :: [[Int]]
e2 = [[1,2],[3,4]]
e3 :: Num a => [([Char], a)]
e3 = [ ("a",7) ]
e4 ::Num a => [(Char, a)]
e4 = [ ('a',7) ]
e5 :: Num a => a -> a
e5 x = x * 2
e6 :: (a,b) -> a
e6 (x,y) = x
e7 :: a -> (a,a)
e7 x = (x, x)

-- D:
foo1 :: a -> b -> (a,b)
foo1 a b = (a, b)
foo2 :: a -> b -> (a, b)
foo2 a = \b -> (a, b)
foo3 :: a -> b -> (a,b)
foo3 = \a b -> (a, b)
foo4 :: a -> b -> (a, b)
foo4 = \a -> \b -> (a, b)
foo5 :: b -> a -> (b, a)
foo5 = \b -> \a -> (b,a)
foo6 :: b -> a -> (b, a)
foo6 = \b -> \a -> (b,a)
-- foo 1,2,3,4 og 6 har samme type. 6 er av samme type selv om tupelen har annen rekkefølge
-- foo 5 er ulik

-- G(E):
f :: Int -> Int -> Int
f x y = x + y
g :: (Int, Int) -> Int
g x = fst x + snd x



















