module Uke6 where
-- author: qez008

-- Oppgaver fra boken -------------------------------------------------------------------
-- 8.3:
data Tree a = Leaf a | Node (Tree a) (Tree a)
-- some test trees:
tree1 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)) -- perfectly balanced
tree2 = Node (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4) -- left sided
tree3 = Leaf 1 -- singleton tree
tree4 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3) -- should be balanced

balance :: Tree a -> Bool
balance (Leaf _) = True
balance (Node l r) = abs (nLeaves l - nLeaves r) <= 1 && balance l && balance r

nLeaves :: Tree a -> Int
nLeaves (Leaf _) = 1
nLeaves (Node l r) = nLeaves l + nLeaves r

-- some test func
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l r) = flatten l ++ flatten r

-- 8.5
data Expr = Val Int | Add Expr Expr
-- test expressions:
expr1 = Add (Add (Val 1) (Val 2)) (Val 3)
expr2 = Add (Add (Val 7) (Val 2)) (Add (Val 43) (Add (Val 1) (Add (Val 4) (Val 2))))

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g e = case e of
  Val x -> f x
  Add x y -> g (folde f g x) (folde f g y)

-- 8.6
eval :: Expr -> Int -- Evaluate expression
size :: Expr -> Int -- Number of values in expression

eval e = folde id (+) e
size e = folde (\x -> 1) (+) e


-- B ------------------------------------------------------------------------------------
data Exp = S | U | Og Exp Exp | El Exp Exp | Ik Exp
-- Test expressions
e1 = (Og S U)
e2 = (Og (Ik (Og S U)) S)
e3 = (Og (Og S S) S)
e4 = (El (Og U S) U)

-- (a)
folde' :: a -> a -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> Exp -> a
folde' s u o e i exp = case exp of
  S -> s
  U -> u
  Og x y -> o (folde' s u o e i x) (folde' s u o e i y)
  El x y -> e (folde' s u o e i x) (folde' s u o e i y)
  Ik x -> i (folde' s u o e i x)

-- (b)
evb :: Exp -> Bool
evb exp = folde' True False (&&) (||) not exp

-- (c)
evh :: Exp -> Int
evh exp = folde' 1 1 (+) (+) (+1) exp
-- TODO
evh' :: Exp -> Int
evh' exp = folde' 1 1 tt tt (+1) exp
  where tt =(\x y -> 1 + max x y) -- tallest tree

-- (d)
evi :: Exp -> Int
evi exp = folde' 1 5 (+) (*) negate exp
