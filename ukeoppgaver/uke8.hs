-- pro x y = x
-- pro' = \x -> \y -> x
-- [Ø | \x -> \y -> x :: t] {}
-- t4: [x :: a | \y -> x :: b] {t = a -> b}
-- t4: [x ::a, y :: c | x :: d] {t = a -> b, b = c -> d}
-- t2: [x ::a, y :: c | Ø] {t = a -> b, b = c -> d, d = a}

-- [Ø | \y -> 2 :: t] {}
-- t4: [y :: a | 2 :: b] {t = a -> b}
-- t1: [y :: a | Ø] {t = a -> b, b = Num}

