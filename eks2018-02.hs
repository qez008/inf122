module Eks201802 where

  --1
  --1.1
  mengde :: Eq t => [t] -> Bool
  mengde [] = True
  mengde (x:xs) = if x `elem` xs then False else mengde xs

  --1.2
  rep :: Eq t => [t] -> [t]
  rep [] = []
  rep (x:xs) = if x `elem` xs then rep xs else x:rep xs

  --1.3
  del :: Eq t => [t] -> [t] -> Bool
  del [] other = True
  del (x:xs) other = if x `elem` other then del xs other else False

  --1.4
  eq :: Eq t => [t] -> [t] -> Bool
  eq xs ys = del xs ys && del ys xs

  --1.5
  eqG :: Eq t => (t -> t -> Bool) -> [t] -> [t] -> Bool
  eqG p l1 l2 = delG p l1 l2 && delG p l2 l1
    where delG :: Eq t => (t -> t -> Bool) -> [t] -> [t] -> Bool
          delG _ [] _         = True
          delG p (x:xs) other = or [p x y | y <- other] && delG p xs other

  --1.6
  ps :: Eq t => [t] -> [[t]]
  ps [] = [[]]
  ps ls = undefined
  
  solve [] result = result
  solve (x:xs) result
    | null result = solve xs result
    | otherwise   = solve xs (map ([x]++) result)
