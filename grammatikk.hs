module Grammatikkk where

{-
 E -> E + E -> E * E + E -> Int * E + E -> Int * Int + Int -> Digit * Digit + Digit -> 2*3*4
 
 AST:
       +
     /   \
    *     1
  /   \
 2     3

Pre og postfiks notasjon:
  E -> + E E | * E E | ite E E E
  Entydige

data Ast = A Ast Ast | M Ast Ast | Ite Ast Ast | V Int

tokenize :: String -> [String]

parseE :: [String] -> (AST, [String])
parseE ("+":rs) = let (e1, r1) = parseE rs; (e2, r2) = parse r1
                   in (A e1 e2, r2)
parseE ("*":rs = let (e1, r1) = parseE rs; (e2, r2) = parseE r1 
                  in (M e1 e2, r2)
parseE ("ite":rs) = let (e1, r1) = parseE rs; (e2, r2) = parseE r1;(e3, r3) = parseE r2
                     in (Ite e1 e2 e3, r3)
parseE (x:rs) = if (onlyDigits x) then (V (read x :: Int), rs)
                else error "sdf"

 -}
