-- Morten Bergmann (qez008)
module Oblig2 where
  import Data.Char

  data Ast = Tall Int | Sum Ast Ast | Mult Ast Ast | Min Ast | Var String
    deriving (Eq, Show)

  -- 1 ----------------------------------------------------------------------------------
  parse :: String -> Ast
  parse str
    | null (snd result) = fst result
    | otherwise = error "failed to parse all tokens"
    where result = betterParse (tokenize str "+*-" " ")
    -- result is a pair with the ast and a list of strings. if the list of strings is not
    -- empty the parseExpr function must not have parsed everything and the ast is invalid

  parseExpr :: [String] -> (Ast, [String])
  parseExpr ("+":ts) = let (e1, r1) = parseExpr ts 
                           (e2, r2) = parseExpr r1 
                        in (Sum e1 e2, r2)
  parseExpr ("*":ts) = let (e1, r1) = parseExpr ts
                           (e2, r2) = parseExpr r1 
                        in (Mult e1 e2, r2)
  parseExpr ("-":ts) = let (e, r) = parseExpr ts 
                        in (Min e, r) 
  parseExpr (t:ts)
    | all isDigit t = (Tall (read t :: Int), ts) 
    | otherwise     = (Var t, ts) 

  tokenize :: String -> [Char] -> [Char] -> [String]
  tokenize [] _ _ = []
  tokenize (x:xs) t s 
    | elem x t  = [x] : tokenize xs t s
    | elem x s  = tokenize xs t s
    | otherwise = let word = takeWhile (notin (t++s)) (x:xs)
                      rest = dropWhile (notin (t++s)) (x:xs)
                      notin ls = \z -> not (elem z ls) 
                   in word:tokenize rest t s

  -- 2 ----------------------------------------------------------------------------------
  viss :: Ast -> String
  viss ast = addIndent (show ast) 0
    where
      addIndent :: String -> Int -> String
      addIndent [] _ = "\n"
      addIndent (x:xs) ind
        | x == '(' = "\n" ++ indent (ind +1) ++ addIndent xs (ind +1)
        | x == ')' = indent ind ++ addIndent xs (ind -1) 
        | x == '"' = addIndent xs ind -- removes unwanted '"'s from the returned string
        | otherwise = x:addIndent xs ind
  
      indent :: Int -> String
      indent x = take (3*x) (repeat ' ')

  vis :: Ast -> IO ()
  vis ast = putStr (viss ast)
  
  -- 3 ----------------------------------------------------------------------------------
  evi :: Ast -> Int
  evi ast = folde id (+) (*) negate ast
  
  evb :: Ast -> Bool
  evb ast = folde odd (||) (&&) not ast

  folde :: (Int -> t) -> (t -> t -> t) -> (t -> t -> t) -> (t -> t) -> Ast -> t
  folde f1 f2 f3 f4 ast = 
    case ast of
      Tall x   -> f1 x
      Sum x y  -> f2 (folde f1 f2 f3 f4 x) (folde f1 f2 f3 f4 y)
      Mult x y -> f3 (folde f1 f2 f3 f4 x) (folde f1 f2 f3 f4 y)
      Min x    -> f4 (folde f1 f2 f3 f4 x)

  -- 4 ----------------------------------------------------------------------------------  
  evix :: Ast -> Int -> Int
  evix ast i = folde' id (+) (*) negate id ast i
  
  evbx :: Ast -> Int -> Bool
  evbx ast b = folde' odd (||) (&&) not odd ast b
  
  folde' :: (Int -> t) -> (t -> t -> t) -> (t -> t -> t) 
         -> (t -> t) ->  (a -> t) -> Ast -> a -> t
  folde' f1 f2 f3 f4 f5 ast var = 
    case ast of
      Tall x   -> f1 x
      Sum  x y -> f2 (folde' f1 f2 f3 f4 f5 x var) (folde' f1 f2 f3 f4 f5 y var) 
      Mult x y -> f3 (folde' f1 f2 f3 f4 f5 x var) (folde' f1 f2 f3 f4 f5 y var) 
      Min  x   -> f4 (folde' f1 f2 f3 f4 f5 x var)
      Var  _   -> f5 var 

 -- løsning:

  -- parse str = case parseExpr (tokenize str) of
  --    (xs, []) -> xs
  --    (xs, rest) -> error "Feil" 


  betterParse :: [String] -> (Ast, [String])
  betterParse [] = error "err"
  betterParse (t:ts)
    | t == "+"      = (Sum a1 a2, r2)
    | t == "*"      = (Mult a1 a2, r2)
    | t == "-"      = (Min a1, r1)
    | all isDigit t = (Tall (read t :: Int), ts)
    | all isAlpha t = (Var t, ts)
    | otherwise     = error "feil"
    where (a1, r1) = betterParse ts
          (a2, r2) = betterParse r1