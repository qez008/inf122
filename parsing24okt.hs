import Data.Char

data Ast = V String | N String [Ast] deriving (Show, Read)

tokenzie :: String -> [Char] -> [Char] -> [String]
tokenzie [] _ _ = []
tokenzie (x:xs) t s 
  | x `elem` t = [x]:tokenzie xs t s
  | x `elem` s = tokenzie xs t s
  | otherwise = (takeWhile p (x:xs)) : tokenzie (dropWhile p (x:xs)) t s
  where
    p = (notIn (t++s))
    notIn  xs = \x -> not (elem x xs)

tokens :: String -> [String]
tokens str = tokenzie str "()=" ", "

parse :: [String] -> [(Ast,Ast)]
parse [] = []
parse xs = let (t1, "=":r1) = parseT xs
               (t2,r2) = parseT r1 
            in (t1,t2):(parse r2)

parseT :: [String] -> (Ast, [String])
parseT (x:"(":xs) = let (args, r1) = parseArg xs [] in (N x args, r1)
parseT (x:xs) = if isUpper x then (V x, xs) else (N x [], xs)

parseArg :: [String] -> [Ast] -> ([Ast],[String]) 
parseArg (")":xs) ar = (ar, xs)
parseArg xs ar = let (t1, r1) = parseT xs 
                  in parseArg r1 (ar++[t1])

app :: (Ast, Ast) -> Ast -> Ast
-- app (X,t) - f(g(X,Y),s) = f(g(t,Y),s)
app (V x, t) (V y)      = if x == y then t else (V y)
app (V x, t) (N n args) = N n (map (app (V x, t)) args)

appE :: (Ast, Ast) -> (Ast, Ast) -> (Ast, Ast)
appE s (v,h) = (app s v, app s h)

appL s eqs = map (appE s) eqs

uni :: [(Ast, Ast)] -> [(Ast,Ast)] -> [(Ast, Ast)]
uni ls [] = if done ls then ls else uni [] ls
uni ls ((N y ar, V x):rs) = uni ls ((V x, N y ar):re) 
uni ls ((V x, N y ar):rs) = 
  if occFeil (V x) (N y ar) then 
    error "Occurs check feil ved " ++ (visAst (V x)) ++ " og " ++ (visAst (N y ar))
  else
    uni ((appL (V x, N y ar) ls) ++ [(V x, N y ar)]) (appL (V x, N y ar) rs)
uni ls ((N x ax, N y ay):rs) = if x == y then uni ls (rs++(zip ax ay))
                               else error "ulike funksjonsnavn"
uni ls ((V x, V y):rs) = if x == y then uni ls rs
                         else uni ((appL (V x, V y) ls)++[(V x, V y)]) (appL (V x, V y) rs)


visAst (Y x) = x
visAst (N f []) = f 
visAst (N f args) = f ++ "(" ++ (visL args) ++ ")"

vsiL [] = ""
visL [x] = visAst x
visL (x:xs) = visAst x ++ ", " ++ visL xs 

visE (a, b) = visAst a ++ " = " ++ visAst b

visLE xs = map (appE s) xs