module Oblig1 where
-- bergmann

import Data.Char
import Data.List

lowerOrDigit :: Char -> Bool
lowerOrDigit x 
    | isLower x = True
    | isDigit x = True
    | otherwise = False

testChar xs = do{
    show (head xs)
}