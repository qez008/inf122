import Data.List
import System.IO

double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns

primeNumbers = [2, 3, 5, 7]
morePrimes = primeNumbers ++ [11, 13, 17]

revPrimes = reverse primeNumbers
firstPrime = head morePrimes
secondPrime = morePrimes !! 1
first3Primes = take 3 morePrimes
is1InList = 1 `elem` morePrimes
is2InList = 2 `elem` morePrimes
isXInList x = x `elem` morePrimes

zeroToTen = [0..10]
evenList = [2,4..20]
oddList = [1,3..20]
letterList = ['A'..'Z']
letterList2 = ['A'..'z']
letterList3 = letterList ++ ['a'..'z']
everyOtherLetter = ['a','c'..'z']
infiniteList = [10,20..]

sayHello = do
    putStrLn "Whats your name?"
    name <- getLine
    putStrLn $ "Hello " ++ name


