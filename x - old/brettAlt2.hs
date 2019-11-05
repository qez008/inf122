import System.IO

brett x = putStrLn (" " ++ writeTop x ++ writeRow x x)

writeTop x = writeTaux x (x-1)
writeTaux x i = if i >= 0 then [intToDigit ((x-1) mod 10)] ++ " " ++ writeTaux x (i-1)
                else ""

writeRow x i = if i >= 0 then rep "." x ++ (show (x-i-1))++ "\n" ++ writeRow x (i-1) 
                         else ""

rep str 0 = ""
rep str n = str ++ rep str (n-1) 
