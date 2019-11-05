
-- Input/Output

tst = do putStr "Testing. Testing. "
         putStr "1, 2, "
         putStrLn "3"

zeroTo x = [0..x]

write x = do putStrLn x

vis nR = do clr
            writeTop nR

clr = putStr "\ESC[2J"

writeTop [] = return
writeTop [x:xs] = do
    putStr x
    writeTop xs

