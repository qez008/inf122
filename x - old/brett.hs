import System.IO

vis nR = do clr
            writeTop nR
            mapM_ (\i-> writeRow i nR)[1..nR]
            

clr = putStr "\ESC[2J"
writeTop nR = writeAt (lft + 1, 0)
    ( (concat [(show (mod i 10))++"" | i <– [1..nR]])++"\n")
lft = 3
writeAt (x,y) xs = do goto (x,y)
                      putStr xs

goto (x,y) = putStr("\ESC["++show x++";"++show y++"H")
writeRow i nR = do writeAt (if i > 9 then (lft-2) else lft - 1, 1+i) (show i)
                   mapM_ (\i -> putStr " .")[1..nR]
                   putStrLn


