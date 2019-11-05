module Uke7 where
  import Data.Char
  import System.Console.ANSI

  -- A
  putString :: String -> IO ()
  putString str = sequence_ [putChar c | c <- str]

  -- B
  data FileOrFolder = File | Folder [FileOrFolder]
  prettyPrint :: FileOrFolder -> IO ()
  prettyPrint f = prettyPrint' f 0
    where 
      prettyPrint' :: FileOrFolder -> Int -> IO ()
      prettyPrint' f' ind = do
        putStr (indent ind)
        case f' of
         File -> putStrLn ("-File")
         Folder fof -> do
           putStrLn ("-Folder")
           sequence_ [prettyPrint' x (ind +2) | x <- fof]
      indent :: Int -> String
      indent n = take n (repeat ' ')
      -- or indent n = take n ([' ',' '..])

  -- C1
  trekant :: Int -> IO ()
  trekant 1 = putStrLn "*"
  trekant n = do 
    trekant (n-1)
    putStrLn (take n (repeat '*'))

  -- C2
  juletre :: Int -> IO ()
  juletre h = solve h 0
    where 
      solve :: Int -> Int -> IO ()
      solve 0 _ = return ()
      solve n m = do 
        solve (n-1) (m+1)
        putStr (take m (repeat ' '))
        putStrLn (level n)
      level :: Int -> String
      level n = concat (replicate n "* ")
  
  -- D
  visListe :: [String] -> IO ()
  visListe ls = solve ls 1 0
    where 
      solve :: [String] -> Int -> Int -> IO ()
      solve [] _ ma = col ma
      solve (x:xs) n ma = 
        do putStrLn (show n ++ ":" ++ seperate x)
           solve xs (n+1) (newMax ma x)
      newMax :: Int -> String -> Int
      newMax old str = max old (length str)
      col :: Int -> IO ()
      col n = putStrLn ("  " ++ seperate (map intToDigit [1..n]))
      seperate :: String -> String
      seperate [] = []
      seperate (x:xs) = " " ++ [x] ++ seperate xs

  -- D2
  modListe :: [String] -> Int -> Int -> IO ()
  modListe ls i r = visListe (rem ls i r)
    where
      rem :: [String] -> Int -> Int -> [String]
      rem ls i r = take (i-1) ls ++ (modify (ls !! (i-1)) r) : drop i ls
      modify :: String -> Int -> String
      modify s r = take (length s - r) s
  
  -- E | Game of Nim ---------------------------------------------------------------------
  type Board = [Int]
  
  initial :: Board
  initial = [5,4,3,2,1]

  next :: Int -> Int
  next 1 = 2
  next _ = 1

  finished :: Board -> Bool
  finished = all (== 0)

  move :: Board -> Int -> Int -> Board
  move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n - num else n

  putBoard :: Board -> IO ()
  putBoard [a,b,c,d,e] = do
    putRow 1 a
    putRow 2 b
    putRow 3 c
    putRow 4 d
    putRow 5 e
    where
      putRow :: Int -> Int -> IO ()
      putRow row num = do
        putStr (show row)
        putStr ": "
        putStrLn (concat (replicate num "* "))
  -- any other board that doesn't match the pattern above throws an error for now
  putBoard _ = error "unsupported board"

  newline :: IO ()
  newline = putChar '\n'
  
  nim :: IO ()
  nim = do 
    clearScreen
    play initial [] 1
  
  -- game loop. calls itself recursivly untill the game is over
  play :: Board -> [Board] -> Int -> IO ()
  play board history player 
    | finished board =
      do newline
         putBoard board
         newline
         putStrLn ("Player " ++ (show (next player)) ++ " wins")
    | otherwise = 
      do putBoard board
         putStrLn ("Player " ++ show player)
         putStrLn "Enter row and amount to remove:"
         inp <- getLine
         case validCmd (words inp) board of
           Move (a,b) -> 
             do clearScreen
                play (move board a b) (board:history) (next player)
           Undo -> 
             if null history then
               do clearScreen
                  putStrLn "Can't go further back\n"
                  play board [] player 
             else
               do clearScreen
                  play (head history) (tail history) player
           Error msg ->
             do clearScreen
                putStrLn $ msg ++ ", player " ++ show player ++ "\n"
                play board history player

  -- Like Maybe, but has 3 possible types. 
  -- Error is like Nothing, but with an error message. 
  data Cmd = Move (Int, Int) | Undo | Error String 

  validCmd :: [String] -> Board -> Cmd
  validCmd strs board
    | null strs = Error "Empty input"
    | validUndo strs = Undo
    | not (allDigits strs) = Error "Enter numbers only"
    | length strs /= 2 = Error "Too many arguments"
    | amount < 1 = Error "Remove atleast 1"
    | index < 1 = Error "Index out of bounds"
    | index > 5 = Error "Index out of bounds"
    | board !! (index-1) < 1 = Error "Row is empty"
    | otherwise = Move (index, amount) 
    where 
      index = x !! 0
      amount = x !! 1
      x = digitsToInts strs
      
  validUndo :: [String] -> Bool 
  validUndo strs = strs == ["0"]

  undo :: Board -> [Board] -> Int -> IO ()
  undo board history player 
    | null history = 
      do clearScreen
         putStrLn "Can't go further back\n"
         play board [] player 
    | otherwise = 
      do clearScreen
         play (head history) (tail history) player

  allDigits :: [String] -> Bool
  allDigits xs = and (map (all isDigit) xs)

  digitsToInts :: [String] -> [Int]
  digitsToInts strs = map (read :: String -> Int) strs