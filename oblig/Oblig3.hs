-- Morten Bergmann (qez008)
module Oblig3 where
  import Data.Char (intToDigit, isDigit)
  import Data.Bits (xor)
  import Numeric (showIntAtBase)

  type Player = Int

  next :: Player -> Player
  next 1 = 2
  next _ = 1   

  type Board = [Int]

  -- data type for handling user inputs
  data Cmd = Nim Int | Chomp Int | How | Move (Int, Int) | Quit | Error String 
    deriving Show

  verifyInput :: [String] -> Cmd 
  verifyInput cmd = 
    case cmd of
      ["?"] -> How
      ["q"] -> Quit
      ["n", y] -> if wordIsNumber y then Nim (readInt y) 
                  else Error "Nim requires an int between 1 and 9 as an argument"
      ["c", y] -> if wordIsNumber y then Chomp (readInt y)
                  else Error "Chomp requires an int between 1 and 9 as an argument"
      [x, y]   -> if wordIsNumber x && wordIsNumber y then Move (readInt x, readInt y)
                  else Error "Invalid command"
      _ -> Error "Invalid argument(s)"
  

  -- data type containing functions specific to each game mode
  data Game = 
    Game { board :: Int -> Board -- creates a starting board - not the board itself
         , valid :: Board -> (Int, Int) -> Bool -- checks if the move is valid
         , move  :: Board -> (Int, Int) -> Board -- performs a move - modifies a board
         , fin  :: Board -> Bool -- checks if there are any valid moves left
         , rules :: String
         , prompt :: String
         , computer :: Board -> Int -> (Int, Int) -- computers strategy
         , winner :: Player -> Player
         }

  spill :: IO ()
  spill =
   do putStr "n(im) x / c(homp) x / q(uit) > "
      inp <- getLine
      case verifyInput (words inp) of
        Nim n -> if (n < 1 || n > 9) then invalidSize else createGame nim n
        Chomp n -> if (n < 1 || n > 9) then invalidSize else createGame chomp n
        Quit -> putStrLn "Bye!"
        Error msg -> do putStrLn msg; spill
        _ -> do putStrLn "Unknown input"; spill
    where 
      createGame game size = do let b = board game size
                                putBoard b -- show the board here
                                generiskSpill b 1 game
      invalidSize = do putStrLn "Invalid board size - pick a number between 1 and 9" 
                       spill

  generiskSpill :: Board -> Player -> Game -> IO ()
  generiskSpill board player game
    | fin game board = 
      let msg = if winner game player == 1 then "You win!" else "Computer wins!" 
       in do putStrLn msg; spill
    | player == 1 = 
     do putStr (prompt game)
        inp <- getLine
        case verifyInput (words inp) of
          Quit -> 
            do putStrLn "Quit to menu."; spill
          How -> 
            do putStrLn (rules game); repeatPlay
          Move (r, a) -> 
            if valid game board (r, a) then
             do let newBoard = move game board (r, a)
                putBoard newBoard
                generiskSpill newBoard (next player) game
            else 
              do putStrLn "Invalid move - try again."; repeatPlay
          _ -> 
            do putStrLn "Invalid move - try again."; repeatPlay
    | otherwise = 
     do putStrLn "Computers move:"
        let cm = (computer game board (length board))
        let newBoard = move game board cm
        putBoard newBoard
        generiskSpill newBoard (next player) game
    where
      repeatPlay = generiskSpill board player game

    
  --------------------------------- Helper Funcs -----------------------------------------

  putBoard :: Board -> IO ()
  putBoard b = 
   do putRows b 1
      putCol (length b)
    where 
      putRows :: Board -> Int -> IO ()
      putRows [] _ = return ()
      putRows (x:xs) ln = do
        putStrLn (show ln ++ concat (replicate x " *"))
        putRows xs (ln+1)
                      
      putCol :: Int -> IO ()
      putCol n = putStrLn ("  " ++ concat (map (\x -> show x ++ " ") [1..n])) 

  readInt :: String -> Int
  readInt str = read str :: Int

  wordIsNumber :: [Char] -> Bool -- Checks if all the characters in a string are digits
  wordIsNumber xs = all isDigit xs

  
  ---------------------------------- Game of Nim -----------------------------------------
  
  nim :: Game
  nim = Game b nimValid nimMove nimFin nimRules p nimStrat next
      where
        b = \n -> [1..n] 
        p = "Nim: r a / ? / q > "

  nimRules :: String
  nimRules 
    = "–––––––––––––––––––---–––– Rules of Nim –––––––––---––––––––––––-\n"
   ++ "| Each player, in turn, must take at least one stone, but they  |\n"
   ++ "| may take more than one stone as long as they all come from    |\n"
   ++ "| the same pile. It's allowed to make a pile empty, effectively |\n"
   ++ "| removing the pile out of the game. When a player is unable to |\n"
   ++ "| move (if there is no stone left), the game ends.              |\n"
   ++ "–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––"
  
  nimFin :: Board -> Bool
  nimFin = all (== 0)

  nimValid :: Board -> (Int, Int) -> Bool
  nimValid b (r, a)
    | r < 1 =           False
    | r > length b =    False
    | b !! (r-1) == 0 = False
    | b !! (r-1) < a =  False
    | a < 1 =           False
    | otherwise =       True

  nimMove :: Board -> (Int, Int) -> Board
  nimMove b (row,num) = [update r n | (r,n) <- zip [1..] b]
    where update r n = if r == row then n - num else n


  {-  
    --------------------------------------------------------------------------------------
    Strategy for winning nim - Always
    --------------------------------------------------------------------------------------
      
    The strategy follows these simple rules:

      - If there is only one row left, take the whole row (easy).
      - Look for a move that leaves the nim sum of the board equal to 0 (nim sum?).
      - If such a move can't be found, do anything (but this never happens*).

    Nim Sum is the same as converting every row to binary and applying xor to all rows.
    If the nim sum is 0 then the next player is bound to lose (acording to logic).

    The winning strategy is perhaps better explained at: https://en.wikipedia.org/wiki/Nim

    nimStrat takes an Int n. This n is the amount to remove. It starts out as the size of
    the board. If there are no good moves that removes n bricks from the board, it tries 
    (n-1) and so on until it hits 0. If it hits 0 then there are no good moves and it will 
    return any valid move. 
    The function will remove as many bricks as possible each turn. When you are bound to 
    lose its better to get it over with quickly :)

    * unless the computer is forced make the first move
    --------------------------------------------------------------------------------------  
  -}

  nimStrat :: Board -> Int -> (Int, Int)
  nimStrat b n 
    | nRowsLeft b == 1 = lastRow b
    | n == 0 = (head (rowsLeft b), 1)
    | otherwise = if null $ goodMoves b n then nimStrat b (n-1) else head $ goodMoves b n

  -- A list of moves that are valid and will make the nim sum of the board equal to 0
  goodMoves :: Board -> Int -> [(Int, Int)]
  goodMoves b n = [(x,n) | x <- rowsLeft b, nimValid b (x,n) && nim0 (nimMove b (x,n))]

  nim0 :: Board  -> Bool
  nim0 b = nimSum b == 0

  nimSum :: Board -> Int
  nimSum b = foldl1 xor (map bin b)

  bin :: Int -> Int
  bin i = read (showIntAtBase 2 intToDigit i "") :: Int
  
  -- index of all rows greater than 0
  rowsLeft :: Board -> [Int]
  rowsLeft b = [y | (x,y) <- zip b [1,2..], x > 0]

  -- counts rows greater than 0
  nRowsLeft :: Board -> Int 
  nRowsLeft b = length (filter (/= 0) b) -- or length (rowsLeft b)

  -- returns index and value of first non 0 row
  lastRow :: Board -> (Int, Int)
  lastRow b = head [(r, a) | (r, a) <- zip [1..(length b)] b, a > 0]

  --------------------------------- Game of Chomp ----------------------------------------
  
  chomp :: Game
  chomp = Game b chompValid chompMove chompFin chompRules p chompStrat id
    where 
      b = \n -> take n [n,n..]
      p = "Chomp: r k / ? / q > "

  chompRules :: String
  chompRules 
    = "––––––––––––––––––-–––– Rules of Chomp ––––––––––––––––––--–––––\n"
   ++ "| Players take turns picking a square. With each choice, all   |\n"
   ++ "| squares above and to the right of the picked square are      |\n"
   ++ "| removed. The person  forced to take the last square loses.   |\n"
   ++ "––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––"
   

  chompFin :: Board -> Bool
  chompFin = all (==0)

  chompValid :: Board -> (Int, Int) -> Bool
  chompValid b m@(r, _) = r >= 1 && r <= length b && chompMove b m /= b

  chompMove :: Board -> (Int, Int) -> Board
  chompMove b (r, k) = [if row > r then x else min x (k-1) | (x, row) <- zip b [1,2..] ]
  -- for every row less or eqaul r, the value should be k or less

  {- This is a silly strat - It just picks the first available square -}
  chompStrat :: Board -> Int -> (Int, Int)
  chompStrat b _ = lastRow b
  
