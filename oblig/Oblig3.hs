-- Morten Bergmann (qez008)
module Oblig3 where
  import Data.Char (isDigit, intToDigit)
  import Data.Bits (xor)
  import Numeric (showIntAtBase)

  -------------------------------------- User Input --------------------------------------

  data Cmd = Nim Int | Chomp Int | How | Move (Int, Int) | Quit | Error String 
    deriving Show

  verifyInput :: [String] -> Cmd 
  verifyInput cmd = 
    case cmd of
      ["?"] -> How
      ["q"] -> Quit
      ["n", x] ->
        case readInt x of
          Just n  -> if sizeOk n then Nim n else Error "Invalid board size"
          Nothing -> Error "Nim requires an int between 1 and 9 as an argument"
      ["c", x] -> 
        case readInt x of
          Just n  -> if sizeOk n then Chomp n else Error "Invalid board size"
          Nothing -> Error "Chomp requires an int between 1 and 9 as an argument"
      [x, y] -> 
        case (readInt x, readInt y) of
          (Just x', Just y') -> Move (x', y')
          _ -> Error "Invalid command"
      _ -> Error "Invalid argument(s)"
    where 
      sizeOk size = (size > 0 && size < 10)
              
  readInt :: String -> Maybe Int
  readInt str = if all isDigit str then Just (read str :: Int) else Nothing

  -------------------------------------- Prompt (1) --------------------------------------

  spill :: IO ()
  spill =
   do putStr "n(im) x / c(homp) x / q(uit) > "
      inp <- getLine
      case verifyInput (words inp) of
        Nim n   -> initGame nim n
        Chomp n -> initGame chomp n
        Quit    -> putStrLn "Bye!"
        Error m -> do putStrLn m; spill
        _       -> do putStrLn "Unknown input"; spill

  initGame :: Game -> Int -> IO ()
  initGame game n = let b = createBoard game n in do putBoard b ; generiskSpill b 1 game

  -- record containing functions specific to each game mode
  data Game = 
    Game { createBoard :: Int -> Board 
         , valid :: Board -> (Int, Int) -> Bool -- checks if a move is valid
         , move :: Board -> (Int, Int) -> Board -- performs a move - modifies a board
         , rules :: String
         , prompt :: String
         , computer :: Board -> Int -> (Int, Int) -- computers strategy
         , winner :: Player -> Player
         }

  -------------------------------------- Prompt (2) --------------------------------------

  generiskSpill :: Board -> Player -> Game -> IO ()
  generiskSpill board player game
    -- game over:
    | fin board =
      let msg = if winner game player == 1 then "You win!" else "Computer wins!" 
       in do putStrLn msg; spill
    -- players turn:
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
    -- computers turn:
    | otherwise = 
     do putStrLn "Computers move:"
        let cm = (computer game board (length board))
        let newBoard = move game board cm
        putBoard newBoard
        generiskSpill newBoard (next player) game
    where
      repeatPlay = generiskSpill board player game

  type Player = Int

  next :: Player -> Player
  next 1 = 2
  next _ = 1   
    
  type Board = [Int]

  fin :: Board -> Bool
  fin = all (== 0)

  putBoard :: Board -> IO ()
  putBoard b = 
   do putRows b 1
      putCol (length b)

  putRows :: Board -> Int -> IO ()
  putRows [] _ = return ()
  putRows (x:xs) ln = 
   do putStrLn (show ln ++ concat (replicate x " *"))
      putRows xs (ln+1)
                      
  putCol :: Int -> IO ()
  putCol n = putStrLn ("  " ++ concat (map (\x -> show x ++ " ") [1..n])) 

  
  ---------------------------------- Game of Nim -----------------------------------------
  
  nim :: Game
  nim = Game b nimValid nimMove nimRules p nimStrat next
    where
      b = \n -> [1..n] 
      p = "Nim: r a / ? / q > "


  nimRules :: String
  nimRules 
    = "----------------------- Rules of Nim -----------------------\n"
   ++ "| Each player, in turn, must take one or more stones from  |\n"
   ++ "| a pile. It's allowed to make a pile empty, effectively   |\n"
   ++ "| removing the pile out of the game. When a player is      |\n"
   ++ "| unable to move (if theres no stone left), the game ends. |\n"
   ++ "| r a = row amount                                         |\n"
   ++ "------------------------------------------------------------"

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
  -- Does not create a list of ALL good moves
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
  chomp = Game b chompValid chompMove chompRules p chompStrat id
    where 
      b = \n -> take n [n,n..]
      p = "Chomp: r k / ? / q > "

  chompRules :: String
  chompRules 
    = "--------------------- Rules of Chomp ----------------------\n"
   ++ "| Players take turns picking a square. With each choice,  |\n"
   ++ "| all squares above and to the right of the picked square |\n"
   ++ "| are removed. The person forced to take the last square  |\n"
   ++ "| loses. r k = coordinate of square you want to remove.   |\n"
   ++ "-----------------------------------------------------------"

  -- checks that the row is within bounds
  -- if performing the move modifies the board in some way the move is valid
  chompValid :: Board -> (Int, Int) -> Bool
  chompValid b m@(r, _) = r >= 1 && r <= length b && chompMove b m /= b

  chompMove :: Board -> (Int, Int) -> Board
  chompMove b (r, k) = [if r' > r then k' else min k' (k-1) | (r', k') <- zip [1,2..] b]
  -- for every r' less or eqaul r, the value should be k or less

  -- This is a silly strategy - It just picks the first available square
  chompStrat :: Board -> Int -> (Int, Int)
  chompStrat b _ = lastRow b

  ---------------------------------------- Tests -----------------------------------------

  {- nim sanity test: 

     Computer VS Computer
     One computer should always perform a good move, putting the game in a nim 0 state.
     When in a nim 0 state there should be no good moves thus the other computer should
     be forced to perform a bad move every round. Performing a bad move makes the nim
     sum not 0.

     If the nim sum after a round is 0, then it should be not 0 after the next. If the
     nim sum is not 0, the computer strategy must put the game in a nim 0 state,
     otherwise the strategy is incorrect.

  -}
  sanityTest :: Board -> IO ()
  sanityTest board 
    | fin board = do putBoard board ; putStrLn "Game Over \nTest successful"
    | invariant = putStrLn "test failed" 
    | otherwise = do putBoard board
                     putStrLn $ "Nim sum: " ++ show (nimSum board)
                     putStrLn $ "Good moves: " ++ show (gm board)
                     sanityTest (move nim (board) (computer nim board 9))
    where 
      -- True if the game is in a state it shouldn't be in:
      invariant = if nim0 board then length (gm board) > 0 else null (gm board)

  -- get all good moves
  gm :: Board -> [(Int,Int)]
  gm b = concat $ map (\n-> [x | x <- goodMoves b n]) [1..(length b)]
