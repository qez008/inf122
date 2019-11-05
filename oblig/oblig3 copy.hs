-- Morten Bergmann (qez008)
module Oblig3 where
  import System.Console.ANSI
  import Data.Char

  -- spill -------------------------------------------------------------------------------
  -- data type for handling user inputs:
  data Cmd = Nim Int | Chomp Int | How | Move (Int, Int) | Quit | Error String 
    deriving Show

  spill :: IO ()
  spill =
   do -- clearScreen
       putStr "n(im) x / c(homp) x / q(uit) >"
       inp <- getLine
       case verifyInput (words inp) of
         Nim n -> if (n < 1 || n > 9) then wrongSize else play [1..n] 1 nimm
         Chomp n -> if (n < 1 || n > 9) then wrongSize else play (take n [n,n..]) 1 chompm
         Quit -> return ()
         Error msg -> nextRound msg spill
         _ -> nextRound "Unknown input" spill

  wrongSize :: IO ()
  wrongSize =
   do putStrLn "Invalid board size"
      spill

  verifyInput :: [String] -> Cmd 
  verifyInput cmd = 
    case cmd of
      ["?"] -> How
      ["q"] -> Quit
      ["n", y] -> if wordIsNumber y then Nim (readInt y) 
                  else error "Nim requires an int as an argument"
      ["c", y] -> if wordIsNumber y then Chomp (readInt y)
                  else error "Chimp requires an int as an argument"
      [x, y]   -> if wordIsNumber x && wordIsNumber y then Move (readInt x, readInt y)
                  else Error "Invalid command"
      _ -> Error "Invalid argument(s)"

  -- general game funcs ------------------------------------------------------------------
  data GameMode v m f r p = 
    GameMode { valid :: Board -> (Int, Int) -> Bool 
             , move  :: Board -> (Int, Int) -> Board
             , fin  :: Board -> Bool 
             , rules :: String
             , prompt :: String
             }

  nimm :: GameMode v m f r p
  nimm = GameMode validNim nimMove finished nimRules "Nim: r a / ? / q >"

  chompm :: GameMode v m f r p
  chompm = GameMode chompValid chompMove finished chompRules "Chomp: r k / ? / q"

  play' = play [1..9] 1 nimm
  play :: Board -> Int -> GameMode v m f r p -> IO ()
  play board player gm
    | fin gm board = let actor = if player == 1 then "Player" else "Computer" 
                      in do putStrLn (actor ++ " won!"); spill
    | player == 1 = 
     do putBoard board
        putStr (prompt gm)
        inp <- getLine
        case verifyInput (words inp) of
          How -> 
            nextRound (rules gm) repeatPlay
          Error msg ->
            nextRound msg repeatPlay
          Quit -> 
            nextRound "Player quits" spill
          Move (r, a) -> 
            if validNim board (r-1, a) then
              play (move gm board (r, a)) (next player) gm
            else 
              nextRound "Invalid nimMove" repeatPlay
          _ -> 
            nextRound "Unkown input" repeatPlay
    | otherwise = 
     do let compMove = (head $ possibleMoves board, 1)
        play (move gm board compMove) (next player) gm
    where
      repeatPlay = play board player gm

  type Board = [Int]

  next :: Int -> Int
  next 1 = 2
  next _ = 1

  putBoard :: Board -> IO ()
  putBoard b = 
    do putRows b 1
       putCol (length b)
    where 
      putRows :: Board -> Int -> IO ()
      putRows [] _ = return ()
      putRows (x:xs) ln = do
        putRow ln x
        putRows xs (ln+1)

      putRow :: Int -> Int -> IO ()
      putRow row num = do
        putStr (show row)
        putStrLn (concat (replicate num " *"))

      putCol :: Int -> IO ()
      putCol n = putStrLn ("  " ++ concat (map (\x -> show x ++ " ") [1..n]))

  -- Game of Nim | Fra bok og uke7.hs ----------------------------------------------------
  nim :: Int -> IO ()
  nim n = if (n < 1 || n > 9) then nextRound "invalid board size" spill
          else nextRound "" (playNim [1..n] 1)

  nimRules :: String
  nimRules 
    = "––––––––––––––––––––––––––––––––– Rules of Nim ––––––––––––––––––––––––––––––––––\n"
   ++ "| Each player, in turn, must take at least one stone, but they may take more    |\n"
   ++ "| than one stone as long as they all come from the same pile. It's allowed to   |\n"
   ++ "| make a pile empty, effectively removing the pile out of the game. When a      |\n"
   ++ "| player is unable to nimMove, the game ends. Naturally, as long as there is a  |\n"
   ++ "| stone, either player can take that stone, and thus can nimMove. So the ending |\n"
   ++ "| condition can be rephrased, where the game ends if there is no stone left.    |\n"
   ++ "–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––\n"

  playNim :: Board -> Int -> IO ()
  playNim board player 
    | finished board =
      do newline
         putBoard board
         newline
         putStrLn ("Player " ++ (show (next player)) ++ " wins")
         spill
    | player == 2 = computerPlaysNim board player
    | otherwise   = playerPlaysNim board player
  
  finished :: Board -> Bool
  finished = all (== 0)

  playerPlaysNim :: Board -> Int -> IO ()
  playerPlaysNim board player =
    do putBoard board
       putStr "Nim: r a / ? / q >"
       inp <- getLine
       case verifyInput (words inp) of
         How -> 
           nextRound nimRules (playNim board player)
         Error msg ->
           nextRound msg (playNim board player)
         Quit -> 
           nextRound "Player quits" spill
         Move (r, a) -> 
           if validNim board (r-1, a) then
             nextRound (moveMsg "Player" r a) (playNim (nimMove board (r,a)) (next player))
           else 
             nextRound "Invalid nimMove" (playNim board player)
         _ -> 
           nextRound "Unkown input" (playNim board player)

  computerPlaysNim :: Board -> Int -> IO ()
  computerPlaysNim board player =
    do putStrLn $ moveMsg "Computer" (head $ possibleMoves board) 1
       playNim (nimMove board ((head $ possibleMoves board), 1)) (next player)

  possibleMoves :: Board -> [Int]
  possibleMoves b = [y | (x,y) <- zip b [1,2..], x > 0]

  validNim :: Board -> (Int, Int) -> Bool
  validNim b (r, a)
    | r < 0         = False
    | r >= length b = False
    | b !! r == 0   = False
    | b !! r < a    = False
    | a < 1         = False
    | otherwise     = True

  nimMove :: Board -> (Int, Int) -> Board
  nimMove board (row,num) = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n - num else n

  moveMsg :: String -> Int -> Int -> String
  moveMsg actor r a = actor ++ " removes " ++ show a ++ " from row " ++ show r

  -- chomp -------------------------------------------------------------------------------
  chomp :: Int -> IO ()
  chomp n = if (n < 1 || n > 9) then nextRound "invalid board size" spill
            else nextRound "" (playChomp (take n [n,n..]) 1)

  chompRules :: String
  chompRules 
    = "––––––––––––––––––––––––––– Rules of Chomp ––––––––––––––––––––––––––––\n"
   ++ "| Players take turns picking a square. With each choice, all squares  |\n"
   ++ "| below and to the right of the picked square are removed. The person |\n"
   ++ "| forced to take the last square loses.                               |\n"
   ++ "–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––\n"

  playChomp :: Board -> Int -> IO ()
  playChomp board player 
    | finished board   = putStrLn "game over"
    | player == 2 = chompComputer board player
    | otherwise   = chompPlayer board player

  chompComputer :: Board -> Int -> IO ()
  chompComputer = undefined

  chompPlayer :: Board -> Int -> IO ()
  chompPlayer board player =
   do putBoard board
      putStr "Chop: r k / ? / q >"
      inp <- getLine
      case verifyInput (words inp) of
        How -> 
          nextRound chompRules (playChomp board player)
        Quit -> 
          nextRound "Player quits" spill
        Move m -> 
          if chompValid board m then playChomp (chompMove board m) player
          else nextRound "Invalid move" (playChomp board player)
        _ -> 
          nextRound "undefined behaviour" (playChomp board player)

  chompValid :: Board -> (Int, Int) -> Bool
  chompValid b (r, k) = True

  chompMove :: Board -> (Int, Int) -> Board
  chompMove b (r, k) = b 
  
  -- helper funcs ------------------------------------------------------------------------
  readInt :: String -> Int
  readInt str = read str :: Int

  wordIsNumber :: [Char] -> Bool -- Checks if all the characters in a string is a digit
  wordIsNumber xs = all isDigit xs

  -- clears the screen, prints a message and runs a function
  nextRound :: String -> IO () -> IO () -- use this when the user gives invalid inputs
  nextRound msg func =
    do clearScreen
       putStrLn msg   -- error message to the user
       func         -- the function to be repeated

  newline :: IO ()
  newline = putChar '\n'
  