import Control.Concurrent
import System.Console.ANSI

dostuff :: IO ()
dostuff = do
  putStr "Hold up!"
  wait 10 "s"
  putStrLn " ..ok go"
  getLine >>= putStrLn

wait :: Int -> String -> IO () -- wait seconds
wait time unit = case unit of
  "ms" -> threadDelay (1000 * time)
  "s" -> threadDelay (1000000 * time)
  _ -> error ("unknown time unit | \"" ++ unit ++ "\" is not a unit , use \"ms\" or \"s\" ")

move = move' 1 1

move' x y = do
  inp <- getChar
  clearScreen
  case inp:[] of 
    "D[\ESC" -> putStr "Left"
    "C[\ESC" -> putStr "Right"
    _ -> putStr ""



-- implementation of getLine, but supports backspace
getInp :: String -> IO String
getInp xs = do 
  x <- getChar
  case x of
    '\n'   -> return xs
    '\DEL' -> do -- when pressing delete '^?' is entered
                 -- cursor is moved 2 places so we need to move back 3
                 cursorBackward 3 
                 clearFromCursorToScreenEnd
                 -- remove one from input
                 getInp (drop 1 xs)
    _ -> getInp (x:xs)