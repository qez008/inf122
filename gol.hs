-- Game of Life --

{-  
  Implementation from the book (p 135 -137) 
  with some improvements like use of delayThread instead of some work func 
-}

module GameOfLife where 
  import System.Console.ANSI
  import Control.Concurrent

  begin :: IO ()
  begin = life glider

  -- some test methods
  drawFrame :: Int -> Int -> Pos -> IO ()
  drawFrame h w (x, y) = do
    clearScreen
    drawHL (x, y) (w+1)
    drawHL (x+h, y) (w+1)
    drawVL (x, y) (h -1)
    drawVL (x, y+w) (h-1)
    putStrLn "\n\n"
    where
      draw :: IO ()
      draw = do
        clearScreen
        putStr "\ESC[3;2H"
        putStr "test"
        putStr "\ESC[3;2H"
        putStrLn "12"
      
      drawHL :: Pos -> Int -> IO () -- draw horizontal line
      drawHL p n = do
        goto p
        -- dots:
        let dottedLine 0 = []
            dottedLine x = if even x then ' ':dottedLine (x-1) else '*':dottedLine (x-1)
         in putStrLn (dottedLine n)
        -- no dots: putStr (replicate n '-')
      
      drawVL :: Pos -> Int -> IO () -- draw vertical lines
      drawVL _  0 = putStr ""
      drawVL (x, y) n = do
        goto (x+1, y)
        putStr "*" -- or "|"
        drawVL (x+1, y) (n-1)


  frame :: IO ()
  frame = drawFrame 25 50 (2,3)
  -- end test methods

  -- from the book
  type Pos = (Int, Int)
  
  goto :: Pos -> IO ()
  goto (x,y) = putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "H")

  width :: Int
  width = 26

  height :: Int
  height = 53

  writeat :: Pos -> String -> IO ()
  writeat (x,y) str = do
    goto (x+2,y+3)
    putStr str

  type Board = [Pos]

  glider :: Board
  glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

  showCells :: Board -> IO ()
  showCells b = sequence_ [writeat p "O" | p <- b]

  isAlive :: Board -> Pos -> Bool
  isAlive b p = elem p b

  isEmpty :: Board -> Pos -> Bool
  isEmpty b p = not $ isAlive b p

  neighbs :: Pos -> [Pos]
  neighbs (x,y) = map wrap [(x+1, y), (x+1, y+1), (x, y+1), (x-1, y+1), 
                            (x-1, y), (x-1, y-1), (x, y-1), (x+1, y-1)]
    where
      wrap :: Pos -> Pos
      wrap (x,y) = (((x-1) `mod` width) +1,
                   ((y-1) `mod` height) +1)

  liveNeighbs :: Board -> Pos -> Int
  liveNeighbs b = length . filter (isAlive b) . neighbs

  survivors :: Board -> [Pos]
  survivors b = [p | p <- b, elem (liveNeighbs b p) [2, 3]]

  births :: Board -> [Pos]
  births b = [(x,y) | x <- [1..width],
                      y <- [1..height],
                      isEmpty b (x,y),
                      liveNeighbs b (x,y) == 3]

  rmdups :: Eq a => [a] -> [a]
  rmdups [] = []
  rmdups (x:xs) = x : rmdups (filter (/= x) xs)

  nextGen :: Board -> Board
  nextGen b = survivors b ++ births b

  life :: Board -> IO ()
  life b = do 
    drawFrame (width +1) (height+1) (2,3)
    showCells b
    threadDelay 5000
    life (nextGen b)

  wait :: Int -> IO ()
  wait n = sequence_ [return () | _ <- [1..n]]