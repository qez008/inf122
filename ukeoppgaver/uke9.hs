{-   

   ___   _  _______  ______    ______    _______  ___   _  _______  __   __  _______  _______ 
  |   | | ||       ||    _ |  |    _ |  |       ||   | | ||       ||  | |  ||       ||       |
  |   |_| ||   _   ||   | ||  |   | ||  |    ___||   |_| ||_     _||  |_|  ||    ___||_     _|
  |      _||  | |  ||   |_||_ |   |_||_ |   |___ |      _|  |   |  |       ||   |___   |   |  
  |     |_ |  |_|  ||    __  ||    __  ||    ___||     |_   |   |  |       ||    ___|  |   |  
  |    _  ||       ||   |  | ||   |  | ||   |___ |    _  |  |   |  |   _   ||   |___   |   |  
  |___| |_||_______||___|  |_||___|  |_||_______||___| |_|  |___|  |__| |__||_______|  |___| 

-}

module Uke9 where
  -- A):
  -- Bevis, ved induksjon på trærne at
  --   1. fmap id = id
  --   2. fmap (f . g) = (fmap f) . (fmap g)

  -- data Tree1 = Leaf1 Int | Node1 Tree1 Tree1
  
  -- blad :: Tree1 -> Int
  -- blad (Leaf1 _) = 1                  -- b1
  -- blad (Node1 l r) = blad l + blad r  -- b2

  -- inode :: Tree1 -> Int
  -- inode (Leaf1 _) = 0                       -- n1
  -- inode (Node1 l r) = inode l + inode r + 1 -- n2

  -- blad t = 1 + inode t -- for all t :: Tree1
  
  {-  Bevis:

        t = Leaf _ : blad (Leaf _) = 1 = 1 + 0 = inode (Leaf _)
        (blad t1) = 1 + inode (t1)
        (blad t2) = 1 + inode (t2)
        blad (Node t1 t2) = (blad t1) + (blad t2) = inode t1 + 1 + inode t2 + 1 = inode t1 + inode t2 + 2
        = inode (Node t1 t2) + 1

    -}

  {- B:

    Bevis at for enhver ikke-tom liste ls@[x1,x2..xn] holder likheten:
      foldr f v ls = f x1 (f x2 (..(f xn)..))

    f1: foldr f v [] = v
    f2: foldr f v (x1:(x2..xn)) = f x1 (foldr f v [x2..xn])
    ih: foldr f v [x2..xn] = f x2 (..(f xn v)..)

    Basis:

      foldr f v [x] =f2= f x (foldr f v []) =f1= (f x v)
      foldr f v [x1,x2..xn] =f2= f x1 (foldr f v [x2..xn]) =ih= f x1 (f x2 (..(f xn v)..))

  -}
  {- C1. Skriv en rekursiv funksjon bsum :: Fractional t => Int -> t, som regner ut 
     summen av alle brøkene på formen 1/(n*(n+1)) for alle n fom. 1 tom. det aktuelle 
     argumentet (som antas >= 1), f.eks.,

         bsum 1 = 1/(1*2) = 0.5
         bsum 3 = 1/(1*2) + 1/(2*3) + 1/(3*4) = 0.75                                     
  -}
  bsum :: Fractional t => Int -> t
  bsum 1 = 1/(1*(2))
  bsum x = 1/(n) + bsum (x-1)
    where n = fromIntegral $ x * (x+1) -- had to use fromIntegral to avoid type error??
  
  -- C4. Bruk C3 for å implementere bsum på en enkel ikke-rekursiv måte
  bsum' :: Fractional t => Int -> t
  bsum' x = sum $ take x [1/(n*(n+1)) | n <- map fromInteger [1,2..]]
  -- again, fromInteger solved type error. why?

  -- D. Definer en parametrisert dataype av binære trær:      
  data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving Show

  -- og en funksjon 
  fmap' :: (a -> b) -> Tree a -> Tree b
  -- ved
  fmap' f (Leaf x) = Leaf (f x)
  fmap' f (Node ve x ho) = Node (fmap' f ve) (f x) (fmap' f ho)