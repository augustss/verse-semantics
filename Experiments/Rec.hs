{-# LANGUAGE RecursiveDo #-}
import Control.Applicative

u x = pure x

x ==. y | x == y = [x]
        | otherwise = []

-- [(1,2),(1,3),(2,2),(2,3)]
ex1 :: [(Int,Int)]
ex1 = mdo
  x <- u 1 <|> u 2
  y <- u 2 <|> u 3
  pure (x,y)

-- [(2,1),(3,2),(3,1),(3,2)]
ex2 :: [(Int,Int)]
ex2 = mdo
  x <- u (y+1) <|> u 3
  y <- u 1 <|> u 2
  pure (x,y)

-- Deadlock
ex3 :: [(Int,Int)]
ex3 = mdo
  x <- [1..y]
  y <- u 1 <|> u 2
  pure (x,y)

-- Deadlock
ex5 :: [(Int,Int)]
ex5 = mdo
  x <- y ==. 4 <|> u 2
  y <- u 3 <|> u 4
  pure (x,y)

