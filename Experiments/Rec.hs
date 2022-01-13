{-# LANGUAGE RecursiveDo #-}
import Control.Applicative

-- u generates a singleton list
u :: a -> [a]
u x = pure x

x ==. y | x == y = [x]
        | otherwise = []

--  <|> is just (++)

{-
class (Monad m) => MonadFix m where
        mfix :: (a -> m a) -> m a

mfixList :: (a -> [a]) -> [a]
mfixList f = let (x:xs) = f x in x:xs

-}

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
-- Tim is happy that this deadlocks
ex3 :: [(Int,Int)]
ex3 = mdo
  x <- [1..y]
  y <- u 1 <|> u 2
  pure (x,y)

-- Deadlock
-- But Tim's impl squeezes through a narrow gap
ex5 :: [(Int,Int)]
ex5 = mdo
  x <- y ==. 4 <|> u 2
       -- (y ==. 4) yields an empty list or singleton,
       --           depending on y
  y <- u 3 <|> u 4
  pure (x,y)

ex7 = mdo
  x <- u 1 <|> u x
  pure x

{- Discussion

Currently:   Env = Var -> V
Could:       Env = Var -> Maybe V


E[[ v ]] rho = case rho(v) of
                 Just v  -> [v]
                 Nothing -> []


Thought expt:       Env = Var -> V*

E[[ v ]] rho = rho(v)

E[[ x+x ]] rho = too many values.
But ok for 0/1!!

Termination

  x := :false; if( loop ) then x else 1
  Does this fail or diverge?  It should fail

  x := :false; print("hello"); ...
  Must fail.


E[[.]] :: Env -> Clo*
Clo = (Env,Expr)

E[[ defrec { x = r } in e ]] rho
  = [  E[[ e ]] rho'
    |  x1 <- eval(
  where
    rho' = rho[ xi :-> <rho', ei> ]


-}
