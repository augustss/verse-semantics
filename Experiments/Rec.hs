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

instance MonadFix [] where
    mfix f = case fix (f . head) of
               []    -> []
               (x:_) -> x : mfix (tail . f)

mfixList :: (a -> [a]) -> [a]
mfixList f = let xs = f (head xs)
             in
             case xs of
               []    -> []
               (x:_) -> x : mfixList (tail . f)

-- Unrolling this shows us what happens:
mfixList f = let xs1 = f (head xs1) in
             case xs1 of {
               []     -> [] ;
               (x1:_) -> x :

             let xs2 = tail (f (head xs2)) in
             case xs2 of {
               []    -> [] ;
               (x2:_) -> x2 :

             let xs3 = tail (tail (f (head xs3))) in
             case xs3 of {
               []    -> [] ;
               (x3:_) -> x3 :

             ....

Another way to write it:

mfixList :: forall a. (a -> [a]) -> [a]
mfixList f = go 0
 where
   go :: Int -> [a]
   go n = let mx :: Maybe a
              mx = indexM n (f (the mx))
          in case mx of
             Nothing -> []
             Just x  -> x : go (n+1)
   -- Assuming result has at least i elements, the i'th element of
   -- the result list is xi where
   --     xi = the (indexM n (f xi))
   -- In each iteration we apply f afresh to a thunk, and ignore
   -- all but the i'th element of the result.

indexM :: Int -> [a] -> Maybe a
-- Get the i'th element, or Nothing if it has too few elements
indexM _ []     = Nothing
indexM 0 (x:_)  = Just x
indexM n (_:xs) = indexM (n-1) xs

the :: Maybe a -> a
the Nothing  = error "Deadlock"
the (Just x) = x
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
