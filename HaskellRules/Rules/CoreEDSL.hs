module Rules.CoreEDSL where

import Rules.Core hiding (def)
import TRS.Bind

--------------------------------------------------------------------------------

data Verse a =
  V (String -> [Ident] -> ([Ident], [Expr], a, String))

-- V (\i vs -> (ws, es, a, j)) means:
-- given a suggested variable name i
--   given the variables in scope vs
--     generate the program
--       Exi ws . es ; [ a ]
-- and suggest variable name j after this

instance Functor Verse where
  fmap f (V m) =
    V (\i vs -> let (ws, es, x, j) = m i vs
                 in (ws, es, f x, j))

instance Applicative Verse where
  pure x =
    V (\i _ -> ([], [], x, i))

  V mf <*> V mx =
    V (\i vs -> let (ws1, es1, f, j1) = mf i vs
                    (ws2, es2, x, j2) = mx j1 (vs++ws1)
                 in (ws1++ws2, es1++es2, f x, j2))

instance Monad Verse where
  return = pure

  V m >>= k =
    V (\i vs -> let (ws1, es1, x, j1) = m i vs
                    V m'              = k x
                    (ws2, es2, y, j2) = m' j1 (vs++ws1)
                 in (ws1++ws2, es1++es2, y, j2))

--------------------------------------------------------------------------------
-- primitives

(<?) :: Verse a -> String -> Verse a
V m <? i = V (\_ vs -> m i vs)

suggested :: Verse String
suggested = V (\i _ -> ([], [], i, "_"))

visible :: Verse [Ident]
visible = V (\i vs -> ([], [], vs, i))

new :: Verse Ident
new =
  do i <- suggested
     vs <- visible
     let xs = map ident ([ i | i /= "_"] ++ [ i ++ show k | k <- [(1::Int)..] ])
         v  = head (filter (not . (`elem` vs)) xs)
     V (\j _ -> ([v], [], v, j))

splice :: Expr -> Verse ()
splice e = V (\i _ -> ([], [e], (), i))

code :: Verse a -> Verse ([Expr], a)
code (V m) = V (\i vs -> let (ws, es, x, j) = m i vs
                          in (ws, [], (es,x), j))

scope :: Verse a -> Verse ([Ident], a)
scope (V m) = V (\i vs -> let (ws, es, x, j) = m i vs
                           in ([], es, (ws,x), j))

--------------------------------------------------------------------------------
-- helper to convert things into programs

class Program a where
  program :: a -> Verse Expr

instance Program Expr where
  program = return

instance Program a => Program (Verse a) where
  program m = m >>= program

-- compose the program with the existentials at the right place

block :: Program a => a -> Verse Expr
block a =
  do (ws,(es,e)) <- scope (code (program a))
     return (foldr ((Exi .) . Bind) (foldr (:>:) e es) ws)

-- existentials

exists :: Verse Expr
exists = Var <$> new

-- choice

(.|.) :: (Program a, Program b) => a -> b -> Verse Expr
a .|. b =
  do e1 <- block a
     e2 <- block b
     def (e1 :|: e2) <? "p"

(.@.) :: (Program a, Program b) => a -> b -> Verse Expr
f .@. x =
  do ff <- def f <? "f"
     xx <- def x <? "x"
     def (ff :@: xx)

-- equality

(.=.) :: (Program a, Program b) => a -> b -> Verse Expr
a .=. b =
  do e1 <- program a
     e2 <- program b
     if isVal e1 then
       do splice (e1 :=: e2)
          return e1
      else if isVal e2 then
       do splice (e2 :=: e1)
          return e2
      else
       do x <- def e1 <? "x"
          splice (x :=: e2)
          return x

-- definitions

def :: Program a => a -> Verse Expr
def a =
  do v <- exists
     e <- program a
     if isVal e then
       do return e
      else
       do (v :: Expr) .=. e

lam :: Program a => (Expr -> a) -> Verse Expr
lam body =
  do v <- new
     e <- block (body (Var v))
     def (Lam (Bind v e)) <? "f"

timlam :: Program a => (Expr -> Verse a) -> Verse Expr
timlam pbody =
  do v <- suggested
     def (do verify $
               lam (\x -> do (pres, post) <- code (pbody x)
                             _ <- assume (foldr (:>:) (Arr []) pres)
                             assert post) <? v
             lam (\x -> do post <- pbody x
                           assume post) <? v) <? "t"

assert :: Program a => a -> Verse Expr
assert a =
  do e <- block a
     def (Assert e) <? "a"

assume :: Program a => a -> Verse Expr
assume a =
  do e <- block a
     def (Assume e) <? "a"

verify :: Program a => a -> Verse ()
verify a =
  do e <- block a
     splice (Verify [] [] e)

-- primitives

int :: Program a => a -> Verse Expr
int a =
  do v <- def a
     splice (Op IsInt :@: v)
     return v

-- run function

verse :: Program a => a -> Expr
verse a = let V m           = block a
              (_ , _, e, _) = m "_" []
           in e
