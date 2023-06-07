module Main where

import Rules.Verifier
import TRS.Bind
import TRS.TRS
import TRS.Traced
import Rules.Core
import qualified Epic.Print as P

--------------------------------------------------------------------------------

sys = icfpVerifier

--------------------------------------------------------------------------------

exis :: [Ident] -> Expr -> Expr
exis ys e = foldr ((Exi .) . Bind) e ys

tlam :: Ident -> [Ident] -> Expr -> Expr -> Expr
tlam x ys e1 e2 =
      Verify (Val $ Lam $ Bind x $ exis ys (Assume e1 :>: Assert e2))
  :>: (Val $ Lam $ Bind x $ exis ys (e1 :>: Assume e2))

tlamAbs :: Ident -> [Ident] -> Expr -> Expr -> Expr
tlamAbs x ys e1 e2 =
      (Val $ Lam $ Bind x $ exis ys (e1 :>: Assume e2))


--------------------------------------------------------------------------------

verify :: Expr -> IO ()
verify e = trace (head (nrDone (normalFormFuelTracePlain sys (-1) e)))
 where
  trace (x :<-- []) =
    do P.pp x
  
  trace (x :<-- ((r,y):rys)) =
    do trace (y :<-- rys)
       putStrLn ("--" ++ show r ++ "-->")
       P.pp x

--------------------------------------------------------------------------------

isInt x = Op IsInt :@: x

int x = isInt x :>: x

suc = tlamAbs vx [] (int x) (Exi (Bind vy (int y)))
 where
  vx = ident "x"
  x  = Var vx
  vy = ident "y"
  y  = Var vy

f = tlam vh0 [vh] (h :=: tlam vx0 [vx] (x :=: int x0) (Exi (Bind vy (y :=: (h0 :@: x) :>: int y))))
                  (Exi (Bind vy (y :=: (h :@: Val (Int 3)) :>: int y)))
 where
  vh0 = ident "h0"
  h0  = Var vh0
  vh  = ident "h"
  h   = Var vh
  vx  = ident "x"
  x   = Var vx
  vx0 = ident "x0"
  x0  = Var vx0
  vy  = ident "y"
  y   = Var vy

e = Exi (Bind vg (Exi (Bind vs (g :=: f :>: s :=: suc :>: g :@: s))))
 where
  vg = ident "g"
  g  = Var vg
  vs = ident "suc"
  s  = Var vs
  
main :: IO ()
main = verify e

--------------------------------------------------------------------------------

