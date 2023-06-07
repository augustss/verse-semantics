module Main where

import Rules.Verifier
import TRS.Bind
import TRS.TRS
import TRS.Traced
import Rules.Core
import qualified Epic.Print as P

import qualified TRSVerifier.Verifier as R

--------------------------------------------------------------------------------

sys = trivVerifier

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
verify e =
  do x <- trace (head (nrDone (normalFormFuelTracePlain sys (-1) e)))
     if collect done (&&) x
       then putStrLn "+++ done; no ASSERT or VERIFY left +++"
       else putStrLn "*** NOT done; some ASSERT or VERIFY left! ***"
 where
  done (Assert _) = False
  done (Verify _) = False
  done _          = True
  
  trace (x :<-- []) =
    do P.pp x
       return x
  
  trace (x :<-- ((r,y):rys)) =
    do trace (y :<-- rys)
       putStrLn ("--[" ++ r ++ "]-->")
       P.pp x
       return x

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
main = sequence_ [ verify e | ("ex4",e,_) <- R.tests ]

--------------------------------------------------------------------------------

