module Main where

import Rules.Verifier
import TRS.Bind
import TRS.TRS
import TRS.Traced
import Rules.Core
import qualified Epic.Print as P
import Control.Monad (forM_, when)
import Prelude hiding (succ, sum)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  --sequence_ [ verify True e | ("ex4",e,_) <- tests ]
  _ <- runTests
  return ()
--------------------------------------------------------------------------------

sys :: TRSystem Expr
sys = icfpVerifier

verify :: Bool -> Expr -> IO Bool
verify b e =
  do when b $
       do putStr (unlines (showTrace tr))
          if done
            then putStrLn "+++ done; no ASSERT left +++"
            else putStrLn "*** NOT done; some ASSERT left! ***"
     return done
 where
  norms           = normalFormFuelTracePlain sys (-1) e
  tr@(x :<-- _):_ = nrDone norms ++ nrLeft norms
  done            = isDone x

isDone :: Expr -> Bool
isDone = collect done (&&)
 where
  done (Assert _) = False
  done _          = True

--------------------------------------------------------------------------------
-- | Top-level function for running the verifier.
--------------------------------------------------------------------------------

runTests :: IO Bool
runTests = and <$> mapM runTest tests

runTest :: (String, Expr, Bool) -> IO Bool
runTest (testName, e, expected) = do
  putStr $ "Running test: " ++ testName ++ " ..."
  res <- verify False e
  let ok = res == expected
  putStrLn $ if ok then show ok else "***FALSE***"
  return ok

isSafe :: Result -> Bool
isSafe Accept = True
isSafe _    = False

testAbs :: Expr -> IO ()
testAbs = test icfpVerifier

testConc :: Expr -> IO ()
testConc = test icfpActual

test :: TRSystem Expr -> Expr -> IO ()
test v = P.pp . run v

showStepS :: Expr -> IO ()
showStepS e = do
  forM_ (stepS icfpVerifier e) $ \e' -> do
    putStrLn (P.prettyShow e')

run :: TRSystem Expr -> Expr -> Traced Expr
run v e = head (nrDone nf)
  where
    nf = normalFormFuelTracePlain v 1000 e

data Result = Accept | Reject
  deriving (Show)

instance P.Pretty Result where
  pPrint Accept = P.text "accept"
  pPrint Reject = P.text "reject"

---------------------------------------------------------------------------------------------------
-- | Verifier tests
---------------------------------------------------------------------------------------------------

tests :: [(String, Expr, Bool)]
tests =
  [ ("ex00", ex00, True)
  , ("ex0", ex0, True)
  , ("ex0'", ex0', False)
  , ("ex1", ex1, True)
  , ("ex2", ex2, True)
  , ("ex2'", ex2', False)
  , ("ex3", ex3, True)
  , ("ex4", ex4, True)
  , ("ex5", ex5, True)
  , ("ex6", ex6, True)
  ]

-------------------------------------------------------------------------------------------
iNT :: Expr -> Expr
iNT e = INT e :>: e

lET :: Ident -> Expr -> Expr -> Expr
lET x e1 e2 = EXI x ((Var x :=: e1) :>: e2)

lETs :: [(Ident, Expr)] -> Expr -> Expr
lETs xes e = foldr (\(x, e1) e2 -> lET x e1 e2) e xes

sub :: Expr -> Expr -> Expr
sub e1 e2 = Op Sub :@: Arr [e1, e2]

leq :: Expr -> Expr -> Expr
leq e1 e2 = Op Le :@: Arr [e1, e2]

ite :: Expr -> Expr -> Expr -> Expr
ite e1 e2 e3 = (Assume e1 :>: e2) :|: e3

exis :: [Ident] -> Expr -> Expr
exis ys e = foldr ((Exi .) . Bind) e ys

tlam :: Ident -> [Ident] -> Expr -> Expr -> Expr
tlam x ys e1 e2 =
      Verify (Val $ Lam $ Bind x $ exis ys (Assume e1 :>: Assert e2))
  :>: (Val $ Lam $ Bind x $ exis ys (e1 :>: Assume e2))

tlamAbs :: Ident -> [Ident] -> Expr -> Expr -> Expr
tlamAbs x ys e1 e2 =
      (Val $ Lam $ Bind x $ exis ys (e1 :>: Assume e2))


-------------------------------------------------------------------------------------------

ex00 :: Expr
ex00 = Assert (Int 2 :=: Int 2 :>: Int 2)

--  forall x. int[x] => forall y.  int[y] => forall z. int[z] => x=y => succeeds{ exists a b. a=x; b=a; b=y}
ex0 :: Expr
ex0 = LAM x (LAM y (LAM z (
        Assume (INT (Var x) :>: INT (Var y) :>: INT (Var z) :>: Var x :=: Var y)
        :>:
        Assert (EXI a $ EXI b $ (Var a :=: Var x) :>: Var b :=: Var a :>: Var b :=: Var y :>: Int 0)
      )))
  where
    x = ident "x"
    y = ident "y"
    z = ident "z"
    a = ident "a"
    b = ident "b"

--  forall x. int[x] => forall y.  int[y] => forall z. int[z] => x=z => succeeds{ exists a b. a=x; b=a; b=y}
ex0' :: Expr
ex0' = LAM x (LAM y (LAM z (
        Assume (INT (Var x) :>: INT (Var y) :>: INT (Var z) :>: Var x :=: Var z)
        :>:
        Assert (EXI a $ EXI b $ (Var a :=: Var x) :>: Var b :=: Var a :>: Var b :=: Var y :>: Int 0)
      )))
  where
    x = ident "x"
    y = ident "y"
    z = ident "z"
    a = ident "a"
    b = ident "b"

-------------------------------------------------------------------------------------------
-- ex1' (andy's variant with x = suc x)
{-
exi suc.
  suc = \a. INT[a]; assume { EXI b. INT[b]; b }
  (\v. assume {int[x]} ; assert { exi r. r = succ(x); INT[r]; r }
-}
ex1 ::Expr
ex1 = lET succ (LAM a (iNT (Var a) :>: Assume (EXI b (iNT (Var b)))))
        (LAM x (Assume (iNT (Var x)) :>: Assert (EXI r (Var r :=: Var succ :@: Var x :>: iNT (Var r) ))))
  where
    succ = ident "succ"
    a    = ident "a"
    b    = ident "b"
    x    = ident "x"
    r    = ident "r"

-------------------------------------------------------------------------------------------
-- f = \x. assume{x = 3}; assert{ exi r. r = (x = 3; 3); r }

ex2 :: Expr
ex2 = LAM x (Assume (Var x :=: Int 3) :>: Assert (EXI r (Var r :=: (Var x :=: Int 3 :>: Int 3) :>: Var r)))
  where
    x = ident "x"
    r = ident "r"

-- f = \x. assume{x = 3}; assert{ exi r. r = (x = 3; 3); r }
ex2' :: Expr
ex2' = LAM x (Assume (Var x :=: Int 3) :>: Assert (EXI r (Var r :=: (Var x :=: Int 4 :>: Int 4) :>: Var r)))
  where
    x = ident "x"
    r = ident "r"

-------------------------------------------------------------------------------------------
{-

f(x:FOO):FOO = 708 - x

FOO(x) = (x = 666 | x = 42); x
-}
-- f = \v. exi x. assume{x = FOO(v)}; assert{exi r. r = 708 - x; FOO(r)}

ex3 :: Expr
ex3 = EXI foo $ (Var foo :=: LAM y (((Var y :=: Int 666) :|: (Var y :=: Int 42)) :>: Var y)) :>:
        LAM v (One {- to force SX/CX -}
                (EXI x $ Assume ((Var x :=: Var foo :@: Var v) :>: Var x) :>:
                        Assert (EXI r (Var r :=: (Int 708 `sub` Var x) :>: (Var foo :@: Var r)))))
  where
    foo = ident "foo"
    x = ident "x"
    v = ident "v"
    r = ident "r"
    y = ident "y"

-------------------------------------------------------------------------------------------

{-
PHIL: this is not valid as the verifier should reject a type test on 'any' ; instead this should be 'comparable'

x should be 'comparable' not 'any'

sum(x:any):int := if nat(x) then add(x, sum(dec(x))) else 0
  where
    nat(x:any) := int(x); 0<=x; x
    dec(x:int):int
    add(x:int, y:int):int
-}
ex4 :: Expr
ex4 =  lETs
          [ (nat, LAM x (iNT (Var x) :>: leq (Int 0) (Var x) :>: Var x) )
          , (add, LAM x (LAM y (iNT (Var x) :>: iNT (Var y) :>: Assume (EXI r (iNT (Var r))))))
          , (dec, LAM x (iNT (Var x) :>: Assume (EXI r (iNT (Var r)))))
          , (sum, LAM x (Assume (EXI r (iNT (Var r)))))
          ]
          (LAM x (Assert (EXI r ((Var r :=: ite (Var nat :@: Var x)
                                              (Assert (lETs
                                                        [ (t0, Var dec :@: Var x)
                                                        , (t1, Var sum :@: Var t0)
                                                        ]
                                                        ((Var add :@: Var x) :@: Var t1)
                                                      ))
                                              (Int 0))
                                  :>: iNT (Var r)))))
  where
    nat = ident "nat"
    dec = ident "dec"
    add = ident "add"
    sum = ident "sum"
    x   = ident "x"
    r   = ident "r"
    y   = ident "y"
    t0  = ident "t0"
    t1  = ident "t1"


{-
add x (sum (dec x))

let
  t0 = dec x
  t1 = sum t0
in
  add x t1

-}


{-

assert {ex r. assume {isint(x)};
              assume {le(<0, x>)};
              (r = assert {ex t0. ex t1.
                            isint(x);
                            (t0 = assume {ex r. isint(r); r});
                            (t1 = assume {ex r. isint(r); r});
                            t1});
              isint(r);
        }
-}

----

ex5 :: Expr
ex5 = LAM x (Assert (EXI r ((Var r :=: ite (INT (Var x)) (Int 10) (Int 20)) :>: INT (Var r))))
  where
    x = ident "x"
    r = ident "r"

---

suc = tlamAbs vx [] (iNT x) (Exi (Bind vy (iNT y)))
 where
  vx = ident "x"
  x  = Var vx
  vy = ident "y"
  y  = Var vy

f = tlam vh0 [vh] (h :=: tlam vx0 [vx] (x :=: iNT x0) (Exi (Bind vy (y :=: (h0 :@: x) :>: iNT y))))
                  (Exi (Bind vy (y :=: (h :@: Val (Int 3)) :>: iNT y)))
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

ex6 = Exi (Bind vg (Exi (Bind vs (g :=: f :>: s :=: suc :>: g :@: s))))
 where
  vg = ident "g"
  g  = Var vg
  vs = ident "suc"
  s  = Var vs
