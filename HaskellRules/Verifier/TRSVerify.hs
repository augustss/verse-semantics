{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where

import Rules.Verifier
import TRS.Bind
import TRS.TRS
import TRS.Traced
import Rules.Core
import Rules.CoreEDSL
import qualified Epic.Print as P
import Prelude hiding (succ, sum)

--------------------------------------------------------------------------------

main :: IO ()
main =
  do _ <- runTests
     return ()

---------------------------------------------------------------------------------------------------
-- | Verifier tests
---------------------------------------------------------------------------------------------------

tests :: [(String, Expr, Bool)]
tests =
  [ ("ex00", ex00, True)
  , ("ex01", ex01, True)
  , ("ex0", ex0, True)
  , ("ex0'", ex0', False)
  , ("ex1", ex1, True)
  , ("ex2", ex2, True)
  , ("ex2'", ex2', False)
  , ("ex3", ex3, True)
  , ("ex4", ex4, True)
  , ("ex5", ex5, True)
  , ("ex6", ex6, True)
  , ("ex_rigid2flex", ex_rigid2flex, True)
  , ("ex_flex2rigid1", ex_flex2rigid1, False)
  , ("ex_flex2rigid2", ex_flex2rigid2, True)
  , ("ex_stuck1", ex_stuck1, False)
  , ("ex_stuck2", ex_stuck2, False)
  , ("ex_stuck3", ex_stuck3, False)
  , ("ex_if0", ex_if0, True)
  , ("ex_if1", ex_if1, False)
  , ("ex_if2", ex_if2, True)
  , ("ex_tim_0", ex_tim_0, False)
  , ("ex_tim_1", ex_tim_1, False)
  , ("ex_asm_subst", ex_asm_subst, True)
  ]

--------------------------------------------------------------------------------
-- | Top-level function for running the verifier.
--------------------------------------------------------------------------------

sys :: TRSystem Expr
sys = icfpVerifier

runTests :: IO Bool
runTests = and <$> mapM runTest tests

runTest :: (String, Expr, Bool) -> IO Bool
runTest (testName, e, expected) =
  do putStr $ "Running test: " ++ testName ++ " ..."
     --P.pp e
     case Rules.Verifier.verify icfpeVerifier e of
       (True, _) | expected ->
         do putStrLn " OK (verified)"
            return True

       (False, _) | not expected ->
         do putStrLn " OK (failed)"
            return True

       (True, _tr@(x :<-- _)) ->
         do putStrLn " *** VERIFIED, but expected FAILED:"
            putStr (unlines (showTrace _tr))
            P.pp x
            return False

       (False, _tr@(x :<-- _)) ->
         do putStrLn " *** FAILED, but expected VERIFIED:"
            putStr (unlines (showTrace _tr))
            P.pp x
            return False

-------------------------------------------------------------------------------------------



eXIs :: [Ident] -> Expr -> Expr
eXIs xs e = foldr EXI e xs

lAMs :: [Ident] -> Expr -> Expr
lAMs xs e = foldr LAM e xs

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
ite = If
-- ite e1 e2 e3 = (Assume e1 :>: e2) :|: e3

--ite e1 e2 e3 = One( (e1 :>: Lam (Bind x e2)) :|: Lam (Bind x e3) ) :@: Arr []
-- where
--  x = identNotIn (free (e2,e3))
--ite e1 e2 e3 = Exi (Bind x ( Var x :=: One( (e1 :>: Arr []) :|: Int 0 )
--                         :>: (((Var x :=: Arr []) :>: e2) :|: ((Var x :=: Int 0) :>: e3))
--                           ))
-- where
--  x = identNotIn (free (e2,e3))

tlamOblig :: Ident -> [Ident] -> Expr -> Expr -> Expr
tlamOblig x ys e1 e2 = Verify (Lam $ Bind x $ exis ys (Assume e1 :>: Assert e2))

tlamAbs :: Ident -> [Ident] -> Expr -> Expr -> Expr
tlamAbs x ys e1 e2 = Lam $ Bind x $ exis ys (e1 :>: Assume e2)


tlam :: Ident -> [Ident] -> Expr -> Expr -> Expr
tlam x ys e1 e2 = tlamOblig x ys e1 e2 :>: tlamAbs x ys e1 e2

{-

ex00 / True
g() := (2 = 2; 2)

ex01 / True
g(x:int) := x

ex02 / True
g(x:int):int := x

ex0 / True
g(x:int, y:int where x = y) := { a := x; b := a; b = y }

ex0' / True
:verify g(x:int, y:int) := { a := x; b := a; b = y }

ex0 / True (ideally but this hangs!)
:verify g(x:int, y:int, z:int where x = y) := { a := x; b := a; b = y }

ex0' / False (ideally but this hangs!)
:verify g(x:int, y:int, z:int where x = z) := { a := x; b := a; b = y }

ex5 /True
:verify g(x:any):int := { if int[x] then 10 else 20 }

ex5' / False (note: int(x) means call should succeed which, in this case, may not happen!)
:verify g(x:any):int := { if int(x) then 10 else 20 }

> :verify g(x:int, y:int) := { a := x; b := a; b = y }
g(x : int, y : int) := {a := x; b := a; b = y}
Desugared:
ex g $i1.
   (g = (($i1 = (verify {\$x2.
                         ex $x3 $x4 x y.
                            assume {(x = (\$$x. isint($$x); $$x)($x3));
                                    (y = (\$$x. isint($$x); $$x)($x4));
                                    ($x2 = <$x3, $x4>);
                                    $x2};
                            assert {ex a b. (a = x); (b = a); (b = y); b}};
                 (\$x2.
                  ex $x3 $x4 x y.
                     ((x = (\$$x. isint($$x); $$x)($x3));
                      (y = (\$$x. isint($$x); $$x)($x4));
                      ($x2 = <$x3, $x4>);
                      $x2);
                     assume {ex a b. (a = x); (b = a); (b = y); b})));
         $i1));
   g


> :verify g(x:int, y:int, z:int) := { a := x; b := a; b = y }
g(x : int, y : int, z : int) := {a := x; b := a; b = y}
Desugared:
ex g $i1.
   (g = (($i1 = (verify {\$x2.
                         ex $x3 $x4 $x5 x y z.
                            assume {(x = (\$$x. isint($$x); $$x)($x3));
                                    (y = (\$$x. isint($$x); $$x)($x4));
                                    (z = (\$$x. isint($$x); $$x)($x5));
                                    ($x2 = <$x3, $x4, $x5>);
                                    $x2};
                            assert {ex a b. (a = x); (b = a); (b = y); b}};
                 (\$x2.
                  ex $x3 $x4 $x5 x y z.
                     ((x = (\$$x. isint($$x); $$x)($x3));
                      (y = (\$$x. isint($$x); $$x)($x4));
                      (z = (\$$x. isint($$x); $$x)($x5));
                      ($x2 = <$x3, $x4, $x5>);
                      $x2);
                     assume {ex a b. (a = x); (b = a); (b = y); b})));
         $i1));
   g


> :verify g(x:int, y:int where x = y) := { a := x; b := a; b = y }
g(x : int, y : int where x = y) := {a := x; b := a; b = y}
Desugared:
ex g $i2.
   (g = (($i2 = (verify {\$x3.
                         ex $x4 $x5 x $x1 y $i6.
                            assume {(x = (\$$x. isint($$x); $$x)($x4));
                                    ($x1 = ((y = (\$$x. isint($$x); $$x)($i6)); y));
                                    (x = y);
                                    ($x5 = $x1);
                                    ($x3 = <$x4, $x5>);
                                    $x3};
                            assert {ex a b. (a = x); (b = a); (b = y); b}};
                 (\$x3.
                  ex $x4 $x5 x $x1 y $i6.
                     ((x = (\$$x. isint($$x); $$x)($x4));
                      ($x1 = ((y = (\$$x. isint($$x); $$x)($i6)); y));
                      (x = y);
                      ($x5 = $x1);
                      ($x3 = <$x4, $x5>);
                      $x3);
                     assume {ex a b. (a = x); (b = a); (b = y); b})));
         $i2));
   g
Verified

-}

exNotValid :: Expr
exNotValid = eXIs [g, i1] $ (Var g :=: (blob1 :>: blob2) :>: Var i1) :>: Var g
    where
     g = ident "g"
     i1 = ident "i1"
--     x2 = ident "x2"
--     x = ident "x"


blob1 :: Expr
blob1 = Var i1 :=: (Verify (LAM x2 ( EXI x ((Assume ((Var x :=: iNT(Var x2)) :>: Var x) :>: Assert (Var x))))))
  where
    i1 = ident "i1"
    x2 = ident "x2"
    x = ident "x"

blob2 :: Expr
blob2 = LAM x2 (EXI x ((Var x :=: iNT(Var x2)) :>: Assume (Var x)))
  where
    x2 = ident "x2"
    x  = ident "x"

-------------------------------------------------------------------------------------------
ex00 :: Expr
ex00 = Assert (Int 2 :=: Int 2 :>: Int 2)

-------------------------------------------------------------------------------------------
-- :verify g(x:int) := x
ex01 :: Expr
ex01 = verse $ lam (\x -> Assert x)


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


{-
"hanging" desugared version of

g(x:int, y:int, z:int) := { a := x; b := a; b = y }

\xt.
    ex $x3 $x4 $x5 x y z.
      assume {(x  = (\$$x. isint($$x); $$x)($x3));
              (y  = (\$$x. isint($$x); $$x)($x4));
              (z  = (\$$x. isint($$x); $$x)($x5));
              (xt = <$x3, $x4, $x5>);
              xt};
      assert {ex a b. (a = x); (b = a); (b = y); b}

-}

-- hangs the icfpVerifier due to EXI-SWAP blowup, but not icfpeVerifier
ex00_hang :: Expr
ex00_hang =
  LAM xt $
    eXIs [x3, x4, x5, x, y, z] $
      Assume (
              (Var x  :=: iNT (Var x3)) :>:
              (Var y  :=: iNT (Var x4)) :>:
              (Var z  :=: iNT (Var x5)) :>:
              (Var xt :=: Arr [Var x3, Var x4, Var x5]) :>:
              Var xt
             )
      :>: Assert (Int 0)
      -- :>: Assert (eXIs [a, b] ( (Var a :=: Var x) :>:  (Var b :=: Var a) :>: (Var b :=: Var y) :>: Var b))
  where
    x3 = ident "x3"
    x4 = ident "x4"
    x5 = ident "x5"
    x  = ident "x"
    y  = ident "y"
    z  = ident "z"
    -- a  = ident "a"
    -- b  = ident "b"
    xt = ident "xt"

ex01_hang :: Expr
ex01_hang =
  lAMs [x3, x4, x5] $
    eXIs [x, y, z] $
      Assume (
              (Var x  :=: iNT (Var x3)) :>:
              (Var y  :=: iNT (Var x4)) :>:
              (Var z  :=: iNT (Var x5)) :>:
              Arr []
              -- (Var xt :=: Arr [Var x3, Var x4, Var x5]) :>:
              -- Var xt
             )
      :>: Assert (eXIs [a, b] ((Var a :=: Var x) :>: (Var b :=: Var a) :>: (Var b :=: Var y) :>: Var b))
  where
    x3 = ident "x3"
    x4 = ident "x4"
    x5 = ident "x5"
    x  = ident "x"
    y  = ident "y"
    z  = ident "z"
    a  = ident "a"
    b  = ident "b"
    -- xt = ident "xt"


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

-- TODO:PORT
ex3 :: Expr
ex3 = Verify $
      EXI foo $ (Var foo :=: LAM y (((Var y :=: Int 666) :|: (Var y :=: Int 42)) :>: Var y)) :>:
        LAM v ({- to force SX/CX -}
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
          (Verify (LAM x (Assert (EXI r ((Var r :=: ite (Var nat :@: Var x)
                                              (Assert (lETs
                                                        [ (t0, Var dec :@: Var x)
                                                        , (t1, Var sum :@: Var t0)
                                                        ]
                                                        ((Var add :@: Var x) :@: Var t1)
                                                      ))
                                              (Int 0))
                                  :>: iNT (Var r))))))
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


----

ex5 :: Expr
ex5 = Verify $ LAM x (Assert (EXI r ((Var r :=: ite (INT (Var x)) (Int 10) (Int 20)) :>: INT (Var r))))
  where
    x = ident "x"
    r = ident "r"

---

ex6 :: Expr
ex6 = verse $
  do suc <- def (lam (\x -> do _ <- int x
                               assume $ do y <- exists <? "y"
                                           int y) <? "x") <? "suc"
     g   <- def (timlam (\h ->
                  do h' <- timlam (\x ->
                             do x' <- int x
                                return $ do y <- def (h :@: x') <? "y"
                                            int y)
                     return $
                       do y <- def (h' :@: Int 3) <? "y"
                          int y) <? "h") <? "g"
     -- return (Arr [g, suc])
     return (g :@: suc)

--- examples testing rigid/flexible ---


ex_rigid2flex :: Expr
ex_rigid2flex = verse $
  timlam $ \x ->
    do x' <- int x
       return $
         do y <- exists
            x' .=. y

-- TODO:PORT
ex_flex2rigid1 :: Expr
ex_flex2rigid1 = verse $
  timlam $ \x ->
    do x' <- int x
       return $
         do x' .=. Int 3

ex_flex2rigid2 :: Expr
ex_flex2rigid2 = verse $
  timlam $ \x ->
    do x' <- int x
       x' .=. Int 3
       return $
         do x' .=. Int 3

--- examples testing getting stuck ---

ex_stuck1 :: Expr
ex_stuck1 = verse $
  timlam $ \_x ->
    do return (exists <? "y")

ex_stuck2 :: Expr
ex_stuck2 = verse $
  timlam $ \x ->
    do return (do y <- exists <? "y"
                  z <- def (Arr [x,y]) <? "z"
                  y .=. Arr [x,z])

ex_stuck3 :: Expr
ex_stuck3 = verse $
  timlam $ \_x ->
    do return (do y <- exists <? "y"
                  def (ite (y :=: Int 3) (y :=: Int 3) (y :=: Int 4)))

--- examples testing If with `mustDecide` ---

-- this *should* VERIFY
ex_if0 :: Expr
ex_if0 = verse $
  timlam $ \b -> return (ite b (Int 3) (Int 4))

-- this *should not* VERIFY
ex_if1 :: Expr
ex_if1 = verse $
  timlam $ \_x ->
    do return (do b <- exists <? "b"
                  def (ite b (Int 3) (Int 4)))

ex_if2 :: Expr
ex_if2 = Verify $ LAM x $ (Assume (iNT (Var x))) :>: Assert (ite (leq (Int 0) (Var x)) (Int 1) (Int 2))
  where
    x = ident "x"

ex_inc :: Expr
ex_inc = tlamOblig y [x]
            (INT (Var y) :>: (Var x :=: Var y) :>: Var x)
            (INT (Var x) :>: INT (Int 1) :>: Assume (EXI r (INT (Var r) ) ))
  where
    x = ident "x"
    y = ident "y"
    r = ident "r"



-- example showing Tim's caution re: using assumes: make sure you cannot use a post-condition to prove its
-- own precondition. The exact example from August 2nd https://docs.google.com/document/d/18zJNCViVEmj8NzjE-zKMV0T1BGkW4__i24mM0huwGTY/edit#heading=h.ah81el9ovz9v
--   a&b:int => f(a=b):type{a=b} => exists x. x=f[x] # Must be rejected
-- I can't quite replicate the unsoundness due to desugarer obfuscations, so here's a plain CORE version
--   \a b f. assume {int[a]; int[b]; f = (\z. a = b; assume{a=b})}; assert {a = b; f[0]}

ex_tim_0 :: Expr
ex_tim_0 = lAMs [a, b, f] $
             Assume (iNT (Var a) :>: iNT (Var b) :>: (Var f :=: LAM z (Var a :=: Var b :>: Assume (Var a :=: Var b))))
             :>:
             Assert (Var a :=: Var b :>: (Var f :@: Int 0) :>: Int 0)
  where
    a = ident "a"
    b = ident "b"
    f = ident "f"
    z = ident "z"

ex_tim_1 :: Expr
ex_tim_1 = lAMs [a, b, f] $
             Assume (iNT (Var a) :>: iNT (Var b) :>: (Var f :=: LAM z (Var a :=: Var b :>: Assume (Var a :=: Var b))))
             :>:
             Assert ((Var f :@: Int 0) :>: Var a :=: Var b :>:  Int 0)
  where
    a = ident "a"
    b = ident "b"
    f = ident "f"
    z = ident "z"

ex_asm_subst :: Expr
ex_asm_subst = lAMs [a] $
                 Assume (Var a :=: Int 10)
                 :>:
                 Assert (sub (Var a) (Int 1) :=: Int 9 :>: Int 0)
  where
    a = ident "a"