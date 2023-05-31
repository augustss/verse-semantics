module TRSVerifier.Verifier (runTests,testAbs, testConc, pshow, reduce, showStepS) where

import qualified TRS.TRS as TRS
import Rules.Core hiding (Wrong)
import Epic.Print
import TRS.Traced
import Control.Monad (forM_)
import Rules.Verifier
import TRS.Bind (Bind (Bind), ident, Ident)
import Prelude hiding (sum)
--------------------------------------------------------------------------------
-- | Top-level function for running the verifier.
--------------------------------------------------------------------------------

runTests :: IO Bool
runTests = and <$> mapM runTest tests

runTest :: (String, Expr, Bool) -> IO Bool
runTest (name, e, expected) = do
  res     <- verify e
  let ok = isSafe res == expected
  putStrLn $ "Running test: " ++ name ++ " ..." ++ show ok
  -- putStrLn $ prettyShow res
  return ok

isSafe :: Result -> Bool
isSafe Accept = True
isSafe _    = False

testAbs :: Expr -> IO ()
testAbs = test trivVerifier

testConc :: Expr -> IO ()
testConc = test icfpVerifier

pshow :: (Pretty a) => a -> IO ()
pshow = putStrLn . prettyShow

verify :: Expr -> IO Result
verify e = return (if has then Reject else Accept)
  where
    e'   = term (run trivVerifier e)
    has  = hasAssertOrFail e'

reduce :: Expr -> Expr
reduce = term . run trivVerifier

test :: TRS.TRSystem Expr -> Expr -> IO ()
test v = putStrLn . prettyShow . run v

showStepS :: Expr -> IO ()
showStepS e = do
  forM_ (TRS.stepS trivVerifier e) $ \e' -> do
    putStrLn (prettyShow e')

run :: TRS.TRSystem Expr -> Expr -> Traced Expr
run v e = head (TRS.nrDone nf)
  where
    nf = TRS.normalFormFuelTracePlain v 1000 e

data Result = Accept | Reject
  deriving (Show)

instance Pretty Result where
  pPrint Accept = text "accept"
  pPrint Reject = text "reject"

hasAssertOrFail :: Expr -> Bool
hasAssertOrFail = go
  where
    go (Assert _)        = True
    go Fail              = True
    go (Lam (Bind _ e))  = go e
    go (Exi (Bind _ e))  = go e
    go (e1 :=: e2)       = go e1 || go e2
    go (e1 :>: e2)       = go e1 || go e2
    go (e1 :|: e2)       = go e1 || go e2
    go (e1 :@: e2)       = go e1 || go e2
    go (One e)           = go e
    go (All e)           = go e
    go (Assume e)        = go e
    go (Arr es)          = any go es
    go (Split e1 e2 e3)  = any go [e1,e2,e3]
    go (BlockC e)        = go e
    go (Store _ e)       = go e
    go _                 = False

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
{-
exi suc.
  suc = \a. INT[a]; assume { EXI b. INT[b]; b }
  (\v. assume {int[x]} ; assert { exi r. r = succ(x); INT[r]; r }
-}
ex1 ::Expr
ex1 = lET suc (LAM a (iNT (Var a) :>: Assume (EXI b (iNT (Var b)))))
      (LAM x (Assume (iNT (Var x)) :>: Assert (EXI r (Var r :=: Var suc :@: Var x :>: iNT (Var r) ))))
  where
    suc = ident "succ"
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
{- f(x:FOO):FOO = 708 - x
     where
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

bob = EXI t0 $ EXI t1 $
        INT (Var x) :>:
        (Var t0 :=: Assume (EXI r (INT (Var r) :>: Var r))) :>:
        (Var t1 :=: Assume (EXI r (INT (Var r) :>: Var r))) :>:
        (Var t1)
  where
    x = ident "x"
    r = ident "r"
    t0 = ident "t0"
    t1 = ident "t1"

bo0 = Assert $
        EXI t0 $ EXI t1 $
          INT (Var x) :>:
          (Var t0 :=: Assume (EXI r (INT (Var r) :>: Var r))) :>:
          (Var t1 :=: Assume (EXI s (INT (Var s) :>: Var s))) :>:
          (Var t1)
  where
    x = ident "x"
    r = ident "r"
    s = ident "s"
    t0 = ident "t0"
    t1 = ident "t1"
