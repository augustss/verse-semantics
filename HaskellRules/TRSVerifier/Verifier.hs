{-# LANGUAGE PatternSynonyms #-}
module TRSVerifier.Verifier (runTests,testAbs, testConc, pshow, reduce, showStepS) where

import qualified TRS.TRS as TRS
import Rules.Core hiding (Wrong)
import Epic.Print
import TRS.Traced
import Control.Monad (forM_)
import Rules.Verifier
import TRS.Bind (Bind (Bind), ident)

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
    has  = hasAssert e'

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

hasAssert :: Expr -> Bool
hasAssert = go
  where
    go (Assert _) = True
    go (Lam (Bind _ e))  = go e
    go (Exi (Bind _ e))  = go e
    go (e1 :=: e2) = go e1 || go e2
    go (e1 :>: e2) = go e1 || go e2
    go (e1 :|: e2) = go e1 || go e2
    go (e1 :@: e2) = go e1 || go e2
    go (One e) = go e
    go (All e) = go e
    go (Assume e) = go e
    go (Arr es) = any go es
    go (Split e1 e2 e3) = any go [e1,e2,e3]
    go (BlockC e) = go e
    go (Store _ e) = go e
    go _ = False

---------------------------------------------------------------------------------------------------
-- | Verifier tests
---------------------------------------------------------------------------------------------------

tests :: [(String, Expr, Bool)]
tests =
  [ ("ex00", ex00, True)
  , ("ex0", ex0, True)
  , ("ex0'", ex0', False)
  ]

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



pattern INT :: Expr -> Expr
pattern INT e = Op IsInt :@: e

--  forall x. int[x] => forall y.  int[y] => forall z. int[z] => x=z => succeeds{ exists a b. a=x; b=a; b=y}


{-
  [
    -- Unification
    ("ex0", ex0, aINT, True)
  , ("ex1", ex1, aINT, True)
  , ("ex4_2", ex4_2, aINT, True)
  , ("ex4_int_2", ex4_int_2, aINT, True)
  , ("ex6", ex6, aINT, True)
  , ("ex6'", ex6', aINT, True)
  , ("f3", f3, aINT, True)

    -- Function calls
  , ("succ_0", succ_0, aINT, True)
  , ("plus_1_2", plus_1_2, aINT, True)
  , ("incr", incr, aINT, True)
  , ("incrBad", incrBad, aINT, False)
  , ("fooXB", withPrims fooXB, aINT, True)
  , ("ex_if_1_2", ex_if_1_2, aINT, True)
  , ("barB", barB, aINT, True)

    -- Branches
  , ("exInt0", exIntTest0, aINT, True)
  , ("exInt1", exIntTest1, aINT, True)
  , ("exInt1'", exIntTest1', aINT, False)
  , ("exInc1_99", exIncW (Int 99), aINT, True)
  , ("exIntTest2", exIntTest2, aINT, True)

    -- Tim-Lambda examples
  , ("exInc1_INT", exIncW (aval aINT), aINT, True)
  , ("exInc1_ANY", exIncW (aval aANY), aINT, False)
  , ("exFGW0", exFGW0, aANY, True)
  , ("exFGW1", exFGW1, aANY, True)
  , ("exFGW2", exFGW2, aANY, False)
  , ("exFGFoo", exFGFooBar, aANY, True)
  ]

---------------------------------------------------------------------------------------------------
-- | Examples with Unification --------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- >>> verify ex0 aINT
-- True

-- ex0a = ex0 `Vis` aINT
-- exists x. x = 3; x
ex0 :: Expr
ex0 = EXI x $ (Var x :=: Int 3) :>: Var x
  where
    x = ident "x"

{-
  succeeds 'int' e --> '{v:int(v)}'


  f(x:int):int := x + 1

  (f = \(x:int). x + 1); f 3   --->>   f = (\x:INT -> ANY); f 3
                                       f = THING; (THING 3)

  e --->># a

  e ---->> e'

  a(e') == a
-}


ex0a :: Expr
ex0a = withPrims $ Var sucX :@: ex0

-- >>> verify ex1 aINT
-- True

-- exists x. exists y. x = 30; y = x; y
ex1 :: Expr
ex1 = EXI x $ EXI y $ (Var x :=: Int 30) :>: (Var y :=: Var x) :>: Var y
  where
    x = ident "x"
    y = ident "y"

-- exists a. a = SIG; (exists x. x = one { a | 2 }; x = 2; x)
ex4_with :: Expr -> Expr
ex4_with sig = EXI a ((Var a :=: sig) :>:
                EXI x ((Var x :=: One (Var a :|: Int 2) ) :>:
                  (Var x :=: Int 2) :>: Var x))
  where
    x = ident "x"
    a = ident "a"

-- >>> verify ex4_int aINT
-- False

-- >>> verify ex4_2 aINT
-- True

-- >>> verify ex4_int_2 aINT
-- True

ex4_int, ex4_2, ex4_int_2 :: Expr
ex4_int   = ex4_with (aval aINT)
ex4_2     = ex4_with (Int 2)
ex4_int_2 = ex4_with (aval (sngINT 2))

-- >>> verify ex6 aINT
-- True

-- exists x. exists y. x = ((y = a; y) | 2); y = 1; x = 2; x
ex6 ::Expr
ex6 = One $ EXI x $ EXI y $ (Var x :=: ( ( (Var y :=: Var a) :>: Var y) :|: Int 2)) :>: (Var y :=: Int 1) :>: (Var x :=: Int 2) :>:  Var x
  where
    x = ident "x"
    y = ident "y"
    a = ident "a"

-- >>> verify ex6' aINT
-- True

-- exists x. exists y. x = (1 | 2); y = 1; x = 2; x
ex6' ::Expr
ex6' = One $ EXI x $ EXI y $ (Var x :=: (Int 1 :|: Int 2)) :>: (Var y :=: Int 1) :>: (Var x :=: Int 2) :>:  Var x
  where
    x = ident "x"
    y = ident "y"

-- >>> verify f3 aINT
-- True

-- exists a. a = INT(3); a = 3; a
f3 :: Expr
f3 = EXI a $ (Var a :=: aval (sngINT 3)) :>: (Var a :=: Int 3) :>: Var a
  where
    a = ident "z"

---------------------------------------------------------------------------------------------------
-- | Examples with function calls -----------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- >>> verify succ_0 aINT
-- True

succ_0 ::Expr
succ_0 = withPrims $ Var sucX :@: Int 0

-- >>> verify plus_1_2 aINT
-- True

plus_1_2 :: Expr
plus_1_2 = withPrims $ bin addX (Int 1) (Int 2)

-- >>> verify incr aINT
-- True

-- exists x, y. x = INT; y = add x 1; y
incr :: Expr
incr = withPrims $ EXI x $ EXI y $ Var x :=: aval aINT :>: Var y :=: bin addX x (Int 1) :>: Var y
  where
    x = ident "xx"
    y = ident "yy"

-- exists x, y. x = ANY; y = add x 1; y
incrBad :: Expr
incrBad = withPrims $ EXI x $ EXI y $ Var x :=: aval aANY :>: Var y :=: bin addX x (Int 1) :>: Var y
  where
    x = ident "xx"
    y = ident "yy"

exElim :: Expr
exElim = EXI x $ Var x :=: aval aINT :>: Var x
  where
    x = ident "xx"

-- >>> verify (withPrims fooXB) aINT
-- True

fooXB :: Expr
fooXB = EXI x (Var x :=: aval aINT :>: fooB)
  where
    x = ident "xx"

fooB :: Expr
fooB = EXI y $ EXI z $
        Var y :=: Var z :>:
        Var z :=: bin addX x (Int 1) :>:
        Var y
  where
    x = ident "xx"
    y = ident "yy"
    z = ident "zz"


---------------------------------------------------------------------------------------------------
-- | Examples with IF-THEN-ELSE -------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
{-
exApp :: Expr
exApp = withPrims $ EXI x $ Var x :=: Vval aANY :>: Var intX :@: Var x
  where
    x = ident "x"

test'' :: Expr
-- test'' = Vval aINT :>: thunk (Int 1)
test'' = EXI x $
         (Var x :=: Vval aANY) :>:
           asm (isINT (Vr x)) (Vval (AFun (Bind (ident "_") (aANY, TRUE, sngINT 1))))
  where
    x = ident "x"

-}

-- >>> verify ex_if_1_2 aINT
-- True

-- ex x. if int(x) then 1 else 2
ex_if_1_2 :: Expr
ex_if_1_2 = withPrims $ EXI x $ Var x :=: aval aANY :>: If cond (Int 1) (Int 2)
  where
    x = ident "xx"
    cond = Var intX :@: Var x
    -- cond = Int 2023
    -- cond = (Int 2023 :=: Int 0) :>: Int 99

-- HEREHEREHEREHEREHERE why does it "work" with `ite` but not `If` ?



-- Phil's `bar` example

barB :: Expr
barB = withEnv ((barX, aval barV) : prims) $
        EXI y $ EXI z $
          (Var y :=: aval aINT) :>:
          (Var z :=: aval aINT) :>:
          ite (Var z :=: Int 666)
            (bin addX y z)
            (bin barX y (bin addX z (Int 1)))
  where
    y = ident "y"
    z = ident "z"
    barX = ident "bar"
    barV = addV -- same signature

-- bar(Y:int, Z:int):int := if (Z == 666) (Y + Z) (loop (Y, Z+1))

---------------------------------------------------------------------------------------------------
-- | Examples with "path-sensitivity" i.e. which use the results of tests in if-then-else
---------------------------------------------------------------------------------------------------
exZoo :: Expr
exZoo = One ( ((aval intV :@: aval aINT) :>: aThunk 1) :|: aThunk 2)

aThunk :: Integer -> Expr
aThunk i = aval $ AFun (Bind (ident "_") (aANY, TRUE, sng aINT i))

exFoo :: Expr
exFoo = One ((EXI x $
              Var x :=: aval (sng aANY k) :>:
              Vasm (isINT (Vr x)) :>:
              ((aval addV :@: Var x) :@: Int 1))
            :|: Int 99)
  where
    x = ident "xx"
    k = ident "k"

exIntTest0 :: Expr
exIntTest0 = withPrims $
               EXI x $ EXI z $ Var x :=: aval (sng aANY k) :>:
                 xite z (Var intX :@: Var x) (Var z `Vis` aINT) (Int 10)
  where
    x = ident "xx"
    z = ident "zz"
    k = ident "k"

-- NOTE: we're strengthening `x` to be a singleton ANY(k) here...
exIntTest1 :: Expr
exIntTest1 = withPrims $
               EXI x $ Var x :=: aval (sng aANY k) :>:
               If (Var intX :@: Var x) (bin addX x (Int 1)) (Int 0)
  where
    x = ident "xx"
    k = ident "k"

-- NOTE: we're strengthening `x` to be a singleton ANY(k) here...
exIntTest1' :: Expr
exIntTest1' = withPrims $
               EXI x $ Var x :=: aval (sng aANY k) :>:
               ite (Var intX :@: Int 100) (bin addX x (Int 1)) (Int 0)
  where
    x = ident "xx"
    k = ident "k"


-- TODO: the below FAILS because we can't FORCE the guard BEFORE the `is INT` check
exIntTest2 :: Expr
exIntTest2 = withPrims $
               EXI x $ Var x :=: aval (sng aANY k) :>:
               If (Var intX :@: Var x) (Var x `Vis` aINT) (Int 0)
  where
    x = ident "xx"
    k = ident "k"

---------------------------------------------------------------------------------------------------
-- | Tests with TLam
---------------------------------------------------------------------------------------------------

-- \(x := INT). exists y = add x 1; y
exInc0 :: Expr
exInc0 = withPrims $ TLAM x (aval aINT) $ EXI y $  Var y :=: bin addX x (Int 1) :>: Var y
  where
    x = ident "xx"
    y = ident "yy"

-- inc(x:INT):INT := y := add(x, 1); y
-- inc[zv]
exIncW :: Expr -> Expr
exIncW zv = withPrims $
              EXI z $
              EXI inc $ Var inc :=: TLAM x (aval aINT) (EXI y (Var y :=: bin addX x (Int 1) :>: Var y)) :>:
                Var z :=: zv :>:
                (Var inc :@: Var z)
  where
    x = ident "xx"
    y = ident "yy"
    z = ident "zz"
    inc = ident "inc"

bob = withPrims $
        EXI inc $ Var inc :=: TLAM x (aval aINT) (EXI y (Var y :=: bin addX x (Int 1) :>: Var y))
  where
    x = ident "xx"
    y = ident "yy"
    z = ident "zz"
    inc = ident "inc"
-- f(g(x:int):int):=g[4];
-- f[h(x:rational):=x/2]
exFGW :: Expr -> Expr
exFGW hv = withPrims $ EXI f $ EXI h $
        Var f :=: TLAM g (TLAM z (aval aINT) (aval aINT)) ((Var g :@: Int 4) `Vis` aANY) :>:
        Var h :=: hv :>:
        Var f :@: Var h
  where
    f = ident "f"
    g = ident "g"
    h = ident "h"
    z = ident "zz"

-- h(x:int) := x
exFGW0 :: Expr
exFGW0 = exFGW (TLAM x (aval aINT) (Var x))
  where
    x = ident "xx"

-- h(x:int) := x+1
exFGW1 :: Expr
exFGW1 = exFGW (TLAM x (aval aINT) (bin addX x (Int 1)))
  where
    x = ident "xx"

exFGW2 :: Expr
exFGW2 = exFGW (TLAM x (aval aRAT) (bin divX x (Int 2)))
  where
    x = ident "xx"

exFGFooBar :: Expr
exFGFooBar = EXI f $ EXI h $
              Var f :=: TLAM g (TLAM z (aval aBAR) (aval aFOO)) ((Var g :@: Int 666) `Vis` aANY) :>:
              Var h :=: TLAM x (aval aFOO) (Int 666) :>:
              Var f :@: Var h
  where
    f = ident "f"
    g = ident "g"
    h = ident "h"
    z = ident "zz"
    x = ident "xx"

aFOO :: AVal
aFOO = aBase (Bind v ((v $== (666 :: Integer)) :||: (v $== (42 :: Integer)))) [] where v = ident "a"

aBAR :: AVal
aBAR = aBase (Bind v (v $== (666 :: Integer))) [] where v = ident "a"


exFOO :: Expr
exFOO = TLAM x (aval (sng aANY k)) $ (Var x :=: Int 666 :>: Var x) :|: (Var x :=: Int 42 :>: Var x)
  where
    x = ident "xx"
    k = ident "kk"

-- ex x. IF int[x] THEN suc[x] ELSE 0

exIntSlack :: Expr
exIntSlack = withPrims $
              EXI x $ EXI y $
                Var x :=: aval (sng aANY k) :>:
                Var intX :@: Var x :>:
                Var y :=: Var x :>:
                (Var y `Vis` aINT)
  where
    y = ident "yy"
    x = ident "xx"
    k = ident "kk"

---------------------------------------------------------------------------------------------------
type Env = [(Ident, Expr)]

withEnv :: Env -> Expr -> Expr
withEnv env expr = foldr (\(x, v) e -> EXI x $ Var x :=: v :>: e) expr env

withPrims :: Expr -> Expr
withPrims = withEnv prims

prims :: Env
prims =
 [ (sucX, aval sucV),
   (addX, aval addV),
   (intX, aval intV),
   (divX, aval divV)
 ]

intX :: Ident
intX = ident "int"


-- x:ANY == isInt$(x) ==> INT[x]
-- \x. asm(p); x
-- LAM x (Vasm (isINT (Vr x)) :>: Var x)
intV :: AVal
intV = AFun (Bind x (aANY, isINT (Vr x) , sng aINT x))
  where
    x = ident "x"

addX :: Ident
addX = ident "add"

-- x:INT -> y:INT -> INT
addV :: AVal
addV = AFun (Bind x (aINT, TRUE, AFun (Bind y (aINT, TRUE, aINT))))
  where
    x = ident "x"
    y = ident "y"

sucX :: Ident
sucX = ident "suc"

-- succV :: x:INT -> INT
sucV :: AVal
sucV = AFun (Bind x (aINT, TRUE, aINT))
  where
    x = ident "x"

divX :: Ident
divX = ident "div"

-- x:RAT -> y:INT -> RAT
divV :: AVal
divV = AFun (Bind x (aRAT, TRUE, AFun (Bind y (aINT, TRUE, aRAT))))
  where
    x = ident "x"
    y = ident "y"




asm :: Form -> Expr -> Expr
asm f e = Vasm f :>: e

ite :: Expr -> Expr -> Expr -> Expr
ite e1 e2 e3 = One ( (e1 :>: thunk e2) :|: thunk e3)
                 :@: Int 0 -- RJ: using 0 instead of <> to allow `A-APP` ...

ite' :: Expr -> Expr -> Expr -> Expr
ite' e1 e2 e3 = One ( (e1 :>: e2) :|: e3)


-- if (x := e1) then e2 else e3
xite :: Ident -> Expr -> Expr -> Expr -> Expr
xite x e1 e2 e3 = One (EXI x (Var x :=: e1 :>: thunk e2) :|: thunk e3)
                  :@: Int 0 -- RJ: using 0 instead of <> to allow `A-APP` ...


thunk :: Expr -> Expr
thunk = LAM (ident "_")

-}