{-# LANGUAGE PatternSynonyms #-}
module TRSVerifier.Verifier (runTests,testAbs, testConc, pshow, reduce, showStepS) where

import qualified TRS.TRS as TRS
import Rules.Core hiding (Wrong)
import Epic.Print
import TRS.Traced
import Control.Monad (forM_)
import Rules.Verifier
import TRS.Bind (Bind (Bind), ident, Ident)

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
  , ("ex1", ex1, True)
  , ("ex2", ex2, True)
  , ("ex2'", ex2', False)
  ]

-------------------------------------------------------------------------------------------
pattern INT :: Expr -> Expr
pattern INT e = Op IsInt :@: e

iNT :: Expr -> Expr
iNT e = INT e :>: e

lET :: Ident -> Expr -> Expr -> Expr
lET x e1 e2 = EXI x ((Var x :=: e1) :>: e2)
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