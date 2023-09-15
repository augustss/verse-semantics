-- Tiny experiments with desugaring
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Verifier.TRSDesugar where
import TRS.Bind (Bind (..), Ident, ident)
import Rules.Core
import Control.Monad.State.Strict (State, MonadState (get, put), evalState)
import Epic.Print (prettyShow)
-- import Epic.Print (prettyShow)


--------------------------------------------------------------------------------
-- | A SmallSource
--------------------------------------------------------------------------------

data SExpr
    = Id Ident                  -- ^ x
    | Lit Integer               -- ^ k
    | Bop SExpr SExpr           -- ^ t + t
    | App SExpr SExpr           -- ^ t[t]
    | Def Ident SExpr           -- ^ x := t
    | Equ SExpr SExpr           -- ^ t = t
    | Ty  SExpr                 -- ^ :t
    | As  SExpr SExpr           -- ^ t |> t
    | Seq SExpr SExpr           -- ^ t;t
    | Fun SExpr SExpr           -- ^ fun [exi xs] (t1) {t2}
    deriving (Eq, Show)

sInt :: SExpr
sInt = Id (ident "int")

tInt :: SExpr
tInt = Ty sInt

--------------------------------------------------------------------------------
{- Example 00:
    3 => 23
==  fun(3=4){23}
-}
ex00 :: SExpr
ex00 = Fun (Lit 3) (Lit 23)


{- Example 01:
    3=4 => 23
==  fun(3=4){23}
-}
ex01 :: SExpr
ex01 = Fun (Lit 3 `Equ` Lit 4) (Lit 23)

ex02 :: SExpr
ex02 = Def x (Lit 10)
  where x = ident "x"

--------------------------------------------------------------------------------

{- Example 1:

    a:int => b:int => a + b
==  fun(a := :int){ fun(b := :int) { a + b } }

-}
ex1 :: SExpr
ex1 = Fun (Def a tInt) (
        Fun (Def b tInt) (
          Fun (Def c tInt) (
            (Id a `Bop` Id b) `Bop` Id c
          )
        )
      )
      where
        a = ident "a"
        b = ident "b"
        c = ident "c"



--------------------------------------------------------------------------------
{- Example 2:

    f(:int):int => f[99]
==  fun (f := :fun(:int){:int}){ f [99] }
==  fun [f] (f = :fun(:int){:int}) { f[99] }

-}
ex2 :: SExpr
ex2 = Fun (Def f (Ty (Fun tInt tInt))) ( Id f `App` Lit 99 )
  where
    f = ident "f"



--------------------------------------------------------------------------------
{- Example 3:
    f(:int) := 10 => f[0]
==  fun (f := :fun(:int){10}) { f[0]}

    fun (f := fun(:int){10}) { f[0]}
-}

--------------------------------------------------------------------------------
data Mode
    = I     -- ^ "use"  / implementation
    | V     -- ^ "check" / verification
    deriving (Show)

add :: Expr -> Expr -> Expr
add e1 e2 = Op Add :@: Arr [e1, e2]



dsDump :: SExpr -> IO ()
dsDump = putStrLn . prettyShow . desugar

desugar :: SExpr -> Expr
desugar t = evalState (d t) (MkDS 0 ())

d :: SExpr -> D Expr
d t@(Fun {})      = (:>:) <$> (Verify <$> v t) <*> i t
d (Ty  t)         = do {y <- fresh; Exi . Bind y <$> m I t y}
d (As  {})        = undefined
d (Def x t)       = do {e <- d t; pure $ Exi (Bind x (Var x :=: e :>: Var x))}
d (Id x)          = pure (Var x)
d (Lit k)         = pure (Int k)
d (Bop t1 t2)     = add   <$> d t1 <*> d t2
d (App t1 t2)     = (:@:) <$> d t1 <*> d t2
d (Equ t1 t2)     = (:=:) <$> d t1 <*> d t2
d (Seq t1 t2)     = (:>:) <$> d t1 <*> d t2

m :: Mode -> SExpr -> Ident -> D Expr
-- M_V[[ :fun(t1){t2}]]f'  := \ix'. ix := M_I[[t1]]ix';  asm{z := f'[ix]; M_V[[t2]]z}
m V (Ty (Fun t1 t2)) f' = do
    ix  <- fresh
    ix' <- fresh
    z   <- fresh
    e1  <- m I t1 ix'
    e2  <- m V t2 z
    pure $ LAM ix' (def ix e1 (Assume (def z (Var f' :@: Var ix) e2)))

-- M_I[[ :fun(t1){t2}]]f'  := VERIFY(\ix'. asm{ix := M_V[[t1]]ix'}; suc{ z := f'[ix]; M_I[[t2]]z}); f'
m I (Ty (Fun t1 t2)) f' = do
    ix  <- fresh
    ix' <- fresh
    z   <- fresh
    e1  <- m V t1 ix'
    e2  <- m I t2 z
    pure $ Verify (LAM ix' (EXI ix (Assume (Var ix :=: e1) :>: Assert (def z (Var f' :@: Var ix) e2)))) :>: Var f'

m _ (Ty t)      y  = (:@:)       <$> d t      <*> pure (Var y)
m k (Def x t)   y  = (Var x :=:) <$> m k t y
m k (Equ t1 t2) y  = (:=:)       <$> m k t1 y <*> m k t2 y
m k (Seq t1 t2) y  = (:>:)       <$> d t1     <*> m k t2 y
m k (As t1 t2)  y  = do {e <- m k t1 y; t <- d t2; e `as` t}
m _ t           y  = (Var y :=:) <$> d t

as :: Expr -> Expr -> D Expr
as e t = do
    x <- fresh
    pure $ Verify (Assert (t :@: e)) :>: Assume (Uni (Bind x (t :@: Var x)))

v :: SExpr -> D Expr
v (Fun t1 t2) = do
    let xs = defs t1
    y  <- fresh
    LAM y . exis xs <$> ((:>:) <$> (Assume <$> m V t1 y) <*> v t2)

v t = Assert <$> d t

i :: SExpr -> D Expr
i (Fun t1 t2) = do
    let xs = defs t1
    y  <- fresh
    LAM y . exis xs <$> ((:>:) <$> m I t1 y <*> i t2)

i t = Assume <$> d t

defs :: SExpr -> [Ident]
defs ((Def x _) `Seq` t) = x : defs t
defs (Def x _)           = [x]
defs _                   = []

type D = State DS
data DS = MkDS { dsFresh :: !Int, dsBoo :: () }

fresh :: D Ident
fresh = do
  s <- get
  let n = dsFresh s
  put $! s { dsFresh = n+1 }
  pure (ident ("$x" ++ show n))
