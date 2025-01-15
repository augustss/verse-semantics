module Main where

import Data.List( inits )
import qualified Data.Set as S
import Rules.Core
import TRS.TRS
import TRS.Bind
import Epic.Print
import Rules.Systems
import TRS.Tarjan

{-
type Slice = Expr -- no one
type Val = Expr

data Know = Know
  { vars     = [Ident]
  , succeeds = [Expr]
  , heap     = [(Ident,Val)]
  }

unify :: Know -> Val -> Val -> Maybe Know
unify = undefined

absrun :: Know -> Slice -> [(Know, Val)]
absrun know v@Val{} =
  [(know, v)]

absrun know Fail =
  []

absrun know (e1 :|: e2) =
  absrun know e1 ++ absrun know e2

absrun know ((v :=: e1) :>: e2) =
  do (know1,v1) <- absrun know e1
     Just know2 <- [unify know1 v v1]
     absrun know2 e2

absrun know (Exi bnd) =
  absrun know{ vars = x:vars know } e 
 where
  Bind x e = alphaRename (vars know) bnd

absrun know (One e) =
-}

ifThenElse :: [Ident] -> Expr -> Expr -> Expr -> Expr
ifThenElse xs c p q =
  One ((foldr (\x -> Exi . Bind x) (c :>: Lam (Bind y p)) xs) :|: Lam (Bind y q)) :@: Arr []
 where
  y = identNotIn (free (p,q))

isNat :: Expr -> Expr
isNat e =
  Op Ge :@: Arr [e,Int 0]

f :: (Expr -> Expr) -> Expr -> Expr
f frec x = ifThenElse
            [x']
            (Var x' :=: isNat (Op Sub :@: Arr [x,Int 1]))
            (Op Mul :@: Arr [x,frec (Var x')])
            (Int 1)
 where
  x' = identNotIn (free x)

sys :: ESystem
Right sys = lookupSystem "ICFP"

normalForm :: Expr -> Expr
normalForm e =
  let e0      = preProcess sys (ruleEnv sys) e
      next e  = map snd $ step (rules sys) (ruleEnv sys) e
      Just es = tarjan1 maxBound next e0
   in minimum es

main :: IO ()
main =
  do putStrLn ("SYSTEM: " ++ sname sys)
     putStrLn "-- expr --"
     pp e
     putStrLn "-- normal form --"
     pp e'
 where
  e  = ifThenElse [y] (Var y :=: isNat (Var x)) (isNat (f frec (Var y))) (Arr [])
  e' = normalForm e
  fv = ident "f"
  x  = ident "x"
  y  = ident "y"

  frec x = isNat x :>: Exi (Bind y (isNat (Var y)))

{-

type Names = Set Ident

names0 :: Names
names0 = S.empty

name :: Names -> Ident -> (Names, Ident)
name seen x = (S.insert x' seen, x')
 where
  x':_ = [ y | y <- [ x, Name (show x ++ "'") ]
                 ++ [ Name (show x ++ show i)
                    | i <- [1..]
                    ]
             , not (y `S.member` seen)
             ]

type Constr = (Val, Val)

data Slice = Slice
  { global = [Constr]
  , fails  = [[Constr]]
  , result = Val
  }
 deriving ( Eq, Ord, Show )

slices :: Names -> Expr -> [(Slice,Names)]
slices names v@Val{} =
  [ ([], v, names) ]

flatten ((v :=: e1) :>: e2) =
  [ (us1 ++ [(v,v1)] ++ us2, v2, names2)
  | (us1,v1,names1) <- flatten names e1
  , (us2,v2,names2) <- flatten names1 e2
  ]

flatten names (e1 :|: e2) =
  flatten names e1 ++
  flatten names e2

flatten names (Exi (Bind x e)) =
  flatten names' (subst [(x,Var x')] e)
 where
  (names',x') = name names x

flatten names Fail =
  []

slices names (One e) =
  [ Slice [] 
  | sls@(_:_) <- inits (slices e)
  ]

flatten names e =
  error ("flatten (" ++ show e ++ ") not implemented yet")

-}

{- some examples that I want to work:

isInt(one{ isInt(x) | 3 | <> }) does not fail

Three cases:

1. isInt(x) does not fail:

  isInt(one{ isInt(x) | 3 | <> })
= isInt(isInt(x))
= isInt(x)

does not fail

2. isInt(x) fails:

  isInt(one{ isInt(x) | 3 | <> })
= isInt(one{ 3 | <> })
= isInt(3)
= 3

does not fail


isInt(x) |- isInt(x+1)
-- I want this to be coded in terms of has_effects

f has_effects {fail} |- f(3) + f(5)  has_effects {fail}

f has_effects {}, a has_effects X |- if f(3) then a() else b()  has_effects X

-}


