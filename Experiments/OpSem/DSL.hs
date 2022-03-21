{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module OpSem.DSL(
  Exp,
  pattern (:=), pattern Fst, pattern Snd, pattern Pair, pattern Sel,
  (===), (|||), (#), (%), if_, for, semi, where_, var, lam, do_, appS,
  (<.), (<=.), (>.), (>=.),
  (==>), (@@), case_, let_,
  range, array, app, failure, err, print_,
  ) where
import Data.String ( IsString(..) )
import OpSem.Error
import OpSem.Exp

---------------------
--      Sugar
---------------------

instance Num Exp where
  (+) = PrimBin "+"
  (-) = PrimBin "-"
  (*) = PrimBin "*"
  negate = PrimUn "negate"
  abs = PrimUn "abs"
  fromInteger = Con

instance Real Exp
instance Enum Exp
instance Integral Exp where
  div = PrimBin "div"

instance IsString Exp where
  fromString = Var

infix 4 <., <=., >., >=.
(<.), (<=.), (>.), (>=.) :: Exp -> Exp -> Exp
(<.) = PrimBin "<"
(<=.) = PrimBin "<="
(>.) = PrimBin ">"
(>=.) = PrimBin ">="

infixl 4 |||
(|||) :: Exp -> Exp -> Exp
x ||| y = Alt (addDef x) (addDef y)

infixl 3 #
(#) :: Exp -> Exp -> Exp
(#) = Pair

infixl 5 ===
(===) :: Exp -> Exp -> Exp
(===) = Equal

infix 2 :=
pattern (:=) :: Exp -> Exp -> Exp
pattern (:=) x e <- Set (Var -> x) e
  where (:=) (Var x) e = Set x e
        (:=) _ _ = internalError ":="

pattern Fst :: Exp -> Exp
pattern Fst e = App e (Con 0)
pattern Snd :: Exp -> Exp
pattern Snd e = App e (Con 1)
pattern Pair :: Exp -> Exp -> Exp
pattern Pair e1 e2 = Array [e1, e2]
pattern Sel :: Exp -> Integer -> Exp
pattern Sel e i = App e (Con i)

-- Sequencing, evaluate both and return second
infixl 1 `semi`, %
semi :: Exp -> Exp -> Exp
semi x y = Semi x y
(%) :: Exp -> Exp -> Exp
(%) x y = Semi x y

-- Sequencing, evaluate both and return first
infix 1 `where_`
where_ :: Exp -> Exp -> Exp
where_ x y = Where x y

for :: Exp -> Exp -> Exp
for e1 e2 = For (addDef e1) (addDef e2)

if_ :: Exp -> Exp -> Exp -> Exp
if_ e1 e2 e3 = If (addDef e1) (addDef e2) (addDef e3)

do_ :: Exp -> Exp
do_ e = Do (addDef e)

let_ :: Exp -> Exp -> Exp
let_ e1 e2 = Let (addDef e1) e2

lam :: Exp -> Exp -> Exp
lam (Var n) e = Lam n (addDef e)
lam _ _ = internalError "lam"

var :: Exp -> Exp
var (Var n) = SetAny n
var _ = internalError "var"

infixr 3 ==>
(==>) :: Exp -> Exp -> Exp
a@(Var _) ==> b = lam a b
a ==> b = lam "&x" $ do_ (a === Var "&x" % b)

infixl 9 @@
(@@) :: Exp -> Exp -> Exp
(@@) = App

case_ :: Exp -> [Exp] -> Exp
case_ e arms =
  let_ ("&e" := e) $
  if_ ("&x" := foldr (|||) Fail (map (@@ Var "&e") arms)) (Var "&x") Fail

-- Application that must not fail
appS :: Exp -> Exp -> Exp
appS f a = if_ ("&x" := App f a) (Var "&x") Wrong

range :: Exp -> Exp
range = Range

array :: [Exp] -> Exp
array = Array

app :: Exp -> Exp -> Exp
app = App

failure :: Exp
failure = Fail

err :: Exp
err = Error

print_ :: Exp -> Exp
print_ = PrimUn "print"
