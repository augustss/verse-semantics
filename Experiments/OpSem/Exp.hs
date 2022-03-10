{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE PatternSynonyms #-}
module OpSem.Exp(
  Name,
  Exp(..),
    pattern (:=), pattern Fst, pattern Snd, pattern Pair, pattern Sel,
    (===), (|||), (#), iF, for, semi, wher, var, lam, doo,
  SExp(..),
  addDef,
  ) where
import Data.List ( nub )
import Data.String ( IsString(..) )
import GHC.Stack ( HasCallStack )

--------------------------------
--
-- Code
--
--------------------------------

{- BNF syntax for the language
   e ::= x
      |  k
      |  (s1 | s2)
      |  (e = k)
      |  x := e
      |  (e1,...,en)
      |  e[i]
      |  e1 + e2
      |  :false
      |  for(s1){e2}
      |  do{s}
      |  :e
   s ::= def {x1,...} in e
-}

type Name = String

data Exp = Var Name
         | Con Integer
         | Semi Exp Exp  -- e1; e2 === (e1, e2)[1]
         | Where Exp Exp  -- e1 where e2 === (e1, e2)[0]
         | Alt SExp SExp
         | Equal Exp Exp
         | Set Name Exp
         | SetAny Name
         | Array [Exp]   -- (e1, ..., en)  aka  array{e1, ..., en}
         | Plus Exp Exp
         | Fail
         | For SExp SExp
         | If SExp SExp SExp
         | Do SExp
         | Range Exp     -- :e
         | Lam Name SExp
         | AppS Exp Exp
         | AppI Exp Exp
         | Error
  deriving (Show)

data SExp     -- A scope-limiting construct
  = Def [Name]   -- Bring these variables into scope
        Exp      -- In this expression
  deriving (Show)

---------------------
--      Sugar
---------------------

instance Num Exp where
  (+) = Plus
  fromInteger = Con

instance IsString Exp where
  fromString = Var

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
pattern (:=) :: Name -> Exp -> Exp
pattern (:=) x e = Set x e

pattern Fst :: Exp -> Exp
pattern Fst e = AppS e (Con 0)
pattern Snd :: Exp -> Exp
pattern Snd e = AppS e (Con 1)
pattern Pair :: Exp -> Exp -> Exp
pattern Pair e1 e2 = Array [e1, e2]
pattern Sel :: Exp -> Integer -> Exp
pattern Sel e i = AppS e (Con i)

-- Sequencing, evaluate both and return second
infixl 1 `semi`
semi :: Exp -> Exp -> Exp
semi x y = Semi x y

-- Sequencing, evaluate both and return first
infix 1 `wher`
wher :: Exp -> Exp -> Exp
wher x y = Where x y

for :: Exp -> Exp -> Exp
for e1 e2 = For (addDef e1) (addDef e2)

iF :: Exp -> Exp -> Exp -> Exp
iF e1 e2 e3 = If (addDef e1) (addDef e2) (addDef e3)

doo :: Exp -> Exp
doo e = Do (addDef e)

lam :: Name -> Exp -> Exp
lam n e = Lam n (addDef e)

var :: Name -> Exp
var = SetAny

-- Add all variables defined in the current scope.
addDef :: HasCallStack => Exp -> SExp
addDef e | xs /= nub xs = error $ "Duplicate := " ++ show (e, xs)
         | otherwise = Def xs e
  where xs = findSet e

findSet :: Exp -> [Name]
findSet Var {}   = []
findSet Con {}   = []
findSet (Semi e1 e2) = findSet e1 ++ findSet e2
findSet (Where e1 e2) = findSet e1 ++ findSet e2
findSet Alt {}   = []
findSet Fail     = []
findSet For {}   = []
findSet If {}   = []
findSet Do {}    = []
findSet Lam {}   = []
findSet (AppS  e1 e2) = findSet e1 ++ findSet e2
findSet (AppI  e1 e2) = findSet e1 ++ findSet e2
findSet (Equal e1 e2) = findSet e1 ++ findSet e2
findSet (Set x e) = x : findSet e
findSet (SetAny x) = [x]
findSet (Array es) = concatMap findSet es
findSet (Plus e1 e2) = findSet e1 ++ findSet e2
findSet (Range e) = findSet e
findSet Error = []

