{-# OPTIONS_GHC -Wall #-}
module OpSem.Exp(
  Name,
  Exp(..),
  SExp(..),
  addDef,
  Eval(..),
  ) where
import Data.List ( nub )
import GHC.Stack ( HasCallStack )
import Text.PrettyPrint.HughesPJClass

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
         | PrimBin String Exp Exp  -- primitive binary functions, e.g., +
         | Fail
         | For SExp SExp
         | If SExp SExp SExp
         | Do SExp
         | Let SExp Exp
         | Range Exp     -- :e
         | Lam Name SExp
         | App Exp Exp
         | Error
  deriving (Show, Eq, Ord)

data SExp     -- A scope-limiting construct
  = Def [Name]   -- Bring these variables into scope
        Exp      -- In this expression
  deriving (Show, Eq, Ord)

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
findSet If {}    = []
findSet Do {}    = []
findSet (Let _ e) = findSet e
findSet Lam {}   = []
findSet (App  e1 e2) = findSet e1 ++ findSet e2
findSet (Equal e1 e2) = findSet e1 ++ findSet e2
findSet (Set x e) = x : findSet e
findSet (SetAny x) = [x]
findSet (Array es) = concatMap findSet es
findSet (PrimBin _ e1 e2) = findSet e1 ++ findSet e2
findSet (Range e) = findSet e
findSet Error = []

instance Pretty Exp where
  pPrint = text . show
  
class (Show a) => Eval a where
  eval :: Exp -> [a]
