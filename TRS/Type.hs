{-# LANGUAGE PatternSynonyms #-}
module Type where

import Bind
import TRS
import TRSCore
import RulesPOPL
import Data.List( union )

data Type
  = Base Value -- a function with at most one result
  | Type :-> Type
 deriving ( Eq, Ord, Show )

instance Free Type where
  free (Base t)  = free t
  free (a :-> b) = free a `union` free b

asVal :: Expr -> (Value -> Expr) -> Expr
Val v `asVal` h = h v
e     `asVal` h = Def (Bind x ((VAR x :=: e) :>: h (Var x)))
 where
  h_ = h (Var (ident ""))
  x  = identNotIn (free h_)

wrap :: Expr -> Expr -> Expr -> Type -> Expr
wrap unr bad e (Base t) =
  e `asVal` \x -> One ((t :@: x) :|: bad)

wrap unr bad e (a :-> b) =
  LAM x (wrap unr bad (e -@- wrap bad unr (VAR x) a) b)
 where
  x = identNotIn (free (e,a,b))

  ef -@- ex =
    ef `asVal` \f ->
      ex `asVal` \x ->
        f :@: x

pattern UNR = VAR (Name "UNR")
pattern BAD = VAR (Name "BAD")

hasType :: Expr -> Type -> Expr
hasType e t = wrap UNR BAD e t

check :: Expr -> Type -> IO ()
check e t =
  do putStrLn ("Expr: " ++ show e)
     putStrLn ("Type: " ++ show t)
     putStrLn ("Wrap: " ++ show et)
     putStrLn ("Norm: " ++ show et')
     if BAD `elem` map VAR (free et')
       then putStrLn "*** TYPE CHECK FAILED"
       else putStrLn "+++ TYPE CHECK SUCCEEDED"
 where
  et        = hasType e t
  (_,et'):_ = normalForms typeRules et

int :: Value
int = VLAM x ((IsINT :@: (Var x)) :>: VAR x)
 where
  x = ident "x"

typeRules :: Rule Expr
typeRules = rules +++ rulesUNR

rulesUNR :: Rule Expr
rulesUNR lhs =
  "UNR-seq-L" `name`
    do UNR :>: _ <- [lhs]
       return UNR
 ++
  "UNR-seq-R" `name`
    do _ :>: UNR <- [lhs]
       return UNR
 ++
  "UNR-unif-L" `name`
    do UNR :=: _ <- [lhs]
       return UNR
 ++
  "UNR-unif-R" `name`
    do _ :=: UNR <- [lhs]
       return UNR
 ++
  "UNR-def" `name`
    do Def (Bind _ UNR) <- [lhs]
       return UNR

main :: IO ()
main =
  do check (INT 0) (Base int)

