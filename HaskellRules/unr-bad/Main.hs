{-# LANGUAGE PatternSynonyms #-}
module Main where

import TRS.Bind
import TRS.TRS
import Rules.Core
import Rules.Systems
import Data.List( union )

--------------------------------------------------------------------------------

data Type
  = Base Expr -- a function with at most one result
  | Type :-> Type
 deriving ( Eq, Ord, Show )

instance Free Type where
  free (Base t)  = free t
  free (a :-> b) = free a `union` free b

asVal :: Expr -> (Value -> Expr) -> Expr
Val v `asVal` h = h v
e     `asVal` h = Def (Bind x ((Var x :=: e) :>: h (Var x)))
 where
  h_ = h (Var (ident ""))
  x  = identNotIn (free (h_,e))

wrap :: Expr -> Expr -> Expr -> Type -> Expr
wrap _unr bad e (Base t) =
  e `asVal` \x -> One ((t :@: x) :|: bad)

wrap unr bad e (a :-> b) =
  LAM v (wrap unr bad (e -@- wrap bad unr (Var v) a) b)
 where
  v = identNotIn (free (e,a,b))

  ef -@- ex =
    ef `asVal` \f ->
      ex `asVal` \x ->
        f :@: x

pattern UNR, BAD :: Expr
pattern UNR = Var (Name "UNR")
pattern BAD = Var (Name "BAD")

hasType :: Expr -> Type -> Expr
hasType e t = wrap UNR BAD e t

typeCheck :: Expr -> Type -> IO ()
typeCheck e t =
  do putStrLn ("Expr: " ++ show e)
     putStrLn ("Type: " ++ show t)
     putStrLn ("Wrap: " ++ show et)
     putStrLn ("Norm: " ++ show (simp et))
     if BAD `elem` map Var (free et')
       then putStrLn "*** TYPE CHECK FAILED"
       else putStrLn "+++ TYPE CHECK SUCCEEDED"
 where
  et = hasType e t

simp :: Expr -> Expr
simp e = caseSplit e'
 where
  (_,e'):_ = normalForms defaultTRSFlags typeRules et

  caseSplit (Lam (Bind x e)) = Lam (Bind x (caseSplit e))
  caseSplit 

  block (Lam (Bind x

int :: Value
int = LAM x ((Op IsInt :@: (Var x)) :>: Var x)

x = ident "x"

typeRules :: Rule Expr
typeRules = rules (head allSystems) <> rulesUNR <> rulesExtra

rulesExtra :: Rule Expr
rulesExtra _ lhs =
  "isInt-isInt" `name`
    do 

rulesUNR :: Rule Expr
rulesUNR _ lhs =
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
  do typeCheck (Int 0) (Base int)
     typeCheck (LAM x (Var x)) (Base int :-> Base int)


