{-# LANGUAGE PatternSynonyms #-}
module Main where

import TRS.Bind
import TRS.TRS
import Rules.Core
import Rules.Systems
import Data.List( union )

--------------------------------------------------------------------------------

data Type
  = Base Value -- a function with at most one result
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
  x  = identNotIn (free h_)

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
     putStrLn ("Norm: " ++ show et')
     if BAD `elem` map Var (free et')
       then putStrLn "*** TYPE CHECK FAILED"
       else putStrLn "+++ TYPE CHECK SUCCEEDED"
 where
  et       = hasType e t
  (_,et'):_ = normalForms defaultTRSFlags typeRules et

int :: Value
int = LAM x ((Op IsInt :@: (Var x)) :>: Var x)
 where
  x = ident "x"

typeRules :: Rule Expr
typeRules = rules (head allSystems) <> rulesUNR

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


