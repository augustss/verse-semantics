module Main where

import FOL
--import Data.List( intercalate )
--import Data.Char( isDigit, isAlpha, toUpper )
--import qualified Data.Set as S
--import qualified Data.Map as M
import Rules.Core
import TRS.Bind

--------------------------------------------------------------------------------
-- Logical Semantics of VC 

-- results e t:
-- "compute all the ways the expression e can produce a value corresponding to the term t"
results :: Expr -> Term -> [Form]
results (Val v)             t = [t .=. term v]
results Fail                t = []
results (Op op :@: v)       t = [app op v t]
results (e1 :|: e2)         t = results e1 t ++ results e2 t
results ((v :=: e1) :>: e2) t = [q1 :&&: q2 | q1 <- results e1 (term v), q2 <- results e2 t]
results (e1 :>: e2)         t = [q1 :&&: q2 | q1 <- success e1, q2 <- results e2 t]
results (Exi (Bind x e))    t = [Exists (Bind x q) | q <- results e t]
results (One e)             t = [ones (results e t) (fails e)]
 where
  ones []     _      = FALSE
  ones [q]    _      = q
  ones (q:qs) (f:fs) = q :||: (f :&&: ones qs fs)

results e _ = error ("results: " ++ show e)

-- success e:
-- "compute all the ways the expression e can produce some value"
-- success e =~= map (Exists t .) (results e t), but without the extra quantifier
success :: Expr -> [Form]
success (Val v)             = [TRUE]
success Fail                = []
success (Op op :@: v)       = [appSuccess op v]
success (e1 :|: e2)         = success e1 ++ success e2
success ((v :=: e1) :>: e2) = [q1 :&&: q2 | q1 <- results e1 (term v), q2 <- success e2]
success (e1 :>: e2)         = [q1 :&&: q2 | q1 <- success e1, q2 <- success e2]
success (Exi (Bind x e))    = [Exists (Bind x q) | q <- success e]
success (One e)             = [bigOr (success e)]
 where
  bigOr []     = FALSE
  bigOr [q]    =  q
  bigOr (q:qs) = q :||: bigOr qs

success e = error ("success: " ++ show e)

-- fails e:
-- "compute all the ways the expression e can fail"
fails :: Expr -> [Form]
fails e = map Not (success e)

-- term v: (trivially) convert the value v into a first-order term t
term :: Expr -> Term
term (Var v)  = Vr v
term (Int k)  = Ap (ident (show k)) []
term (Arr vs) = Ap (ident "tup") (map term vs)
term e        = error ("term: " ++ show e)

-- primitive operators
-- TODO: add something about types
app :: Op -> Expr -> Term -> Form
app op (Arr [a,b]) t | op `elem` [Ge,Le,Gt,Lt] =
  appSuccess op (Arr [a,b]) :&&:
  (t .=. term a)

app Add (Arr [a,b]) t =
  t .=. Ap (ident "+") [term a, term b]

app op a _ =
  error ("app: " ++ show op ++ "@" ++ show a)

appSuccess :: Op -> Expr -> Form
appSuccess Ge (Arr [a,b]) =
  Pred (ident "<=") [term b, term a]

appSuccess Le (Arr [a,b]) =
  Pred (ident "<=") [term a, term b]

appSuccess Gt (Arr [a,b]) =
  Not (Pred (ident "<=") [term a, term b])

appSuccess Lt (Arr [a,b]) =
  Not (Pred (ident "<=") [term b, term a])

appSuccess _ _ =
  TRUE

--------------------------------------------------------------------------------
-- Running Some Examples

main :: IO ()
main =
  do putStrLn "-- PROGRAM --"
     print e2
     putStrLn "-- FORMULA --"
     let [q] = success (One e3)
         pr  = Forall $ Bind a $ q
     putStrLn (show pr)
     b <- prove pr
     if b then
       putStrLn "==> program does not fail"
      else
       putStrLn "==> program may fail"
 where
  e0  = Exi $ Bind x $
              (Var x :=: Var a)
          :>: Var x
 
  e1  = Exi $ Bind x $ Exi $ Bind y $
              (Var x :=: One ( (Var y :=: Var a :>: Var y) :|: Int 2 ))
          :>: (Var y :=: Int 1)
          :>: (Var x :=: Int 2)
          :>: Var x

  e2  = Exi $ Bind x $
              (Var x :=: One ( (Op Ge :@: Arr [Var a, Int 3]) :|: Int 2 ))
          :>: (Op Ge :@: Arr [Var x, Var a])
  
  e3  = ifThenElse (Op Ge :@: Arr [Var a, Int 3])
          (Int 1 :|: Int 2)
          (Int 3 :|: Int 3 :|: Int 4)

  x  = ident "x"
  y  = ident "y"
  a  = ident "input"

{-
ifThenElse :: [Ident] -> Expr -> Expr -> Expr -> Expr
ifThenElse xs c p q =
  One ((foldr (\x -> Exi . Bind x) (c :>: Lam (Bind y p)) xs) :|: Lam (Bind y q)) :@: Arr []
 where
  y = identNotIn (free (p,q))
-}

ifThenElse :: Expr -> Expr -> Expr -> Expr
ifThenElse c p q =
  Exi $ Bind y $ 
    (Var y :=: One ((c :>: Int 1) :|: Int 2))
    :>:
    (((Var y :=: Int 1 :>: p)) :|: (Var y :=: Int 2 :>: q))
 where
  y:_ = identsNotIn (free (p,q))

isNat :: Expr -> Expr
isNat e =
  Op Ge :@: Arr [e,Int 0]

{-
f :: (Expr -> Expr) -> Expr -> Expr
f frec x = ifThenElse
            [x']
            (Var x' :=: isNat (Op Sub :@: Arr [x,Int 1]))
            (Op Mul :@: Arr [x,frec (Var x')])
            (Int 1)
 where
  x' = identNotIn (free x)
-}

--------------------------------------------------------------------------------

