module Verifier.Verify where

import Verifier.FOL
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
results (Val v)          t = [t .=. term v]
results Fail            _t = []
results (Op op :@: v)    t = [app op v t]
results (e1 :|: e2)      t = results e1 t ++ results e2 t
results (v :=: e)        t = [q :&&: (t' .=. t) | let t' = term v, q <- results e t']
results (e1 :>: e2)      t = [q1 :&&: q2 | q1 <- success e1, q2 <- results e2 t]
results (Exi (Bind x e)) t = [Exists (Bind x q) | q <- results e t]
results (One e)          t = [ones (results e t) (fails e)]
 where
  ones []     _      = FALSE
  ones [q]    _      = q
  ones (q:qs) (f:fs) = q :||: (f :&&: ones qs fs)

results e _ = error ("results: " ++ show e)

-- success e:
-- "compute all the ways the expression e can produce some value"
-- success e =~= map (Exists t .) (results e t), but without the extra quantifier
success :: Expr -> [Form]
success (Val _v)            = [TRUE]
success Fail                = []
success (Op op :@: v)       = [appSuccess op v]
success (e1 :|: e2)         = success e1 ++ success e2
success (v :=: e)           = results e (term v)
success (e1 :>: e2)         = [q1 :&&: q2 | q1 <- success e1, q2 <- success e2]
success (Exi (Bind x e))    = [Exists (Bind x q) | q <- success e]
success (One e)             = [bigOr (success e)]
 where
  bigOr []     = FALSE
  bigOr [q]    = q
  bigOr (q:qs) = q :||: bigOr qs

success e = error ("success: unimplemented " ++ show e)

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

verify :: [Ident] -> Expr -> IO Bool
verify is ee = do
  let [q] = success (One ee)
      pr = foldr (\ i e -> Forall $ Bind i e) q is
  putStrLn $ "Formula is " ++ show pr
  prove pr

--------------------------------------------------------------------------------

