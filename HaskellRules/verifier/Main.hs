module Main where

import Data.List( intercalate )
import Data.Char( isAlpha, toUpper )
import qualified Data.Set as S
import Rules.Core
import TRS.Bind

--------------------------------------------------------------------------------

showIdent :: Ident -> String
showIdent (Name s) = s
showIdent (Prim p) = "pr_" ++ show p ++ "_"

showVar :: Ident -> String
showVar (Name (c:s))
  | isAlpha c    = toUpper c : s
  | otherwise    = "V" ++ (c:s)
showVar (Prim p) = "PR_" ++ show p ++ "_"

data Term
  = Vr Ident
  | Ap Ident [Term]
 deriving ( Eq, Ord )

instance Free Term where
  free (Vr x)    = [x]
  free (Ap _ ts) = free ts

instance Show Term where
  show (Vr v)    = showVar v
  show (Ap c []) = showIdent c
  show (Ap f ts) = showIdent f ++ "(" ++ intercalate "," (map show ts) ++ ")"

data Form
  = FALSE
  | TRUE
  | Pred Ident [Term]
  | Not Form
  | Form :&&: Form
  | Form :||: Form
  | Forall (Bind Form)
  | Exists (Bind Form)
 deriving ( Eq, Ord )

instance Free Form where
  free (Pred _ ts) = free ts
  free (Not p)     = free p
  free (p :&&: q)  = free (p,q)
  free (p :||: q)  = free (p,q)
  free (Forall b)  = free b
  free (Exists b)  = free b
  free _           = []

instance Show Form where
  show FALSE       = "$false"
  show TRUE        = "$true"
  show (Pred r []) = showIdent r
  show (Pred r [s,t]) | showIdent r == "=" = show s ++ " = " ++ show t
  show (Pred r ts) = showIdent r ++ "(" ++ intercalate "," (map show ts) ++ ")"
  show (Not p)     = "~" ++ show1 p
  show (p :&&: q)  = showAnd [p,q]
  show (p :||: q)  = showOr [p,q]
  show (Forall b)  = "!" ++ showBind b
  show (Exists b)  = "?" ++ showBind b

showAnd ps = intercalate " & " (map show1 (flat ps))
 where
  flat ((p :&&: q) : ps) = flat (p:q:ps)
  flat (p:ps)            = p : flat ps
  flat []                = []
  
showOr ps = intercalate " | " (map show1 (flat ps))
 where
  flat ((p :||: q) : ps) = flat (p:q:ps)
  flat (p:ps)            = p : flat ps
  flat []                = []
  
showBind (Bind x p) = "[" ++ showVar x ++ "]: " ++ show1 p  

show1 p
  | isAtom p  = show p
  | otherwise = "(" ++ show p ++ ")"
 where
  isAtom FALSE      = True
  isAtom TRUE       = True
  isAtom (Not _)    = True
  isAtom (Pred p _) = not (isOp p)
  isAtom _          = False
  
  isOp v = showIdent v == "="

(.=.) :: Term -> Term -> Form
s .=. t = Pred (ident "=") [s,t]

---

term :: Expr -> Term
term (Var v)  = Vr v
term (Int k)  = Ap (ident (show k)) []
term (Arr vs) = Ap (ident "tup") (map term vs)
term e = error ("term: " ++ show e)

forms :: Expr -> [Term -> Form]
forms (Val v)             = [\t -> t .=. term v]
forms Fail                = []
forms (Op op :@: v)       = [\t -> app op v t]
forms (e1 :|: e2)         = forms e1 ++ forms e2
forms ((v :=: e1) :>: e2) = [\t -> q1 (term v) :&&: q2 t | q1 <- forms e1, q2 <- forms e2]
forms (e1 :>: e2)         = forms (Exi (Bind x ((Var x :=: e1) :>: e2)))
                              where x = identNotIn (free (e1,e2))
forms (Exi (Bind x e))    = [\t -> Exists (Bind x (q t)) | q <- forms e]
forms (One e)             = [ones (forms e)]
 where
  ones []     _ = FALSE
  ones [q]    t = q t
  ones (q:qs) t = qt :||: (Forall (Bind x (Not (q (Vr x)))) :&&: ones qs t)
   where
    qt = q t
    x  = identNotIn (free qt)
forms e = error ("forms: " ++ show e)

app :: Op -> Expr -> Term -> Form
app Ge (Arr [a,b]) t =
  Pred (ident "ge") [term a, term b] :&&: (t .=. term a)

app op a _ =
  error ("app: " ++ show op ++ "@" ++ show a)

---

main :: IO ()
main =
  do putStrLn "-- expr --"
     print e
     putStrLn "-- formula --"
     let [q] = forms e
     putStr $ unlines $
       [ "fof(goal, conjecture,"
       , "  " ++ show (Forall (Bind a (Exists (Bind t (q (Vr t))))))
       , ")."
       ]
 where
  e  = One (Exi $ Bind x $ Exi $ Bind y $
             (Var x :=: One ( (Var y :=: Var a :>: Var y) :|: Int 2 ))
         :>: (Var y :=: Int 1)
         :>: (Var x :=: Int 2)
         :>: Var x)

  t  = ident "result"
  x  = ident "x"
  y  = ident "y"
  a  = ident "a"

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


