module Rules.Core where

import qualified Data.Map as M
import TRS.Bind

data Expr
  -- values
  = Var Ident
  | Int Integer
  | Arr [Expr]
  | Lam (Bind Expr)
  | Op Op
  
  -- programs
  | Expr :=: Expr    -- unification      =
  | Expr :>: Expr    -- seq. composition ;
  | Expr :|: Expr    -- choice           |
  | Expr :@: Expr    -- application      v1[v2]
  | Exi (Bind Expr)
  | Fail

  -- one/all
  | One Expr
  | All Expr
  -- | Split Expr
  
  -- verifier
  | Some Expr
  | Val :>>: Expr    -- guard           |>   <-- black triangle
  | Check Effect Expr
  | Verify (BindList ([Assump],Expr))

  -- only for contexts
  | HOLE
 deriving ( Eq, Show )

data Op = Add | Sub | Gt | IsInt
 deriving ( Eq, Ord, Show )

data Assump
  = NOTHING_HERE_YET 
 deriving ( Eq, Ord, Show )

data Effect
  = Fail_Effect
  | Succeeds
  | Decides
 deriving ( Eq, Show )

type Context = Expr
type Val     = Expr

instance Variables Assump where
  variables f NOTHING_HERE_YET = []

instance Variables Expr where
  variables f (Var x)      = [x]
  variables f (Arr es)     = variables f es
  variables f (Lam bnd)    = variables f bnd
  variables f (e1 :=: e2)  = variables f (e1,e2)
  variables f (e1 :>: e2)  = variables f (e1,e2)
  variables f (e1 :|: e2)  = variables f (e1,e2)
  variables f (e1 :@: e2)  = variables f (e1,e2)
  variables f (One e)      = variables f e
  variables f (All e)      = variables f e
  variables f (Some e)     = variables f e
  variables f (e1 :>>: e2) = variables f (e1,e2)
  variables f (Check fx e) = variables f e
  variables f (Exi bnd)    = variables f bnd
  variables f (Verify bnd) = variables f bnd
  variables f e            = []

isSkolem :: Ident -> Bool
isSkolem (Name ('$':_)) = True
isSkolem _              = False

unbindAs :: Ident -> Bind Expr -> Expr
unbindAs x bnd = subst [(y,Var x)] e where (y,e) = unsafeUnbind bnd

alphaRename :: [Ident] -> Bind Expr -> (Ident,Expr)
alphaRename = alphaRenameWith (\x y -> subst [(x,Var y)])

-- sorts binders and renames variables
norm :: Expr -> Expr
norm e = alpha 0 e
 where
  var i = ident ("_" ++ show i)
 
  alpha k (Arr es)     = Arr (map (alpha k) es)
  alpha k (Lam bnd)    = Lam (bind x (alpha (k+1) e)) where x = var k; e = unbindAs x bnd
  alpha k (e1 :=: e2)  = alpha k e1 :=: alpha k e2
  alpha k (e1 :>: e2)  = alpha k e1 :>: alpha k e2
  alpha k (e1 :|: e2)  = alpha k e1 :|: alpha k e2
  alpha k (e1 :@: e2)  = alpha k e1 :@: alpha k e2
  alpha k (One e)      = One (alpha k e)
  alpha k (All e)      = All (alpha k e)
  alpha k (Some e)     = Some (alpha k e)
  alpha k (e1 :>>: e2) = alpha k e1 :>>: alpha k e2
  alpha k (Check fx e) = Check fx (alpha k e)
  alpha k e@(Exi _)    = alphaExi k [] e
  alpha k (Verify bnd) = error "alpha Verify undefined"
  alpha k e            = e

  alphaExi k xs (Exi bnd) = alphaExi k (x:xs) e
   where
    (x,e) = unsafeUnbind bnd

  alphaExi k xs e = exis (map snd tab) (subst [(x,Var y)|(x,y)<-tab] e')
   where
    n   = length xs
    e'  = alpha (k+n) e
    ys  = free e'
    tab = filter (`elem` xs) ys `zip` [ var i | i <- [k..] ]

    exis []     e = e
    exis (y:ys) e = Exi (bind y (exis ys e))

subst :: Subst Expr -> Expr -> Expr
subst sub (Var x)      = head $ [e | (y,e) <- sub, y == x] ++ [Var x]
subst sub (Arr es)     = Arr (map (subst sub) es)
subst sub (Lam bnd)    = Lam (substBind Var subst sub bnd)
subst sub (e1 :=: e2)  = subst sub e1 :=: subst sub e2
subst sub (e1 :>: e2)  = subst sub e1 :>: subst sub e2
subst sub (e1 :|: e2)  = subst sub e1 :|: subst sub e2
subst sub (e1 :@: e2)  = subst sub e1 :@: subst sub e2
subst sub (One e)      = One (subst sub e)
subst sub (All e)      = All (subst sub e)
subst sub (Some e)     = Some (subst sub e)
subst sub (e1 :>>: e2) = subst sub e1 :>>: subst sub e2
subst sub (Check fx e) = Check fx (subst sub e)
subst sub (Exi bnd)    = Exi (substBind Var subst sub bnd)
subst sub (Verify bnd) = error "subst Verify undefined"
subst sub e            = e

match :: (Expr -> [(String,Expr)]) -> Expr -> [(String,Expr)]
match step e = step e ++ recurse e
 where
  recurse (Arr es)     = [ (s, Arr (take i es ++ [e'] ++ drop (i+1) es))
                         | i <- [0..length es-1]
                         , (s,e') <- match step (es!!i)
                         ]
  recurse (Lam bnd)    = [ (s, Lam (bind x e')) | (s,e') <- match step e ]
                       where (x,e) = unsafeUnbind bnd
  recurse (e1 :=: e2)  = [ (s, e1' :=: e2)  | (s,e1') <- match step e1 ]
                      ++ [ (s, e1  :=: e2') | (s,e2') <- match step e2 ]
  recurse (e1 :>: e2)  = [ (s, e1' :>: e2)  | (s,e1') <- match step e1 ]
                      ++ [ (s, e1  :>: e2') | (s,e2') <- match step e2 ]
  recurse (e1 :|: e2)  = [ (s, e1' :|: e2)  | (s,e1') <- match step e1 ]
                      ++ [ (s, e1  :|: e2') | (s,e2') <- match step e2 ]
  recurse (e1 :@: e2)  = [ (s, e1' :>: e2)  | (s,e1') <- match step e1 ]
                      ++ [ (s, e1  :>: e2') | (s,e2') <- match step e2 ]
  recurse (One e)      = [ (s, One e') | (s,e') <- match step e ]
  recurse (All e)      = [ (s, One e') | (s,e') <- match step e ]
  recurse (Some e)     = [ (s, One e') | (s,e') <- match step e ]
  recurse (e1 :>>: e2) = [ (s, e1' :>>: e2)  | (s,e1') <- match step e1 ]
                      ++ [ (s, e1  :>>: e2') | (s,e2') <- match step e2 ]
  recurse (Check fx e) = [ (s, Check fx e') | (s,e') <- match step e ]
  recurse e@(Exi _)    = [ (s, exis body') | (s,body') <- match step body ]
                       where (exis,body) = unExi e
  recurse (Verify bnd) = error "match Verify undefined"
  recurse e            = []

  unExi (Exi bnd) = (Exi . bind x . exis, body)
   where
    (x,e)       = unsafeUnbind bnd
    (exis,body) = unExi e
  unExi e         = (id, e)

