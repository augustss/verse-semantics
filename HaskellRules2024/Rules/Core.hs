module Rules.Core where

import qualified Data.Map as M
import TRS.Bind

--------------------------------------------------------------------------------
-- main expression datatype

data Expr
  -- values
  = Var Ident
  | Int Integer
  | Arr [Val]
  | Lam (Bind Expr)
  | Op Op
  
  -- programs
  | Expr :=: Expr    -- unification      =
  | Expr :>: Expr    -- seq. composition ;
  | Expr :|: Expr    -- choice           |
  | Val  :@: Val     -- application      v1[v2]
  | Exi (Bind Expr)
  | Fail

  -- one/all
  | One Expr
  | All Expr
  -- | Split Expr  -- maybe later
  
  -- verifier
  | Some Val
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

--------------------------------------------------------------------------------
-- values

type Val     = Expr

isVal :: Expr -> Bool
isVal (Var x)   = True
isVal (Int k)   = True
isVal (Arr es)  = all isVal es
isVal (Lam bnd) = valid e where (_,e) = unsafeUnbind bnd
isVal (Op op)   = True
isVal _         = False

--------------------------------------------------------------------------------
-- valid expressions

-- checks if an expression is syntactically valid
valid :: Expr -> Bool
valid ((a :=: e1) :>: e2) = isVal a && valid e1 && valid e2
valid (e1 :|: e2)         = valid e1 && valid e2
valid (a1 :@: a2)         = isVal a1 && isVal a2
valid (Exi bnd)           = valid e where (_,e) = unsafeUnbind bnd
valid Fail                = True
valid (One e)             = valid e
valid (All e)             = valid e
valid (Some a)            = isVal a
valid (a :>>: e)          = isVal a && valid e
valid (Check fx e)        = valid e
valid (Verify bnds)       = error "check Verify undefined"
valid e                   = isVal e

-- valid (prep e) == True
prep :: Expr -> Expr
prep (Var x)       = Var x
prep (Int k)       = Int k
prep (Arr as)      = prepVals as (\vs -> Arr vs)
prep (Lam bnd)     = Lam (bind x (prep e)) where (x,e) = unsafeUnbind bnd
prep (Op op)       = Op op
prep (e1 :>: e2)   = prepSeq e1 (prep e2)
prep (a  :=: e)    = prepVal a (\v -> (v :=: prep e) :>: v)
prep (e1 :|: e2)   = prep e1 :|: prep e2
prep (a1 :@: a2)   = prepVal a1 (\v1 -> prepVal a2 (\v2 -> v1 :@: v2))
prep (Exi bnd)     = Exi (bind x (prep e)) where (x,e) = unsafeUnbind bnd
prep Fail          = Fail
prep (One e)       = One (prep e)
prep (All e)       = All (prep e)
prep (Some a)      = prepVal a (\v -> Some v)
prep (a :>>: e)    = prepVal a (\v -> v :>>: e)
prep (Check fx e)  = Check fx (prep e)
prep (Verify bnds) = error "prep Verify undefined"

prepSeq :: Expr -> Expr -> Expr
prepSeq (a :=: e1) e2 = prepVal a (\v -> (v :=: prep e1) :>: e2)
prepSeq e1         e2 = prepVal e1 (\_ -> e2)

prepVal :: Expr -> (Val -> Expr) -> Expr
prepVal a f
  | isVal pa  = f pa
  | otherwise = Exi (bind x ((Var x :=: pa) :>: f (Var x)))
 where
  pa = prep a
  x  = identNotIn (free (pa, f (Var (ident "?"))))

prepVals :: [Expr] -> ([Val] -> Expr) -> Expr
prepVals []     f = f []
prepVals (a:as) f = prepVal a (\v -> prepVals as (f . (v:)))

--------------------------------------------------------------------------------
-- variables

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

instance Variables Assump where
  variables f NOTHING_HERE_YET = []

isSkolem :: Ident -> Bool
isSkolem (Name ('$':_)) = True
isSkolem _              = False

--------------------------------------------------------------------------------
-- binders

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
  alpha k (Lam bnd)    = Lam (bind x (alpha (k+1) e))
                       where x = var k; e = unbindAs x bnd
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

--------------------------------------------------------------------------------
-- substitution

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

--------------------------------------------------------------------------------
-- rewriting

type Rule = Expr -> [(String,Expr)]

-- apply a rule everywhere (recursively) in the expression
everywhere :: Rule -> Rule
everywhere step e = step e ++ recurse e
 where
  recurse (Arr es)     = [ (s, Arr (take i es ++ [e'] ++ drop (i+1) es))
                         | i <- [0..length es-1]
                         , (s,e') <- everywhere step (es!!i)
                         ]
  recurse (Lam bnd)    = [ (s, Lam (bind x e')) | (s,e') <- everywhere step e ]
                       where (x,e) = unsafeUnbind bnd
  recurse (e1 :=: e2)  = [ (s, e1' :=: e2)  | (s,e1') <- everywhere step e1 ]
                      ++ [ (s, e1  :=: e2') | (s,e2') <- everywhere step e2 ]
  recurse (e1 :>: e2)  = [ (s, e1' :>: e2)  | (s,e1') <- everywhere step e1 ]
                      ++ [ (s, e1  :>: e2') | (s,e2') <- everywhere step e2 ]
  recurse (e1 :|: e2)  = [ (s, e1' :|: e2)  | (s,e1') <- everywhere step e1 ]
                      ++ [ (s, e1  :|: e2') | (s,e2') <- everywhere step e2 ]
  recurse (e1 :@: e2)  = [ (s, e1' :>: e2)  | (s,e1') <- everywhere step e1 ]
                      ++ [ (s, e1  :>: e2') | (s,e2') <- everywhere step e2 ]
  recurse (One e)      = [ (s, One e')  | (s,e') <- everywhere step e ]
  recurse (All e)      = [ (s, All e')  | (s,e') <- everywhere step e ]
  recurse (Some e)     = [ (s, Some e') | (s,e') <- everywhere step e ]
  recurse (e1 :>>: e2) = [ (s, e1' :>>: e2)  | (s,e1') <- everywhere step e1 ]
                      ++ [ (s, e1  :>>: e2') | (s,e2') <- everywhere step e2 ]
  recurse (Check fx e) = [ (s, Check fx e') | (s,e') <- everywhere step e ]
  recurse e@(Exi _)    = [ (s, exis body') | (s,body') <- everywhere step body ]
                       where (exis,body) = unExis e
  recurse (Verify bnd) = error "everywhere Verify undefined"
  recurse e            = []

-- treat "exi x1 .. exi xn" as one block when matching
unExis :: Expr -> (Expr -> Expr, Expr)
unExis (Exi bnd) = (Exi . bind x . exis, body)
 where
  (x,e)       = unsafeUnbind bnd
  (exis,body) = unExis e
unExis e         = (id, e)

-- structural rules matching
matchExi :: Expr -> [Bind Expr]
matchExi e =
  [ bnd'
  | Exi bnd <- [e]
  , let (x,e') = unsafeUnbind bnd
  , bnd' <- bnd : [ bind y (Exi (bind x e''))
                  | bnd' <- matchExi e'
                  , let (y,e'') = unsafeUnbind bnd'
                  ]
  ]

matchEq :: Expr -> [(Expr,Expr)]
matchEq e =
  [ (lhs, rhs)
  | e1 :=: e2 <- [e]
  , (lhs,rhs) <- (e1,e2) : [ (Var y, Var x)
                           | (Var x, Var y) <- [(e1,e2)]
                           ]
  ]

--------------------------------------------------------------------------------
-- contexts

type Context = Expr

contexts :: Expr -> [(Context,Expr)]
contexts e = (HOLE,e) : recurse e
 where
  recurse (Arr es)     = [ (Arr (take i es ++ [ctx] ++ drop (i+1) es), h)
                         | i <- [0..length es-1]
                         , (ctx,h) <- contexts (es!!i)
                         ]
  recurse (Lam bnd)    = [ (Lam (bind x ctx), h) | (ctx,h) <- contexts e ]
                       where (x,e) = unsafeUnbind bnd
  recurse (e1 :=: e2)  = [ (ctx :=: e2,  h) | (ctx,h) <- contexts e1 ]
                      ++ [ (e1  :=: ctx, h) | (ctx,h) <- contexts e2 ]
  recurse (e1 :>: e2)  = [ (ctx :>: e2,  h) | (ctx,h) <- contexts e1 ]
                      ++ [ (e1  :>: ctx, h) | (ctx,h) <- contexts e2 ]
  recurse (e1 :|: e2)  = [ (ctx :|: e2,  h) | (ctx,h) <- contexts e1 ]
                      ++ [ (e1  :|: ctx, h) | (ctx,h) <- contexts e2 ]
  recurse (e1 :@: e2)  = [ (ctx :@: e2,  h) | (ctx,h) <- contexts e1 ]
                      ++ [ (e1  :@: ctx, h) | (ctx,h) <- contexts e2 ]
  recurse (One e)      = [ (One ctx, h)  | (ctx,h) <- contexts e ]
  recurse (All e)      = [ (All ctx, h)  | (ctx,h) <- contexts e ]
  recurse (Some e)     = [ (Some ctx, h) | (ctx,h) <- contexts e ]
  recurse (e1 :>>: e2) = [ (ctx :>>: e2,  h) | (ctx,h) <- contexts e1 ]
                      ++ [ (e1  :>>: ctx, h) | (ctx,h) <- contexts e2 ]
  recurse (Check fx e) = [ (Check fx ctx, h) | (ctx,h) <- contexts e ]
  recurse e@(Exi _)    = [ (exis ctx, h) | (ctx,h) <- contexts body ]
                       where (exis,body) = unExis e
  recurse (Verify bnd) = error "contexts Verify undefined"
  recurse e            = []

(<@) :: Context -> Expr -> Expr
Arr as       <@ h = Arr (map (<@ h) as)
Lam bnd      <@ h = Lam (bind x (e <@ h)) where (x,e) = unsafeUnbind bnd
(e1 :>: e2)  <@ h = (e1 <@ h) :>: (e2 <@ h)
(e1 :=: e2)  <@ h = (e1 <@ h) :=: (e2 <@ h)
(e1 :|: e2)  <@ h = (e1 <@ h) :|: (e2 <@ h)
(e1 :@: e2)  <@ h = (e1 <@ h) :@: (e2 <@ h)
Exi bnd      <@ h = Exi (bind x (e <@ h)) where (x,e) = unsafeUnbind bnd
One e        <@ h = One (e <@ h)
All e        <@ h = All (e <@ h)
Some e       <@ h = Some (e <@ h)
(e1 :>>: e2) <@ h = (e1 <@ h) :>>: (e2 <@ h)
Check fx e   <@ h = Check fx (e <@ h)
Verify bnds  <@ h = error "context plugging Verify undefined"
HOLE         <@ h = h
e            <@ h = e

--------------------------------------------------------------------------------

