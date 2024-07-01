module Rules.Core where

import Prelude hiding( (<>) )
import Epic.Print

import qualified Data.Map as M
import Data.List( union, intersperse )
import TRS.Bind
import TRS.Traced
import Test.QuickCheck
import Control.Monad( liftM2 )

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
 deriving ( Eq, Ord )

data Op = Add | Sub | Gt | IsInt
 deriving ( Eq, Ord, Show )

data Assump
  = NOTHING_HERE_YET
 deriving ( Eq, Ord, Show )

data Effect
  = Fails
  | Succeeds
  | Decides
 deriving ( Eq, Ord )

instance Show Effect where
  show Fails    = "fails"
  show Succeeds = "succeeds"
  show Decides  = "decides"

--------------------------------------------------------------------------------
-- show -- TODO: use pretty printing library

instance Pretty Expr where
  pPrintPrec = pPrintPrecE

pPrintPrecE :: PrettyLevel -> Rational -> Expr -> Doc
pPrintPrecE lvl prec e
  = case e of
       Var x   -> ppr 0    x
       Lit l   -> ppr prec l
       Arr as  -> char '<' <> fsep (punctuate comma (map (ppr 0) as)) <> char '>'
       Lam bnd -> "\\" ++ pprBind bnd
  where
    ppr :: forall a. Pretty a => Rational -> a -> Doc
    ppr = pPrintPrec lvl

pprBind :: Bind 

instance Show Expr where
  show (Var x)       = show x
  show (Int k)       = show k
  show (Arr as)      = "<" ++ concat (intersperse "," (map show as)) ++ ">"
  show (Lam bnd)     = "\\" ++ showBind bnd
  show (Op op)       = show op
  show ((a :=: e1) :>: e2) = show1 a ++ " = " ++ show1 e1 ++ "; " ++ show0 e2
  show (e1 :>: e2)   = show1 e1 ++ "; " ++ show1 e2
  show (e1 :=: e2)   = show1 e1 ++ " = " ++ show1 e2
  show (e1 :|: e2)   = show1 e1 ++ " | " ++ show1 e2
  show (a1 :@: a2)   = show1 a1 ++ "[" ++ show a2 ++ "]"
  show (Exi bnd)     = "exi " ++ showBind bnd
  show Fail          = "fail"
  show (One e)       = "one{" ++ show e ++ "}"
  show (All e)       = "all{" ++ show e ++ "}"
  show (Some a)      = "some(" ++ show a ++ ")"
  show (a :>>: e)    = show1 a ++ "|>" ++ show1 e
  show (Check fx e)  = "check<" ++ show fx ++ ">{" ++ show e ++ "}"
  show (Verify bnds) = error "show Verify undefined"

showBind :: Bind Expr -> String
showBind bnd = show x ++ ". " ++ show e where (x,e) = unsafeUnbind bnd

show0, show1 :: Expr -> String
show0 = showP 0
show1 = showP 1

showP :: Int -> Expr -> String
showP p e | parens e  = "(" ++ show e ++ ")"
          | otherwise = show e
 where
  parens (Lam _)    = True
  parens (_ :>: _)  = 1 <= p
  parens (_ :=: _)  = True
  parens (_ :|: _)  = True
  --parens (_ :@: _)  = True
  parens (Exi _)    = True
  parens (_ :>>: _) = True
  parens _          = False
  
--------------------------------------------------------------------------------
-- values

type Val = Expr

isVal :: Expr -> Bool
isVal (Var x)   = True
isVal e         = isHNF e

isHNF :: Expr -> Bool
isHNF (Int k)   = True
isHNF (Arr es)  = all valid es
isHNF (Lam bnd) = valid e where (_,e) = unsafeUnbind bnd
isHNF (Op op)   = True
isHNF _         = False

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
prep (a :>>: e)    = prepVal a (\v -> v :>>: prep e)
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
-- TODO: new normalization for x=y
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
  recurse (e1 :@: e2)  = [ (s, e1' :@: e2)  | (s,e1') <- everywhere step e1 ]
                      ++ [ (s, e1  :@: e2') | (s,e2') <- everywhere step e2 ]
  recurse (One e)      = [ (s, One e')  | (s,e') <- everywhere step e ]
  recurse (All e)      = [ (s, All e')  | (s,e') <- everywhere step e ]
  recurse (Some e)     = [ (s, Some e') | (s,e') <- everywhere step e ]
  recurse (e1 :>>: e2) = [ (s, e1' :>>: e2)  | (s,e1') <- everywhere step e1 ]
                      ++ [ (s, e1  :>>: e2') | (s,e2') <- everywhere step e2 ]
  recurse (Check fx e) = [ (s, Check fx e') | (s,e') <- everywhere step e ]
  recurse e@(Exi _)    = [ (s, exis <@ body') | (s,body') <- everywhere step body ]
                       where (exis,body) = unExis e
  recurse (Verify bnd) = error "everywhere Verify undefined"
  recurse e            = []

-- treat "exi x1 .. exi xn" as one block when matching
unExis :: Expr -> (Context, Expr)
unExis (Exi bnd) = (Exi (bind x exis), body)
 where
  (x,e)       = unsafeUnbind bnd
  (exis,body) = unExis e
unExis e         = (HOLE, e)

-- structural rules matching
matchExi_alphaRename :: [Ident] -> Expr -> [(Context, Ident, Expr)]
matchExi_alphaRename zs e =
  [ cxe
  | Exi bnd <- [e]
  , let (x,ex) = alphaRename zs bnd
        cxes   = matchExi_alphaRename (x:zs) ex
  , cxe <- -- just add "bind x" to the exis
           [ (Exi (bind x ctx),y,ey)
           | (ctx,y,ey) <- cxes
           ]
           -- add a case where "bind x" is the variable we're matching on
        ++ case cxes of
             [] -> [ (HOLE,x,ex) ]
             _  -> [ (Exi (bind y ctx),x,ey)
                   | (ctx,y,ey) <- [head cxes]
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

-- normalize
normalize :: Rule -> Expr -> Traced Expr
normalize rule e = go (-1) [] e  -- go 99 [] e
 where
  go fuel tr e =
    case rule e of
      []                        -> e :<-- tr
      (s,e'):_ | fuel==0        -> abort "OUT-OF-FUEL"
               | not (valid e') -> abort "INVALID"
               | otherwise      -> go (fuel-1) ((s,e):tr) e'
              where
               abort msg = e' :<-- ((s ++ "-**" ++ msg ++ "**",e):tr)

--------------------------------------------------------------------------------
-- arbitrary

instance Arbitrary Op where
  arbitrary = elements [Add, Sub, Gt, IsInt]

instance Arbitrary Expr where
  arbitrary = sized (arbExprWith xs)
   where
    xs = take 3 (identsNotIn [])

  shrink (Int k)      = [ Int k' | k' <- shrink k ]
  shrink (Op _)       = [ Int 0, Int 1 ]
  shrink (Arr es)     = es
                     ++ [ Arr es' | es' <- shrink es ]
  shrink (Lam bnd)    = shrinkBind Lam bnd
  shrink (e1 :=: e2)  = [ e1, e2 ]
                     ++ [ e1' :=: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :=: e2' | e2' <- shrink e2 ]
  shrink (e1 :>: e2)  = [ e1, e2 ]
                     ++ [ e1' :>: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :>: e2' | e2' <- shrink e2 ]
  shrink (e1 :|: e2)  = [ e1, e2 ]
                     ++ [ e1' :|: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :|: e2' | e2' <- shrink e2 ]
  shrink (e1 :@: e2)  = [ e1, e2 ]
                     ++ [ e1' :@: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :@: e2' | e2' <- shrink e2 ]
  shrink (One e)      = [ e ] ++ [ One e'  | e' <- shrink e ]
  shrink (All e)      = [ e, One e ] ++ [ All e'  | e' <- shrink e ]
  shrink (Some e)     = [ e ] ++ [ Some e' | e' <- shrink e ]
  shrink (e1 :>>: e2) = [ e1, e2 ]
                     ++ [ e1' :>>: e2  | e1' <- shrink e1 ]
                     ++ [ e1  :>>: e2' | e2' <- shrink e2 ]
  shrink (Check fx e) = [ e ] 
                     ++ [ Check fx e' | e' <- shrink e ]
  shrink (Exi bnd)    = shrinkBind Exi bnd
  shrink Fail         = [ Int 0 ]
  --shrink (Verify bnd) = error "shrink Verify undefined"
  shrink e            = []

arbExprWith :: [Ident] -> Int -> Gen Expr
arbExprWith xs n =
  frequency $
  [ (1, Var `fmap` elements xs) | not (null xs) ] ++
  [ (1, Int `fmap` arbitrary)
  , (a, Arr `fmap` arbExprs)
  , (a, Lam `fmap` arbBind)
  , (1, Op  `fmap` arbitrary)
  , (b, liftM2 (:=:) arbExpr2 arbExpr2)
  , (b, liftM2 (:>:) arbExpr2 arbExpr2)
  , (b, liftM2 (:|:) arbExpr2 arbExpr2)
  , (a, liftM2 (:@:) arbExpr2 arbExpr2)
  , (b, Exi `fmap` arbBind)
  , (1, return Fail)
  , (b, One `fmap` arbExpr1)
  , (b, All `fmap` arbExpr1)
{-
  | Some Val
  | Val :>>: Expr    -- guard           |>   <-- black triangle
  | Check Effect Expr
  | Verify (BindList ([Assump],Expr))
-}
  ]
 where
  a = 0 `max` (n `min` 5) -- for bigger values
  b = 0 `max` n           -- for recursive expressions
  arbExpr1 = arbExprWith xs (n-1)
  arbExpr2 = arbExprWith xs (n `div` 2)
  arbExprs = do k <- elements [0,1,2,3,5]
                sequence [ arbExprWith xs (if k <= 1 then n-k else n`div`k)
                         | i <- [1..k]
                         ]
  arbBind  = frequency $
             [ (1, liftM2 bind (elements xs) (arbExprWith xs (n-1))) | not (null xs) ] ++
             [ (4, let x = identNotIn xs in bind x `fmap` arbExprWith (x:xs) (n-1)) ]

shrinkBind :: Arbitrary a => (Bind a -> a) -> Bind a -> [a]
shrinkBind con bnd = [ t ] ++ [ con (bind x t') | t' <- shrink t ]
 where
  (x,t) = unsafeUnbind bnd

instance CoArbitrary Expr where
  coarbitrary = coarbitrary . show -- not completely honest!

--------------------------------------------------------------------------------
-- contexts

type Context = Expr

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

bvs :: Context -> [Ident]
bvs ctx = explore [] ctx
 where
  explore xs (Arr es)     = foldr union [] (map (explore xs) es)
  explore xs (Lam bnd)    = exploreBind xs bnd
  explore xs (e1 :=: e2)  = explore xs e1 `union` explore xs e2
  explore xs (e1 :>: e2)  = explore xs e1 `union` explore xs e2
  explore xs (e1 :|: e2)  = explore xs e1 `union` explore xs e2
  explore xs (e1 :@: e2)  = explore xs e1 `union` explore xs e2
  explore xs (One e)      = explore xs e
  explore xs (All e)      = explore xs e
  explore xs (Some e)     = explore xs e
  explore xs (e1 :>>: e2) = explore xs e1 `union` explore xs e2
  explore xs (Check fx e) = explore xs e
  explore xs (Exi bnd)    = exploreBind xs bnd
  explore xs (Verify bnd) = error "bvs Verify undefined"
  explore xs HOLE         = xs
  explore xs e            = []
  
  exploreBind xs bnd = explore ([x] `union` xs) e where (x,e) = unsafeUnbind bnd

isContext :: Context -> Bool
isContext (Arr es)     = any isContext es
isContext (Lam bnd)    = isContext e where (x,e) = unsafeUnbind bnd
isContext (e1 :=: e2)  = isContext e1 || isContext e2
isContext (e1 :>: e2)  = isContext e1 || isContext e2
isContext (e1 :|: e2)  = isContext e1 || isContext e2
isContext (e1 :@: e2)  = isContext e1 || isContext e2
isContext (One e)      = isContext e
isContext (All e)      = isContext e
isContext (Some e)     = isContext e
isContext (e1 :>>: e2) = isContext e1 || isContext e2
isContext (Check fx e) = isContext e
isContext (Exi bnd)    = isContext e where (x,e) = unsafeUnbind bnd
isContext (Verify bnd) = error "isContext Verify undefined"
isContext HOLE         = True
isContext e            = False

--------------------------------------------------------------------------------

