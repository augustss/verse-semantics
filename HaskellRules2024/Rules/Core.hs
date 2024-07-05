{-# LANGUAGE PatternSynonyms #-}

module Rules.Core
  ( -- The data type itself
    Expr(..), pattern LitInt
  , Effect(..), Assump(..), Ident(..)
  , Lit(..), Ptr, Path(..)
  , isVal, isHNF, isSkolem
  , prep, norm

    -- Rewriting
  , Rule, Context, isContext, (<@)
  , everywhere, normalize

    -- Binding and substitution
  , subst, bvs
  , unbindAs, unExis
  , alphaRename, matchExi_alphaRename, matchEq

    -- Primops
  , PrimOp(..), allPrimOps, primOpString
  ) where

import Prelude hiding( (<>) )
import Epic.Print

import Data.List( union, intersperse )
import TRS.Bind
import TRS.Traced
import Test.QuickCheck

import Control.Monad( liftM2 )
import Data.Scientific(Scientific)

--------------------------------------------------------------------------------
--
--            The main expression datatype
--
--------------------------------------------------------------------------------

data Expr
  -- values
  = Var Ident
  | Lit Lit
  | Arr [Val]
  | Lam (Bind Expr)
  | Op PrimOp

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
  | Val :>>: Expr    -- guard |>   <-- black triangle
  | Check Effect Expr
  | Verify (BindList ([Assump],Expr))

  -- only for contexts
  | HOLE
 deriving ( Eq, Ord )

--------------------------------------------------------------------------------
--
--                 PrimOps
--
--------------------------------------------------------------------------------

data PrimOp
 = -- Operations on integers
   Add | Sub | Mul | Div

   -- Relational
 | Gt | Lt | Eq | NEq

   -- Type tests
 | IsInt | IsStr
 deriving
   ( Eq, Ord, Bounded, Enum, Show )

allPrimOps :: [PrimOp]
allPrimOps = [minBound .. maxBound]

primOpString :: PrimOp -> String
primOpString Add = "+"
primOpString Sub = "-"
primOpString Mul = "*"
primOpString Div = "/"

primOpString Gt  = ">"
primOpString Lt  = "<"
primOpString Eq  = "="
primOpString NEq = "/="

primOpString IsInt = "isInt$"
primOpString IsStr = "isStr$"



--------------------------------------------------------------------------------
--
--                 Literals
--
--------------------------------------------------------------------------------

data Lit
  = LInt Integer            -- d
  | LRat Scientific String  -- d.d
  | LChar Char              -- 'c'
  | LStr String             -- "str"
  | LPath Path              -- /path/to/something
  | LPtr Ptr                -- not a textual literal, just used when translating back.
  deriving (Eq, Ord)

pattern LitInt :: Integer -> Expr
pattern LitInt i = Lit (LInt i)

instance Pretty Lit where
  pPrintPrec l p lit =
    case lit of
      LInt i
        | i >= 0 -> text $ show i
        | otherwise -> maybeParens (p >= 10) $ text $ show i
      LRat r s -> text (show r ++ s)
      LChar c -> text (show c)
      LStr s -> text (show s)
      LPath s -> pPrintPrec l p s
      LPtr ptr -> text ("R#" ++ show ptr)

instance Show Lit where
  show = prettyShow


--------------------------------------------------------
--               Pointers (= refs)
--------------------------------------------------------

type Ptr = Int    -- ToDo: newtype

--------------------------------------------------------
--               Path
--------------------------------------------------------

newtype Path = Path String
  deriving (Eq, Ord, Show)

instance Pretty Path where
  pPrintPrec _ _ (Path s) = text s


--------------------------------------------------------------------------------
--
--                 Assumptions
--
--------------------------------------------------------------------------------

data Assump
  = NOTHING_HERE_YET
 deriving ( Eq, Ord, Show )

instance Pretty Assump where
  pPrint _ = text "[asm]"  -- ToDo

--------------------------------------------------------------------------------
--
--                 Effects
--
--------------------------------------------------------------------------------

data Effect
  = Fails
  | Succeeds
  | Decides
 deriving ( Eq, Ord )

instance Show Effect where
  show Fails    = "fails"
  show Succeeds = "succeeds"
  show Decides  = "decides"

instance Pretty Effect where
  pPrint eff = text (show eff)

--------------------------------------------------------------------------------
--
--                 Pretty-printing
--
--------------------------------------------------------------------------------

instance Pretty Expr where
  pPrintPrec = pPrintPrecE

instance Pretty PrimOp where
  pPrint op = text (primOpString op)

pPrintPrecE :: PrettyLevel -> Rational -> Expr -> Doc
pPrintPrecE lvl prec the_expr
  = case the_expr of
       HOLE       -> text "HOLE"
       Fail       -> text "fail"
       Var x      -> pPrint x
       Lit i      -> pPrint i
       Op op      -> pPrint op

       e1 :=: e2   -> mbPar0 $ ppr1 e1 <+> char '=' <+> ppr1 e2
       e1 :|: e2   -> mbPar0 $ ppr1 e1 <+> char '|' <+> ppr1 e2
       e1 :@: e2   -> ppr1 e1 <> brackets (ppr0 e2)
       e@(_ :>: _) -> sep (punctuate semi $ map ppr1 (gatherSeqs e))
       e1 :>>: e2  -> mbPar0 $ ppr1 e1 <+> text ";;" <+> ppr1 e2

       Arr as  -> char '<' <> fsep (punctuate comma $ map ppr0 as) <> char '>'
       One e   -> text "one" <> braces (ppr0 e)
       All e   -> text "all" <> braces (ppr0 e)
       Lam bnd -> mbPar0 $ char '\\' <> pprBind bnd
       Exi {}  -> mbPar0 $ sep [ text "exi" <+> (fsep (map pPrint bndrs)) <> char '.'
                               , indent (ppr0 body) ]
               where
                  (bndrs, body) = unpackExis the_expr

       Some e  -> text "some" <> parens (ppr0 e)
       Check fx e -> cat [ text "check" <> char '<' <> pPrint fx <> char '>'
                         , indent (braces (ppr0 e)) ]

       Verify bl -> cat [ text "verify" <> parens (sep [ fsep (punctuate comma (map pPrint ids)) <> char ';'
                                                       , fsep (punctuate comma (map pPrint as)) ])
                        , indent (braces (ppr0 body)) ]
           where
             (ids, (as, body)) = alphaRenameVerifyBody (free bl) bl
  where
    ppr0 = pPrintPrecE lvl 0
    ppr1 = pPrintPrecE lvl 1

    mbPar0 = maybeParens (prec > 0)

gatherSeqs :: Expr -> [Expr]
gatherSeqs (e1 :>: e2) = e1 : gatherSeqs e2
gatherSeqs e           = [e]

unpackExis :: Expr -> ([Ident], Expr)
-- Gather up a contiguous block of Exis, alpha-renaming
unpackExis orig_e
  = go (free orig_e) orig_e
  where
    go forb e
      | Exi bnd <- e
      , (bndr, body)   <- alphaRename (free bnd) bnd
      , (bndrs, inner) <- go (bndr:forb) body
      = (bndr:bndrs, inner)
    go _ other = ([], other)


pprBind :: Bind Expr -> Doc
pprBind bnd
  = pPrint bndr <> char '.' <+> pPrint body
  where
    (bndr, body) = alphaRename (free bnd) bnd

instance Show Expr where
  show (Var x)       = show x
  show (Lit k)       = show k
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
  show (Verify {})   = error "show Verify undefined"
  show HOLE          = "HOLE"

showBind :: Bind Expr -> String
showBind bnd = show x ++ ". " ++ show e where (x,e) = unsafeUnbind bnd

show0, show1 :: Expr -> String
show0 = showP 0
show1 = showP 1

showP :: Int -> Expr -> String
showP p e | need_parens e  = "(" ++ show e ++ ")"
          | otherwise      = show e
 where
  need_parens (Lam _)    = True
  need_parens (_ :>: _)  = 1 <= p
  need_parens (_ :=: _)  = True
  need_parens (_ :|: _)  = True
  --need_parens (_ :@: _)  = True
  need_parens (Exi _)    = True
  need_parens (_ :>>: _) = True
  need_parens _          = False

--------------------------------------------------------------------------------
--
--                 Values
--
--------------------------------------------------------------------------------

type Val = Expr

isVal :: Expr -> Bool
isVal (Var _) = True
isVal e       = isHNF e

isHNF :: Expr -> Bool
isHNF (Lit _)   = True
isHNF (Op _)    = True
isHNF (Arr es)  = all valid es
isHNF (Lam bnd) = valid e where (_,e) = unsafeUnbind bnd
isHNF _         = False


--------------------------------------------------------------------------------
--
--                 Valid expressions
--
--------------------------------------------------------------------------------

valid :: Expr -> Bool
-- Checks if an expression is syntactically valid
valid ((a :=: e1) :>: e2) = isVal a && valid e1 && valid e2
valid (e1 :|: e2)         = valid e1 && valid e2
valid (a1 :@: a2)         = isVal a1 && isVal a2
valid (Exi bnd)           = valid e where (_,e) = unsafeUnbind bnd
valid Fail                = True
valid (One e)             = valid e
valid (All e)             = valid e
valid (Some a)            = isVal a
valid (a :>>: e)          = isVal a && valid e
valid (Check _ e)         = valid e
valid (Verify bl)         = valid e where (_, (_as,e)) = unsafeUnbindList bl
valid e                   = isVal e

prep :: Expr -> Expr
-- Valid (prep e) == True
prep (Var x)       = Var x
prep (Lit k)       = Lit k
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
prep (Verify bl)   = Verify (bindList xs (as, prep e))
                     where (xs,(as,e)) = unsafeUnbindList bl
prep HOLE          = error "prep HOLE undefined"

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
--
--                 Variables
--
--------------------------------------------------------------------------------

instance Variables Expr where
  variables _ (Var x)      = [x]
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
  variables f (Check _ e)  = variables f e
  variables f (Exi bnd)    = variables f bnd
  variables f (Verify bnd) = variables f bnd
  variables _ _            = []

instance Variables Assump where
  variables _f NOTHING_HERE_YET = []

isSkolem :: Ident -> Bool
isSkolem (Name ('$':_)) = True
isSkolem _              = False


--------------------------------------------------------------------------------
--
--                 Binders
--
--------------------------------------------------------------------------------

unbindAs :: Ident -> Bind Expr -> Expr
unbindAs x bnd = subst [(y,Var x)] e where (y,e) = unsafeUnbind bnd

alphaRename :: [Ident] -> Bind Expr -> (Ident,Expr)
-- Open up the binding, but avoiding any of the binders in `forb`
alphaRename forb t = alphaRenameBindWith (\x y -> subst [(x,Var y)]) forb t

alphaRenameVerifyBody :: [Ident] -> BindList ([Assump], Expr) -> ([Ident], ([Assump], Expr))
alphaRenameVerifyBody forb bl
  = alphaRenameBindListWith ren forb bl
  where
     ren :: [(Ident,Ident)] -> ([Assump],Expr) -> ([Assump],Expr)
     ren prs (as,e) = (as, subst [(x,Var y) | (x,y) <- prs] e)

-- Sorts binders and renames variables
-- TODO: new normalization for x=y
norm :: Expr -> Expr
norm orig_e = alpha 0 orig_e
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
  alpha _ (Verify {})  = error "alpha Verify undefined"
  alpha _ e            = e

  alphaExi k xs (Exi bnd) = alphaExi k (x:xs) e
   where
    (x,e) = unsafeUnbind bnd

  alphaExi k xs e = exis (map snd tab) (subst [(x,Var y)|(x,y)<-tab] e')
   where
    n   = length xs
    e'  = alpha (k+n) e
    ys  = free e'
    tab = filter (`elem` xs) ys `zip` [ var i | i <- [k..] ]

    exis []     e2 = e2
    exis (z:zs) e2 = Exi (bind z (exis zs e2))

--------------------------------------------------------------------------------
--
--            Substitution
--
--------------------------------------------------------------------------------

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
subst sub (Verify bl)  = Verify (substBinds Var substVerify sub bl)
subst _   e            = e

substVerify :: Subst Expr -> ([Assump],Expr) -> ([Assump],Expr)
substVerify sub (as,e) = (as, subst sub e)   -- ToDo: fix me.   Sadly (Subst Expr) is not really what we want

--------------------------------------------------------------------------------
--
--            Rewriting
--
--------------------------------------------------------------------------------

type Rule = Expr -> [(String,Expr)]

-- apply a rule everywhere (recursively) in the expression
everywhere :: Rule -> Rule
everywhere step orig_e = step orig_e ++ recurse orig_e
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
  recurse (Verify bl)  = [ (s, Verify (bindList xs (as,e')))  | (s,e') <- everywhere step e ]
                       where
                         (xs,(as,e)) = unsafeUnbindList bl
  recurse _            = []

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
normalize rule orig_e = go (-1) [] orig_e  -- go 99 [] e
 where
  go :: Int -> [(String,Expr)] -> Expr -> Traced Expr
  go fuel tr e =
    case rule e of
      []                        -> e :<-- tr
      (s,e'):_ | fuel==0        -> abort "OUT-OF-FUEL"
               | not (valid e') -> abort "INVALID"
               | otherwise      -> go (fuel-1) ((s,e):tr) e'
              where
               abort msg = e' :<-- ((s ++ "-**" ++ msg ++ "**",e):tr)


--------------------------------------------------------------------------------
--
--            Arbitrary
--
--------------------------------------------------------------------------------

instance Arbitrary PrimOp where
  arbitrary = elements allPrimOps

instance Arbitrary Expr where
  arbitrary = sized (arbExprWith xs)
   where
    xs = take 3 (identsNotIn [])

  shrink (LitInt k) = [ LitInt k' | k' <- shrink k ]  -- ToDo: other literals

  shrink (Op _)       = [ LitInt 0, LitInt 1 ]   -- ToDo: explain

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
  shrink Fail         = [ LitInt 0 ]
  --shrink (Verify bnd) = error "shrink Verify undefined"
  shrink _            = []

arbExprWith :: [Ident] -> Int -> Gen Expr
arbExprWith xs n =
  frequency $
  [ (1, Var `fmap` elements xs) | not (null xs) ] ++
  [ (1, LitInt `fmap` arbitrary)
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
                         | _ <- [1..k]
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
--
--            Contexts
--
--------------------------------------------------------------------------------

type Context = Expr

(<@) :: Context -> Expr -> Expr
-- (C <@ e) fills the hole in C with e. Often written C[e]
Arr as        <@ h = Arr (map (<@ h) as)
Lam bnd       <@ h = Lam (bind x (e <@ h)) where (x,e) = unsafeUnbind bnd
(e1 :>: e2)   <@ h = (e1 <@ h) :>: (e2 <@ h)
(e1 :=: e2)   <@ h = (e1 <@ h) :=: (e2 <@ h)
(e1 :|: e2)   <@ h = (e1 <@ h) :|: (e2 <@ h)
(e1 :@: e2)   <@ h = (e1 <@ h) :@: (e2 <@ h)
Exi bnd       <@ h = Exi (bind x (e <@ h)) where (x,e) = unsafeUnbind bnd
One e         <@ h = One (e <@ h)
All e         <@ h = All (e <@ h)
Some e        <@ h = Some (e <@ h)
(e1 :>>: e2)  <@ h = (e1 <@ h) :>>: (e2 <@ h)
Check fx e    <@ h = Check fx (e <@ h)
e@(Verify {}) <@ _ = e   -- No HOLE inside Verify. ToDo: check
HOLE          <@ h = h
e             <@ _ = e

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
  explore xs (Check _ e)  = explore xs e
  explore xs (Exi bnd)    = exploreBind xs bnd
  explore _  (Verify {})  = error "bvs Verify undefined"
  explore xs HOLE         = xs
  explore _xs _e          = []

  exploreBind xs bnd = explore ([x] `union` xs) e where (x,e) = unsafeUnbind bnd

isContext :: Context -> Bool
-- There is a HOLE, outside a Verify (ToDo: is the "outside Verify" right?
isContext (Arr es)     = any isContext es
isContext (Lam bnd)    = isContext e where (_,e) = unsafeUnbind bnd
isContext (e1 :=: e2)  = isContext e1 || isContext e2
isContext (e1 :>: e2)  = isContext e1 || isContext e2
isContext (e1 :|: e2)  = isContext e1 || isContext e2
isContext (e1 :@: e2)  = isContext e1 || isContext e2
isContext (One e)      = isContext e
isContext (All e)      = isContext e
isContext (Some e)     = isContext e
isContext (e1 :>>: e2) = isContext e1 || isContext e2
isContext (Check _ e)  = isContext e
isContext (Exi bnd)    = isContext e where (_,e) = unsafeUnbind bnd
isContext (Verify {})  = False
isContext HOLE         = True
isContext _            = False


