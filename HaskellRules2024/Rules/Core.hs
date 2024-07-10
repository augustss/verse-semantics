{-# LANGUAGE PatternSynonyms #-}

module Rules.Core
  ( -- The data type itself
    Expr(..), pattern LitInt
  , Ident(..)
  , Lit(..), Ptr, Path(..)
  , isVal, isHNF, isSkolem
  , prep, norm
  , pPrintSmallExpr

    -- Assupmtions
  , Assump(..), GroundVal(..), isPosAssump

    -- Rewriting
  , Rule, Context, isContext, (<@)
  , everywhere, normalize
  , RuleEnv(..), extendRuleEnv

    -- Binding and substitution
  , subst, bvs
  , unbindAs, unExis
  , alphaRename, matchExi_alphaRename, matchEq
  , alphaRenameVerify

    -- Effects
  , Effect(..), canSucceed, canFail

  -- Primops
  , PrimOp(..), allPrimOps, primOpString, primOpCanFail
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
 | Gt | Lt | NEq | GEq | LEq

   -- Type tests
 | IsInt | IsStr
 deriving
   ( Eq, Ord, Bounded, Enum, Show )

allPrimOps :: [PrimOp]
allPrimOps = [minBound .. maxBound]

primOpString :: PrimOp -> String
primOpString Add = "intAdd$"
primOpString Sub = "intSub$"
primOpString Mul = "intMul$"
primOpString Div = "intDiv$"

primOpString Gt  = "intGT$"
primOpString GEq = "intGE$"
primOpString Lt  = "intLT$"
primOpString LEq = "intLE$"
primOpString NEq = "intNE$"

primOpString IsInt = "isInt$"
primOpString IsStr = "isStr$"

primOpCanFail :: PrimOp -> Bool
primOpCanFail Gt    = True
primOpCanFail Lt    = True
primOpCanFail NEq   = True
primOpCanFail GEq   = True
primOpCanFail LEq   = True
primOpCanFail IsInt = True
primOpCanFail IsStr = True

primOpCanFail Add = False
primOpCanFail Sub = True
primOpCanFail Mul = True
primOpCanFail Div = True

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

data GroundVal = GVVar Ident
               | GVLit Lit
               | GVArr [GroundVal]
  deriving( Eq, Ord, Show )

data Assump
  = A_GVEq Ident GroundVal            -- r = gv
  | A_PrimOp Ident PrimOp GroundVal   -- r = op[gv]
  | A_Fails Assump                    -- not( a )
 deriving ( Eq, Ord, Show )

instance Pretty Assump where
  pPrint (A_GVEq i gv)      = pPrint i <+> text "="  <+> pPrint gv
  pPrint (A_PrimOp i op gv) = pPrint i <+> text "=" <+> pPrint op <> brackets (pPrint gv)
  pPrint (A_Fails a)        = text "not" <> parens (pPrint a)

instance Pretty GroundVal where
  pPrint (GVVar i)   = pPrint i
  pPrint (GVLit l)   = pPrint l
  pPrint (GVArr gvs) = char '<' <> fsep (punctuate comma $ map pPrint gvs) <> char '>'

isPosAssump :: Assump -> Bool
isPosAssump (A_GVEq {})   = True
isPosAssump (A_PrimOp {}) = True
isPosAssump (A_Fails {})  = False


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

canSucceed :: Effect -> Bool
-- True if one result is acceptable
canSucceed Succeeds = True
canSucceed Decides  = True
canSucceed Fails    = False

canFail :: Effect -> Bool
-- True if no results is acceptable
canFail Succeeds = False
canFail Decides  = True
canFail Fails    = True

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
       Op op      -> char '!' <> pPrint op

       e1 :=: e2   -> mbPar0 $ ppr1 e1 <+> char '=' <+> ppr1 e2
       e1 :|: e2   -> mbPar0 $ ppr1 e1 <+> char '|' <+> ppr1 e2
       e1 :@: e2   -> ppr1 e1 <> brackets (pp_call_arg e2)
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
             (ids, (as, body)) = alphaRenameVerify (free bl) bl
  where
    ppr0 = pPrintPrecE lvl 0
    ppr1 = pPrintPrecE lvl 1

    mbPar0 = maybeParens (prec > 0)

    -- Reduce clutter: omit the angle brackets for a multi-arg call.
    -- That is, print f[3,2] rather than f[<3,2>]
    pp_call_arg (Arr es) = fsep (punctuate comma $ map ppr0 es)
    pp_call_arg e2       = ppr0 e2

pPrintSmallExpr :: Expr -> Doc
-- Show only a small expression; otherwise return "<big>"
pPrintSmallExpr e
  | exprSize e < 10 = pPrint e
  | otherwise       = text "<big>"

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
--                 Size
--
--------------------------------------------------------------------------------

exprSize :: Expr -> Int
exprSize (Var {})      = 1
exprSize (Lit {})      = 1
exprSize (Op {})       = 1
exprSize Fail          = 1
exprSize HOLE          = 1
exprSize (Arr as)      = 1 + sum (map exprSize as)
exprSize (Lam bnd)     = 1 + bindSize bnd
exprSize (e1 :>: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :=: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :|: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :@: e2)   = 1 + exprSize e1 + exprSize e2
exprSize (e1 :>>: e2)  = 1 + exprSize e1 + exprSize e2
exprSize (Exi bnd)     = 1 + bindSize bnd
exprSize (One e)       = 1 + exprSize e
exprSize (All e)       = 1 + exprSize e
exprSize (Some a)      = 1 + exprSize a
exprSize (Check _ e)  = 1 + exprSize e
exprSize (Verify bl)   = 10 + exprSize e
                       where
                         (_rs,(_as,e)) = unsafeUnbindList bl

bindSize :: Bind Expr -> Int
bindSize bnd = exprSize (snd (unsafeUnbind bnd))

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
isHNF (Lit {}) = True
isHNF (Op {})  = True
isHNF (Arr es) = all isVal es   -- ToDo: This had 'valid' stuff too, strangely
isHNF (Lam {}) = True           -- valid e where (_,e) = unsafeUnbind bnd
                                -- ToDo: why valid????
isHNF _        = False


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
prep (e1 :>: e2)   = prepSeq e1 e2
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
prepSeq (a :=: e1) e2 = prepVal a (\v -> (v :=: prep e1) :>: prep e2)
prepSeq e1         e2 = (Var underscore :=: prep e1) :>: prep e2

prepVal :: Expr -> (Val -> Expr) -> Expr
-- (prepVal e K) makes applies K to the value of e,
-- perhaps by adding an existential, thus (exi x. x = e; K[x])
prepVal a k
  | isVal pa  = k pa
  | otherwise = Exi (bind x ((Var x :=: pa) :>: k (Var x)))
 where
  pa = prep a
  x  = identNotIn (free (k pa))  -- UGH!  ToDo: quadratic in prepVals

prepVals :: [Expr] -> ([Val] -> Expr) -> Expr
prepVals []     f = f []
prepVals (a:as) f = prepVal a (\v -> prepVals as (f . (v:)))

--------------------------------------------------------------------------------
--
--                 Variables
--
--------------------------------------------------------------------------------

instance Variables Expr where
  variables f (Var x)      = variables f x
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
  variables f (A_GVEq i gv)       = [i] `union` variables f gv
  variables f (A_PrimOp i _ gv)   = [i] `union` variables f gv
  variables f (A_Fails a)         = variables f a

instance Variables GroundVal where
  variables _f (GVVar i)   = [i]
  variables _f (GVLit {})  = []
  variables f  (GVArr gvs) = variables f gvs

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

alphaRenameVerify :: [Ident] -> BindList ([Assump], Expr) -> ([Ident], ([Assump], Expr))
alphaRenameVerify forb bl
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
  skvar i = ident ("_r" ++ show i)

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
  alpha k (Verify bl)  = let (rs, (as,e)) = unsafeUnbindList bl
                             rs' = map skvar [k+1..k+n]
                             n   = length rs
                             sub = rs `zip` rs'
                             e'  = alpha (k+n) e
                         in Verify (bindList rs' (map (substAssump sub) as, substSkol sub e'))
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
-- Domain of substitution does not include skolems
subst sub orig_e = go orig_e
  where
    go (Var x)      = head $ [e | (y,e) <- sub, y == x] ++ [Var x]
    go (Arr es)     = Arr (map go es)
    go (Lam bnd)    = Lam (substBind Var subst sub bnd)
    go (e1 :=: e2)  = go e1 :=: go e2
    go (e1 :>: e2)  = go e1 :>: go e2
    go (e1 :|: e2)  = go e1 :|: go e2
    go (e1 :@: e2)  = go e1 :@: go e2
    go (One e)      = One (go e)
    go (All e)      = All (go e)
    go (Some e)     = Some (go e)
    go (e1 :>>: e2) = go e1 :>>: go e2
    go (Check fx e) = Check fx (go e)
    go (Exi bnd)    = Exi    (substBind  Var subst     sub bnd)
    go (Verify bl)  = Verify (substBinds Var go_verify sub bl)
    go e            = e

    go_verify :: Subst Expr -> ([Assump],Expr) -> ([Assump],Expr)
    go_verify sub' (as,e) = (as, subst sub' e)

substSkol :: Subst Ident -> Expr -> Expr
-- Domain of substitution is skolem variables; range is just an identifier
substSkol sub orig_e = go orig_e
  where
    go (Var x)      = Var (head $ [e | (y,e) <- sub, y == x] ++ [x])
    go (Arr es)     = Arr (map go es)
    go (Lam bnd)    = Lam (substBind id substSkol sub bnd)
    go (e1 :=: e2)  = go e1 :=: go e2
    go (e1 :>: e2)  = go e1 :>: go e2
    go (e1 :|: e2)  = go e1 :|: go e2
    go (e1 :@: e2)  = go e1 :@: go e2
    go (One e)      = One (go e)
    go (All e)      = All (go e)
    go (Some e)     = Some (go e)
    go (e1 :>>: e2) = go e1 :>>: go e2
    go (Check fx e) = Check fx (go e)
    go (Exi bnd)    = Exi    (substBind  id substSkol sub bnd)
    go (Verify bl)  = Verify (substBinds id go_verify sub bl)
    go e            = e

    go_verify :: Subst Ident -> ([Assump],Expr) -> ([Assump],Expr)
    go_verify sub' (as,e) = (map (substAssump sub') as, substSkol sub' e)

substAssump :: Subst Ident -> Assump -> Assump
substAssump sub (A_GVEq x gv)      = A_GVEq (lookupIdSubst sub x) (substGV sub gv)
substAssump sub (A_PrimOp x op gv) = A_PrimOp (lookupIdSubst sub x) op (substGV sub gv)
substAssump sub (A_Fails asm)      = A_Fails (substAssump sub asm)

substGV :: Subst Ident -> GroundVal -> GroundVal
substGV sub (GVVar x)  = GVVar (lookupIdSubst sub x)
substGV _   (GVLit l)  = GVLit l
substGV sub (GVArr vs) = GVArr (map (substGV sub) vs)

lookupIdSubst :: Subst Ident -> Ident -> Ident
lookupIdSubst sub x = head $ [y | (x',y) <- sub, x==x']

--------------------------------------------------------------------------------
--
--            Rewriting
--
--------------------------------------------------------------------------------

data RuleEnv = RE { skolVars :: [Ident], assumps :: [Assump] }

emptyRuleEnv :: RuleEnv
emptyRuleEnv = RE { skolVars = [], assumps = [] }

extendRuleEnv :: RuleEnv -> [Ident] -> [Assump] -> RuleEnv
extendRuleEnv rule_env@(RE { skolVars = skols, assumps = asms }) new_skols new_asms
  = rule_env { skolVars = new_skols ++ skols, assumps = new_asms ++ asms }

type Rule = RuleEnv -> Expr -> [(String,Expr)]

stepRule :: Rule -> Expr -> [(String,Expr)]
stepRule rule expr = rule emptyRuleEnv     -- Empty set of skolems
                          expr

-- apply a rule everywhere (recursively) in the expression
everywhere :: Rule -> Rule
everywhere step env orig_e = step env orig_e ++ recurse orig_e
 where
  recurse (Arr es)     = [ (s, Arr (take i es ++ [e'] ++ drop (i+1) es))
                         | i <- [0..length es-1]
                         , (s,e') <- everywhere step env (es!!i)
                         ]
  recurse (Lam bnd)    = [ (s, Lam (bind x e')) | (s,e') <- everywhere step env e ]
                       where (x,e) = unsafeUnbind bnd
  recurse (e1 :=: e2)  = [ (s, e1' :=: e2)  | (s,e1') <- everywhere step env e1 ]
                      ++ [ (s, e1  :=: e2') | (s,e2') <- everywhere step env e2 ]
  recurse (e1 :>: e2)  = [ (s, e1' :>: e2)  | (s,e1') <- everywhere step env e1 ]
                      ++ [ (s, e1  :>: e2') | (s,e2') <- everywhere step env e2 ]
  recurse (e1 :|: e2)  = [ (s, e1' :|: e2)  | (s,e1') <- everywhere step env e1 ]
                      ++ [ (s, e1  :|: e2') | (s,e2') <- everywhere step env e2 ]
  recurse (e1 :@: e2)  = [ (s, e1' :@: e2)  | (s,e1') <- everywhere step env e1 ]
                      ++ [ (s, e1  :@: e2') | (s,e2') <- everywhere step env e2 ]
  recurse (One e)      = [ (s, One e')  | (s,e') <- everywhere step env e ]
  recurse (All e)      = [ (s, All e')  | (s,e') <- everywhere step env e ]
  recurse (Some e)     = [ (s, Some e') | (s,e') <- everywhere step env e ]
  recurse (e1 :>>: e2) = [ (s, e1' :>>: e2)  | (s,e1') <- everywhere step env e1 ]
                      ++ [ (s, e1  :>>: e2') | (s,e2') <- everywhere step env e2 ]
  recurse (Check fx e) = [ (s, Check fx e') | (s,e') <- everywhere step env e ]
  recurse e@(Exi _)    = [ (s, exis <@ body') | (s,body') <- everywhere step env body ]
                       where (exis,body) = unExis e
  recurse (Verify bl)  = [ (s, Verify (bindList rs (as,e')))
                         | (s,e') <- everywhere step env' e ]
                       where
                         env' = extendRuleEnv env rs as
                         (rs,(as,e)) = unsafeUnbindList bl   -- ToDo: is unsafe ok? I think not
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
-- Matches (v = e), and also (v1 = v2) returning (v2 = v1)
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
    case stepRule rule e of
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


