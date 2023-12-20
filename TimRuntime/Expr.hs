{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Expr where
import Prelude hiding ((<>))
import qualified Prelude as P
import Control.Arrow(first)
import Control.Monad
import Data.Char
--import Data.List
import qualified Data.Set as S
import Data.Set(Set)
import TRS.TRS
import TRS.Traced hiding (trace)
import Epic.Print
import Debug.Trace

-- Fixes:
--   * G never shrinks
--   * stop infinite rule repetition
--     by test for existing assumption
--   * tuple-expand has no j
--   * tuple-expand adds unknown assumptions
--   * tuple-elim-choose needs to restrict i as well
--     could also demand that i<-n is not an assumption
-- 

{-
? axiom
no operation
program-intro
fx-weaken
sequence-intro
sequence-refine-fx
* exists-intro
* atom-intro
* tuple-intro
* unify-head-left
* unify-head-right
* unify-disjoint
* tuple-flex
* tuple-unify-length
* tuple-unify-element
* tuple-expand
* tuple-elim-succeed
* tuple-elim-fail
tuple-elim-choose
lambda-intro
lambda-elim
choice-intro
choice-elim
iterate-intro
iterate-refine-fx
iterate-elim-done
iterate-elim-skip
iterate-elim-sat
cond-false
cond-true
read-intro
read-elim
write-intro
write-elim
interact-intro
interact-elim

-}

------------------------------

type Ident = String
newtype VarIdent = VarIdent Ident
  deriving (Show, Eq, Ord)
newtype TupleIdent = TupleIdent Ident
  deriving (Show, Eq, Ord)
newtype DecisionIdent = DecisionIdent Ident
  deriving (Show, Eq, Ord)
newtype PointerIdent = PointerIdent Ident
  deriving (Show, Eq, Ord)

newtype Number = Number Integer         -- n ::== 0 | 1 | ..
  deriving (Show, Eq, Ord)

data Atom                               -- a ::== n | () | Path | ..
  = ANumber Number
  | AEmpty
  | APath String
  deriving (Show, Eq, Ord)

data Var                                -- ::== i | j | k | f | g | h | w | x | y | z | t[n]
  = Var VarIdent
  | VarT TupleIdent
  | VTuple TupleIdent Number
  deriving (Show, Eq, Ord)

type DecisionVar = DecisionIdent        -- ::= d | e

type PointerVar = PointerIdent          -- ::= p | q

data EffectSpecifier                    -- fx ::=
  = Succeeds | Fails | Satisfies | Abstracts
  | Imperatives | Interacts | Throws | Reads | Writes
  | Pendings | InteractsPending | ThrowsPending | ReadsPending | WritesPending
  deriving (Show, Eq, Ord)

data Head                               -- h ::=
  = HAtom Atom                          -- a
  | HTuple Var Var                      -- tuple(x) z     -- x is the size, z is base the name of elements
  | HLambda Var Var Op                  -- lambda i z. op
  deriving (Show, Eq, Ord)

data OpEqRhs
  = OEHead  Head                   -- x=h
  | OEVar   Var                    -- x=y
  | OEFail                         -- x=fail
  | OEApply Var Var                -- x=y[z]
  | OEDeref PointerVar             -- x=y^     ??y should be p??
  deriving (Show, Eq, Ord)

pattern OEqHead :: Var -> Head -> Op
pattern OEqHead  x h   = OEq x (OEHead h)
pattern OEqVar :: Var -> Var -> Op
pattern OEqVar   x v   = OEq x (OEVar v)
pattern OEqFail :: Var -> Op
pattern OEqFail  x     = OEq x OEFail
pattern OEqApply :: Var -> Var -> Var -> Op
pattern OEqApply x f a = OEq x (OEApply f a)
pattern OEqDeref :: Var -> PointerVar -> Op
pattern OEqDeref x p   = OEq x (OEDeref p)

data Op                                 -- op ::=
  = OEq      Var OpEqRhs                -- x = ...
  | OSet     PointerVar Var             -- set x=y
  | OChoice  Op    Op                   -- op0 | op1
  | OSeq     Op    Op                   -- op0; op1
  | OExists  [Var]                      -- exists xs
  | OIterate Var DecisionVar Op (Var, Var, Op) (Var, Op) -- iterate(v) d. op0 then w1 w2. op1 else w3. op2
  | ONop                                -- nop
  | OPrint   Var                        -- print(x)
  deriving (Show, Eq, Ord)

data Program                            -- P ::=
  = Program DecisionVar Op              --   program d. op
  deriving (Show, Eq, Ord)

{-
data ScopeContext op                    -- sc[OP] ::=
  = SCOP op                             --   OP
  | SCOPop (ScopeContext op) Op         --   sc[OP]; op
  | SCopOP Op (ScopeContext op)         --   op; sc[OP]
  deriving (Show, Eq, Ord)

data DecisionStart d op                 -- ds[D,OP]
  = DSProgram d Op Var op               --   program D opv. OP  ??opv??
  | DSIterate Var d op (Var, Var, Op) (Var, Op) --   iterate(v) D.     OP then w1 w2. op1 else w3. op2
  deriving (Show, Eq, Ord)

data DecisionContext d op                   -- dc[D,OP] ::=
  = DC1 (DecisionStart d (ScopeContext op)) --   ds[D,sc[OP]]
  | DC2 (DecisionStart DecisionVar (ScopeContext (DecisionContext d op))) -- ds[e,sc[dc[D,OP]]]
  deriving (Show, Eq, Ord)
-}

data Assumption                             -- A ::=
  = AFX (Set EffectSpecifier) Op            --   fx{op}  # operation op has effects fx here
  | AFlex Var DecisionVar                   --   x@d     # variable x is flexible in decision context identified by d
  | AUnifyHead Var Head                     --   x<-h    # variable x is flexibly unified with h
  | AUnifyVar  Var Var                      --   x<-y    # variable x is flexibly unified with y
  | ARead PointerVar Var                    --   p:=x    # reading pointer p yields value x
  | ADone DecisionVar                       --   d:=done # no more iterations of decision context
  | AOp DecisionVar Op                      --   d:=op   # next iteration of decision context identified by d runs op
  deriving (Show, Eq, Ord)

type AssumptionSet = Set Assumption

data Fx a = Fx (Set EffectSpecifier) a
  deriving (Show, Eq, Ord)

data Config = AssumptionSet :- Fx Program | Done
  deriving (Show, Eq, Ord)

--------------------------------------------------------

ppIdent :: PrettyLevel -> Rational -> Ident -> Doc
ppIdent _ _ i = text i

instance Pretty VarIdent where
  pPrintPrec l p (VarIdent i) = ppIdent l p i
instance Pretty TupleIdent where
  pPrintPrec l p (TupleIdent i) = ppIdent l p i
instance Pretty DecisionIdent where
  pPrintPrec l p (DecisionIdent i) = ppIdent l p i
instance Pretty PointerIdent where
  pPrintPrec l p (PointerIdent i) = ppIdent l p i

instance Pretty Number where
  pPrintPrec l p (Number i) = pPrintPrec l p i

instance Pretty Atom where
  pPrintPrec l p (ANumber n) = pPrintPrec l p n
  pPrintPrec _ _ AEmpty = text "()"
  pPrintPrec _ _ (APath q) = text q

instance Pretty EffectSpecifier where
  pPrintPrec _ _ e = text $ take 3 $ map toLower $ show e

instance Pretty Head where
  pPrintPrec l p (HAtom a) = pPrintPrec l p a
  pPrintPrec l _ (HTuple x z) = text "tuple" <> parens (pPrintPrec l 0 x) <> pPrintPrec l 10 z
  pPrintPrec l p (HLambda i z op) = maybeParens (p > 0) $ text "lambda" <+> pPrintPrec l 0 i <+> pPrintPrec l 0 z <> text "." <+> pPrintPrec l 10 op

instance Pretty Var where
  pPrintPrec l _ (Var v) = pPrintPrec l 0 v
  pPrintPrec l _ (VarT v) = braces $ pPrintPrec l 0 v
  pPrintPrec l _ (VTuple i n) = pPrintPrec l 0 i <> brackets (pPrintPrec l 0 n)

instance Pretty Op where
  pPrintPrec l p (OEqHead v h) = maybeParens (p > 5) $ pPrintPrec l 0 v <> text "=" <> pPrintPrec l 5 h
  pPrintPrec l p (OEqVar  v h) = maybeParens (p > 5) $ pPrintPrec l 0 v <> text "=" <> pPrintPrec l 0 h
  pPrintPrec l p (OEqFail v  ) = maybeParens (p > 5) $ pPrintPrec l 0 v <> text "=fail"
  pPrintPrec l p (OEqApply x y z) = maybeParens (p > 5) $ pPrintPrec l 0 x <> text "=" <> pPrintPrec l 0 y <> brackets (pPrintPrec l 0 z)
  pPrintPrec l p (OEqDeref v q) = maybeParens (p > 5) $ pPrintPrec l 0 v <> text "=" <> pPrintPrec l 0 q <> text "^"
  pPrintPrec _ _ (OEq _ _) = undefined -- can't happen
  pPrintPrec l p (OSet q v) = maybeParens (p > 5) $ text "set " <> pPrintPrec l 0 q <> text "=" <> pPrintPrec l 0 v
  pPrintPrec l p (OChoice op1 op2) = maybeParens (p > 4) $ pPrintPrec l 4 op1 <+> text "|" <+> pPrintPrec l 4 op2
  pPrintPrec l p (OSeq op1 op2) = maybeParens (p > 3) $ pPrintPrec l 3 op1 <> text ";" <+> pPrintPrec l 3 op2
  pPrintPrec l p (OExists xs) = maybeParens (p > 5) $ text "exists" <+> hsep (map (pPrintPrec l 0) xs)
  pPrintPrec l p (OIterate v d op0 (w1,w2,op1) (w3,op2)) = maybeParens (p > 2) $
    text "iterate" <+> parens (pPrintPrec l 0 v) <+> pPrintPrec l 0 d <> text "." <> pPrintPrec l 2 op0 <+>
    text "then" <+> pPrintPrec l 0 w1 <+> pPrintPrec l 0 w2 <> text "." <+> pPrintPrec l 2 op1 <+>
    text "else" <+> pPrintPrec l 0 w3 <+> pPrintPrec l 2 op2
  pPrintPrec _ _ ONop = text "nop"
  pPrintPrec l _ (OPrint v) = text "print" <> parens (pPrintPrec l 0 v)

instance Pretty Program where
  pPrintPrec l _ (Program d op) = text "program" <+> pPrintPrec l 0 d <> text "." <+> pPrintPrec l 0 op

ppFx :: PrettyLevel -> Set EffectSpecifier -> Doc
ppFx _ sfx | sfx == topfx = text "topfx"
ppFx l sfx =
  case S.toList sfx of
    [fx] -> pPrintPrec l 0 fx
    fxs -> parens $ hsep $ punctuate (text ",") $ map (pPrintPrec l 0) fxs

instance Pretty Assumption where
  pPrintPrec l _ (AFX fx op) = ppFx l fx <> braces (pPrintPrec l 0 op)
  pPrintPrec l _ (AFlex v d) = pPrintPrec l 0 v <> text "@" <> pPrintPrec l 0 d
  pPrintPrec l _ (AUnifyHead v h) = pPrintPrec l 0 v <> text "<-" <> pPrintPrec l 0 h
  pPrintPrec l _ (AUnifyVar v h) = pPrintPrec l 0 v <> text "<-" <> pPrintPrec l 0 h
  pPrintPrec l _ (ARead p v) = pPrintPrec l 0 p <> text ":=" <> pPrintPrec l 0 v
  pPrintPrec l _ (ADone d) = pPrintPrec l 0 d <> text ":=done"
  pPrintPrec l _ (AOp d op) = pPrintPrec l 0 d <> text ":=" <> pPrintPrec l 10 op

instance Pretty a => Pretty (Fx a) where
  pPrintPrec l _ (Fx fx a) = ppFx l fx <> braces (pPrintPrec l 0 a)

instance Pretty Config where
  pPrintPrec l _ (as :- p) = pPrintPrec l 0 (S.toList as) <+> text "|-" <+> pPrintPrec l 0 p
  pPrintPrec _ _ Done = text "Done"

--------------------------------------------------------

startConfig :: Op -> Config
startConfig op = S.empty :- Fx topfx (Program d op)
  where d = DecisionIdent "dt"

topfx :: S.Set EffectSpecifier
topfx = S.fromList [Succeeds, Reads, Writes, Interacts]

--    G.IsKnown[x]              ::== G, x<-a or
--                                   G, x<-tuple(i) t, i<-n1] and for all 0<=n0<n1, G.IsKnown[t[n0]].

allowAfter :: EffectSpecifier -> Set EffectSpecifier
allowAfter _fx = undefined

{-
bvs :: Op -> Set Var
bvs (OEqHead _ (HTuple _ t)) = S.singleton t                 -- BVS(x=tuple(i) t)                                   ---> {t}
bvs (OChoice op0 op1) = bvs op0 `S.union` bvs op1            -- BVS(op0|op1)                                        ---> BVS(op0)+BVS(op1)
bvs (OSeq op0 op1) = bvs op0 `S.union` bvs op1               -- BVS(op0; op1)                                       ---> BVS(op0)+BVS(op1)
bvs (OExists xs) = S.fromList xs                             -- BVS(exists xs)                                      ---> {xs}
bvs (OIterate _ d op0 _ _) = S.singleton d `S.union` bvs op0 -- BVS(iterate(v) d. op0 then w1 w2. op1 else w1. op2) ---> {d}+BVS(op0)
                                                             -- BVS(cond(x) y. op0)                                 ---> {y}+BVS(op0)
bvs _ = S.empty                                              -- BVS(any other operation op)                         ---> {}
-}

type Context a = a -> [(a -> a, a)]

scopeContext :: Context Op
scopeContext lhs = scopeContext1 lhs ++ [(id, lhs)]

scopeContext1 :: Context Op
scopeContext1 lhs =
    do OSeq op1 op2 <- [lhs]
       (ctx, hole) <- scopeContext op1
       pure ((`OSeq` op2) . ctx, hole)
  ++
    do OSeq op1 op2 <- [lhs]
       (ctx, hole) <- scopeContext op2
       pure ((op1 `OSeq`) . ctx, hole)

class DecisionStart a where
  decisionStart :: a -> [(DecisionVar -> Op -> a, DecisionVar, Op)]
instance DecisionStart Program where decisionStart = decisionStartProgram
instance DecisionStart Op where decisionStart = decisionStartOp

decisionStartProgram :: Program -> [(DecisionVar -> Op -> Program, DecisionVar, Op)]
decisionStartProgram lhs =
    do Program d op <- [lhs]
       pure (Program, d, op)

decisionStartOp :: Op -> [(DecisionVar -> Op -> Op, DecisionVar, Op)]
decisionStartOp lhs =
    do OIterate v d op thn els <- [lhs]
       pure (\ d' op' -> OIterate v d' op' thn els, d, op)

decisionContext :: DecisionStart a => a -> [(DecisionVar -> Op -> a, DecisionVar, Op)]
decisionContext lhs =
    do (ctxds, d, sc) <- decisionStart lhs
       (ctxsc, op)    <- scopeContext sc
       pure (\ d' op' -> ctxds d' (ctxsc op'), d, op)
  ++
    do (ctxds, e, sc) <- decisionStart lhs
       (ctxsc, dc)    <- scopeContext sc
       (ctxdc, d, op) <- decisionContext dc
       pure (\ d' op' -> ctxds e (ctxsc (ctxdc d' op')), d, op)

assumeContext :: Set Assumption -> [(Set Assumption, Assumption)]
assumeContext = map (first S.fromList) . listCtx . S.toList

listCtx :: [a] -> [([a], a)]
listCtx [] = []
listCtx (a:as) = (as, a) : map (first (a:)) (listCtx as)

allOps :: Program -> [Op]
allOps a =
  let (_, _, op) = head $ decisionStartProgram a
      flat (OSeq op1 op2) = flat op1 ++ flat op2
      flat o = [o]
  in  flat op


instance Rec Config where
  data RuleEnv Config = None
  rec r s ae = r s ae

sys :: TRSystem Config
sys = TRSystem {
  sname = "TimRun", description = "Tim's runtime rules", ruleEnv = None,
  preProcess = \ _ x -> x, postProcess = \ _ x -> x, rules = allRules,
  rules2 = \ _ _ -> [], rulesHaveStructural = False, confluenceRules = \ _ _ -> [],
  validExpr = \ _ _ -> True, sortRewrites = id
  }

{-
topfx = (succeeds\/transacts\/interacts)

The program is 'exists x. x=3'

We need to conclude:

y@a   |- topfx{program a . nop; y=3}
----------------------------------------- exists-intro
empty |- topfx{program a . exists y; y=3}


-}

gadd :: [Assumption] -> S.Set Assumption -> S.Set Assumption
gadd asms g = foldr S.insert g asms

afx :: EffectSpecifier -> Op -> Assumption
afx fx = AFX (S.singleton fx)

dummy :: Var
dummy = Var $ VarIdent "_"

pattern Num :: Integer -> Head
pattern Num i = HAtom (ANumber (Number i))

pattern VarI :: Ident -> Var
pattern VarI i = Var (VarIdent i)

pattern VarTI :: Ident -> Var
pattern VarTI i = VarT (TupleIdent i)

infixr 0 +>
(+>) :: Op -> Op -> Op
(+>) = OSeq

infix 5 =#
(=#) :: Var -> Head -> Op
(=#) = OEqHead

infix 5 =$
(=$) :: Var -> Var -> Op
(=$) = OEqVar

allRules :: Rule Config
allRules =
  axiom P.<>
  existsIntro P.<>
  atomIntro P.<>
  tupleIntro P.<>
  unifyHead P.<>
  tuple

axiom :: Rule Config
axiom _ lhs =
  "axiom" `name`
  do
    (g :-  Fx fx (Program _ op)) <- [lhs]
    guard (S.member (AFX fx op) g)
    pure Done

existsIntro :: Rule Config
existsIntro _ lhs =
  "exists-intro" `name`
  do
    (g :- Fx fx e) <- [lhs]
    (ctx, d, OExists xs) <- decisionContext e
    let g' = foldr (\ x -> S.insert (AFlex x d)) g xs
    pure (g' :- Fx fx (ctx d ONop))

atomIntro :: Rule Config
atomIntro _ lhs =
  "atom-intro" `name`
  do
    (g :- p@(Fx _ e)) <- [lhs]
    (_g', AFlex x d) <- assumeContext g
    (_, d', eq@(OEqHead x' h@HAtom{})) <- decisionContext e
    guard (d == d' && x == x')
    guard (not (AUnifyHead x h `S.member` g))
    pure $ gadd [AUnifyHead x h, afx Satisfies eq] g :- p

tupleIntro :: Rule Config
tupleIntro _ lhs =
  "tuple-intro" `name`
  do
    (g :- p@(Fx _ e)) <- [lhs]
    (g', AFlex x d) <- assumeContext g
    (_g'', AFlex i d') <- assumeContext g'
    (_, d'', eq@(OEqHead x' h@(HTuple i' _))) <- decisionContext e
    guard (d == d' && d == d'' && x == x' && i == i')
    guard (not (AUnifyHead x h `S.member` g))
    pure $ gadd [AUnifyHead x h, afx Satisfies eq] g :- p

tuple :: Rule Config
tuple _ lhs =
  "tuple-unify-length" `name`
  do
    (g :- (Fx fx e)) <- [lhs]
    (ctx, d, eq@(OEq x _)) <- decisionContext e
    (g', AUnifyHead x'  (HTuple i _t)) <- assumeContext g
    (_,  AUnifyHead x'' (HTuple j _u)) <- assumeContext g'
    guard (x == x' && x == x'' && i /= j)
    guard (let ops = allOps e in (i =$ j) `notElem` ops && (j =$ i) `notElem` ops)
    pure $ g :- Fx fx (ctx d (eq +> i =$ j))
 ++
  "tuple-expand" `name`
  do
    (g :- p@(Fx _ e)) <- [lhs]
    (_, _, (OEq x _)) <- decisionContext e
    (_g', AUnifyHead x' (HTuple i (VarT t))) <- assumeContext g
    (_g'', AUnifyHead i' (HAtom (ANumber (Number n)))) <- assumeContext g
    guard (x == x' && i == i' && n > 0)
    let ts = [ AUnifyVar dummy $ VTuple t (Number k) | k <- [0..n-1] ]
    guard (head ts `notElem` g)
    pure $ gadd ts g :- p
 ++
  "tuple-unify-element" `name`
  do
    (g :- (Fx fx e)) <- [lhs]
    (ctx, d, eq@(OEq x _)) <- decisionContext e
    (g',  AUnifyHead x'  (HTuple _i (VarT t))) <- assumeContext g
    (g'', AUnifyHead x'' (HTuple _j (VarT u))) <- assumeContext g'
    (_,   AUnifyVar  _z  (VTuple t' n))       <- assumeContext g''
    guard (x == x' && x == x'' && t == t')
    let teq = VTuple t n =$ VTuple u n
        teq' = VTuple u n =$ VTuple t n
    guard (let ops = allOps e in teq `notElem` ops && teq' `notElem` ops)
    pure $ g :- Fx fx (ctx d (eq +> teq))
 ++
  "tuple-flex" `name`
  do
    (g :- p@(Fx _ e)) <- [lhs]
    (_, _, (OEq tu@(VTuple u _) _)) <- decisionContext e
    (_, AFlex (VarT u') d) <- assumeContext g
    guard (u == u')
    let fl = AFlex tu d
    guard (not (fl `S.member` g))
    pure $ gadd [fl] g :- p
 ++
  "tuple-elim" `name`  -- -succeed and -fail
  do
    (g :- (Fx fx e)) <- [lhs]
    (ctx, d, eq@(OEqApply x y i)) <- decisionContext e
    (g1, AFlex x' d' ) <- assumeContext g
    (g2, AFlex i' d'') <- assumeContext g1
    (g3, AUnifyHead i'' (Num n0)) <- assumeContext g2
    (g4, AUnifyHead j   (Num n1)) <- assumeContext g3
    (_,  AUnifyHead y' (HTuple j' (VarT t))) <- assumeContext g4
    guard (x == x' && y == y' && d == d' && d == d'' && i == i' && i == i'' && j == j')
    let a =
          if (0 <= n0 && n0 < n1) then
            -- succeed
            OEqVar x $ VTuple t (Number n0)
           else
            -- fail
            OEqFail x
    guard (a `notElem` allOps e)
    pure $ g :- Fx fx (ctx d (eq +> a))
 ++
  "tuple-elim-choose" `name`
  do
    (g :- (Fx fx e)) <- [lhs]
    (ctx, d, eq@(OEqApply x y i)) <- decisionContext e
    (g1, AFlex x' d') <- assumeContext g
    (g2, AFlex i' d'') <- assumeContext g1
    (g3, AUnifyHead j   (Num n)) <- assumeContext g2
    (g4,  AUnifyHead y'  (HTuple j' (VarT t))) <- assumeContext g3
    guard (x == x' && y == y' && d == d' && d == d'' && i == i' && j == j')
    guard $ null $ [ () | AUnifyHead i'' (Num _) <- S.toList g4, i == i'' ]  -- make sure i doesn't have a value
    let ops = [ op | k <- [0 .. n-1], op <- [ OEqVar x (VTuple t (Number k)), OEqHead i (Num k) ], op `notElem` aops ]
        aops = allOps e
    guard (not (null ops))
    pure $ g :- Fx fx (ctx d (foldr (+>) eq ops))

unifyHead :: Rule Config
unifyHead _ lhs =
  "unify-head-left" `name`
  do
    (g :- p@(Fx _ e)) <- [lhs]
    (_g', AFlex x d) <- assumeContext g
    (_g'', AUnifyHead y h) <- assumeContext g
    (_, d', eq@(OEqVar x' y')) <- decisionContext e
    guard (d == d' && x == x' && y == y')
    guard (not (AUnifyHead x h `S.member` g))
    pure $ gadd [AUnifyHead x h, afx Satisfies eq] g :- p
 ++
  "unify-head-right" `name`
  do
    (g :- p@(Fx _ e)) <- [lhs]
    (_g', AFlex y d) <- assumeContext g
    (_g'', AUnifyHead x h) <- assumeContext g
    (_, d', OEqVar x' y') <- decisionContext e
    guard (d == d' && x == x' && y == y')
    guard (not (AUnifyHead y h `S.member` g))
    pure $ gadd [AUnifyHead y h, afx Satisfies $ OEqVar y x] g :- p
 ++
  "unify-disjoint" `name`
  do
    (g :- p@(Fx _ e)) <- [lhs]
    (g', AUnifyHead x h1) <- assumeContext g
    (_g'', AUnifyHead x' h2) <- assumeContext g'
    (_, _, eq@(OEq x'' _)) <- decisionContext e
    guard (x == x' && x == x'')
    guard (disjointHead h1 h2)
    let a = afx Fails eq
    guard (not (a `S.member` g))
    pure $ gadd [a] g :- p

disjointHead :: Head -> Head -> Bool
disjointHead (HAtom a1) (HAtom a2) = a1 /= a2
disjointHead (HAtom _) _ = True
disjointHead (HTuple _ _) (HTuple _ _) = False -- XXX not sure
disjointHead (HTuple _ _) _ = True
disjointHead (HLambda _ _ _) (HLambda _ _ _) = False -- XXX not sure
disjointHead (HLambda _ _ _) _ = True

-------

tup :: Var -> String -> [OpEqRhs] -> Op
tup res zname vals =
  let zi = TupleIdent zname
      z = VarT zi
      x = Var  $ VarIdent $ zname ++ "_sz"
      t = res =# HTuple x z
      s = x =# Num (toInteger (length vals))
      ex = OExists [x]
      es = zipWith (\ i v -> VTuple zi (Number i) `OEq` v) [0..] vals
  in  foldr (+>) t (ex : s : es)

pptr :: Config -> IO ()
pptr = mapM_ pp . f . normalFormFuelTracePlain sys 100
  where f x = if null (nrLeft x) then nrDone x else trace "**** no fuel " (nrLeft x)

test1 :: Config
test1 = startConfig $
  OExists [x,y] +> x =# Num 5 +> y =$ x
  where x = VarI "x"; y = VarI "y"

res1 :: [Traced Config]
res1 = nrDone $ normalFormFuelTracePlain sys 100 test1

test2 :: Config
test2 = startConfig $
  OExists [x,i] +> x =# HTuple i t
  where x = VarI "x"; i = VarI "i"; t = VarI "t"

test3 :: Config
test3 = startConfig $
  OExists [x,y,a,b,z,w] +>
  tup x "z" [OEHead $ Num 1, OEHead $ Num 10] +>
  tup y "w" [OEVar a, OEVar b] +>
  x =$ y
  where x = VarI "x"; y = VarI "y"; a = VarI "a"; b = VarI "b"; z = VarTI "z"; w = VarTI "w"

test4 :: Config
test4 = startConfig $
  OExists [x] +>
  x =# Num 1 +>
  x =# Num 2
  where x = VarI "x"
