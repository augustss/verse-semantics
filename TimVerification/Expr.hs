--
-- from TimNotes/VerseSpecification-2023-Nov-28.txt
--
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Expr where
import Prelude hiding ((<>))
import qualified Prelude as P
import Control.Monad
import Data.Char
import Data.Data(Data)
import Data.List
import Data.Ratio
import Epic.Print
import Epic.Uniplate
import TRS.TRS
import TRS.Traced(term)
import Debug.Trace

type Q = Rational

data Expr
  = ExprAtom Atom
  deriving (Eq, Ord, Show, Data)

data Path = Path String
  deriving (Eq, Ord, Show, Data)

data Ident = Ident String
  deriving (Eq, Ord, Show, Data)

data Atom = AtomRational Q | AtomPath Path | AtomUnit -- XXX ...
  deriving (Eq, Ord, Show, Data)

data Variable = Variable Ident
  deriving (Eq, Ord, Show, Data)

data Context = Context Ident
  deriving (Eq, Ord, Show, Data)

data OperationVariable = OperationVariable Ident
  deriving (Eq, Ord, Show, Data)

data SolveOrImply = Solve | Imply
  deriving (Eq, Ord, Show, Data, Enum, Bounded)

data VerifyOrEval = Verify | Eval
  deriving (Eq, Ord, Show, Data, Enum, Bounded)

data Syntax
  = SyntaxList [Syntax]
  | SyntaxExpr Expr
  deriving (Eq, Ord, Show, Data)

data AvailableFx = None | EffectSpecifier :& EffectSpecifier
  deriving (Eq, Ord, Show, Data)

data VarianceSpecifier = Open | Closed
  deriving (Eq, Ord, Show, Data, Enum, Bounded)

data Lambda = Lambda { lambda_variance         :: VarianceSpecifier
                     , lambda_effects          :: EffectSpecifier
                     , lambda_arg              :: Variable
                     , lambda_domain_context   :: Context
                     , lambda_domain_input     :: Variable
                     , lambda_domain_output    :: Variable
                     , lambda_domain_operation :: Operation
                     , lambda_range_context    :: Context
                     , lambda_range_input      :: Variable
                     , lambda_range_output     :: Variable
                     , lambda_range_operation  :: Operation
                     }
  deriving (Eq, Ord, Show, Data)

data Head
  = HeadAtom Atom
  | HeadLambda Lambda
  | HeadTuple Vertex Variable
  | HeadNom Path Vertex
  deriving (Eq, Ord, Show, Data)

data Vertex
  = VertexVariable Variable
  | VertexHead Head
  | VertexCall Vertex Vertex
  deriving (Eq, Ord, Show, Data)

data Program = Program Context Variable Variable Operation | ProgramSeq Program Program | ProgramDone
  deriving (Eq, Ord, Show, Data)

data Operation
  = OpUnify Vertex Vertex
  | OpCall Vertex Vertex Vertex
  | OpFail Vertex
  | OpSeq Operation Operation
  | OpChoice Operation Operation
  | OpExists [Variable]
  -- OpCond
  | OpScope Vertex Operation
  | OpBeta Lambda Context Operation Context Operation
  | OpIterate Vertex Context Vertex Operation Context Vertex Operation Context Operation
  -- OpCast
  | OpExplore EffectSpecifier Context Operation Context Operation
  -- OpIn
  -- OpStage
  | OpVerify EffectSpecifier (Maybe Err) Context Operation
  | OpAssume EffectSpecifier Context Operation
--  | OpS Vertex Vertex Syntax
  | OpErr Err
  -- OpVar is not a real Op, it's just for pretty printing a context
  | OpVar String
  deriving (Eq, Ord, Show, Data)

data Err = Err
  deriving (Eq, Ord, Show, Data)

-- ...

data Assumption
  = AEffectOp EffectSpecifier Operation Context
  | AEffectVertex EffectSpecifier Vertex Context
  | AUnify Vertex Vertex
  | AFlex Vertex Context
  | AImpliedEffects EffectSpecifier EffectSpecifier AvailableFx Context
  | ASolveOrImply SolveOrImply Context
  | AVerifyOrEval VerifyOrEval Context
  -- XXX many more
  | ADominates EffectSpecifier Vertex Vertex Context
  deriving (Eq, Ord, Show, Data)

data AssumptionSet = A [Assumption]
  deriving (Eq, Ord, Show, Data)

--pattern A :: [Assumption] -> AssumptionSet
--pattern A xs = AssumptionSet xs

data Config = AssumptionSet :|- Program
  deriving (Eq, Ord, Show, Data)

infixr 0 +>
(+>) :: Operation -> Operation -> Operation
(+>) = OpSeq

{-
-- XXX Eq does not agree with Ord
instance Eq EffectSpecifier where
  fx1 == fx2 = sort (flat fx1) == sort (flat fx2)
    where flat (fxa :\/ fxb) = flat fxa ++ flat fxb
          flat fx = [fx]
          -- XXX expand
-}

------------------------------------------------------------------

ppLower :: Show a => a -> Doc
ppLower = text . map toLower . show

instance Pretty Expr where
  pPrint (ExprAtom a) = pPrint a
instance Pretty Ident where
  pPrint (Ident i) = text i
instance Pretty Path where
  pPrint (Path i) = text i
instance Pretty Atom where
  pPrintPrec l p (AtomRational q) | denominator q == 1 = pPrintPrec l p (numerator q)
                                  | otherwise = pPrintPrec l p (numerator q) <> text "/" <> pPrintPrec l p (denominator q)
  pPrintPrec l p (AtomPath path) = pPrintPrec l p path
  pPrintPrec _ _ (AtomUnit) = text "()"
instance Pretty Variable where pPrint (Variable i) = pPrint i
instance Pretty Context where pPrint (Context i) = pPrint i
instance Pretty OperationVariable where pPrint (OperationVariable i) = pPrint i
instance Pretty SolveOrImply where pPrint = ppLower
instance Pretty VerifyOrEval where pPrint = ppLower
instance Pretty Syntax where
  pPrint (SyntaxList aes) = parens $ hcat (punctuate (text ",") (map pPrint aes))
  pPrint (SyntaxExpr e) = pPrint e
instance Pretty AvailableFx where
  pPrint None = text "none"
  pPrint (fx1 :& fx2) = pPrint (fx1, fx2)
instance Pretty VarianceSpecifier where pPrint = ppLower
instance Pretty Lambda where
  pPrint (Lambda oc fx u c0 i w op0 c1 j z op1) =
    text "lambda" <> pPrint (oc,fx,u) <+> pPrint c0 <+> pPrint i <+> pPrint w <> text "." <+> pPrint op0 <> text ";" <+>
    text "range" <+> pPrint c1 <+> pPrint j <+> pPrint z <> text "." <+> pPrint op1
instance Pretty Head where
  pPrint (HeadAtom a) = pPrint a
  pPrint (HeadLambda l) = pPrint l
  pPrint (HeadTuple u t) = text "tuple" <> parens (pPrint u) <+> pPrint t
  pPrint (HeadNom p u) = text "nom" <+> pPrint p <> text "." <+> pPrint u
instance Pretty Vertex where
  pPrint (VertexVariable x) = pPrint x
  pPrint (VertexHead h) = pPrint h
  pPrint (VertexCall u v) = pPrint u <> brackets (pPrint v)
instance Pretty Program where
  pPrint (Program c i x op) = text "program" <+> pPrint c <+> pPrint i <+> pPrint x <> text "." <+> pPrint op
  pPrint (ProgramSeq pg1 pg2) = pPrint pg1 <> text ";" <+> pPrint pg2
  pPrint ProgramDone = text "DONE"
instance Pretty Operation where -- XXX precedence
  pPrint (OpUnify u v) = pPrint u <+> text "=" <+> pPrint v
  pPrint (OpCall u v p) = pPrint u <+> text "=" <+> pPrint v <> parens (pPrint p)
  pPrint (OpFail u) = pPrint u <+> text "=fail"
  pPrint (OpSeq op0 op1) = pPrint op0 <> text ";" <+> pPrint op1
  pPrint (OpChoice op0 op1) = pPrint op0 <+> text "|" <+> pPrint op1
  pPrint (OpExists xs) = hsep (text "exists" : map pPrint xs)
  -- OpCond
  pPrint (OpScope c op0) = text "scope" <+> pPrint c <> text "." <+> pPrint op0
  pPrint (OpBeta lam c0 op0 c1 op1) =
    text "beta" <> parens (pPrint lam) <+> pPrint c0 <> text "." <+> pPrint op0 <+>
    text "range" <+> pPrint c1 <> text "." <+> pPrint op1
  pPrint (OpIterate u0 c0 v0 op0 c1 v1 op1 c2 op2) =
    text "iterate" <> parens (pPrint u0) <+> pPrint c0 <+> pPrint v0 <> text "." <+> pPrint op0 <+>
    text "then" <+> pPrint c1 <+> pPrint v1 <> text "." <+> pPrint op1 <+>
    text "else" <+> pPrint c2 <+> text "." <+> pPrint op2
  -- OpCast
  pPrint (OpExplore fx c op0 d op1) =
    text "explore" <> parens (pPrint fx) <+> pPrint c <> text "." <+> pPrint op0 <> text ";" <+>
    text "range" <+> pPrint d <> text "." <+> pPrint op1
  -- OpIn
  -- OpStage
  pPrint (OpVerify fx Nothing c op0) = text "verify" <> parens (pPrint fx) <+> pPrint c <> text "." <+> pPrint op0
  pPrint (OpVerify fx (Just err) c op0) = text "verify" <> parens (pPrint (fx, err)) <+> pPrint c <> text "." <+> pPrint op0
  pPrint (OpAssume fx c op0) = text "assume" <> parens (pPrint fx) <+> pPrint c <> text "." <+> pPrint op0
--  pPrint (OpS u v s) = text "S" <> pPrint (u,v,s)
  pPrint (OpErr e) = pPrint e
  pPrint (OpVar s) = text s
instance Pretty Err where
  pPrint Err = text "ERR"
instance Pretty Assumption where
  pPrint (AEffectOp fx arg c) = pPrint fx <> braces (pPrint arg) <> text "@" <> pPrint c
  pPrint (AEffectVertex fx arg c) = pPrint fx <> braces (pPrint arg) <> text "@" <> pPrint c
  pPrint (AUnify v0 v1) = pPrint v0 <> text "<-" <> pPrint v1
  pPrint (AFlex v c) = pPrint v <> text "@" <> pPrint c
  pPrint (AImpliedEffects fx1 fx2 afx c) = pPrint fx1 <> text ":" <> pPrint fx2 <> text ":" <> pPrint afx <> text "@" <> pPrint c
  pPrint (ASolveOrImply si c) = pPrint si <> text "@" <> pPrint c
  pPrint (AVerifyOrEval vb c) = pPrint vb <> text "@" <> pPrint c
  pPrint (ADominates fx u v c) = pPrint fx <> braces (pPrint u <> text ">>" <> pPrint v) <> text "@" <> pPrint c
  
instance Pretty AssumptionSet where
  pPrint (A []) = text "empty"
  pPrint (A as) = hcat (punctuate (text ",") (map pPrint as))
instance Pretty Config where
  pPrint (g :|- pg) = pPrint g <+> text "|-" <+> pPrint pg

instance Pretty (Context -> Operation -> Program) where
  pPrint f = pPrint (f (Context (Ident "C")) (OpVar "OP"))

------------------------------------------------------------------

data Effect
  = FXsucceeds | FXdecides | FXresolves | FXabstracts | FXfails | FXcontradicts
  | FXdiverges | FXdemands | FXallocates | FXvaries
  | FXiterates         | FXreads         | FXwrites         | FXinteracts         | FXthrows         | FXsuspends
  | FXiterates_pending | FXreads_pending | FXwrites_pending | FXinteracts_pending | FXthrows_pending | FXsuspends_pending
  deriving (Eq, Ord, Show, Data)

-- Partial order on Effect (reflexive, transitive closure of adjecent <==)
(<==) :: Effect -> Effect -> Bool
FXsucceeds    <== e = e `elem` [FXsucceeds, FXdecides, FXiterates, FXresolves, FXabstracts]
FXdecides     <== e = e `elem`             [FXdecides, FXiterates, FXresolves, FXabstracts]
FXresolves    <== e = e `elem`                                    [FXresolves, FXabstracts]
FXabstracts   <== e = e `elem`                                                [FXabstracts]
FXfails       <== e = e `elem` [FXfails,    FXdecides, FXiterates, FXresolves, FXabstracts]
FXiterates    <== e = e `elem`                        [FXiterates]
FXcontradicts <== e = e `elem` [FXcontradicts, FXfails, FXsucceeds, FXdecides, FXiterates, FXresolves, FXabstracts]
--
FXvaries      <== e = e `elem` [FXvaries, FXreads, FXallocates]
e1            <== e2 = e1 == e2

newtype EffectSpecifier = ES [Effect] -- the list has no elements x,y where x <== y
  deriving (Eq, Ord, Show, Data)

effs :: [Effect] -> EffectSpecifier
effs = ES . sort

esInvariant :: EffectSpecifier -> EffectSpecifier
esInvariant fx@(ES aes) | or [ e1 <== e2 | e1 <- aes, e2 <- aes, e1 /= e2 ] = error "esInvariant 1"
                        | length aes /= length (nub aes) = error "esInvariant 2"
                        | otherwise = fx

cardinalities :: EffectSpecifier
cardinalities = effs [FXabstracts, FXiterates]
transacts :: EffectSpecifier
transacts = effs [FXreads, FXwrites, FXallocates]
imperatives :: EffectSpecifier
imperatives = effs [FXdiverges, FXdemands, FXreads, FXwrites, FXallocates, FXinteracts, FXthrows, FXsuspends, 
  FXiterates_pending, FXreads_pending, FXwrites_pending, FXinteracts_pending, FXthrows_pending, FXsuspends_pending]
tops :: EffectSpecifier
tops = effs [FXsucceeds, FXdiverges, FXinteracts] \/ transacts
effects :: EffectSpecifier
effects = cardinalities \/ imperatives \/ ES [FXdemands, FXvaries]
pattern Succeeds :: EffectSpecifier
pattern Succeeds = ES [FXsucceeds]
pattern Fails :: EffectSpecifier
pattern Fails = ES [FXfails]
pattern Abstracts :: EffectSpecifier
pattern Abstracts = ES [FXabstracts]
pattern Resolves :: EffectSpecifier
pattern Resolves = ES [FXresolves]
pattern Demands :: EffectSpecifier
pattern Demands = ES [FXdemands]

-- The join of two EffectSpecifier
(\/) :: EffectSpecifier -> EffectSpecifier -> EffectSpecifier
ES fx0 \/ ES fx1 =
  let fx0' = filter (\ e0 -> not $ any (\ e1 -> e0 <== e1) fx1 ) fx0
      fx1' = filter (\ e1 -> not $ any (\ e0 -> e1 <== e0) fx0') fx1
  in  esInvariant $ effs $ fx0' ++ fx1'

-- The meet of two EffectSpecifier
(/\) :: EffectSpecifier -> EffectSpecifier -> EffectSpecifier
ES fx0 /\ ES fx1 =
  esInvariant $ effs $ nub
  [ e | e0 <- fx0, e1 <- fx1, e <- if e0 <== e1 then [e0] else if e1 <== e0 then [e1] else [] ]

-- Partial order on EffectSpecifier
(<===) :: EffectSpecifier -> EffectSpecifier -> Bool
fx0 <=== fx1 = (fx0/\fx1) == fx0

instance Pretty Effect where pPrint = text . drop 2 . show

instance Pretty EffectSpecifier where
  pPrint fx | fx == tops = text "tops"
            | fx == effects = text "effects"
            | fx == transacts = text "transacts"
            | fx == imperatives = text "imperatives"
            | fx == cardinalities = text "cardinalities"
            | fx == ES [] = text "noeffects"
            | ES aes <- fx = hcat (punctuate (text "\\/") (map pPrint aes))

remove :: EffectSpecifier -> Effect -> EffectSpecifier
remove (ES aes) e = ES (filter (/= e) aes)

star :: EffectSpecifier -> EffectSpecifier -> EffectSpecifier
star fx0 fx1 = (fx0\/fx1)/\
  if      m <=== Fails   then Fails\/imperatives
  else if m <=== Demands then Abstracts\/imperatives
  else                       effects `remove` FXdemands
  where m = fx0/\fx1

------------------------------------------------------------------

class NewIdents i o | i -> o where
  newIdents :: (Data a) => a -> i -> o
instance NewIdents String Ident where
  newIdents x s = is !! 0  where is = identsNotIn x [Ident s]
instance NewIdents (String, String) (Ident, Ident) where
  newIdents x (s1, s2) = (is !! 0, is !! 1)  where is = identsNotIn x [Ident s1, Ident s2]
instance NewIdents (String, String, String) (Ident, Ident, Ident) where
  newIdents x (s1, s2, s3) = (is !! 0, is !! 1, is !! 2)  where is = identsNotIn x [Ident s1, Ident s2, Ident s3]
instance NewIdents [String] [Ident] where
  newIdents x ss = is  where is = identsNotIn x (map Ident ss)

identsOf :: Data i => i -> [Ident]
identsOf = universeBi

identsNotIn :: Data i => i -> [Ident] -> [Ident]
identsNotIn x is = idents is \\ identsOf x

-- Make variations on the identifiers
idents :: [Ident] -> [Ident]
idents is = concatMap (\ s -> map (addSuf s) is) sufs
  where sufs = "" : "'" : map show [1::Integer ..]
        addSuf s (Ident i) = Ident (i ++ s)

------------------------------------------------------------------

dsS :: Vertex -> Vertex -> Syntax -> Operation
dsS u v (SyntaxExpr e) = dsE u v e
dsS _ _ _ = undefined

dsE :: Vertex -> Vertex -> Expr -> Operation
dsE u v (ExprAtom a) = OpUnify u h +> OpUnify v h
  where h = VertexHead $ HeadAtom a

------------------------------------------------------------------

instance Rec Config where
  data RuleEnv Config = RuleEnvConfig
  rec r s ae = r s ae

sys :: TRSystem Config
sys = TRSystem {
  sname = "TimRun", description = "Tim's runtime rules", ruleEnv = RuleEnvConfig,
  preProcess = \ _ x -> x, postProcess = \ _ x -> x, rules = allRules,
  rules2 = \ _ _ -> [], rulesHaveStructural = False, confluenceRules = \ _ _ -> [],
  validExpr = \ _ _ -> True, sortRewrites = id
  }

pptr :: Config -> IO ()
pptr = mapM_ ppx . f . normalFormFuelTracePlain sys 100
  where f x = if null (nrLeft x) then nrDone x else trace "**** no fuel " (nrLeft x)

norm :: Config -> Config
norm = term . (!!0) . f . normalFormFuelTracePlain sys 100
  where f x = if null (nrLeft x) then nrDone x else trace "**** no fuel " (nrLeft x)

------------------------------------------------------------------

gAdd :: [Assumption] -> [Assumption] -> [AssumptionSet]
gAdd asms g =
  let asms' = filter (`notElem` g) asms
  in  if null asms' then
        []
      else
        [A $ asms' ++ g]

before :: Rule a -> Rule a -> Rule a
before aRules bRules cfg a =
  case aRules cfg a of
    [] -> bRules cfg a
    as -> as

allRules :: Rule Config
allRules =
  (programRules P.<> 
   assumptionRules P.<>
   unificationRules P.<>
   sequenceRules P.<>
   existsRules P.<>
   dominatorRules)
  `before`
  dominators

------------------------------------------------------------------

-- define-ident
-- define-ident-var
-- define-ident-ref
-- resolve-ident

------------------------------------------------------------------

programRules :: Rule Config
programRules _ (A g :|- pg) =
  "program-intro" `name`
  do
    Program c _i _x _op <- [pg]
    g' <- gAdd [AVerifyOrEval Verify c, ASolveOrImply Solve c, AImpliedEffects effects tops None c] g
    pure $ g' :|- pg
 -- program-sequence
 ++
  "program-elim" `name`
  do
    Program c _i _x op <- [pg]
    AEffectOp fx op' c' <- g
    guard (c == c' && op == op' && fx <=== tops)
    pure $ A [] :|- ProgramDone

assumptionRules :: Rule Config
assumptionRules _ (A g :|- pg) =
  -- fx-weaken
  -- fx-intersects
  "eq-refl" `name`
  do
    AFlex v c <- g
    g' <- gAdd [AEffectOp Succeeds (OpUnify v v) c] g
    pure $ g' :|- pg
 ++
  "eq-symm" `name`
  do
    AEffectOp fx (OpUnify u v) c <- g
    g' <- gAdd [AEffectOp fx (OpUnify v u) c] g
    pure $ g' :|- pg
 ++
  "eq-trans" `name`
  do
    AEffectOp fx  (OpUnify p  q) c  <- g
    AEffectOp fx' (OpUnify q' r) c' <- g
    guard (fx == fx' && c == c' && q == q')
    guard (p /= q && q /= r)
    g' <- gAdd [AEffectOp fx (OpUnify p r) c] g
    pure $ g' :|- pg
  -- eq-call-same  QQQ: is p something in pg, or just taken from thin air
  -- eq-call-computes QQQ: what do the two things below the line mean?
  -- eq-tuple
  -- flow-tuple
  -- tuple-resolves QQQ: what is ...
  -- eq-dominator QQQ: needs side condition?
  -- equation-propagate
  -- equation-fails
 ++
  "equation-resolves" `name`  -- QQQ: needs @c?
  do
    AEffectOp Abstracts (OpUnify _u a) c <- g
    g' <- gAdd [AEffectVertex Resolves a c] g
    pure $ g' :|- pg
  -- flexes-symm
  -- flexes-trans
  -- flexes-fx
  -- sees-ident
  -- atom-rational
  -- atom-int
  -- atom-nat
  -- int-rational
  -- nat-int

unificationRules :: Rule Config
unificationRules _ (A g :|- pg) =
  "unify-flex-left" `name`
  do
    (_ctx, c, OpUnify u v) <- fc pg
    guard (AFlex u c `elem` g)
    g' <- gAdd [AUnify u v] g
    pure $ g' :|- pg
 ++
  "unify-flex-right" `name`
  do
    (_ctx, c, OpUnify u v) <- fc pg
    guard (AFlex v c `elem` g)
    g' <- gAdd [AUnify v u] g
    pure $ g' :|- pg
 ++
  "unify-intro" `name`
  do
    (_ctx, c, op@(OpUnify _u _v)) <- fc pg
    AImpliedEffects fx1 _fx2 _afx3 c' <- g
    guard (c == c')
    g' <- gAdd [AEffectOp fx1 op c] g
    pure $ g' :|- pg
 ++
  -- QQQ: 'fail x'?
  "unify-fail-intro" `name`
  do
    (_ctx, c, op@(OpFail _u)) <- fc pg
    g' <- gAdd [AEffectOp Fails op c] g
    pure $ g' :|- pg
-- unify-head-fails
-- unify-tuple-intro
-- unify-lambda-intro
-- ...

sequenceRules :: Rule Config
sequenceRules _ (A g :|- pg) =
  "sequence-intro" `name`
  do
    (_ctx, c, OpSeq op0 op1) <- fc pg
    AEffectOp fx0 op0' c' <- g
    guard (op0 == op0' && c == c')
    AEffectOp fx1 op1' c'' <- g
    guard (op1 == op1' && c == c'')
    g' <- gAdd [AEffectOp (fx0 `star` fx1) (OpSeq op0 op1) c] g
    pure $ g' :|- pg
  -- sequence-pending-1
  -- sequence-pending-2

existsRules :: Rule Config
existsRules _ (A g :|- pg) =
  "exists-intro" `name`
  do
    (_ctx, c, op@(OpExists xs)) <- fc pg
    g' <- gAdd (AEffectOp Succeeds op c : map (\ x -> AFlex (VertexVariable x) c) xs) g
    pure $ g' :|- pg

dominatorRules :: Rule Config
dominatorRules _ (A g :|- pg) =
  "dominator-equiv" `name`  -- QQQ: fx{v>>u}@c is not among assumptions
  do
    ADominates fx v u c <- g
    g' <- gAdd [AEffectOp fx (OpUnify u v) c] g
    pure $ g' :|- pg
 ++
  "eq-dominator" `name`
  do
    AEffectOp fx (OpUnify u v) c <- g
    g' <- gAdd [ADominates fx v u c] g
    pure $ g' :|- pg

------------------------------------------------------------------

class ExploreStart a where
  es :: a -> [(Context -> Operation -> a, Context, Operation)]
instance ExploreStart Program where
  es (Program e i x op) =
    [(\ e' op' -> Program e' i x op', e, op)]
  es _ = []
instance ExploreStart Operation where
  es (OpExplore fx e op0 d op1) =
    [(\ e' op0' -> OpExplore fx e' op0' d op1, e, op0)
    ,(\ d' op1' -> OpExplore fx e op0 d' op1', d, op1)]
  es _ = []

class FlexibleStart a where
  fs :: a -> [(Context -> Operation -> a, Context, Operation)]
instance FlexibleStart Operation where
  fs (OpIterate u0 c v0 op c1 v1 op1 c2 op2) =
    [(\ c' op' -> OpIterate u0 c' v0 op' c1 v1 op1 c2 op2, c, op)]
  fs (OpVerify fx merr c op) =
    [(\ c' op' -> OpVerify fx merr c' op', c, op)]
  fs (OpAssume fx c op) =
    [(\ c' op' -> OpAssume fx c' op', c, op)]
  fs _ = []

class FlexibleOp a where
  fop :: a -> [(Operation -> a, Operation)]
instance FlexibleOp Operation where
  fop a = (id, a) : fop1 a
    where fop1 (OpSeq op1 op2) =
            [(\ op1' -> OpSeq op1' op2, op1)
            ,(\ op2' -> OpSeq op1 op2', op2)]
          fop1 (OpScope c aop) = do
            (ctx, op) <- fop aop
            pure (\ op' -> OpScope c (ctx op'), op)
-- QQQ: what is p?  beta(L(p)) c0. fop[OP] range c1. op
          fop1 (OpBeta lam c0 op0 c1 op1) = do
            (ctx, op) <- fop op0
            [(\ op' -> OpBeta lam c0 (ctx op') c1 op1, op),
             (\ op' -> OpBeta lam c0 op0 c1 (ctx op'), op)]
          fop1 _ = []

class ExploreOp a where
  eop :: a -> [(Operation -> a, Operation)]
instance ExploreOp Operation where
  eop a = fop a ++
           do
             (ctx, c, op) <- fs a
             pure (\ op' -> ctx c op', op)

-- QQQ: fc does not allow 'program'
class FlexibleContext a where
  fc :: a -> [(Context -> Operation -> a, Context, Operation)]
instance FlexibleContext Program where
  fc (Program ac i x aop) =
    do
      (ctx, op) <- fop aop
      pure (\ c' op' -> Program c' i x (ctx op'), ac, op)
   ++
    do
      (ctx, c, op) <- fc aop
      pure (\ c' op' -> Program ac i x (ctx c' op'), c, op)
  fc _ = []  -- QQQ: should es and fc handle  'pg; pg' ?
instance FlexibleContext Operation where
  fc a =
    do
      -- fop[fs[C,fop[OP]]]
      (fop1ctx, aop) <- fop a
      (fsctx, c, bop) <- fs aop
      (fop2ctx, op) <- fop bop
      pure (\ c' op' -> fop1ctx (fsctx c' (fop2ctx op')), c, op)
     ++
    do
      -- fop[es[C,fop[OP]]]
      (fop1ctx, aop) <- fop a
      (esctx, c, bop) <- es aop
      (fop2ctx, op) <- fop bop
      pure (\ c' op' -> fop1ctx (esctx c' (fop2ctx op')), c, op)
--     ++
--    do
      -- fc[c,fc[C,OP]]  QQQ: is this really right?

------------------------------------------------------------------

dominators :: Rule Config
dominators _ (A g :|- pg) =
  "dominators" `name`
  do
    ADominates fx _ _ c <- g
    let ds = dom fx c g
    g' <- gAdd [ ADominates Succeeds u v c | Dom u v <- ds ] g
    pure $ g' :|- pg

data Dom = Dom Vertex Vertex
  deriving (Eq, Show)
instance Pretty Dom where
  pPrint (Dom u v) = pPrint u <> text ">>" <> pPrint v

-- XXX no accounting for tuples in candidates
-- XXX no accounting for lambda-dominators
dom :: EffectSpecifier -> Context -> [Assumption] -> [Dom]
dom afx ac g =
  let candidates = [ Dom v u | AEffectOp fx (OpUnify v u) c <- g, afx == fx, ac == c ]
      add ds = ds `union` [ Dom v u | AEffectOp fx (OpUnify v w) c <- g, afx == fx, ac == c,
                            AUnify u w' <- g, w == w' ]
      startDs = loop candidates
        where loop xs =
                let xs' = add xs
                in  if length xs == length xs' then xs else loop xs'

      -- XXX not sure what this means: and (v<=w or ...something re comparable)
      keep xs (Dom v u) =
        AEffectOp afx (OpUnify u v) ac `elem` g ||
        and [ AEffectOp afx (OpUnify u w) ac `elem` g || Dom v w `elem` xs
            | AUnify u' w <- g, u == u' ]
      finalDs = loop startDs
        where loop xs =
                let xs' = filter (keep xs) xs
                in  if length xs == length xs' then xs else loop xs'
  in  finalDs

------------------------------------------------------------------

startConfig :: Syntax -> Config
startConfig s = g :|- pg
  where (c, i, x) = newIdents s ("c", "i", "x")
        vi = Variable i
        vvi = VertexVariable vi
        vx = Variable x
        vvx = VertexVariable vx
        vc = Context c
        g = A [AFlex vvi vc, AFlex vvx vc]
        pg = Program vc vi vx $ dsS vvi vvx s

------------------------------------------------------------------

-- example1:  5
example1 :: Syntax
example1 = SyntaxExpr $ ExprAtom $ AtomRational 5
