--
-- from TimNotes/VerseSpecification-2024-May-07.txt
--
{-# OPTIONS_GHC -Wall -Wno-unused-imports -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Main {-(main,
  example1, example2, example3, example4, example5, example6, example7, example8, example9, example10, example11,
  pptr, fpptr, norm, startConfig)-} where
import Prelude hiding ((<>))
import qualified Prelude as P
import Control.Monad
import Data.Char
import Data.Data(Data)
import Data.List hiding(nub)
import Data.Maybe
import Data.Ratio
import System.IO
import Epic.List
import Epic.Print
import Epic.Uniplate
import TRS.TRS
import TRS.Traced(term)
import GHC.Stack
import Debug.Trace

ppLower :: Show a => a -> Doc
ppLower = text . map toLower . show

xsep :: [Doc] -> Doc
xsep = fsep

--------------------------------------------

type Q = Rational

data Ident = Ident String
  deriving (Eq, Ord, Show, Data)

instance Pretty Ident where
  pPrint (Ident i) = text i

data Expr
  = ExprAtom Atom              -- atom
  | ExprUnify Expr Expr        -- s0=s1
  | ExprChoice Expr Expr       -- s0|s1
  | ExprArray [Expr]           -- array{s1,...}
  | ExprCallClosed Expr Expr   -- s0[s1]
  | ExprCallOpen Expr Expr     -- s0(s1)
  | ExprDeoption Expr          -- s0?
  | ExprOption Expr            -- option{s0}
  | ExprPreColon Expr          -- :s0
  | ExprFunction Expr (Maybe VarianceSpecifier) Expr Expr
  | ExprSpec Expr [Expr]       -- s0<s1><s2>...
  | ExprDef Expr Expr          -- s0:=s1
  | ExprPostHat Expr           -- s0^
  | ExprFx EffectSpecifier     -- fx
  | ExprColon Expr Expr        -- s0:s1
  | ExprVar EVariable          -- x
  | ExprUnderscore             -- _
  | ExprArrow Expr Expr        -- s0->s1
  | ExprStage Expr Expr Expr   -- :s0<s2>=s1

  | ExprLambda Expr Expr
  | ExprExists EVariable

  -- Unholy mix of levels
  | ExprVertex Vertex          -- u
  deriving (Eq, Ord, Show, Data)

instance Pretty Expr where
  pPrint (ExprAtom a) = pPrint a
  pPrint (ExprUnify e1 e2) = pPrint e1 <+> text "=" <+> pPrint e2
  pPrint (ExprChoice e1 e2) = pPrint e1 <+> text "|" <+> pPrint e2
--
  pPrint (ExprDef x e) = pPrint x <+> text ":=" <+> pPrint e
  pPrint (ExprVar x) = pPrint x
  pPrint ExprUnderscore = text "_"
  pPrint (ExprLambda e1 e2) = pPrint e1 <+> text "=>" <+> pPrint e2
  pPrint (ExprFunction e1 oc fx e2) = text "function" <> parens (pPrint e1) <> hcat [maybe empty f oc, f fx) <> braces (pPrint e2)
    where f e = text "<" <> pPrint e <> pPrint ">"
  pPrint (ExprCallClosed e1 e2) = pPrint e1 <> brackets (pPrint e2)
  pPrint (ExprCallOpen e1 e2) = pPrint e1 <> parens (pPrint e2)
  pPrint (ExprColon e1 e2) = parens $ pPrint e1 <> text ":" <> pPrint e2
  pPrint (ExprArray xs) = text "array" <> (braces $ sep $ punctuate (text ",") (map pPrint xs))
  pPrint (ExprExists x) = pPrint x <> text ":any"
  pPrint (ExprDeoption x) = pPrint x <> text "?"
  pPrint (ExprOption x) = text "option" <> braces (pPrint x)
  pPrint (ExprPreColon x) = text ":" <> pPrint x
  pPrint (ExprPostHat x) = pPrint x <> text "^"
  pPrint (ExprSpec x ss) = foldl f (pPrint x) ss
    where f s e = s <> text "<" <> pPrint e <> pPrint ">"
  pPrint (ExprFx fx) = pPrint fx
  pPrint (ExprArrow e1 e2) = pPrint e1 <> text "->" <> pPrint e2
  pPrint (ExprVertex u) = text "$" <> pPrint u

data EVariable = EVariable Ident
  deriving (Eq, Ord, Show, Data)

instance Pretty EVariable where pPrintPrec l p (EVariable i) = pPrintPrec l p i

data VarianceSpecifier = Open | Closed
  deriving (Eq, Ord, Show, Data, Enum, Bounded)

instance Pretty VarianceSpecifier where pPrint = ppLower

data Atom
  = AtomRational Q
  | AtomPointer Pointer
  | AtomPath Path
  | AtomUnit
  | AtomPrimitive String -- Print, operator'^', prefix'set'
  | AtomEffect EffectSpecifier
  | AtomVariance VarianceSpecifier
  deriving (Eq, Ord, Show, Data)

instance Pretty Atom where
  pPrintPrec l p (AtomRational q) | denominator q == 1 = pPrintPrec l p (numerator q)
                                  | otherwise = pPrintPrec l p (numerator q) <> text "/" <> pPrintPrec l p (denominator q)
  pPrintPrec l p (AtomPointer ptr) = pPrintPrec l p ptr
  pPrintPrec l p (AtomPath path) = pPrintPrec l p path
  pPrintPrec _ _ (AtomUnit) = text "()"
  pPrintPrec _ _ (AtomPrimitive s) = text s
  pPrintPrec l p (AtomEffect e) = pPrintPrec l p e
  pPrintPrec l p (AtomVariance v) = pPrintPrec l p v

data Path = Path String
  deriving (Eq, Ord, Show, Data)

instance Pretty Path where
  pPrint (Path i) = text i

newtype Pointer = Pointer Ident
  deriving (Eq, Ord, Show, Data)

instance Pretty Pointer where pPrint (Pointer i) = pPrint i

newtype Variable = Variable Ident
  deriving (Eq, Ord, Show, Data)

instance Pretty Variable where pPrint (Variable i) = pPrint i

newtype Context = Context Ident
  deriving (Eq, Ord, Show, Data)

instance Pretty Context where pPrint (Context i) = pPrint i

newtype OperationVariable = OperationVariable Ident
  deriving (Eq, Ord, Show, Data)

instance Pretty OperationVariable where pPrint (OperationVariable i) = pPrint i

newtype EffectVariable = EffectVariable Ident
  deriving (Eq, Ord, Show, Data)

instance Pretty EffectVariable where pPrint (EffectVariable i) = pPrint i

data Syntax
  = SyntaxList [Syntax]
  | SyntaxExpr Expr
  | SyntaxUnquote Vertex
  deriving (Eq, Ord, Show, Data)

instance Pretty Syntax where
  pPrint (SyntaxExpr e) = pPrint e
  pPrint (SyntaxList aes) = parens $ hcat (punctuate (text ";") (map pPrint aes))
  pPrint (SyntaxUnquote u) = parens $ text "unquote" <+> pPrint u

data EqualityStrength = EqualityStrength EffectSpecifier -- only succeeds\/ambiguates | decides\/ambiguates | abstracts
  deriving (Eq, Ord, Show, Data)

instance Pretty EqualityStrength where
  pPrintPrec l p (EqualityStrength e) = pPrintPrec l p e

data ImpliedFx = None | Some EffectSpecifier
  deriving (Eq, Ord, Show, Data)

instance Pretty ImpliedFx where
  pPrint None = text "none"
  pPrint (Some fx) = pPrint fx

data Lambda = Lambda { lambda_variance         :: VarianceSpecifier
                     , lambda_effects          :: EffectSpecifier
                     , lambda_arg              :: Vertex
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

instance Pretty Lambda where
  pPrint (Lambda oc fx u c0 i w op0 c1 j z op1) =
    xsep [
      text "lambda" <> pPrint (oc,fx,u) <+> pPrint c0 <+> pPrint i <+> pPrint w <> text "." <+> pPrint op0 <> text ";",
      nest 2 (text "range" <+> pPrint c1 <+> pPrint j <+> pPrint z <> text "." <+> pPrint op1) ]

data Head
  = HeadAtom Atom
  | HeadLambda Lambda
  | HeadMacro Lambda
  | HeadTuple Vertex Variable                         -- tuple(u) x
  | HeadNom Path Variable                             -- nom(Path) x
  deriving (Eq, Ord, Show, Data)

instance Pretty Head where
  pPrint (HeadAtom a) = pPrint a
  pPrint (HeadLambda l) = pPrint l
  pPrint (HeadMacro l) = parens $ text "macro" <+> pPrint l
  pPrint (HeadTuple u t) = parens $ text "tuple" <> parens (pPrint u) <+> pPrint t
  pPrint (HeadNom p u) = text "nom" <+> pPrint p <> text "." <+> pPrint u

data Vertex
  = VertexVariable Variable                           -- x
  | VertexHead Head                                   -- head
  | VertexCall Vertex Vertex                          -- u(v)
  deriving (Eq, Ord, Show, Data)

instance Pretty Vertex where
  pPrint (VertexVariable x) = pPrint x
  pPrint (VertexHead h) = pPrint h
  pPrint (VertexCall u v) = pPrint u <> parens (pPrint v)

-- The operation is always of the form
--  explore(top_allows,fxv) {...; scope c {op}; ...}
type Program = Operation

data Operation
  = OpUnify Vertex Vertex                                         -- u=v
  | OpFail Vertex                                                 -- u=fail
  | OpSeq Operation Operation                                     -- op0; op1
  | OpChoice Operation Operation                                  -- op0|op1
  | OpExists [Variable]                                           -- exists x0 ...
  | OpScope Context Operation                                     -- scope c op0
  | OpBeta (Lambda, Vertex) Context Operation {-range-} Context Operation -- beta(L(p)) c0 op0 range c1 op1
  | OpIn EffectSpecifier (Maybe EffectVariable) Context Variable Variable Operation      -- in(fx,fxv|none) c i x op0
  | OpStage EffectSpecifier EffectVariable Context Variable Variable Operation {-value-} Context Variable Operation
                                                                  -- stage(fx,opv) c0 i x {op0} value c1 y {op1}
  | OpVerify EffectSpecifier Err Context Operation                -- verify(fx,err) c. op0
  | OpAssume EffectSpecifier Context Operation                    -- assume(fx) c. op0
  | OpIterate Operation                                           -- iterate {op_0; ...; op_n}
  | OpFold Variable Context Operation {-then-} Context Variable Operation {-else-} Context Operation
                                                                  -- fold(x0) c0 {op0} then c1 x1 {op1} else c2 {op2}
  | OpExplore EffectSpecifier EffectVariable Operation            -- explore(fx,fxv) {op_0; ...; op_n}
  | OpCastDynamic Vertex [RHS]                                    -- cast(u)         {HeadPattern_0 => {op_0}; ...}
  | OpCastStatic Vertex OperationVariable Err [RHS]               -- cast(u,opv,err) {HeadPattern_0 => {op_0}; ...}
  | OpErr Err                                                     -- err
  | OpSyntax OperationVariable Vertex Vertex Syntax               -- S(u,v,s)
  | OpResolved OperationVariable Operation                        -- resolved(opv){op}
  | OpDefine Ident Defineable                                     -- define Ident {definable}

  -- OpMetaVar is not a real Op, it's just for pretty printing a context
  | OpMetaVar String
  -- OpDone is not a real Op, it's just used to indicate that we are done
  | OpDone
  deriving (Eq, Ord, Show, Data)

data RHS = RHS HeadPattern Operation
  deriving (Eq, Ord, Show, Data)

instance Pretty Operation where -- XXX precedence
  pPrint (OpUnify u v) = xsep [pPrint u <+> text "=", pPrint v]
  pPrint (OpFail u) = pPrint u <+> text "=fail"
  pPrint (OpSeq op0 op1) = xsep [pPrint op0 <> text ";", pPrint op1]
  pPrint (OpChoice op0 op1) = pPrint op0 <+> text "|" <+> pPrint op1
  pPrint (OpExists xs) = hsep (text "exists" : map pPrint xs)
  pPrint (OpScope c op0) = text "scope" <+> pPrint c <+> braces (pPrint op0)
  pPrint (OpBeta (l,p) c0 op0 c1 op1) =
    xsep [text "beta" <> parens (pPrint l) <> parens (pPrint p) <+> pPrint c0 <+> braces (pPrint op0),
         nest 2 (text "range" <+> pPrint c1 <+> braces (pPrint op1))]
  pPrint (OpIn fx mfxv c i x op0) =
    text "in" <> parens (pPrint fx <> text "," <+> maybe (text "none") pPrint mfxv) <+> pPrint c <+> pPrint i <+> pPrint x <+> braces (pPrint op0)
  pPrint (OpStage fx fxv c0 i x op0 c1 y op1) =
    text "stage" <> parens (pPrint (fx, fxv)) <+> pPrint c0 <+> pPrint i <+> pPrint x <+> braces (pPrint op0) <+>
    text "value" <+> pPrint c1 <+> pPrint y <+> braces (pPrint op1)
  pPrint (OpVerify fx err c op0) = text "verify" <> parens (pPrint (fx, err)) <+> pPrint c <> text "." <+> pPrint op0
  pPrint (OpAssume fx c op0) = text "assume" <> parens (pPrint fx) <+> pPrint c <+> braces (pPrint op0)
  pPrint (OpIterate op) = text "iterate" <+> braces (pPrint op)
  pPrint (OpFold x0 c0 op0 c1 x1 op1 c2 op2) =
    text "next" <> parens (pPrint x0) <+> pPrint c0 <+> braces (pPrint op0) <+>
    text "then" <+> pPrint c1 <+> pPrint x1 <+> braces (pPrint op1) <+>
    text "else" <+> pPrint c2 <+> braces (pPrint op2)
  pPrint (OpExplore fx fxv op) = text "explore" <> parens (pPrint (fx, fxv)) <+> braces (pPrint op)
  pPrint (OpCastDynamic u rhss) = sep $ text "cast" <> parens (pPrint u) : map (nest 2 . pPrint) rhss
  pPrint (OpCastStatic u opv err rhss) = sep $ text "cast" <> parens (pPrint (u, opv, err)) : map (nest 2 . pPrint) rhss
  pPrint (OpErr e) = pPrint e
  pPrint (OpSyntax opv u v s) = text "syntax" <> pPrint (opv,u,v) <> braces (pPrint s)
  pPrint (OpResolved opv op) = text "resolved" <> parens (pPrint opv) <> braces (pPrint op)
  pPrint (OpDefine i d) = text "define" <+> pPrint i <+> pPrint d

  pPrint (OpMetaVar s) = text s
  pPrint OpDone = text "DONE"

instance Pretty RHS where
  pPrint (RHS pat op) = pPrint pat <+> text "=>" <+> pPrint op

data Defineable = DefVertex Vertex | DefDeref Vertex
  deriving (Eq, Ord, Show, Data)

instance Pretty Defineable where
  pPrintPrec l p (DefVertex v) = pPrintPrec l p v
  pPrintPrec l p (DefDeref v) = pPrintPrec l p v <> text "^"

data Err = Err String                 -- Some error code
  deriving (Eq, Ord, Show, Data)

instance Pretty Err where
  pPrint (Err s) = text $ "ERR-" ++ s

data HeadPattern = PatHead Head  -- | ...
  deriving (Eq, Ord, Show, Data)

instance Pretty HeadPattern where
  pPrintPrec l p (PatHead h) = pPrintPrec l p h

opChoices :: [Operation] -> Operation
opChoices = foldr1 OpChoice

------------------------------------------------------

type ExploreContext = Context
type FlexContext = Context
type ScopeContext = Context

type Ctx = Operation -> Operation
type CtxExplore = ExploreContext -> Ctx
type CtxFlex = FlexContext -> Ctx
type CtxScope = ScopeContext -> Ctx
type CtxC = Context -> Ctx
type CtxCC = Context -> CtxC

scopeStartOp :: Operation -> [(CtxScope, Context, Operation)]
scopeStartOp (OpScope sc op) = [(OpScope, sc, op)]
scopeStartOp (OpBeta lp sc0 op0 sc1 op1) =
  [(\ sc0' op0' -> OpBeta lp sc0' op0' sc1   op1,   sc0, op0)
  ,(\ sc1' op1' -> OpBeta lp sc0  op0  sc1'  op1',  sc1, op1)
  ]
scopeStartOp _ = []

flexibleStartOp :: Operation -> [(CtxFlex, Context, Operation)]
flexibleStartOp (OpIn fx mfxv fc i x op) = [(\ fc' op' -> OpIn fx mfxv fc' i x op', fc, op)]
flexibleStartOp (OpStage fx fxv fc0 i x op0 fc1 y op1) =
  [(\ fc op -> OpStage fx fxv fc  i x op  fc1 y op1, fc0, op0)
  ,(\ fc op -> OpStage fx fxv fc0 i x op0 fc  y op,  fc1, op1)]
flexibleStartOp (OpVerify fx err fc op) = [(OpVerify fx err, fc, op)]
flexibleStartOp (OpAssume fx fc op) = [(OpAssume fx, fc, op)]
flexibleStartOp (OpFold x0 fc op d1 x1 op1 d2 op2) = [(\ fc' op' -> OpFold x0 fc' op' d1 x1 op1 d2 op2, fc, op)]
flexibleStartOp _ = []

exploreStartOp :: Operation -> [(CtxExplore, ExploreContext, Operation)]
exploreStartOp (OpExplore fx fxv aop) = do
  (sctx, sop) <- scopeSpan aop
  (ctx, ec, op) <- scopeStartOp sop
  pure (\ ec' op' -> OpExplore fx fxv (sctx (ctx ec' op')), ec, op)
exploreStartOp _ = []

iterateStartOp :: Operation -> [(CtxFlex, FlexContext, Operation)]
iterateStartOp (OpIterate aop) = do
  (sctx, sop) <- scopeSpan aop
  (ctx, ec, op) <- flexibleStartOp sop
  pure (\ ec' op' -> OpIterate (sctx (ctx ec' op')), ec, op)
iterateStartOp _ = []

splitOp :: Operation -> [([Context] -> Operation, [Context])]
splitOp (OpExplore fx fxv aop) = splitOp' (OpExplore fx fxv) scopeStartOp aop
splitOp (OpIterate aop) = splitOp' OpIterate flexibleStartOp aop
splitOp _ = []

splitOp' :: (Operation -> Operation) ->
            (Operation -> [(CtxScope, Context, Operation)]) ->
            Operation ->
            [([Context] -> Operation, [Context])]
splitOp' actx start (OpSeq aop0 aop1) = do
  (ctx0, cs0) <- splitOp' actx start aop0
  (ctx1, cs1) <- splitOp' actx start aop1
  pure (\ cs' -> let (cs0', cs1') = splitAt (length cs0) cs' in actx (OpSeq (ctx0 cs0') (ctx1 cs1')), cs0 ++ cs1)
splitOp' actx start aop = do
  (ctx, c, op) <- start aop
  let unsing [x] = x
      unsing _ = error "unsing"
  pure (\ cs' -> actx (ctx (unsing cs') op), [c])

{-
-- All operations a tree of OpSeq
opsInSeq :: Operation -> [Operation]
opsInSeq (OpSeq op0 op1) = opsInSeq op0 ++ opsInSeq op1
opsInSeq op = [op]
-}
  
scopeSpan :: Operation -> [(Ctx, Operation)]
scopeSpan aop = [(id, aop)] ++
  case aop of
    OpSeq op0 op1 -> do
      (ctx, op) <- scopeSpan op0
      pure (\ op' -> OpSeq (ctx op') op1, op)
     ++ do
      (ctx, op) <- scopeSpan op1
      pure (\ op' -> OpSeq op0 (ctx op'), op)
    OpResolved opv op ->
      pure (\ op' -> OpResolved opv op', op)
    _ -> []

flexibleSpan :: Operation -> [(Ctx, Operation)]
flexibleSpan aop =
  scopeSpan aop
 ++
  do
    (ctx0, op0) <- scopeSpan aop
    (ctx1, c, op1) <- scopeStartOp op0
    (ctx2, op) <- flexibleSpan op1
    pure (\ op' -> ctx0 (ctx1 c (ctx2 op')), op)

iterateSpan :: Operation -> [(Ctx, Operation)]
iterateSpan aop =
  flexibleSpan aop
 ++
  do
    (ctx0, op0) <- flexibleSpan aop
    (ctx1, c, op1) <- flexibleStartOp op0
    (ctx2, op) <- iterateSpan op1
    pure (\ op' -> ctx0 (ctx1 c (ctx2 op')), op)

exploreSpan :: Operation -> [(Ctx, Operation)]
exploreSpan aop =
  iterateSpan aop
 ++
  do
    (ctx0, op0) <- iterateSpan aop
    (ctx1, c, op1) <- iterateStartOp op0
    (ctx2, op) <- exploreSpan op1
    pure (\ op' -> ctx0 (ctx1 c (ctx2 op')), op)

programSpan :: Operation -> [(Ctx, Operation)]
programSpan aop =
  exploreSpan aop
 ++
  do
    (ctx0, op0) <- exploreSpan aop
    (ctx1, c, op1) <- exploreStartOp op0
    (ctx2, op) <- programSpan op1
    pure (\ op' -> ctx0 (ctx1 c (ctx2 op')), op)

scopeOp  :: Operation -> [(CtxScope, ScopeContext, Operation)]
scopeOp aop = do
  start <- [exploreStartOp, iterateStartOp, flexibleStartOp, scopeStartOp]
  (ctx0, sc, op0) <- start aop
  (ctx1, op) <- scopeSpan op0
  pure (\ sc' op' -> ctx0 sc' (ctx1 op'), sc, op)

flexibleOp  :: Operation -> [(CtxFlex, FlexContext, Operation)]
flexibleOp aop = do
  start <- [exploreStartOp, iterateStartOp, flexibleStartOp]
  (ctx0, sc, op0) <- start aop
  (ctx1, op) <- flexibleSpan op0
  pure (\ sc' op' -> ctx0 sc' (ctx1 op'), sc, op)

programOp :: Operation -> [(CtxFlex, FlexContext, Operation)]
programOp aop = do
  (ctx0, op0) <- programSpan aop
  (ctx1, fc, op) <- flexibleOp op0
  pure (\ fc' op' -> ctx0 (ctx1 fc' op'), fc, op)

programUnify :: Operation -> [(FlexContext -> Vertex -> Vertex -> Operation, FlexContext, Vertex, Vertex)]
programUnify aop = do
  (ctx, fc, OpUnify u v) <- programOp aop
  [ (\ fc' u' v' -> ctx fc' (OpUnify u' v'), fc, u, v)
   ,(\ fc' u' v' -> ctx fc' (OpUnify u' v'), fc, v, u) ]

programScope :: Operation -> [(CtxScope, ScopeContext, Operation)]
programScope aop = do
  (ctx0, aop0) <- programSpan aop
  (do (ctx1, sc, op) <- scopeOp aop0; pure (\ sc' op' -> ctx0 (ctx1 sc' op'), sc, op)) ++
    case aop0 of
      OpBeta lp sc op0 c1 op -> pure (\ sc' op' -> OpBeta lp sc' op0 c1 op', sc, op)
      OpStage fx fxv sc i x op0 c1 y op -> pure (\ sc' op' -> OpStage fx fxv sc' i x op0 c1 y op', sc, op)
      _ -> []

programFlexible :: Operation -> [(Context -> Vertex -> Operation, Context, Vertex)]
programFlexible aop = ex1 ++ ex2
  where
    unvar (VertexVariable x) = x
    unvar u = error $ "programFlexible: unvar " ++ show u
    unlam (VertexHead (HeadLambda l)) = l
    unlam u = error $ "programFlexible: unlam " ++ show u
    ex1 = do
      (ctx0, c, aop0) <- programOp aop
      case aop0 of
        OpExists xs -> do
          (ctx1, x) <- listCtx xs
          pure (\ c' u -> ctx0 c' (OpExists (ctx1 (unvar u))), c, VertexVariable x)

        OpUnify u0 u@(VertexHead (HeadLambda _)) -> 
          pure (\ c' u' -> ctx0 c' (OpUnify u0 u'), c, u)
        OpUnify u@(VertexHead (HeadLambda _)) u0 -> 
          pure (\ c' u' -> ctx0 c' (OpUnify u' u0), c, u)

        OpUnify u v@(VertexHead (HeadTuple _ _)) ->
          pure (\ c' v' -> ctx0 c' (OpUnify u v'), c, v)
        OpUnify v@(VertexHead (HeadTuple _ _)) u ->
          pure (\ c' v' -> ctx0 c' (OpUnify v' u), c, v)

        OpBeta (l, p) c0 op0 c1 op1 ->
          pure (\ c' u' -> ctx0 c' (OpBeta (unlam u', p) c0 op0 c1 op1), c, VertexHead (HeadLambda l))

        _ ->
          []
    ex2 = do
      (ctx0, sop0) <- programSpan aop
      case sop0 of
        OpIn fx fxv c i x op ->
          [ (\ c' u' -> ctx0 (OpIn fx fxv c' (unvar u') x op), c, VertexVariable i)
          , (\ c' u' -> ctx0 (OpIn fx fxv c' i (unvar u') op), c, VertexVariable x) ]
        OpStage fx fxv c i x op0 c1 y op1 ->
          [ (\ c' u' -> ctx0 (OpStage fx fxv c' (unvar u') x op0 c1 y op1), c, VertexVariable i)
          , (\ c' u' -> ctx0 (OpStage fx fxv c' i (unvar u') op0 c1 y op1), c, VertexVariable x) ]
        _ ->
          []

listCtx :: [a] -> [(a -> [a], a)]
listCtx [] = []
listCtx (a : as) = (\ a' -> a':as, a) : [ (\ b' -> a : ctx b', b) | (ctx, b) <- listCtx as ]

--instance Show (Integer -> [Integer]) where show f = show (f 99999)

beta :: Operation -> Operation
beta = transform f
  where --f (OpSyntax opv _u _v _s) = OpVar opv
        f (OpDefine _ident _definable) = OpExists []
        f (OpStage fx fxv c0 i x op0 _c1 _y _op1) = OpIn fx (Just fxv) c0 i x op0
        f op = op

data Arg
  = ArgOp Operation
  | ArgFxv EffectVariable
  | ArgV Vertex
  | ArgVU Vertex Vertex
  | ArgVUP Vertex Vertex Vertex
  deriving (Eq, Ord, Show, Data)

instance Pretty Arg where
  pPrintPrec l p (ArgOp op) = pPrintPrec l p op
  pPrintPrec l p (ArgFxv fxv) = pPrintPrec l p fxv
  pPrintPrec l p (ArgV v) = pPrintPrec l p v
  pPrintPrec _ _ (ArgVU v u) = pPrint v <> text "->*" <> pPrint u
  pPrintPrec _ _ (ArgVUP v u p) = pPrint v <> text "->*" <> pPrint u <+> text "at" <+> pPrint p

data Assumption
  = AEffect EffectSpecifier Arg Context                                   -- fx{arg}@c
  | AImpliedEffects EffectSpecifier EffectSpecifier ImpliedFx Context     -- fx1:fx2:ifx@c
  | AFlow Vertex Vertex                                                   -- u->v
  | ASetOpVar OperationVariable Context Operation                         -- opv@c:=op
  | AReadPointer Pointer Vertex                                           -- P^:=u
  deriving (Eq, Ord, Show, Data)

instance Pretty Assumption where
  pPrint (AEffect fx arg c) = pPrint fx <> braces (pPrint arg) <> text "@" <> pPrint c
  pPrint (AImpliedEffects fx1 fx2 afx c) = pPrint fx1 <> text ":" <> pPrint fx2 <> text ":" <> pPrint afx <> text "@" <> pPrint c
  pPrint (AFlow u v) = pPrint u <> text "->" <> pPrint v
  pPrint (ASetOpVar opv c op) = pPrint opv <> text "@" <> pPrint c <+> text ":=" <+> braces (pPrint op)
  pPrint (AReadPointer p v) = pPrint p <> text "^:=" <> pPrint v

data Effect
  = FXsucceeds | FXfails | FXiterates | FXresolves | FXabstracts | FXambiguates
  | FXdiverges | FXdemands
  | FXvaries
  | FXreads | FXwrites | FXallocates
  | FXinteracts | FXthrows | FXsuspends
  | FXunifies
  | FXrejects
  | FXiterates_pending | FXreads_pending | FXwrites_pending | FXinteracts_pending | FXthrows_pending | FXsuspends_pending
  deriving (Eq, Ord, Bounded, Enum, Show, Data)

newtype EffectSpecifier = ES [Effect] -- the effect list has no duplicates and is sorted.
  deriving (Eq, Ord, Show, Data)

instance Pretty EffectSpecifier where
  pPrint _ = undefined
{-
  pPrint afx | afx == tops = text "tops"
             | afx == effects = text "effects"
             | afx == effects' = text "effects'"
             | otherwise =
              let (cfx, cs) = fromJust $ find (\ (a,_) -> a <=== afx) cardEffs
                  sfx = cs : loop (afx `remove` cfx)
                  loop (ES []) = []
                  loop fx@(ES xes) =
                    case find (\ (a,_) -> a <=== fx) otherEffs of
                      Just (xfx, s) -> s : loop (afx `remove` xfx)
                      Nothing -> map show xes
              in  hcat (punctuate (text "\\/") $ map text sfx)
-}
  
pattern Iterates :: EffectSpecifier
pattern Iterates = ES [FXsucceeds, FXfails, FXiterates]
pattern Abstracts :: EffectSpecifier
pattern Abstracts = ES [FXsucceeds, FXfails, FXresolves, FXabstracts]
pattern Reads :: EffectSpecifier
pattern Reads = ES [FXvaries, FXreads]
pattern Writes :: EffectSpecifier
pattern Writes = ES [FXvaries, FXwrites]
pattern Allocates :: EffectSpecifier
pattern Allocates = ES [FXvaries, FXallocates]
pattern Unifies :: EffectSpecifier
pattern Unifies = ES [FXunifies]

pattern Cardinalities :: EffectSpecifier
pattern Cardinalities = ES [FXsucceeds, FXfails, FXiterates, FXresolves, FXabstracts]
imperatives :: EffectSpecifier
imperatives = Unifies\/Diverges\/Transacts\/Suspends\/Interacts\/Throws\/pendings
pendings :: EffectSpecifier
pendings = ES (sort [FXiterates_pending, FXreads_pending, FXwrites_pending, FXinteracts_pending, FXthrows_pending, FXsuspends_pending])
{-

tops :: EffectSpecifier
tops = Succeeds\/Diverges\/Interacts\/Transacts
-}
top_allows :: EffectSpecifier
top_allows = Succeeds\/Unifies\/Diverges\/Transacts\/Interacts
effects :: EffectSpecifier
effects = Cardinalities \/ imperatives \/ Rejects
{-
effects' :: EffectSpecifier
effects' = effects `remove` Demands

pattern Contradicts :: EffectSpecifier
pattern Contradicts = ES []
-}
pattern Succeeds :: EffectSpecifier
pattern Succeeds = ES [FXsucceeds]
pattern Fails :: EffectSpecifier
pattern Fails = ES [FXfails]
pattern Decides :: EffectSpecifier
pattern Decides = ES [FXsucceeds, FXfails]
pattern Resolves :: EffectSpecifier
pattern Resolves = ES [FXsucceeds, FXfails, FXresolves]
pattern Varies :: EffectSpecifier
pattern Varies = ES [FXvaries]
pattern Transacts :: EffectSpecifier
pattern Transacts = ES [FXreads, FXwrites, FXallocates]
pattern Interacts :: EffectSpecifier
pattern Interacts = ES [FXinteracts]
pattern Demands :: EffectSpecifier
pattern Demands = ES [FXdemands]
pattern Diverges :: EffectSpecifier
pattern Diverges = ES [FXdiverges]
pattern Suspends :: EffectSpecifier
pattern Suspends = ES [FXsuspends]
pattern Throws :: EffectSpecifier
pattern Throws = ES [FXthrows]
pattern Rejects :: EffectSpecifier
pattern Rejects = ES [FXrejects]
{-

----------------------------------------------------------

class Desugar syn where
  desugar :: Data a => a -> Vertex -> Vertex -> syn -> Operation


-- ...

data Arg = ArgOp Operation | ArgOpv OperationVariable | ArgV Vertex
  deriving (Eq, Ord, Show, Data)

data Assumption
  = AEffect EffectSpecifier Arg Context                                   -- fx{arg}@c
  | AEqual EqualityStrength Vertex Vertex Context                         -- st{u=v}@c
  | AFlow Vertex Vertex                                                   -- v1->v0
  | AInherits Context Context                                             -- d inherits c
  | ASees Context Context                                                 -- d sees c
  | AFlex Vertex Context                                                  -- v@c
  | AImpliedEffects EffectSpecifier EffectSpecifier ImpliedFx Context     -- fx1:fx2:ifx@c
  | ASolveOrImply SolveOrImply Context                                    -- si@c
  | AVerifyOrBeta VerifyOrBeta Context                                    -- vb@c
  | ASetOpVar OperationVariable Context Operation                         -- opv@c:=op
  | AResolvedIdent Variable Context Vertex                                -- Ident@c:=r
  | AReadPointer Pointer Vertex                                           -- P^:=u
  | ACtxIsOp Context Operation                                            -- c:=op
  deriving (Eq, Ord, Show, Data)

pattern AEffectOp :: EffectSpecifier -> Operation -> Context -> Assumption
pattern AEffectOp fx op c = AEffect fx (ArgOp op) c
pattern AEffectOpv :: EffectSpecifier -> OperationVariable -> Context -> Assumption
pattern AEffectOpv fx opv c = AEffect fx (ArgOpv opv) c
pattern AEffectVertex :: EffectSpecifier -> Vertex -> Context -> Assumption
pattern AEffectVertex fx v c = AEffect fx (ArgV v) c

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

{-
instance Pretty RunLambda where
  pPrint (RunLambda u i w op0) =
    text "lambda" <> parens (pPrint u) <+> pPrint i <+> pPrint w <> text "." <+> pPrint op0
-}
{-
instance Pretty Program where
  pPrint (Program op) = xsep [text "program", nest 2 $ pPrint op]
  pPrint ProgramDone = text "DONE"
-}
instance Pretty Assumption where
  pPrint (AEffectOp fx arg c) = pPrint fx <> braces (pPrint arg) <> text "@" <> pPrint c
  pPrint (AEffectOpv fx arg c) = pPrint fx <> braces (pPrint arg) <> text "@" <> pPrint c
  pPrint (AEffectVertex fx arg c) = pPrint fx <> braces (pPrint arg) <> text "@" <> pPrint c
  pPrint AEffect{} = undefined
  pPrint (AEqual st u v c) = pPrint st <> braces (pPrint u <> text "=" <> pPrint v) <> text "@" <> pPrint c
  pPrint (AFlow v1 v0) = pPrint v0 <> text "->" <> pPrint v1
  pPrint (AFlex v c) = pPrint v <> text "@" <> pPrint c
  pPrint (AImpliedEffects fx1 fx2 afx c) = pPrint fx1 <> text ":" <> pPrint fx2 <> text ":" <> pPrint afx <> text "@" <> pPrint c
  pPrint (ASolveOrImply si c) = pPrint si <> text "@" <> pPrint c
  pPrint (AVerifyOrBeta vb c) = pPrint vb <> text "@" <> pPrint c
  pPrint (AResolvedIdent x c v) = pPrint x <> text "@" <> pPrint c <> text ":=" <> pPrint v
  pPrint (ASetOpVar opv c op) = pPrint opv <> text "@" <> pPrint c <+> text ":=" <+> braces (pPrint op)
  pPrint (ACtxIsOp c op) = pPrint c <> text ":=" <> pPrint op
  pPrint (AInherits c0 c1) = pPrint c0 <+> text "inherits" <+> pPrint c1
  pPrint (ASees c0 c1) = pPrint c0 <+> text "sees" <+> pPrint c1
  pPrint (AReadPointer p v) = pPrint p <> text "^:=" <> pPrint v
--  pPrint (ADominates fx u v c) = pPrint fx <> braces (pPrint u <> text ">>" <> pPrint v) <> text "@" <> pPrint c
instance Pretty NativeOp where
  pPrint (NativeOp n) = text n
  
instance Pretty AssumptionSet where
  pPrint (A []) = text "empty"
  pPrint (A as) = sep (punctuate (text ",") (map pPrint (sort as)))

instance Pretty Config where
  pPrint (g :|- pg) = xsep [pPrint g, text "|-", pPrint pg]

instance Pretty (Operation -> Program) where
  pPrint f = pPrint (f (OpVar "OP"))

instance Pretty (Context -> Operation -> Program) where
  pPrint f = pPrint (f (Context (Ident "SC")) (OpVar "OP"))

instance Pretty (Context -> Context -> Operation -> Program) where
  pPrint f = pPrint (f (Context (Ident "FC")) (Context (Ident "SC")) (OpVar "OP"))

{-
instance Pretty (Context -> Operation -> Program) where
  pPrint f = pPrint (f (Context (Ident "C")) (OpVar "OP"))
-}

------------------------------------------------------------------

data Effect
  = FXsucceeds | FXfails | FXiterates | FXresolves | FXabstracts
  | FXdiverges | FXdemands
  | FXvaries
  | FXreads | FXwrites | FXallocates
  | FXinteracts | FXthrows | FXsuspends
  | FXiterates_pending | FXreads_pending | FXwrites_pending | FXinteracts_pending | FXthrows_pending | FXsuspends_pending
  deriving (Eq, Ord, Bounded, Enum, Show, Data)

newtype EffectSpecifier = ES [Effect] -- the effect list has no duplicates and is sorted.
  deriving (Eq, Ord, Show, Data)

imperatives :: EffectSpecifier
imperatives = Diverges\/Demands\/Reads\/Writes\/Allocates\/Interacts\/Throws\/Suspends\/pendings
tops :: EffectSpecifier
tops = Succeeds\/Diverges\/Interacts\/Transacts
effects :: EffectSpecifier
effects = Cardinalities \/ imperatives \/ Varies
effects' :: EffectSpecifier
effects' = effects `remove` Demands
pendings :: EffectSpecifier
pendings = ES (sort [FXiterates_pending, FXreads_pending, FXwrites_pending, FXinteracts_pending, FXthrows_pending, FXsuspends_pending])

pattern Contradicts :: EffectSpecifier
pattern Contradicts = ES []
pattern Succeeds :: EffectSpecifier
pattern Succeeds = ES [FXsucceeds]
pattern Fails :: EffectSpecifier
pattern Fails = ES [FXfails]
pattern Decides :: EffectSpecifier
pattern Decides = ES [FXsucceeds, FXfails]
pattern Iterates :: EffectSpecifier
pattern Iterates = ES [FXsucceeds, FXfails, FXiterates]
pattern Resolves :: EffectSpecifier
pattern Resolves = ES [FXsucceeds, FXfails, FXresolves]
pattern Abstracts :: EffectSpecifier
pattern Abstracts = ES [FXsucceeds, FXfails, FXresolves, FXabstracts]
pattern Cardinalities :: EffectSpecifier
pattern Cardinalities = ES [FXsucceeds, FXfails, FXiterates, FXresolves, FXabstracts]
pattern Varies :: EffectSpecifier
pattern Varies = ES [FXvaries]
pattern Reads :: EffectSpecifier
pattern Reads = ES [FXvaries, FXreads]
pattern Writes :: EffectSpecifier
pattern Writes = ES [FXvaries, FXwrites]
pattern Allocates :: EffectSpecifier
pattern Allocates = ES [FXvaries, FXallocates]
pattern Transacts :: EffectSpecifier
pattern Transacts = ES [FXvaries, FXreads, FXwrites, FXallocates]
pattern Interacts :: EffectSpecifier
pattern Interacts = ES [FXinteracts]
pattern Demands :: EffectSpecifier
pattern Demands = ES [FXdemands]
pattern Diverges :: EffectSpecifier
pattern Diverges = ES [FXdiverges]
pattern Suspends :: EffectSpecifier
pattern Suspends = ES [FXsuspends]
pattern Throws :: EffectSpecifier
pattern Throws = ES [FXthrows]
--pattern Bottom = ES []

allEffects :: [EffectSpecifier]
allEffects = [ cfx \/ ifx
             | cfx <- [Contradicts, Succeeds, Fails, Decides, Iterates, Resolves, Abstracts, Cardinalities]
             , ifx <- map joins $ sublists [Varies, Reads, Writes, Allocates, Interacts, Demands, Diverges, Suspends, Throws]
             ]

invert :: EffectSpecifier -> EffectSpecifier
invert (ES xs) = ES [ e | e <- [minBound .. maxBound], e `notElem` xs ]

effIn :: Effect -> EffectSpecifier -> Bool
effIn e (ES fx) = e `elem` fx

sublists :: [a] -> [[a]]
sublists = filterM (const [False, True])
-}

-- The join (union) of two EffectSpecifier
(\/) :: EffectSpecifier -> EffectSpecifier -> EffectSpecifier
ES fx0 \/ ES fx1 = ES $ merge fx0 fx1
  where merge [] es1 = es1
        merge es0 [] = es0
        merge (e0:es0) (e1:es1) =
          case compare e0 e1 of
            LT -> e0 : merge es0 (e1:es1)
            EQ -> e0 : merge es0 es1
            GT -> e1 : merge (e0:es0) es1

{-
joins :: [EffectSpecifier] -> EffectSpecifier
joins = foldr (\/) (ES [])

-- The meet (intersection) of two EffectSpecifier
(/\) :: EffectSpecifier -> EffectSpecifier -> EffectSpecifier
ES fx0 /\ ES fx1 = ES $ isect fx0 fx1
  where isect (e0:es0) (e1:es1) =
          case compare e0 e1 of
            LT -> isect es0 (e1:es1)
            EQ -> e0 : isect es0 es1
            GT -> isect (e0:es0) es1
        isect _ _ = []

ameet :: ImpliedFx -> EffectSpecifier -> ImpliedFx
ameet None _ = None
ameet (Some fx1) fx = Some (fx1/\fx)

-- Partial order on EffectSpecifier, fx0 has fewer (or same) effects than fx1
(<===) :: EffectSpecifier -> EffectSpecifier -> Bool
fx0 <=== fx1 = (fx0/\fx1) == fx0

remove :: EffectSpecifier -> EffectSpecifier -> EffectSpecifier
remove (ES fx0) (ES fx1)  = ES (filter (`notElem` fx1) fx0)

_weakenFx :: EffectSpecifier -> EffectSpecifier -> EffectSpecifier
_weakenFx fx weaken =
  if (fx/\Cardinalities) <=== Fails then fx \/ (weaken /\ Fails) else fx \/ (weaken /\ Cardinalities)

instance Pretty Effect where pPrint = text . drop 2 . show

cardEffs :: [(EffectSpecifier, String)]
cardEffs =
  [(Cardinalities, "cardinalities")
  ,(Abstracts, "abstracts")
  ,(Resolves, "resolves")
  ,(Iterates, "iterates")
  ,(Decides, "decides")
  ,(Fails, "fails")
  ,(Succeeds, "succeeds")
  ,(Contradicts, "contradicts")
  ]
otherEffs :: [(EffectSpecifier, String)]
otherEffs =
  [(Transacts, "transacts")
  ,(Reads, "reads")
  ,(Writes, "writes")
  ,(Allocates, "allocates")
  ,(Varies, "varies")
  ,(Demands, "demands")
  ,(Diverges, "diverges")
  ,(Suspends, "suspends")
  ,(Throws, "throws")
  ]

instance Pretty EffectSpecifier where
  pPrint afx | afx == tops = text "tops"
             | afx == effects = text "effects"
             | afx == effects' = text "effects'"
             | otherwise =
              let (cfx, cs) = fromJust $ find (\ (a,_) -> a <=== afx) cardEffs
                  sfx = cs : loop (afx `remove` cfx)
                  loop (ES []) = []
                  loop fx@(ES xes) =
                    case find (\ (a,_) -> a <=== fx) otherEffs of
                      Just (xfx, s) -> s : loop (afx `remove` xfx)
                      Nothing -> map show xes
              in  hcat (punctuate (text "\\/") $ map text sfx)

-- QQQ: How can * be commutative?
-- for all fx diverges*fx = diverges
-- but fails * diverges = fails

star :: EffectSpecifier -> EffectSpecifier -> EffectSpecifier
star fx0 fx1 = (fx0\/fx1)/\
  if      m <=== Fails   then Fails\/imperatives
  else if m <=== Demands then Abstracts\/imperatives
  else                       effects'
  where m = fx0/\fx1

------------------------------------------------------------------

class Bvs a where
  bvs :: a -> [Ident]

{-
instance Bvs Program where
  bvs (Program op) = bvs op
  bvs ProgramDone = []
-}

instance Bvs Operation where
  bvs (OpUnify u v) = bvs u ++ bvs v
  bvs (OpFail u) = bvs u
  bvs (OpSeq op0 op1) = bvs op0 ++ bvs op1
  bvs (OpChoice op0 op1) = bvs op0 ++ bvs op1
  bvs (OpExists vs) = [ x | Variable x <- vs ]
  bvs (OpScope (Context c) op) = c : bvs op
  -- QQQ7: op0 and op1?
  bvs (OpBeta _lp (Context c0) op0 (Context c1) op1) = c0 : c1 : bvs op0 ++ bvs op1
  bvs (OpIterate op) = bvs op
  bvs (OpNext u (Context c0) _v0 op0 _c1 _v1 _op1 _c2 _op2) = c0 : bvs op0 ++ bvs u -- QQQ7: op1, op2
  -- lambda ???
  bvs (OpExplore _fx op) = bvs op
  bvs OpIn{} = []
  bvs OpStage{} = []
  bvs (OpVerify _ _ (Context c) op) = c : bvs op
  bvs (OpAssume _ (Context c) op) = c : bvs op
  bvs (OpSyntax _ _ _) = []
  bvs (OpNative _) = []
  bvs (OpErr _) = []
  bvs (OpVar _) = []
  bvs OpDone = []

instance Bvs Vertex where
  bvs (VertexVariable _) = []
  bvs (VertexHead h) = bvs h
  bvs (VertexCall u v) = bvs u ++ bvs v

instance Bvs Head where
  bvs (HeadAtom _) = []
  bvs (HeadLambda l) = bvs l
--  bvs (HeadRunLambda l) = bvs l
  bvs (HeadTuple u (Variable x)) = x : bvs u
  bvs (HeadNom _ v) = bvs v

instance Bvs Lambda where
  bvs (Lambda _oc _fx u (Context c0) (Variable i) (Variable w) op0
                        (Context c1) (Variable j) (Variable z) op1) = [c0,i,w,c1,j,z] ++ bvs u ++ bvs op0 ++ bvs op1
{-
instance Bvs RunLambda where
  bvs (RunLambda u (Variable i) (Variable z) op0) = i : z : bvs u ++ bvs op0
-}
------------------------------------------------------------------
-}

class NewIdents i o where
  newIdents :: (Data a) => a -> i -> o
instance NewIdents String Ident where
  newIdents x s = is !! 0  where is = identsNotIn x [Ident s]

instance NewIdents (String, String) (Ident, Ident) where
  newIdents x (s1, s2) = (is !! 0, is !! 1)  where is = identsNotIn x [Ident s1, Ident s2]
instance NewIdents (String, String) (OperationVariable, OperationVariable) where
  newIdents x (s1, s2) = (OperationVariable (is !! 0), OperationVariable (is !! 1))  where is = identsNotIn x [Ident s1, Ident s2]
instance NewIdents (String, String) (Context, Context) where
  newIdents x (s1, s2) = (Context (is !! 0), Context (is !! 1))  where is = identsNotIn x [Ident s1, Ident s2]

instance NewIdents (String, String, String) (Ident, Ident, Ident) where
  newIdents x (s1, s2, s3) = (is !! 0, is !! 1, is !! 2)  where is = identsNotIn x [Ident s1, Ident s2, Ident s3]
instance NewIdents (String, String, String, String) (Ident, Ident, Ident, Ident) where
  newIdents x (s1, s2, s3, s4) = (is !! 0, is !! 1, is !! 2, is !! 3)  where is = identsNotIn x [Ident s1, Ident s2, Ident s3, Ident s4]
instance NewIdents (String, String, String, String, String) (Ident, Ident, Ident, Ident, Ident) where
  newIdents x (s1, s2, s3, s4, s5) = (is !! 0, is !! 1, is !! 2, is !! 3, is !! 4)  where is = identsNotIn x [Ident s1, Ident s2, Ident s3, Ident s4, Ident s5]

instance NewIdents [String] [Ident] where
  newIdents x ss = is  where is = identsNotIn x (map Ident ss)
instance NewIdents (Int, String) [Ident] where
  newIdents x (n, s) = take n $ identsNotIn x [Ident s]
instance NewIdents [Ident] [Ident] where
  newIdents x ss = is  where is = identsNotIn x ss

identsOf :: Data i => i -> [Ident]
identsOf = universeBi

identsNotIn :: Data i => i -> [Ident] -> [Ident]
identsNotIn x is = idents is \\ identsOf x

-- Make variations on the identifiers
idents :: [Ident] -> [Ident]
idents is = concatMap (\ s -> map (addSuf s) is) sufs
  where sufs = "" : "'" : map show [1::Integer ..]
        addSuf s (Ident i) = Ident (i ++ s)

{-
-- e[y/x]
substVar :: (Data a) => Variable -> Variable -> a -> a
substVar y x = transformBi f
  where f z | z == x    = y
            | otherwise = z

-- e[y/x]
substCtx :: (Data a) => Context -> Context -> a -> a
substCtx y x = transformBi f
  where f z | z == x    = y
            | otherwise = z

map5 :: (a -> b) -> (a,a,a,a,a) -> (b,b,b,b,b)
map5 f (a1,a2,a3,a4,a5) = (f a1, f a2, f a3, f a4, f a5)

map4 :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
map4 f (a1,a2,a3,a4) = (f a1, f a2, f a3, f a4)

map2 :: (a -> b) -> (a,a) -> (b,b)
map2 f (a1,a2) = (f a1, f a2)

------------------------------------------------------------------

dsS :: Data a => a -> Vertex -> Vertex -> Syntax -> Operation
dsS pg u v (SyntaxExpr e) = dsE pg u v e
dsS pg u v (SyntaxList (Snoc aes e)) =
  let n = length aes
      is = map Variable $ newIdents pg (n, "i")
      xs = map Variable $ newIdents pg (n, "x")
      pg' = (pg, is, xs)
  in  opSeqs $ OpExists (is ++ xs) :
               zipWith3 (dsS pg') (map VertexVariable is) (map VertexVariable xs) aes ++ [dsS pg' u v e]
dsS _ _ _ _ = undefined

dsE :: Data a => a -> Vertex -> Vertex -> Expr -> Operation
dsE _ u v (ExprAtom a) = OpUnify u h +> OpUnify v h
  where h = VertexHead $ HeadAtom a
dsE pg u v (ExprUnify e1 e2) = dsE pg u v e1 +> dsE pg u v e2
dsE pg u v (ExprLambda s0 s1) = dsE pg u v $ ExprFunction s0 Closed [Succeeds] s1
dsE pg u v (ExprFunction s0 oc [] s1) = dsE pg u v $ ExprFunction s0 oc [Succeeds,Transacts] s1
dsE pg u v (ExprFunction s0 oc fxs@(_:_) s1) =
  OpUnify v $ VertexHead $ HeadLambda $ Lambda oc (joins fxs) u d0 i w (dsE pg' vi vw s0) d1 j z (dsE pg' vj vz s1)
  where d0 = Context  $ newIdents pg "d0"
        i  = Variable $ newIdents pg "i"; vi = VertexVariable i
        w  = Variable $ newIdents pg "w"; vw = VertexVariable w
        d1 = Context  $ newIdents pg "d1"
        j  = Variable $ newIdents pg "j"; vj = VertexVariable j
        z  = Variable $ newIdents pg "z"; vz = VertexVariable z
        pg' = (pg, d0, i, w, d1, j, z)
{-
dsE u v ae@(ExprList (Snoc aes e)) =
  let n = length aes
      is = map Variable $ newIdents ae (n, "i")
      xs = map Variable $ newIdents ae (n, "x")
  in  opSeqs $ OpExists (is ++ xs) :
               zipWith3 dsE (map VertexVariable is) (map VertexVariable xs) aes ++ [dsE u v e]
-}
dsE pg u v (ExprAt s0 s1) =
  OpExists [h, g, j, x, z] +>
  OpUnify u vz +>
  OpUnify v vz +>
  dsE pg vh vg s0 +>
  dsE pg vj vx s1 +>
  OpUnify vz (VertexCall vg vx)
  where vars@(h, g, j, x, z) = map5 Variable $ newIdents pg ("h", "g", "j", "x", "z")
        (vh, vg, vj, vx, vz) = map5 VertexVariable vars
-- QQQ8:
-- should it be     S(u,v,array{s0, ..., sn-1}) ---> u=tuple(n) i; v=tuple(n) x; S(u(0),v(0),s0); ...; S(u(n-1),v(n-1),sn-1)
dsE pg u v (ExprArray ss) =
  let (i, x) = map2 Variable $ newIdents pg ("i", "x")
      n = VertexHead (HeadAtom (AtomRational (fromIntegral (length ss))))
      tupi = VertexHead (HeadTuple n i)
      tupx = VertexHead (HeadTuple n x)
      ix a k = VertexCall a (VertexHead (HeadAtom (AtomRational (fromIntegral k))))
      --ds k s = dsE pg (ix u k) (ix v k) s
      ds k s = dsE pg (ix tupi k) (ix tupx k) s
  in
  OpUnify u tupi +>
  OpUnify v tupx +>
  opSeqs [ ds k s | (s, k) <- zip ss [0::Int ..] ]
dsE _ u v x = OpSyntax u v (SyntaxExpr x)
-}

opSeqs :: [Operation] -> Operation
opSeqs = foldr1 OpSeq

{-
------------------------------------------------------------------

instance Rec Config where
  data RuleEnv Config = RuleEnvConfig
  rec r s ae = r s ae

sys :: TRSystem Config
sys = TRSystem {
  sname = "TimRun", description = "Tim's runtime rules", ruleEnv = RuleEnvConfig,
  preProcess = \ _ x -> x, postProcess = \ _ x -> x, rules = allRules,
  rules2 = \ _ _ -> [], rulesHaveStructural = False, confluenceRules = \ _ _ -> [],
  validExpr = \ _ _ -> True, sortRewrites = id,
  displayRules = \ _ -> False
  }

pptr :: Config -> IO ()
pptr = hpptr stdout

hpptr :: Handle -> Config -> IO ()
hpptr h = mapM_ (hppx h) . f . normalFormFuelTracePlain sys 10000
  where f x = if null (nrLeft x) then nrDone x else trace "**** no fuel " (nrLeft x)

fpptr :: FilePath -> Config -> IO ()
fpptr fn c = do
  h <- openFile fn WriteMode
  hpptr h c
  hClose h

norm :: Config -> Config
norm = term . (!!0) . f . normalFormFuelTracePlain sys 10000
  where f x = if null (nrLeft x) then nrDone x else trace "**** no fuel " (nrLeft x)

------------------------------------------------------------------

gAdd :: [Assumption] -> [Assumption] -> [AssumptionSet]
gAdd asms g =
  let asms' = filter (\ a -> not (weaker a g)) asms
      -- weaker1 a1 a2, a1 is a2, but with more effects
      weaker1 :: Assumption -> Assumption -> Bool
      weaker1 (AEffect fx a c) (AEffect fx' a' c') = a == a' && c == c' && fx' <=== fx
      weaker1 a a' = a == a'
      -- weaker a as, if a is weaker than any of the as
      weaker :: Assumption -> [Assumption] -> Bool
      weaker a as = any (weaker1 a) as
  in  if null asms' then
        []
      else
        [A $ asms' ++ filter (\ a -> not (weaker a asms')) g]
--        [A $ asms' ++ g]

before :: Rule a -> Rule a -> Rule a
before aRules bRules cfg a =
  case aRules cfg a of
    [] -> bRules cfg a
    as -> as

------------------------------------------------------------------

flexibleSpan :: Operation -> [(Ctx, Operation)]
flexibleSpan aop =
  scopeSpan aop
 ++
  do
    (ctx1, op1)      <- scopeSpan aop
    (ctx2, sc2, op2) <- scopeStartOp op1
    (ctx3, op3)      <- flexibleSpan op2
    pure (\ op3' -> ctx1 (ctx2 sc2 (ctx3 op3')), op3)

exploreSpan :: Operation -> [(Ctx, Operation)]
exploreSpan aop = do
  scopeSpan aop
 ++
  do
    (ctx1, op1)      <- scopeSpan aop
    (ctx2, sc2, op2) <- scopeStartOp op1
    (ctx3, op3)      <- exploreSpan op2
    pure (\ op3' -> ctx1 (ctx2 sc2 (ctx3 op3')), op3)
 ++
  do
    (ctx1, op1)      <- scopeSpan aop
    (ctx2, sc2, op2) <- flexibleStartOp op1
    (ctx3, op3)      <- exploreSpan op2
    pure (\ op3' -> ctx1 (ctx2 sc2 (ctx3 op3')), op3)

programSpan :: Operation -> [(Ctx, Operation)]
programSpan aop = do
  scopeSpan aop
 ++
  do
    (ctx1, op1)      <- scopeSpan aop
    (ctx2, sc2, op2) <- scopeStartOp op1
    (ctx3, op3)      <- programSpan op2
    pure (\ op3' -> ctx1 (ctx2 sc2 (ctx3 op3')), op3)
 ++
  do
    (ctx1, op1)      <- scopeSpan aop
    (ctx2, sc2, op2) <- flexibleStartOp op1
    (ctx3, op3)      <- programSpan op2
    pure (\ op3' -> ctx1 (ctx2 sc2 (ctx3 op3')), op3)
 ++
  do
    (ctx1, op1) <- scopeSpan aop
    (ctx2, op2) <- exploreStartOp op1
    (ctx3, op3) <- programSpan op2
    pure (\ op3' -> ctx1 (ctx2 (ctx3 op3')), op3)

scopeOp :: Operation -> [(CtxScope, Context, Operation)]
scopeOp aop = do
  (ctx1, op1)      <- scopeSpan aop
  (ctx2, sc2, op2) <- scopeStartOp op1
  pure (\ sc2' op2' -> ctx1 (ctx2 sc2' op2'), sc2, op2)

checkEq :: (HasCallStack) => Context -> Context -> b -> b
checkEq a a' b | a == a' = b
               | a == Context (Ident "FC"), a' == Context (Ident "SC") = b  -- pretty printing hack
               | otherwise = error $ "checkEq: " ++ prettyShow (a, a')

flexibleOp :: Operation -> [(CtxCC, FlexContext, ScopeContext, Operation)]
flexibleOp aop =
  do
    (ctx0, op0)      <- scopeSpan aop
    (ctx1, fc1, op1) <- flexibleStartOp op0
    -- XXX check _fc1 and fc1' are equal?
    pure (\ _fc1 fc1' op1' -> checkEq _fc1 fc1' $ ctx0 (ctx1 fc1' op1'), fc1, fc1, op1)
 ++
  do
    (ctx0, op0)      <- scopeSpan aop
    (ctx1, fc1, op1) <- flexibleStartOp op0
    (ctx2, sc2, op2) <- scopeOp op1
    pure (\ fc1' sc2' op2' -> ctx0 (ctx1 fc1' (ctx2 sc2' op2')), fc1, sc2, op2)

iterateOp :: Operation -> [(CtxCC, FlexContext, ScopeContext, Operation)]
iterateOp aop =
{-
  do
    (ctx0, op0)      <- scopeSpan aop
    (ctx1, fc1, op1) <- iterateStartOp op0
    pure (\ _fc1 fc1' op1' -> checkEq _fc1 fc1' $ ctx0 (ctx1 fc1' op1), fc1, op1)
 ++
-}
  do
    (ctx0, op0)      <- scopeSpan aop
    (ctx1, fc1, op1) <- iterateStartOp op0
    (ctx2, sc2, op2) <- scopeOp op1
    pure (\ fc1' sc2' op2' -> ctx0 (ctx1 fc1' (ctx2 sc2' op2')), fc1, sc2, op2)

exploreOp :: Operation -> [(CtxCC, FlexContext, ScopeContext, Operation)]
exploreOp aop =
{-
  do
    (ctx0, op0)      <- scopeSpan aop
    (ctx1, fc1, op1) <- exploreStartOp op0
    pure (\ _fc1 fc1' op1' -> checkEq _fc1 fc1' $ ctx0 (ctx1 fc1' op1), fc1, op1)
 ++
-}
  do
    (ctx0, op0)      <- scopeSpan aop
    (ctx1, op1)      <- exploreStartOp op0
    (ctx2, sc2, op2) <- scopeOp op1
    pure (\ _sc2 sc2' op2' -> checkEq _sc2 sc2' $ ctx0 (ctx1 (ctx2 sc2' op2')), sc2, sc2, op2)
 ++
  do
    (ctx0, op0)           <- scopeSpan aop
    (ctx1, op1)           <- exploreStartOp op0
    (ctx2, fc2, sc2, op2) <- flexibleOp op1
    pure (\ fc2' sc2' op2' -> ctx0 (ctx1 (ctx2 fc2' sc2' op2')), fc2, sc2, op2)

programOpX :: Operation -> [(CtxCC, FlexContext, ScopeContext, Operation)]
programOpX aop =
{-
  do
    (ctx0, op0) <- scopeSpan aop
    (ctx1, op1) <- exploreStartOp op0
    pure (\ fc1' op1' -> ctx0 (ctx1 fc1' op1), fc1, op1)
 ++
-}
  do
    (ctx0, op0)      <- scopeSpan aop
    (ctx1, op1)      <- exploreStartOp op0
    (ctx2, sc2, op2) <- scopeOp op1
    pure (\ _sc2 sc2' op2' -> checkEq _sc2 sc2' $ ctx0 (ctx1 (ctx2 sc2' op2')), sc2, sc2, op2)
 ++
  do
    (ctx0, op0)           <- scopeSpan aop
    (ctx1, op1)           <- exploreStartOp op0
    (ctx2, fc2, sc2, op2) <- flexibleOp op1
    pure (\ fc2' sc2' op2' -> ctx0 (ctx1 (ctx2 fc2' sc2' op2')), fc2, sc2, op2)
 ++
  do
    (ctx0, op0)           <- scopeSpan aop
    (ctx1, op1)           <- exploreStartOp op0
    (ctx2, fc2, sc2, op2) <- exploreOp op1
    pure (\ fc2' sc2' op2' -> ctx0 (ctx1 (ctx2 fc2' sc2' op2')), fc2, sc2, op2)

programOp :: Operation -> [(CtxCC, FlexContext, ScopeContext, Operation)]
programOp = programOpX

programExploreOp :: Operation -> [(Ctx, Operation)]
programExploreOp aop =
  exploreStartOp aop
 ++
  do
    (ctx1, fc1, sc1, op1) <- programOpX aop
    (ctx2, op2)           <- programExploreOp op1
    pure (\ op2' -> ctx1 fc1 sc1 (ctx2 op2'), op2)

------------------------------------------------------------------

anyOp :: Operation -> [(Ctx, Operation)]
anyOp aop = [(id, aop)] ++ concatMap expnd (
  case aop of
    OpSeq op0 op1 -> [(\ op0' -> OpSeq op0' op1, op0), (\ op1' -> OpSeq op0 op1', op1)]
    OpChoice op0 op1 -> [(\ op0' -> OpChoice op0' op1, op0), (\ op1' -> OpChoice op0 op1', op1)]
    OpScope c op -> [(OpScope c, op)]
    OpBeta lp c0 op0 c1 op1 -> [(\ op0' -> OpBeta lp c0 op0' c1 op1, op0), (\ op1' -> OpBeta lp c0 op0 c1 op1', op1)]
    OpExplore fx op -> [(OpExplore fx, op)]
    OpIterate op -> [(OpIterate, op)]
    OpNext u0 c0 v0 op0 c1 v1 op1 c2 op2 -> [ (\ op0' -> OpNext u0 c0 v0 op0' c1 v1 op1 c2 op2, op0)
                                            , (\ op1' -> OpNext u0 c0 v0 op0 c1 v1 op1' c2 op2, op1)
                                            , (\ op2' -> OpNext u0 c0 v0 op0 c1 v1 op1 c2 op2', op2)]
--    OpCast -> undefined
    OpIn fx c i x op -> [(OpIn fx c i x, op)]
    OpStage fx opv c0 i x op0 c1 y op1 -> [(\ op0' -> OpStage fx opv c0 i x op0' c1 y op1, op0), (\ op1' -> OpStage fx opv c0 i x op0 c1 y op1', op1)]
    OpVerify fx oerr c op0 -> [(OpVerify fx oerr c, op0)]
    OpAssume fx c op0 -> [(OpAssume fx c, op0)]
    _ -> []
    )
 where expnd (ctx, op) = [ (ctx . ictx, iop) | (ictx, iop) <- anyOp op ]

desugarRules :: Rule Config
desugarRules _ (g :|- pg) =
  "S" `name`
  do
    (ctx, op@(OpSyntax u v s)) <- anyOp pg
    let op' = dsS pg u v s
    guard $ op /= op'
    pure $ g :|- ctx op'

------------------------------------------------------------------

allRules :: Rule Config
allRules =
  desugarRules P.<>
  programRules P.<>
  equalRules P.<>
  existsRules

{-
allRules :: Rule Config
allRules =
  fxIntersectsRule `before`
  (desugarRules P.<>
   identRules P.<>
   programRules P.<> 
   assumptionRules P.<>
   unificationRules P.<>
   eliminationRules P.<>
   existsRules P.<>
   condRules P.<>
   scopeRules P.<>
   choiceRules P.<>
--   _fxWeakenRule P.<>
   constRules P.<>
   dominatorRules P.<>
   runtimeRules
  )
  `before`
  dominators
  `before`
  programElimRule
-}

programRules :: Rule Config
programRules _ (A g :|- pg) =
  "program-intro" `name`
  do
    OpExplore atops (OpScope c _op) <- [pg]
    guard (atops == tops)
    g' <- gAdd [AVerifyOrBeta Verify c, ASolveOrImply Solve c, AImpliedEffects effects tops None c] g
    pure $ g' :|- pg
 ++
  "program-elim" `name`
  do
    OpExplore atops op <- [pg]
    guard $ atops == tops
    let scopeDone (OpScope c_i op_i) = not $ null [ () | AEffectOp fx op_i' c_i' <- g, op_i == op_i', c_i == c_i', fx <=== atops ]
        scopeDone _ = False
    guard $ all (scopeDone . snd) (scopeSpan op)
    pure $ A [] :|- OpDone

-- fx-weaken
-- fx-intersect

atomRules :: Rule Config
atomRules _ (A _g :|- _pg) = []

-- equal-atom
-- atom-resolves

------------------------------------------------------------------

equalRules :: Rule Config
equalRules _ (A g :|- pg) =
  "equal-reflexive" `name`
  do
    AFlex v c <- g
    g' <- gAdd [AEffectOp Succeeds (OpUnify v v) c] g
    pure $ g' :|- pg
 ++
  "equal-symmetric" `name`
  do
    AEffectOp fx (OpUnify u v) c <- g
    g' <- gAdd [AEffectOp fx (OpUnify v u) c] g
    pure $ g' :|- pg
 ++
  "equal-transitive" `name`
  do
    AEffectOp fx  (OpUnify p  q) c  <- g
    AEffectOp fx' (OpUnify q' r) c' <- g
    guard (fx == fx' && c == c' && q == q')
    guard (p /= q && q /= r)
    g' <- gAdd [AEffectOp fx (OpUnify p r) c] g
    pure $ g' :|- pg

-- unequal-head-fails
-- container-resolves
-- container-comparable
-- equal-container
-- equal-call-name
-- equal-call-computes
-- equation-propagates
-- equation-fails
-- inherits-trans
-- inherits-fx

------------------------------------------------------------------

existsRules :: Rule Config
existsRules _ (A g :|- pg) =
  "exists-intro" `name`
  do
    (_ctx, c, _sc, op@(OpExists xs)) <- programOp pg
    g' <- gAdd (AEffectOp Succeeds op c : map (\ x -> AFlex (VertexVariable x) c) xs) g
    pure $ g' :|- pg


{-
------------------------------------------------------------------

constRules :: Rule Config
constRules _ (A g :|- pg) =
  "const-eq" `name`
  do
    (_ctx, c, OpUnify _ v@(VertexHead (HeadAtom _))) <- fc pg
    g' <- gAdd [AEffectOp Succeeds (OpUnify v v) c] g
    pure $ g' :|- pg
-}
{-
------------------------------------------------------------------

identRules :: Rule Config
identRules _ (A g :|- pg) =
  "define-ident" `name`
  do -- QQQ: should be? fc[c,S(u,v,s0)]
    (ctx, c, OpS u v (SyntaxExpr (ExprDef x e))) <- fc pg
    let pg' = ctx c (OpS u v (SyntaxExpr e))
    g' <- gAdd [AResolvedIdent x c v] g
    pure $ g' :|- pg'
 ++
-- define-ident-var
-- define-ident-ref
  "resolve-ident" `name`
  do
    (ctx, c, OpS u v (SyntaxExpr (ExprVar x))) <- fc pg
    AResolvedIdent x' c' r <- g
    guard (x == x' && c == c')
    let pg' = ctx c (OpSeq (OpUnify u r) (OpUnify v r))
    pure $ A g :|- pg'
 ++
  "S-exists" `name`
  do
    (ctx, c, OpS u v (SyntaxExpr (ExprExists x))) <- fc pg
    let pg' = ctx c (OpUnify u v)
    g' <- gAdd [AResolvedIdent x c v] g
    pure $ g' :|- pg'    
 ++
  "S-colon" `name`
--    S(u,v,    s2 : ...s0)       ---> exists h f; S(h,f,s0);               exists i y; y=f(u); S(i,v,s2:=...y) (if in a context without imply@c; if s2 not of the form s3->s4)
-- QQQ8: How does the ...y manage to desugar?  The y is source not a source variable
-- maybe y:=f(u)
  do
    (ctx, c, OpS u v (SyntaxExpr (ExprColon s2 s0))) <- fc pg
    guard (ASolveOrImply Imply c `notElem` g)
    let (h, f, i, y) = map4 Variable $ newIdents pg ("h", "f", "i", "y")
    let op =
          OpExists [h, f] +>
          OpS (VertexVariable h) (VertexVariable f) (SyntaxExpr s0) +>
          OpExists [i, y] +>
          OpCall (VertexVariable y) (VertexVariable f) u +>
          OpS (VertexVariable i) v (SyntaxExpr (ExprDef s2 (ExprVar y)))
    g' <- gAdd [AResolvedIdent y c (VertexVariable y)] g
    pure $ g' :|- ctx c op
{-
dsE pg u v (ExprColon s2 s0) =  -- XXX only when not imply@c
  OpExists [h, f] +>
  dsE pg (VertexVariable h) (VertexVariable f) s0 +>
  OpExists [i, y] +>
  OpCall (VertexVariable y) (VertexVariable f) u +>
  dsE pg (VertexVariable i) v (ExprDef s2 (ExprExVar y))
-}

------------------------------------------------------------------

programRules :: Rule Config
programRules _ (A g :|- pg) =
  "program-intro" `name`
  do
    Program c _i _x _op <- [pg]
    g' <- gAdd [AVerifyOrBeta Verify c, ASolveOrImply Solve c, AImpliedEffects effects tops None c] g
    pure $ g' :|- pg
 -- program-sequence

programElimRule :: Rule Config
programElimRule _ (A g :|- pg) =
  "program-elim" `name`
  do
    Program c _i _x op <- [pg]
    AEffectOp fx op' c' <- g
    guard (c == c' && op == op' && fx <=== tops)
    pure $ A [] :|- ProgramDone

_fxWeakenRule :: Rule Config
_fxWeakenRule _ (A g :|- pg) =
  "fx-weaken" `name`  -- QQQ2: Missing @c
  do
    AEffect fx0 a c <- g
    g' <- gAdd [AEffect fx1 a c | fx1 <- allEffects, fx0 <=== fx1 ] g
    pure $ g' :|- pg    

fxIntersectsRule :: Rule Config
fxIntersectsRule _ (A g :|- pg) =
  "fx-intersects" `name`
  do
    _a0@(AEffect fx0 a  c)  <- g
    _a1@(AEffect fx1 a' c') <- g
    guard (a == a' && c == c' && fx0 /= fx1)
    let gr = -- filter (\ aa -> aa /= _a0 && aa /= _a1)
             g
    g' <- gAdd [AEffect (fx0/\fx1) a c] gr
    pure $ g' :|- pg

assumptionRules :: Rule Config
assumptionRules _ (A g :|- pg) =
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
 ++
  -- eq-tuple QQQ2: missing @c
  "eq-tuple" `name`
  do
    AEffectOp Abstracts (OpUnify t@(VertexHead (HeadTuple p _x)) u@(VertexHead (HeadTuple q _y))) c <- g
    AEffectOp fx (OpUnify p' (VertexHead (HeadAtom (AtomRational n)))) c' <- g
    guard (p == p' && c == c' && fx /= Abstracts)
    AEffectOp fx' (OpUnify q' (VertexHead (HeadAtom (AtomRational n')))) c'' <- g
    guard (q == q' && c == c'' && n == n' && fx == fx')
    let ix a b = VertexCall a (VertexHead (HeadAtom (AtomRational b)))
    guard $ all (\ i -> AEffectOp fx (OpUnify (ix t i) (ix u i)) c `elem` g) [0 .. n-1]
    g' <- gAdd [AEffectOp fx (OpUnify t u) c] g
    pure $ g' :|- pg
  -- flow-tuple QQQ2: missing @c
 ++
  "flow-tuple" `name`
  do
    AEffectVertex Abstracts vp@(VertexCall v p) c <- g
    AUnify u v' <- g
    guard (v == v')
    g' <- gAdd [AUnify (VertexCall u p) vp, AEffectOp Abstracts (OpUnify (VertexCall u p) vp) c] g
    pure $ g' :|- pg
    
  -- tuple-resolves QQQ: what is ...
  -- dominator-equiv, below
  -- eq-dominator, below QQQ: needs side condition?  
  -- equation-propagate
 ++
  "equation-fails" `name`
  do
    AEffectOp fx (OpUnify u v) c <- g
    guard (fx <=== Fails)
    g' <- gAdd [AEffectVertex fx u c, AEffectVertex fx v c] g
    pure $ g' :|- pg
 ++
  "equation-resolves" `name`  -- QQQ: needs @c?
  do
    AEffectOp Abstracts (OpUnify _u a) c <- g
    g' <- gAdd [AEffectVertex Resolves a c] g
    pure $ g' :|- pg
 ++
  "flexes-symm" `name`
  do
    AFlexes c d <- g
    g' <- gAdd [AFlexes d c] g
    pure $ g' :|- pg
 ++
  "flexes-trans" `name`
  do
    AFlexes c0 c1 <- g
    AFlexes c1' c2 <- g
    guard (c1 == c1')
    g' <- gAdd [AFlexes c0 c2] g
    pure $ g' :|- pg
 ++
  "flexes-fx" `name`
  do
    AEffect fx arg c <- g
    AFlexes c' d <- g
    guard (c == c')
    g' <- gAdd [AEffect fx arg d] g
    pure $ g' :|- pg
  -- sees-ident
  -- QQQ2: rule with no name
  -- atom-rational
  -- atom-int
  -- atom-nat
  -- int-rational
  -- nat-int

unificationRules :: Rule Config
unificationRules _ cfg@(A g :|- pg) =
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
    g' <- gAdd [AEffectOp (fx1/\Abstracts) op c] g
    pure $ g' :|- pg
 ++
  -- QQQ: 'fail x'?
  "unify-fail-intro" `name`
  do
    (_ctx, c, op@(OpFail _u)) <- fc pg
    g' <- gAdd [AEffectOp Fails op c] g
    pure $ g' :|- pg
 ++
  "unify-head-fails" `name`
  do
    AEffectOp fx op@(OpUnify (VertexHead head0) (VertexHead head1)) c <- g
    guard (disjointHead head0 head1)
    traceM $ "disjointHead " ++ prettyShow (head0, head1)
    g' <- gAdd [AEffectOp (fx /\ Fails) op c] g
    pure $ g' :|- pg

 ++
  "unify-tuple-intro" `name`
  do
    (_ctx, c, OpUnify _x t@(VertexHead (HeadTuple _u _t))) <- fc pg
    g' <- gAdd [AFlex t c] g
    pure $ g' :|- pg
{-
 ++
  "unify-tuple-intro-elem" `name`
  do
    (_ctx, c, OpUnify x (VertexHead (HeadTuple u _t))) <- fc pg
    n <- [ n | VertexHead (HeadAtom (AtomRational n)) <- [u] ] ++
         do AEffectOp Abstracts (OpUnify u' (VertexHead (HeadAtom (AtomRational n)))) c' <- g
            guard (u == u' && c == c')
            return n
    g' <- gAdd [AFlex (VertexCall x (VertexHead (HeadAtom (AtomRational i)))) c | i <- [0 .. n-1]] g
    pure $ g' :|- pg
-}
 ++
  "unify-tuple-intro-elem" `name`
  do
    AFlex a@(VertexHead (HeadTuple (VertexHead (HeadAtom (AtomRational n))) _t)) c <- g
    g' <- gAdd [ AFlex (VertexCall a (VertexHead (HeadAtom (AtomRational i)))) c | i <- [0 .. n-1]] g
    pure $ g' :|- pg
 ++
  "unify-lambda-intro" `name`
  do
    (ctx, c, lam@(OpUnify _f l@(VertexHead (HeadLambda (Lambda _oc0 fx u d0 i w op0 d1 j z op1))))) <- fc pg
    let e = OpExplore fx d0 (OpExists [i, w] +> op0)
                         d1 (OpExists [j, z] +> OpUnify (VertexVariable j) call +> op1)
        call = VertexCall u (VertexVariable w)
        e' = freshen cfg e
    g' <- gAdd [AFlex l c] g
    pure $ g' :|- ctx c (lam +> e')
-- call-tuple-intro QQQ8: how can we conclude succeeds{u=v(p)} without knowing p<q?
-- call-tuple-iterates  QQQ2: this has a precondition "lacks", doesn't that lead to non-confluence?  Is p=n the right condition in lacks?
 ++
  "call-tuple-iterates" `name`  -- QQQ8: should v0 be v
  do
    (ctx, c, op@(OpCall _u v p)) <- fc pg
    AEffectOp Abstracts (OpUnify v' (VertexHead (HeadTuple q _v0))) c' <- g
    guard (c == c' && v == v')
    AEffectOp Abstracts (OpUnify q' (VertexHead (HeadAtom (AtomRational n)))) c'' <- g
    guard (q == q' && c == c'' && 0 < n && natural n)
    let pg' = ctx c (op +> opChoices [ OpUnify p (VertexHead (HeadAtom (AtomRational i))) | i <- [0 .. n-1] ])
    pure $ A g :|- pg'
 ++
  "call-tuple-fails" `name`
  do
    (ctx, c, op@(OpCall u v p)) <- fc pg
    AEffectOp fx (OpUnify v' (VertexHead (HeadTuple q _t))) c' <- g
    guard (c == c' && v == v')
    AEffectOp fx' (OpUnify q' (VertexHead (HeadAtom (AtomRational n)))) c'' <- g
    guard (fx == fx' && c == c'' && q == q')
    AEffectOp fx'' (OpUnify p' (VertexHead hd)) c''' <- g
    guard (fx == fx'' && c == c''' && p == p')
    let goodIndex (HeadAtom (AtomRational r)) = natural r
        goodIndex _ = False
    guard (n == 0 || not (goodIndex hd))
    let op' = OpUnify u (VertexCall v p)
    g' <- gAdd [AEffectOp (fx /\ Fails) op c] g
    pure $ g' :|- ctx c (op +> op')

natural :: Rational -> Bool
natural r = denominator r == 1 && r >= 0

-- call-tuple-fails

freshen :: (Data a) => a -> Operation -> Operation
freshen a op =
  let vs = bvs op
      vs' = newIdents a vs
      s = zip vs vs'
      f i | Just i' <- lookup i s = i'
          | otherwise = i
  in  transformBi f op

-- Are the heads definitely disjoint?
disjointHead :: Head -> Head -> Bool
disjointHead (HeadTuple u _) (HeadTuple v _) = u /= v
disjointHead head0 head1 = head0 /= head1

-- Elimination rules
eliminationRules :: Rule Config
eliminationRules _ cfg@(A g :|- pg) =
  "explore-intro" `name`  -- QQQ2: what is rejects?  it's not among the listed effects
                          -- QQQ2: it says 'succeeds:fx\/succeeds:succeeds', but the third
                          --       part is supposed to be none or (fx,fx)
                          -- QQQ2: is 'afx3/\fx' just doing /\ on both parts of the afx3?
  do
    (_ctx, c, OpExplore fx d0 _op0 d1 _op1) <- fc pg
    AVerifyOrBeta vb c' <- g
    AImpliedEffects _fx1 _fx2 afx3 c'' <- g
    guard (c == c' && c == c'')
    guard (fx <=== (Succeeds \/ imperatives))
    g' <- gAdd [ASees d0 c,
                AVerifyOrBeta vb d0,
                ASolveOrImply Imply d0,
                AImpliedEffects Succeeds (fx \/ Succeeds) (Some Succeeds) d0,
                ASees d1 d0,
                AVerifyOrBeta vb d1,
                ASolveOrImply Solve d1,
                AImpliedEffects effects fx (afx3 `ameet` fx) d1
               ] g
    pure $ g' :|- pg          
 ++
-- explore-fx   QQQ2: what is T
 -- call-closed-intro
  "call-closed-intro" `name`
  do
    -- This rule is dangerous, it creates a copy of the lambda expression
    -- with fresh variables.  This means that it can be repeated over and over
    -- since the equality test in gAdd does not use alpha equivalence.
    -- For now: hack it with a boolean false in OpCall
    (ctx, c, oop@(OpCall x u p))       <- fc pg
    ASolveOrImply si c'1               <- g;     guard (c == c'1)
    AImpliedEffects fx1 _fx2 _afx3 c'2 <- g;     guard (c == c'2)
    AFlex x' c'3                       <- g;     guard (x == x' && c == c'3)
    AFlex u' c'4                       <- g;     guard (u == u' && c == c'4)
    AFlex p' c'5                       <- g;     guard (p == p' && c == c'5)
    AEffectOp Abstracts (OpUnify u'' ll) c'6  <- g;     guard (u == u'' && c == c'6)
    VertexHead (HeadLambda lam)        <- [ll]
    Lambda Closed fx v d0 i w op0 d1 j z op1 <- [lam]
    AFlex ll' c1                       <- g;     guard (ll == ll')
    AImpliedEffects _fx4 _fx5 afx6 c1' <- g;     guard (c1 == c1')
    let (e0, e1) = map2 Context $ newIdents cfg ("e0", "e1")
--        vars@(i, w, j, z) = map4 Variable $ newIdents cfg ("i", "w", "j", "z")
        (vi, vw, vj, vz) = map4 VertexVariable (i, w, j, z)
        up = VertexCall u p
        lp = VertexCall (VertexHead $ HeadLambda lam) p
        ops = OpCall x u p +>
              OpUnify u up +>
              OpUnify up lp +>
              OpBeta (lam, p) e0 (OpExists [i,w] +> OpUnify vi p +> op0) e1 (OpExists [j, z] +> OpCall vj v vw +> op1 +> OpUnify lp vz)
        ops' = substCtx e0 d0 $ substCtx e1 d1 ops
        ops'' = freshen (pg, ops') ops'
        as = [ AEffectOp Succeeds oop c                   -- QQQ7 @c ?
             , ASolveOrImply si e0, AVerifyOrBeta Beta e0, AImpliedEffects fx1 effects None e0, AFlexes e0 c
             , ASolveOrImply si e1, AVerifyOrBeta Beta e1, AImpliedEffects fx1 fx      afx6 e1, AFlexes e1 c  -- QQQ7: afx6 instead of afx7?
             ]
    g' <- gAdd as g
    pure $ g' :|- ctx c ops''
    --error $ prettyShow $ g' :|- ctx c ops''
{-
 ++   
  "call-closed-fx" `name`
  do
    (_ctx, c, OpCall x u p) <- fc pg
    AImpliedEffects fx1 fx2 afx3 c'1 <- g;           guard (c == c'1)
    AFlex x' c'2                     <- g;           guard (c == c'2 && x == x')
    AFlex u' c'3                     <- g;           guard (c == c'3 && u == u')
    AFlex p' c'4                     <- g;           guard (c == c'4 && p == p')
    AEffectOp Abstracts (OpUnify u'' ll) c'5 <- g;   guard (c == c'5 && u == u'')
    VertexHead (HeadLambda lam)      <- [ll]
    AFlex (VertexHead (HeadLambda lam')) c1 <- g;    guard (lam == lam')
    AImpliedEffects fx4 fx5 afx6 c1' <- g;           guard (c1 == c1')
    AEffectOp fx0 (OpBeta (lam'', p'') e0 op0 e1 _op1) c'6 <- g;  guard (lam == lam'' && p == p'' && c == c'6)
    AEffectOp fx8 op0' e0'           <- g;           guard (op0 == op0' && e0 == e0')
    let afx7 = case afx6 of None -> None; Some fx6 -> Some $ weakenFx fx6 (fx8 `star` (fx0 /\ fx)) -- QQQ7: fx is not bound
        fx = fx8
    g' <- gAdd [AImpliedEffects fx1 fx afx7 e1] g
    pure $ g' :|- pg
-}
 ++
  "beta-fx" `name`
  do
    (_ctx, c, op@(OpBeta _lp _d0 op0 _d1 op1)) <- fc pg
    AEffectOp fx0 op0' c' <- g
    guard (op0 == op0' && c == c')
    AEffectOp fx1 op1' c'' <- g
    guard (op1 == op1' && c == c'')
    g' <- gAdd [AEffectOp (fx0 `star` fx1) op c] g
    pure $ g' :|- pg

-- beta-pending-1
-- beta-pending-2
 ++
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

-- Conditional QQQ5: cond is never used
condRules :: Rule Config
condRules _ cfg@(A g :|- pg) =
  "cond-intro" `name`
  do
    (_ctx, c, op@(OpCond _u _x _op0)) <- fc pg
    g' <- gAdd [AEffectOp effects op c] g
    pure $ g' :|- pg
 ++
  "cond-reduce-false" `name`
  do
    (ctx, c, (OpCond u _x _op0)) <- fc pg
    AEffectOp Abstracts (OpUnify u' (VertexHead (HeadAtom AtomUnit))) c' <- g   -- QQQ5: abstracts needs @c
    guard (u == u' && c == c')
    pure $ A g :|- ctx c (OpExists [])
 ++
  "cond-reduce-true" `name`
  do
    (ctx, c, (OpCond u y op0)) <- fc pg
    AEffectOp Abstracts (OpUnify u' (VertexHead (HeadTuple u0 _t))) c' <- g  -- QQQ5: two missing @c
    guard (u == u' && c == c')
    AEffectOp Abstracts (OpUnify u0' (VertexHead (HeadAtom (AtomRational 1)))) c'' <- g -- QQQ5: y0 should be u0
    guard (u0 == u0' && c == c'')
    -- QQQ5: there is a mention of d fresh, but no use of d
    let y1 = Variable $ newIdents cfg "y1"
    pure $ A g :|- ctx c (OpExists [y1] +> OpCall (VertexVariable y1) u (VertexHead (HeadAtom (AtomRational 0))) +> substVar y1 y op0)

-- Scope
scopeRules :: Rule Config
scopeRules _ (A g :|- pg) =
  "scope-intro" `name`
  do
    (_ctx, c, sc@(OpScope d op)) <- fc pg
    AEffectOp fx op' d' <- g
    guard (d == d' && op == op')
    g' <- gAdd [AEffectOp fx sc c, ASees d c, AFlexes d c] g
    pure $ g' :|- pg
 ++
  "scope-pending" `name`
  do
    (_ctx, c, sc@(OpScope _d _op)) <- fc pg
    AEffectOp fx op c' <- g
    guard (sc == op && c == c')
    g' <- gAdd [AEffectOp (fx \/ invert pendings) op c] g  ---  QQQ6: the added assumption lacks a @c or maybe @d
    pure $ g' :|- pg

-- iterate-intro
-- iterate-pending
-- iterate-force-eq
-- iterate-succeeds-elim
-- iterate-fails-elim
-- iterate-explore

-- Choice
choiceRules :: Rule Config
choiceRules _ cfg@(A g :|- pg) =
  "choice-intro" `name`
  do
    (_ctx, c, op@(OpChoice _op0 _op1)) <- fc pg
    g' <- gAdd [ AEffectOp effects op c ] g
    pure $ g' :|- pg
 ++
  "choice-solve-elim" `name`  -- QQQ6: is e0 fresh?
  do
    (fctx, c, x) <- fc pg
    (ictx, d, opc@(OpChoice opL opR)) <- ic x
    ASolveOrImply Solve d' <- g
    guard (d == d')
    ACtxIsOp d'' op0 <- g
    guard (d == d'')
    AEffectOp fx opc' d''' <- g
    guard (not $ FXiterates_pending `effIn` fx)
    guard (d == d''' && opc == opc')
    let cop = ACtxIsOp d (ictx d opR)
        gx = filter (/= ACtxIsOp d op0) g
        e0 = Context $ newIdents cfg "e0"
    g' <- gAdd [cop, ACtxIsOp e0 op0] gx
    pure $ g' :|- fctx c (ictx d opL)

--useDominatorRules :: Bool
--useDominatorRules = True

dominatorRules :: Rule Config
{-
dominatorRules _ (A g :|- pg) | useDominatorRules =
  "dominator-equiv" `name`  -- QQQ: fx{v>>u}@c is not among assumptions
  do
    ADominates fx v u c <- g
    g' <- gAdd [AEffectOp fx (OpUnify u v) c] g
    pure $ g' :|- pg
-}
{-
 ++
  "eq-dominator" `name`
  do
    AEffectOp fx (OpUnify u v) c <- g
    g' <- gAdd [ADominates fx v u c] g
    pure $ g' :|- pg
-}
dominatorRules _ _ = []
  
------------------------------------------------------------------

useRuntimeRules :: Bool
useRuntimeRules = False

-- QQQ7: runtime explore rule?
runtimeRules :: Rule Config
runtimeRules _ (g :|- pg) | useRuntimeRules =
  "run-lambda" `name`
  do
    (ctx, c, OpUnify ww (VertexHead (HeadLambda (Lambda _oc _fx u d0 i w op0 d1 j z op1)))) <- fc pg
    let lam = RunLambda u i z $ OpExists [w,j] +> substCtx c d0 op0 +> substCtx c d1 (substCtx c d0 op1)
    pure $ g :|- ctx c (OpUnify ww (VertexHead (HeadRunLambda lam)))
 ++
  "run-in" `name`
  do
    (ctx, c, OpIn _fx d i x op) <- fc pg
    pure $ g :|- ctx c (OpExists [i, x] +> substCtx c d op)
{- QQQ7: what is u?
 ++
  "run-stage" `name`
  do
    (ctx, c, OpStage _fx d0 i x op0 y op1 _) <- fc pg
    pure $ g :|- ctx c (OpExists [i, x, y] +>
                        OpUnify (VertexVariable i) (VertexVariable y) +>
                        OpUnify (VertexVariable u) (VertexVariable x) +>
                        substCtx c d0 op0 +>
                        substCtx c d1 op1)
-}
 ++
  "run-verify" `name`
  do
    (ctx, c, OpVerify _ _ d op) <- fc pg
    pure $ g :|- ctx c (substCtx c d op)
 ++
  "run-assume" `name`
  do
    (ctx, c, OpAssume _ d op) <- fc pg
    pure $ g :|- ctx c (substCtx c d op)
 ++
  "run-scope" `name`
  do
    (ctx, c, OpScope d op) <- fc pg
    pure $ g :|- ctx c (substCtx c d op)
  -- OpCast
runtimeRules _ _ = []

------------------------------------------------------------------

dominators :: Rule Config
dominators _ (A g :|- pg) =
  "dominators" `name`
  do
    fx <- [Succeeds, Decides]
    AEffectOp fx' (OpUnify _ _) c <- g
    guard (fx == fx')
    let ds = dom fx c g
--    g' <- gAdd [ ADominates fx u v c | Dom u v <- ds ] g
    g' <- gAdd [ AEffectOp fx (OpUnify u v) c | Dom u v <- ds ] g
    pure $ g' :|- pg

data Dom = Dom Vertex Vertex
  deriving (Eq, Show)
instance Pretty Dom where
  pPrint (Dom u v) = pPrint u <> text ">>" <> pPrint v

-- XXX no accounting for tuples in candidates
-- XXX no accounting for lambda-dominators
dom :: EffectSpecifier -> Context -> [Assumption] -> [Dom]
dom afx ac g =
  --   starting with dominator candidates
  --       fx{v>>u}@c if fx{v=u}@c or exists w. u<-w and not fx{u=w} and fx{v>>w}@c
  --       fx{vs[p]>>us[q]}@c if fx{vs>>us}@c and abstracts{vs[p]}@c and abstracts{us[q]}@c and fx{p=q}@c
  let initialCandidates =
        [ Dom v u | AEffectOp fx (OpUnify v u) c <- g, afx == fx, ac == c ]  -- 'if fx{v=u}@c'
      addCandidates ds = -- 'or'
        ds `union` [ Dom v u | AUnify u w <- g,                              -- 'exists w. u<-w'
                               AEffectOp afx (OpUnify u w) ac `notElem` g,   -- 'and not fx{u=w}'
                               Dom v w' <- ds, w == w'                       -- 'fx{v>>w}@c'
                   ]
           `union` [ Dom (VertexCall vs p) (VertexCall us q) |                  -- 'fx{vs[p]>>us[q]}@c'
                     Dom vs us <- ds,                                           -- 'if fx{vs>>us}@c'
                     AEffectVertex Abstracts (VertexCall vs' p) c  <- g, vs == vs', c  == ac,  -- 'abstracts{vs[p]}@c'
                     AEffectVertex Abstracts (VertexCall us' q) c' <- g, us == us', c' == ac,  -- 'abstracts{us[q]}@c'
                     AEffectOp fx (OpUnify p' q') c'' <- g, fx == afx, p == p', q == q', c'' == ac -- 'fx{p=q}@c'   -- Why must p=q?
                   ]
      candidates = loop initialCandidates
        where loop xs =
                let xs' = addCandidates xs
                in  if length xs == length xs' then xs else loop xs'

      -- XXX not sure what this means: and (v<=w or ...something re comparable)
      keep xs (Dom v u) =  -- 'keeping fx{v>>u}@c only if'
        AEffectOp afx (OpUnify u v) ac `elem` g ||                           -- 'fx{u=v}@c'
        and [ AEffectOp afx (OpUnify u w) ac `elem` g ||                     -- 'fx{u=w}@c or'
              Dom v w `elem` xs                                              -- 'fx{v>>w}@c'
            | AUnify u' w <- g, u == u' ]                                    -- 'for all w where u<-w'
      finalDoms = loop candidates
        where loop xs =
                let xs' = filter (keep xs) xs
                in  if length xs == length xs' then xs else loop xs'


      -- compute transitive reachability of all 'u<-v'
      reachable = reach [] [ (u, v) | AEffectOp fx (OpUnify v u) c <- g, afx == fx, ac == c ]
      reach r [] = r
      reach r ((u, v) : uvs) =
        let r' = [(u, v)] `union` [ (u, w) | (v', w) <- r, v == v' ] `union` [ (w, v) | (w, u') <- r, u == u'] `union` r
        in  reach r' uvs

  in --trace (show afx ++ " candidates=" ++ prettyShow candidates ++ "\nstartDs=" ++ prettyShow startDs ++ "\nfinal=" ++ prettyShow finalDs)
     --trace ("reachable=" ++ prettyShow reachable) $
     trace ("finalDoms=" ++ prettyShow finalDoms)
     finalDoms
     

------------------------------------------------------------------
-}

startConfig :: Syntax -> Config
startConfig s = A [] :|- startProgram s

startProgram :: Syntax -> Program
startProgram s = pg
  where (c, i, x) = newIdents s ("c", "i", "x")
        vi = Variable i
        vvi = VertexVariable vi
        vx = Variable x
        vvx = VertexVariable vx
        vc = Context c
        pg = OpExplore tops $ OpScope vc $ OpExists [vi, vx] +> OpSyntax vvi vvx s

------------------------------------------------------------------

pattern ERat :: Rational -> Expr
pattern ERat r = ExprAtom (AtomRational r)

-- example1:  5
example1 :: Syntax
example1 = SyntaxExpr $ ExprAtom $ AtomRational 5

{-
-- example2: a:=5; a
example2 :: Syntax
example2 = SyntaxList [SyntaxExpr $ ExprDef a (ERat 5)
                      ,SyntaxExpr $ ExprVar a]
  where a = Variable $ Ident "a"

-- example3: a:=5; a=3
example3 :: Syntax
example3 = SyntaxList [SyntaxExpr $ ExprDef a (ERat 5)
                      ,SyntaxExpr $ ExprUnify (ExprVar a) (ERat 3)]
  where a = Variable $ Ident "a"

-- example4:  1=2
example4 :: Syntax
example4 = SyntaxExpr $ ExprUnify (ERat 1) (ERat 2)

-- example5: f := 1=>2
--      i.e. f := function(1)<succeeds>{2}
example5 :: Syntax
example5 = SyntaxExpr $ ExprDef f $ ExprLambda (ERat 1) (ERat 2)
  where f = Variable $ Ident "f"
  
-- example6: f := 1=>2; f[1]
--      i.e. f := function(1)<succeeds>{2}; f[1]
example6 :: Syntax
example6 = SyntaxList
           [ SyntaxExpr $ ExprDef f $ ExprLambda (ERat 1) (ERat 2)
           , SyntaxExpr $ ExprAt (ExprVar f) (ERat 1)
           ]
  where f = Variable $ Ident "f"
  
-- example7: a=array{1,2} --; a[1]
example7 :: Syntax
example7 = SyntaxList
           [ SyntaxExpr $ ExprDef a $ ExprArray [ERat 1, ERat 2]
--           , SyntaxExpr $ ExprAt (ExprVar a) (ERat 1)
           ]
  where a = Variable $ Ident "a"

-- example8 a:any; a = 1
example8 :: Syntax
example8 = SyntaxList
           [ SyntaxExpr $ ExprExists a
           , SyntaxExpr $ ExprUnify (ExprVar a) (ERat 1)
           ]
  where a = Variable $ Ident "a"

-- example9: a:=array{1,2}; a=array{m:any,n:any}
example9 :: Syntax
example9 = SyntaxList
           [ SyntaxExpr $ ExprDef a $ ExprArray [ExprExists m, ExprExists n],
             SyntaxExpr $ ExprUnify (ExprVar a) $ ExprArray [ERat 1, ERat 2]
           ]
  where a = Variable $ Ident "a"
        m = Variable $ Ident "m"
        n = Variable $ Ident "n"

-- example10: a:=array{1}; a=array{1}
example10 :: Syntax
example10 = SyntaxList
           [ SyntaxExpr $ ExprDef a $ ExprArray [ERat 1],
             SyntaxExpr $ ExprUnify (ExprVar a) $ ExprArray [ERat 1]
           ]
  where a = Variable $ Ident "a"

-- example11 a:=1; a = 1
example11 :: Syntax
example11 = SyntaxList
           [ SyntaxExpr $ ExprDef a (ERat 1)
           , SyntaxExpr $ ExprUnify (ExprVar a) (ERat 1)
           ]
  where a = Variable $ Ident "a"
-}

-}

unwrapColons :: Expr -> (Expr -> Expr, Expr)
unwrapColons (ExprPreColon e) = (ExprPreColon . pre, s) where (pre, s) = unwrapColons e
unwrapColons e = (id, e)

desugar :: Data pgm => pgm -> OperationVariable -> Vertex -> Vertex -> Syntax -> Maybe Operation
desugar _ _ _ _ (SyntaxList []) = undefined
desugar pgm opv u v (SyntaxList ss) = -- SequenceSyntax
  let n1 = length ss - 1
      is = map (VertexVariable . Variable) $ newIdents pgm (n1, "i")
      xs = map (VertexVariable . Variable) $ newIdents pgm (n1, "x")
      opvs = map OperationVariable $ newIdents pgm (n1, "opv")
      ops = zipWith4 OpSyntax opvs is xs (init ss)
  in  Just $ opSeqs $ ops ++ [OpSyntax opv u v (last ss)]
desugar _ _ _ _ (SyntaxUnquote _) = undefined
desugar pgm aopv u v (SyntaxExpr expr) = OpResolved aopv <$> dsE expr
  where
    newIds :: (NewIdents i o) => i -> o
    newIds a = newIdents pgm a
    newOpv s = OperationVariable (newIds s)
    syntax oopv uu vv e = OpSyntax oopv uu vv (SyntaxExpr e)
    syntax1 = syntax (newOpv "opv1")
    ok _ = Just

    dsE (ExprVertex x) = ok "Vertex" $ opSeqs [OpUnify u x, OpUnify v x]
    dsE (ExprAtom a) = ok "Atom" $ opSeqs [OpUnify u n, OpUnify v n] where n = VertexHead (HeadAtom a)
    dsE (ExprUnify s0 s1) = ok "Unify" $ opSeqs [syntax1 u v s0, syntax (newOpv "opv2") u v s1]
    dsE (ExprChoice s0 s1) = ok "Choice" $ opSeqs [OpScope c $ syntax1 u v s0, OpScope d $ syntax (newOpv "opv2") u v s1]
      where (c, d) = newIds ("c", "d")
    -- RangeSyntax
    dsE (ExprArray _es) = undefined  -- Tim's desugaring makes no sense
    -- ArrayListMacro

    -- Application Syntax
    dsE (ExprCallClosed s0 s1) = ok "CallClosed" $
      let vars = map Variable $ newIds ["f","h","i","x","z"]
          [vf, vh, vi, vx, vz] = map VertexVariable vars
          opv2 = newOpv "opv2"
      in  opSeqs [ OpExists vars,
                   OpUnify u vz, OpUnify v vz,
                   syntax1 vh vf s0, syntax opv2 vi vx s1,
                   OpUnify vz (VertexCall vf vx) ]
    dsE (ExprCallOpen s0 s1) = ok "CallOpen" $
      let vars = map Variable $ newIds ["f","h","i","x"]
          [vf, vh, vi, vx] = map VertexVariable vars
          opv2 = newOpv "opv2"
          c = Context $ newIds "c"
          avars = map Variable $ newIds ["f1","x1", "z"]
          [vf1, vx1, vz] = map VertexVariable avars
      in  opSeqs [ OpExists vars,
                   OpUnify u vz, OpUnify v vz,
                   syntax1 vh vf s0, syntax opv2 vi vx s1,
                   OpVerify (Succeeds\/imperatives) (Err "P00") c $ opSeqs [
                     OpExists avars,
                     OpUnify vf1 vf, OpUnify vx1 vx, OpUnify vz (VertexCall vf1 vx1) ]
                 ]
    dsE (ExprDeoption s0) = ok "Deoption" $
      let vars = map Variable $ newIds ["f","h","x","z"]
          [vf, vh, vx, vz] = map VertexVariable vars
          opv0 = OperationVariable $ newIds "opv0"
      in  opSeqs [ OpExists vars,
                   OpUnify u vz, OpUnify v vz,
                   syntax opv0 vh vf s0,
                   OpUnify vz (VertexCall vf vx)
                 ]

    -- Identifier resolution syntax
    dsE ExprUnderscore = ok "Underscore" $
      syntax1 u v (ExprPreColon (ExprVar (EVariable (Ident "any"))))
    -- IdentSyntax has an actual rule
    -- DotIdentSyntax

    -- Identifier definition syntax
    dsE (ExprDef ExprUnderscore s) = ok "UnderscoreDefine" $
      syntax1 u v s
    dsE (ExprDef (ExprVar (EVariable i)) s2) = ok "IdentDefine" $
      opSeqs [ OpDefine i (DefVertex v),
               syntax1 u v s2
             ]
    -- IdentSpecDefine
    -- DotDefine

    -- Destructuring Definition Syntax
    dsE (ExprDef (ExprCallOpen s0 s1) s2) = ok "CallOpenDefine" $
      syntax1 u v $ ExprDef s0 (ExprFunction s1 Nothing [ExprFx (Succeeds\/Transacts)] s2)
    dsE (ExprDef (ExprSpec (ExprCallOpen s0 s1) s3) s2) = ok "CallOpenSpecDefine" $
      syntax1 u v $ ExprDef s0 (ExprFunction s1 Nothing s3 s2)
    -- CallClosedDefine
    -- MultipleDefine
    -- MultipleSpecDefine
    -- ArrayDefine, rule
    -- ArrayListDefine
    -- ArraySpecDefine
    -- TupleDefine
    -- TupleSpecDefine
    dsE (ExprDef (ExprDeoption s0) s1) = ok "DeoptionDefine" $
      syntax1 u v (ExprDef s0 (ExprOption s1))
    -- OptionalDefine
    -- PointerTypeDefine
    -- much pointer and variable desugaring
    -- mutation

    dsE (ExprPreColon s) = ok "InSyntax" $
      syntax1 u v (ExprColon (ExprArrow ExprUnderscore ExprUnderscore) s)
    -- InSpecSyntax
    dsE (ExprColon (ExprArrow s1 s2) as0) = ok "ArrowInDefine" $
      let (pre, s0) = unwrapColons as0
          c = newIds "c"
          cc = Context c
          vars1@[f,h,x,k] = map Variable $ newIds ["f", "h", "x", "k"]
          [vf,vh,vx,vk] = map VertexVariable vars1
          vars2@[g,j,y] = map Variable $ newIds ["g", "j", "y"]
          [vg,vj,vy] = map VertexVariable vars2
      in  opSeqs [ OpExists [f, h],
                   syntax (newOpv "opv2") u u (ExprDef s1 (ExprVertex u)),
                   syntax1 vh vf s0,
                   OpUnify v vx,
                   OpIn effects Nothing cc k x $ opSeqs [
                     OpExists [g, j, y],
                     OpUnify vk u,
                     OpUnify vg vf,
                     OpUnify vy (VertexCall vg vk),
                     syntax (newOpv "opv3") vj vx (ExprDef s2 (pre (ExprVertex vy)))
                     ]
                 ]
    dsE (ExprStage s0 s2 s1) = ok "StageSpec" $
      let xxx=0
      in  opSeqs [ OpVerify Succeeds (Err "U00") $ opSeqs [
                     OpExists [i, w],
                     syntax1 vi vw s2
                     ],
                   OpCast w (newOpv "opv2") (Err X30) $ opSeqs [
                     RHS (PatHead (HeadAtom (AtomEffect fx))) $ opSeqs [
                         OpExists [f, h],
                         syntax (newOpv "opv3") vh vf s0,
                         OpUnify v x,
                         OpStage fx fxv
                           c0 k x (opSeqs [OpExists [g]; OpUnify vk u, OpUnify vg vf, OpUnify x (VertexCall vg vk)])
                           c1 z   (opSeqs [OpExists j f2, syntax (newOpv "opv2") vj vz s1, OpUnify vf2 vf, OpUnify z (VertexCall vf2 vj)]
                       ]
                     ]
                 ]

    dsE _ = Nothing

main :: IO ()
main = do
  --fpptr "ut" $ startConfig example1
  undefined
