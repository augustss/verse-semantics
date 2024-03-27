--
-- from TimNotes/VerseSpecification-2023-Nov-28.txt
--
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Main(main,
  example1, example2, example3, example4, example5, example6, example7,
  pptr, fpptr, norm, startConfig) where
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
import Debug.Trace

type Q = Rational

data Expr
  = ExprAtom Atom
--  | ExprList [Expr]
  | ExprDef Variable Expr
  | ExprVar Variable
  | ExprUnify Expr Expr
  | ExprLambda Expr Expr
  | ExprFunction Expr VarianceSpecifier [EffectSpecifier] Expr
  | ExprAt Expr Expr
  | ExprColon Variable Expr
  | ExprArray [Expr]
  | ExprExists Variable
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

data VerifyOrBeta = Verify | Beta
  deriving (Eq, Ord, Show, Data, Enum, Bounded)

data Syntax
  = SyntaxList [Syntax]
  | SyntaxExpr Expr
  deriving (Eq, Ord, Show, Data)

data AvailableFx = None | Some EffectSpecifier
  deriving (Eq, Ord, Show, Data)

data VarianceSpecifier = Open | Closed
  deriving (Eq, Ord, Show, Data, Enum, Bounded)

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

data RunLambda = RunLambda Vertex Variable Variable Operation
  deriving (Eq, Ord, Show, Data)

data Head
  = HeadAtom Atom
  | HeadLambda Lambda
  | HeadRunLambda RunLambda
  | HeadTuple Vertex Variable                         -- tuple(u) x
  | HeadNom Path Vertex
  deriving (Eq, Ord, Show, Data)

data Vertex
  = VertexVariable Variable                           -- x
  | VertexHead Head                                   -- head
  | VertexCall Vertex Vertex                          -- u[v]
  deriving (Eq, Ord, Show, Data)

data Program = Program Context Variable Variable Operation | ProgramSeq Program Program | ProgramDone
  deriving (Eq, Ord, Show, Data)

data Operation
  = OpUnify Vertex Vertex                             -- u=v
  | OpCallX Bool Vertex Vertex Vertex                 -- u=v(p)
  | OpFail Vertex                                     -- u=fail
  | OpSeq Operation Operation                         -- op0; op1
  | OpChoice Operation Operation                      -- op0|op1
  | OpExists [Variable]                               -- exists x0 ...
  | OpCond Vertex Variable Operation                  -- cond(u) y. op
  | OpScope Context Operation                         -- scope c. op0
  | OpBeta (Lambda, Vertex) Context Operation Context Operation -- beta(L(p)) c0. op0 range c1. op1
  | OpIterate Vertex Context Vertex Operation Context Vertex Operation Context Operation
                                                      -- iterate(u0) c0 v0. op0 then c1 v1. op1 else c2. op2
  -- OpCast
  | OpExplore EffectSpecifier Context Operation Context Operation -- explore(fx) c. op0; range d. op1
  | OpIn EffectSpecifier Context Variable Variable Operation      -- in(fx) c i x . op0
  | OpStage EffectSpecifier Context Variable Variable Operation Context Variable Operation (Maybe Operation)
  | OpVerify EffectSpecifier (Maybe Err) Context Operation        -- verify(fx,?err) c. op0
  | OpAssume EffectSpecifier Context Operation                    -- assume(fx) c. op0
  | OpS Vertex Vertex Syntax                                      -- S(u,v,s)
  | OpErr Err                                                     -- err
  -- OpVar is not a real Op, it's just for pretty printing a context
  | OpVar String
  deriving (Eq, Ord, Show, Data)

opChoices :: [Operation] -> Operation
opChoices = foldr1 OpChoice

pattern OpCall :: Vertex -> Vertex -> Vertex -> Operation
pattern OpCall x y z = OpCallX False x y z

data Err = Err
  deriving (Eq, Ord, Show, Data)

-- ...

data Arg = ArgO Operation | ArgV Vertex
  deriving (Eq, Ord, Show, Data)

data Assumption
  = AEffect EffectSpecifier Arg Context                                   -- fx{arg}@c
  | AUnify Vertex Vertex                                                  -- v0<-v1
  | AFlex Vertex Context                                                  -- v@c
  | AImpliedEffects EffectSpecifier EffectSpecifier AvailableFx Context   -- fx1:fx2:afx@c
  | ASolveOrImply SolveOrImply Context                                    -- si@c
  | AVerifyOrBeta VerifyOrBeta Context                                    -- vb@c
  -- opv:=op
  | AResolvedIdent Variable Context Vertex                                -- Ident@c:=r
  -- P^:=u
  | ACtxIsOp Context Operation                                            -- c:=op
  | AFlexes Context Context                                               -- c flexes d
  | ASees Context Context                                                 -- c sees d
  -- added: QQQ2 add this?
--  | ADominates EffectSpecifier Vertex Vertex Context                      -- fx{v>>u}@c
  deriving (Eq, Ord, Show, Data)

pattern AEffectOp:: EffectSpecifier -> Operation -> Context -> Assumption
pattern AEffectOp fx op c = AEffect fx (ArgO op) c
pattern AEffectVertex:: EffectSpecifier -> Vertex -> Context -> Assumption
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

ppLower :: Show a => a -> Doc
ppLower = text . map toLower . show

instance Pretty Expr where
  pPrint (ExprAtom a) = pPrint a
--
  pPrint (ExprDef x e) = pPrint x <+> text ":=" <+> pPrint e
  pPrint (ExprVar x) = pPrint x
  pPrint (ExprUnify e1 e2) = pPrint e1 <+> text "=" <+> pPrint e2
  pPrint (ExprLambda e1 e2) = pPrint e1 <+> text "=>" <+> pPrint e2
  pPrint (ExprFunction e1 oc fx e2) = text "function" <> parens (pPrint e1) <> hcat (f oc : map f fx) <> braces (pPrint e2)
    where f e = text "<" <> pPrint e <> pPrint ">"
  pPrint (ExprAt e1 e2) = pPrint e1 <> brackets (pPrint e2)
  pPrint (ExprColon e1 e2) = parens $ pPrint e1 <> text ":" <> pPrint e2
  pPrint (ExprArray xs) = brackets $ sep $ punctuate (text ",") (map pPrint xs)
  pPrint (ExprExists x) = pPrint x <> text ":any"
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
instance Pretty VerifyOrBeta where pPrint = ppLower
instance Pretty Syntax where
  pPrint (SyntaxExpr e) = pPrint e
  pPrint (SyntaxList aes) = parens $ hcat (punctuate (text ";") (map pPrint aes))
instance Pretty AvailableFx where
  pPrint None = text "none"
  pPrint (Some fx) = pPrint fx
instance Pretty VarianceSpecifier where pPrint = ppLower
instance Pretty Lambda where
  pPrint (Lambda oc fx u c0 i w op0 c1 j z op1) =
    xsep [
      text "lambda" <> pPrint (oc,fx,u) <+> pPrint c0 <+> pPrint i <+> pPrint w <> text "." <+> pPrint op0 <> text ";",
      nest 2 (text "range" <+> pPrint c1 <+> pPrint j <+> pPrint z <> text "." <+> pPrint op1) ]
instance Pretty RunLambda where
  pPrint (RunLambda u i w op0) =
    text "lambda" <> parens (pPrint u) <+> pPrint i <+> pPrint w <> text "." <+> pPrint op0
instance Pretty Head where
  pPrint (HeadAtom a) = pPrint a
  pPrint (HeadLambda l) = pPrint l
  pPrint (HeadRunLambda l) = pPrint l
  pPrint (HeadTuple u t) = parens $ text "tuple" <> parens (pPrint u) <+> pPrint t
  pPrint (HeadNom p u) = text "nom" <+> pPrint p <> text "." <+> pPrint u
instance Pretty Vertex where
  pPrint (VertexVariable x) = pPrint x
  pPrint (VertexHead h) = pPrint h
  pPrint (VertexCall u v) = pPrint u <> brackets (pPrint v)
instance Pretty Program where
  pPrint (Program c i x op) = xsep [text "program" <+> pPrint c <+> pPrint i <+> pPrint x <> text ".", nest 2 $ pPrint op]
  pPrint (ProgramSeq pg1 pg2) = xsep [pPrint pg1 <> text ";", pPrint pg2]
  pPrint ProgramDone = text "DONE"
instance Pretty Operation where -- XXX precedence
  pPrint (OpUnify u v) = xsep [pPrint u <+> text "=", pPrint v]
  pPrint (OpCallX _ u v p) = pPrint u <+> text "=" <+> pPrint v <> parens (pPrint p)
  pPrint (OpFail u) = pPrint u <+> text "=fail"
  pPrint (OpSeq op0 op1) = parens $ xsep [pPrint op0 <> text ";", pPrint op1]
  pPrint (OpChoice op0 op1) = pPrint op0 <+> text "|" <+> pPrint op1
  pPrint (OpExists xs) = hsep (text "exists" : map pPrint xs)
  pPrint (OpCond u y op) = text "cond" <> parens (pPrint u) <+> pPrint y <> text "." <+> pPrint op
  pPrint (OpScope c op0) = text "scope" <+> pPrint c <> text "." <+> pPrint op0
  pPrint (OpBeta (l,p) c0 op0 c1 op1) =
    xsep [text "beta" <> parens (pPrint l) <> parens (pPrint p) <+> pPrint c0 <> text "." <+> pPrint op0,
         nest 2 (text "range" <+> pPrint c1 <> text "." <+> pPrint op1)]
  pPrint (OpIterate u0 c0 v0 op0 c1 v1 op1 c2 op2) =
    text "iterate" <> parens (pPrint u0) <+> pPrint c0 <+> pPrint v0 <> text "." <+> pPrint op0 <+>
    text "then" <+> pPrint c1 <+> pPrint v1 <> text "." <+> pPrint op1 <+>
    text "else" <+> pPrint c2 <+> text "." <+> pPrint op2
  -- OpCast
  pPrint (OpExplore fx c op0 d op1) =
    xsep [
      text "explore" <> parens (pPrint fx) <+> pPrint c <> text "." <+> pPrint op0 <> text ";",
      text "range" <+> pPrint d <> text "." <+> pPrint op1 ]
  pPrint (OpIn fx c i x op0) = text "in" <> parens (pPrint fx) <+> pPrint c <+> pPrint i <+> pPrint x <> text "." <+> pPrint op0
  pPrint (OpStage fx c0 i x op0 c1 y op1 mop2) =
    text "stage" <> parens (pPrint fx) <+> pPrint c0 <+> pPrint i <+> pPrint x <> text "." <+> pPrint op0 <+>
    text "value" <+> pPrint c1 <+> pPrint y <> text "." <+> pPrint op1 <>
    (case mop2 of Nothing -> empty; Just op2 -> text " upon." <+> pPrint op2)
  pPrint (OpVerify fx Nothing c op0) = text "verify" <> parens (pPrint fx) <+> pPrint c <> text "." <+> pPrint op0
  pPrint (OpVerify fx (Just err) c op0) = text "verify" <> parens (pPrint (fx, err)) <+> pPrint c <> text "." <+> pPrint op0
  pPrint (OpAssume fx c op0) = text "assume" <> parens (pPrint fx) <+> pPrint c <> text "." <+> pPrint op0
  pPrint (OpS u v s) = text "S" <> pPrint (u,v,s)
  pPrint (OpErr e) = pPrint e
  pPrint (OpVar s) = text s
instance Pretty Err where
  pPrint Err = text "ERR"
instance Pretty Assumption where
  pPrint (AEffectOp fx arg c) = pPrint fx <> braces (pPrint arg) <> text "@" <> pPrint c
  pPrint (AEffectVertex fx arg c) = pPrint fx <> braces (pPrint arg) <> text "@" <> pPrint c
  pPrint AEffect{} = undefined
  pPrint (AUnify v0 v1) = pPrint v0 <> text "<-" <> pPrint v1
  pPrint (AFlex v c) = pPrint v <> text "@" <> pPrint c
  pPrint (AImpliedEffects fx1 fx2 afx c) = pPrint fx1 <> text ":" <> pPrint fx2 <> text ":" <> pPrint afx <> text "@" <> pPrint c
  pPrint (ASolveOrImply si c) = pPrint si <> text "@" <> pPrint c
  pPrint (AVerifyOrBeta vb c) = pPrint vb <> text "@" <> pPrint c
  pPrint (AResolvedIdent x c v) = pPrint x <> text "@" <> pPrint c <> text ":=" <> pPrint v
  pPrint (ACtxIsOp c op) = pPrint c <> text ":=" <> pPrint op
  pPrint (AFlexes c0 c1) = pPrint c0 <+> text "flexes" <+> pPrint c1
  pPrint (ASees c0 c1) = pPrint c0 <+> text "sees" <+> pPrint c1
--  pPrint (ADominates fx u v c) = pPrint fx <> braces (pPrint u <> text ">>" <> pPrint v) <> text "@" <> pPrint c
  
instance Pretty AssumptionSet where
  pPrint (A []) = text "empty"
  pPrint (A as) = sep (punctuate (text ",") (map pPrint (sort as)))

instance Pretty Config where
  pPrint (g :|- pg) = xsep [pPrint g, text "|-", pPrint pg]

instance Pretty (Context -> Operation -> Program) where
  pPrint f = pPrint (f (Context (Ident "C")) (OpVar "OP"))

xsep :: [Doc] -> Doc
xsep = fsep

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

ameet :: AvailableFx -> EffectSpecifier -> AvailableFx
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

instance Bvs Program where
  bvs (Program (Context c) _ _ op) = c : bvs op
  bvs (ProgramSeq pg0 pg1) = bvs pg0 ++ bvs pg1
  bvs ProgramDone = []

instance Bvs Operation where
  bvs (OpUnify u v) = bvs u ++ bvs v
  bvs (OpCallX _ u v p) = bvs u ++ bvs v ++ bvs p
  bvs (OpFail u) = bvs u
  bvs (OpSeq op0 op1) = bvs op0 ++ bvs op1
  bvs (OpChoice op0 op1) = bvs op0 ++ bvs op1
  bvs (OpExists vs) = [ x | Variable x <- vs ]
  bvs (OpCond u (Variable y) op) = y : bvs u ++ bvs op
  bvs (OpScope (Context c) op) = c : bvs op
  -- QQQ7: op0 and op1?
  bvs (OpBeta _lp (Context c0) op0 (Context c1) op1) = c0 : c1 : bvs op0 ++ bvs op1
  bvs (OpIterate u (Context c0) _v0 op0 _c1 _v1 _op1 _c2 _op2) = c0 : bvs op0 ++ bvs u -- QQQ7: op1, op2
  -- lambda ???
  bvs (OpExplore _fx (Context c0) op0 (Context c1) op1) = c0 : c1 : bvs op0 ++ bvs op1
  bvs OpIn{} = []
  bvs OpStage{} = []
  bvs (OpVerify _ _ (Context c) op) = c : bvs op
  bvs (OpAssume _ (Context c) op) = c : bvs op
  bvs (OpS _ _ _) = []
  bvs (OpErr _) = []
  bvs (OpVar _) = []

instance Bvs Vertex where
  bvs (VertexVariable _) = []
  bvs (VertexHead h) = bvs h
  bvs (VertexCall u v) = bvs u ++ bvs v

instance Bvs Head where
  bvs (HeadAtom _) = []
  bvs (HeadLambda l) = bvs l
  bvs (HeadRunLambda l) = bvs l
  bvs (HeadTuple u (Variable x)) = x : bvs u
  bvs (HeadNom _ v) = bvs v

instance Bvs Lambda where
  bvs (Lambda _oc _fx u (Context c0) (Variable i) (Variable w) op0
                        (Context c1) (Variable j) (Variable z) op1) = [c0,i,w,c1,j,z] ++ bvs u ++ bvs op0 ++ bvs op1

instance Bvs RunLambda where
  bvs (RunLambda u (Variable i) (Variable z) op0) = i : z : bvs u ++ bvs op0

------------------------------------------------------------------

class NewIdents i o | i -> o where
  newIdents :: (Data a) => a -> i -> o
instance NewIdents String Ident where
  newIdents x s = is !! 0  where is = identsNotIn x [Ident s]
instance NewIdents (String, String) (Ident, Ident) where
  newIdents x (s1, s2) = (is !! 0, is !! 1)  where is = identsNotIn x [Ident s1, Ident s2]
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
  OpCall vz vg vx
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
  in
  OpUnify u tupi +>
  OpUnify v tupx +>
  opSeqs [ dsE pg (ix u k) (ix v k) s | (s, k) <- zip ss [0::Int ..] ]
dsE _ u v x = OpS u v (SyntaxExpr x)

opSeqs :: [Operation] -> Operation
opSeqs = foldr1 OpSeq

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

------------------------------------------------------------------

constRules :: Rule Config
constRules _ (A g :|- pg) =
  "const-eq" `name`
  do
    (_ctx, c, OpUnify _ v@(VertexHead (HeadAtom _))) <- fc pg
    g' <- gAdd [AEffectOp Succeeds (OpUnify v v) c] g
    pure $ g' :|- pg

------------------------------------------------------------------

desugarRules :: Rule Config
desugarRules _ (g :|- pg) =
  "S" `name`
  do
    (ctx, c, op@(OpS u v s)) <- fc pg
    let op' = dsS pg u v s
    guard $ op /= op'
    pure $ g :|- ctx c op'

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
    g' <- gAdd [AEffectOp (fx /\ Fails) op c] g
    pure $ g' :|- pg

 ++
  "unify-tuple-intro" `name`
  do
    (_ctx, c, OpUnify _x t@(VertexHead (HeadTuple _u _t))) <- fc pg
    g' <- gAdd [AFlex t c] g
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

disjointHead :: Head -> Head -> Bool
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
        ops = OpCallX True x u p +>
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

useDominatorRules :: Bool
useDominatorRules = True

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

-- QQQ*: can OP match op0;op1 ?
class FlexibleOp a where
  fop :: a -> [(Operation -> a, Operation)]
instance FlexibleOp Operation where
  fop a = [ (id, a) | not (isOpSeq a) ] ++
          fop1 a
    where fop1 (OpSeq op1 op2) =
              do
                (ctx, op11) <- fop op1
                pure (\ op11' -> OpSeq (ctx op11') op2, op11)
             ++
              do
                (ctx, op21) <- fop op2
                pure (\ op21' -> OpSeq op1 (ctx op21'), op21)
          fop1 (OpScope c aop) = do
            (ctx, op) <- fop aop
            pure (\ op' -> OpScope c (ctx op'), op)
-- QQQ: what is p?  beta(L(p)) c0. fop[OP] range c1. op
          fop1 (OpBeta lam c0 op0 c1 op1) = do
            (ctx, op) <- fop op0
            [(\ op' -> OpBeta lam c0 (ctx op') c1 op1, op),
             (\ op' -> OpBeta lam c0 op0 c1 (ctx op'), op)]
          fop1 _op =
            --trace ("fop1: " ++ prettyShow _op)
            []
isOpSeq :: Operation -> Bool
isOpSeq OpSeq{} = True
isOpSeq _ = False

class ExploreOp a where
  _eop :: a -> [(Operation -> a, Operation)]
instance ExploreOp Operation where
  _eop a = fop a ++
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

class IterateContext a where
  ic :: a -> [(Context -> Operation -> a, Context, Operation)]
instance IterateContext Operation where
  -- ic[D,OP] := iterate(u0) D v0. fop[OP] then d1 v1. op1 else d2. op2
  ic (OpIterate u0 d v0 x d1 v1 op1 d2 op2) = do
    (fopctx, op) <- fop x
    pure (\ d' op' -> OpIterate u0 d' v0 (fopctx op') d1 v1 op1 d2 op2, d, op)
  ic _ = []

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
  let candidates = [ Dom v u | AEffectOp fx (OpUnify v u) c <- g, afx == fx, ac == c ]
{-
      add ds = ds `union` [ Dom v u | AEffectOp fx (OpUnify v w) c <- g, afx == fx, ac == c,
                            AUnify u w' <- g, w == w' ]
-}
      add ds = ds `union` [ Dom v u | AUnify u w <- g,
                                      AEffectOp afx (OpUnify u w) ac `notElem` g,
                                      Dom v w' <- ds, w == w' ]
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
  in trace (show afx ++ " candidates=" ++ prettyShow candidates ++ "\nstartDs=" ++ prettyShow startDs ++ "\nfinal=" ++ prettyShow finalDs)
     finalDs

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
        pg = Program vc vi vx $ OpS vvi vvx s

------------------------------------------------------------------

pattern ERat :: Rational -> Expr
pattern ERat r = ExprAtom (AtomRational r)

-- example1:  5
example1 :: Syntax
example1 = SyntaxExpr $ ExprAtom $ AtomRational 5

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
        any = Variable $ Ident "any"

-- example9: a=array{1,2}; a=array{m:any,n:any}
example9 :: Syntax
example9 = SyntaxList
           [ --SyntaxExpr $ ExprDef a $ ExprArray [ERat 1, ERat 2],
            SyntaxExpr $ ExprDef a $ ExprArray [ExprColon m (ExprVar any), ExprColon n (ExprVar any)]
           ]
  where a = Variable $ Ident "a"
        m = Variable $ Ident "m"
        n = Variable $ Ident "n"
        any = Variable $ Ident "any"

main :: IO ()
main = do
  fpptr "ut" $ startConfig example8
