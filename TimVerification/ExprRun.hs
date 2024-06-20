--
-- from TimNotes/VerseRuntime-2024-Jun-09.txt
--
{-# OPTIONS_GHC -Wall -Wno-unused-imports -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Main where
import Prelude hiding ((<>), reads)
import qualified Prelude as P
import Control.Arrow(second, (***))
import Control.Monad
import Control.Monad.Writer
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

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
  case compare x y of
    LT -> x : merge xs (y:ys)
    EQ -> x : merge xs     ys
    GT -> y : merge (x:xs) ys

--------------------------------------------

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
  | ExprFunction Expr (Maybe OpenWorldSpecifier) Expr Expr
  | ExprSpec Expr [Expr]       -- s0<s1><s2>...
  | ExprDef Expr Expr          -- s0:=s1
  | ExprPostHat Expr           -- s0^
  | ExprFx Effect              -- fx
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
  pPrint (ExprFunction e1 oc fx e2) = text "function" <> parens (pPrint e1) <> hcat [maybe empty f oc, f fx] <> braces (pPrint e2)
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
  pPrint (ExprStage s0 s1 s2) = parens $ text ":" <> pPrint s0 <> text "<" <> pPrint s1 <> text ">" <> text "=" <> pPrint s2
  pPrint (ExprVertex u) = text "$" <> pPrint u

data EVariable = EVariable Ident
  deriving (Eq, Ord, Show, Data)

instance Pretty EVariable where pPrintPrec l p (EVariable i) = pPrintPrec l p i

data OpenWorldSpecifier = Open | Closed
  deriving (Eq, Ord, Show, Data, Enum, Bounded)

instance Pretty OpenWorldSpecifier where pPrint = ppLower

------------------------------------------------------------

type Q = Rational

newtype Pointer = Pointer Ident
  deriving (Eq, Ord, Show, Data)

instance Pretty Pointer where pPrint (Pointer i) = pPrint i

data Path = Path String
  deriving (Eq, Ord, Show, Data)

instance Pretty Path where
  pPrint (Path i) = text i

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

-------------------------------------------------------------

class Lattice a where    -- complete, finite lattice
  (/\) :: a -> a -> a    -- meet
  (\/) :: a -> a -> a    -- join
  inf  :: a              -- infimum
  sup  :: a              -- supremum

data EffCardinality
  =  P_Abstracts | P_Succeeds | P_Fails | P_Contradicts
  deriving (Eq, Ord, Show, Data)
data EffTransacts1
  =  {-P_Transacts     | -} P_Varies    | P_Allocates | P_Reads    | P_Writes {-x | P_No_Transacts -}
  deriving (Eq, Ord, Show, Data)
data EffImperatives1
  =  {-P_Imperatives   | -} P_Interacts | P_Throws    | P_Suspends {-x | P_No_Imperatives -}
  deriving (Eq, Ord, Show, Data)
data EffBlocks1
  =  {-P_Blocks        | -} P_Unblocked_Iterates | P_Unblocked_Reads | P_Unblocked_Writes | P_Unblocked_Imperatives {-x | P_Unblocks -}
  deriving (Eq, Ord, Show, Data)

--data PrimitiveEffect
--  =  P_Effects       | P_No_Effects

ppEff :: Show a => a -> Doc
ppEff = text . map toLower . drop 2 . show

instance Pretty EffCardinality  where pPrint = ppEff
instance Pretty EffTransacts1   where pPrint = ppEff
instance Pretty EffImperatives1 where pPrint = ppEff
instance Pretty EffBlocks1      where pPrint = ppEff

data ESet a = ESet [a]   -- effect set, always ordered
  deriving (Eq, Ord, Show, Data)

--eSet :: [a] -> ESet a
--eSet = ESet . sort
unSet :: ESet a -> [a]
unSet (ESet xs) = xs
eSetUnion :: Ord a => ESet a -> ESet a -> ESet a
eSetUnion (ESet xs) (ESet ys) = ESet (merge xs ys)
eSetIntersect :: Ord a => ESet a -> ESet a -> ESet a
eSetIntersect (ESet xs) (ESet ys) = ESet (intersect xs ys)

data Effect = Eff EffCardinality (ESet EffTransacts1) (ESet EffImperatives1) (ESet EffBlocks1)
  deriving (Eq, Ord, Show, Data)

instance Pretty Effect where
  pPrint e | Just s <- lookup e namedEffects = text ("<" ++ s ++ ">")
  pPrint (Eff c t i b) =
    text "<" <> fsep (punctuate (text " &") (pPrint c : ppt ++ ppi ++ ppb)) <> text ">"
    where ppt | t == P_No_Transacts = [text "no_transacts"]
              | t == P_Transacts = []
              | otherwise = map pPrint $ unSet t
          ppi | i == P_No_Imperatives = [text "no_imperatives"]
              | i == P_Imperatives = []
              | otherwise = map pPrint $ unSet i
          ppb | b == P_Unblocked = [text "unblocked"]
              | otherwise = map pPrint $ unSet b

pattern P_Transacts :: ESet EffTransacts1
pattern P_Transacts = ESet [P_Varies, P_Allocates, P_Reads, P_Writes]
pattern P_No_Transacts :: ESet EffTransacts1
pattern P_No_Transacts = ESet []
pattern P_Imperatives :: ESet EffImperatives1
pattern P_Imperatives = ESet [P_Interacts, P_Throws, P_Suspends]
pattern P_No_Imperatives :: ESet EffImperatives1
pattern P_No_Imperatives = ESet []
pattern P_Blocked :: ESet EffBlocks1
pattern P_Blocked = ESet []
pattern P_Unblocked :: ESet EffBlocks1
pattern P_Unblocked = ESet [P_Unblocked_Iterates, P_Unblocked_Reads, P_Unblocked_Writes, P_Unblocked_Imperatives]

cardUnion :: EffCardinality -> EffCardinality -> EffCardinality
cardUnion P_Abstracts _ = P_Abstracts
cardUnion _ P_Abstracts = P_Abstracts
cardUnion P_Succeeds P_Fails = P_Abstracts
cardUnion P_Fails P_Succeeds = P_Abstracts
cardUnion P_Succeeds _ = P_Succeeds
cardUnion _ P_Succeeds = P_Succeeds
cardUnion P_Fails _ = P_Fails
cardUnion _ P_Fails = P_Fails
cardUnion P_Contradicts P_Contradicts = P_Contradicts

cardIntersect :: EffCardinality -> EffCardinality -> EffCardinality
cardIntersect P_Contradicts _ = P_Contradicts
cardIntersect _ P_Contradicts = P_Contradicts
cardIntersect P_Succeeds P_Fails = P_Contradicts
cardIntersect P_Fails P_Succeeds = P_Contradicts
cardIntersect P_Succeeds _ = P_Succeeds
cardIntersect _ P_Succeeds = P_Succeeds
cardIntersect P_Fails _ = P_Fails
cardIntersect _ P_Fails = P_Fails
cardIntersect P_Abstracts P_Abstracts = P_Abstracts

effects :: Effect
effects = Eff P_Abstracts P_Transacts P_Imperatives P_Blocked
no_effects :: Effect
no_effects = Eff P_Contradicts P_No_Transacts P_No_Imperatives P_Unblocked
succeeds :: Effect
succeeds = Eff P_Succeeds P_Transacts P_Imperatives P_Blocked
fails :: Effect
fails = Eff P_Fails P_Transacts P_Imperatives P_Blocked
abstracts :: Effect
abstracts = Eff P_Abstracts P_Transacts P_Imperatives P_Blocked
imperatives :: Effect
imperatives = Eff P_Abstracts P_Transacts P_Imperatives P_Blocked
unblocked :: Effect
unblocked = Eff P_Abstracts P_Transacts P_Imperatives P_Unblocked
blocked :: Effect
blocked = Eff P_Abstracts P_Transacts P_Imperatives P_Blocked
only_succeeds :: Effect
only_succeeds = Eff P_Succeeds P_No_Transacts P_No_Imperatives P_Unblocked
no_transacts :: Effect
no_transacts = Eff P_Abstracts P_No_Transacts P_Imperatives P_Blocked
no_imperatives :: Effect
no_imperatives = Eff P_Abstracts P_Transacts P_No_Imperatives P_Blocked
throws :: Effect
throws = Eff P_Abstracts P_Transacts (ESet [P_Throws]) P_Blocked
suspends :: Effect
suspends = Eff P_Abstracts P_Transacts (ESet [P_Suspends]) P_Blocked
interacts :: Effect
interacts = Eff P_Abstracts P_Transacts (ESet [P_Interacts]) P_Blocked
reads :: Effect
reads = Eff P_Abstracts (ESet [P_Reads]) P_Imperatives P_Blocked
writes :: Effect
writes = Eff P_Abstracts (ESet [P_Writes]) P_Imperatives P_Blocked
unblocked_writes :: Effect
unblocked_writes = Eff P_Abstracts P_Transacts P_Imperatives (ESet [P_Unblocked_Writes])
unblocked_reads :: Effect
unblocked_reads = Eff P_Abstracts P_Transacts P_Imperatives (ESet [P_Unblocked_Reads])
unblocked_imperatives :: Effect
unblocked_imperatives = Eff P_Abstracts P_Transacts P_Imperatives (ESet [P_Unblocked_Imperatives])
succeeds_done :: Effect
succeeds_done = Eff P_Succeeds P_Transacts P_Imperatives P_Unblocked
fails_done :: Effect
fails_done = Eff P_Fails P_Transacts P_Imperatives P_Unblocked

namedEffects :: [(Effect, String)]
namedEffects = [
  (effects, "effects"),
  (no_effects, "no_effects"),
  (succeeds, "succeeds"),
  (fails, "fails"),
  (imperatives, "imperatives"),
  (unblocked, "unblocked"),
  (blocked, "blocked"),
  (only_succeeds, "only_succeeds"),
  (no_transacts, "no_transacts"),
  (no_imperatives, "no_imperatives"),
  (throws, "throws"),
  (suspends, "suspends"),
  (interacts, "interacts"),
  (reads, "reads"),
  (writes, "writes"),
  (unblocked_writes, "unblocked_writes"),
  (unblocked_reads, "unblocked_reads"),
  (unblocked_imperatives, "unblocked_imperatives"),
  (succeeds_done, "succeeds_done"),
  (fails_done, "fails_done")
  ]

-- The join of two effects in the lattice.
(.+) :: Effect -> Effect -> Effect
Eff c t i b .+ Eff c' t' i' b' = Eff (cardUnion c c') (eSetUnion t t') (eSetUnion i i') (eSetIntersect b b')

-- The meet of two effects in the lattice.
(.&) :: Effect -> Effect -> Effect
Eff c t i b .& Eff c' t' i' b' = Eff (cardIntersect c c') (eSetIntersect t t') (eSetIntersect i i') (eSetUnion b b')

-- Order on Effects, fx0 has fewer (or same) effects than fx1.
(<===) :: Effect -> Effect -> Bool
fx0 <=== fx1 = (fx0 .& fx1) == fx0

sequenceFx :: Effect -> Effect -> Effect
sequenceFx fx0 fx1 = (fx0 .+ fx1) .&
  if (fx0 .& fx1) <=== fails then fails
  else                            effects

downFx  :: Effect -> Effect
downFx fx = unblocked .+ fx

afterFx :: Effect -> Effect
afterFx fx = (unblocked .+ fx)
--  .& (if (iterates  <=== (contradicts    .+ fx)) then blocked else unblocked_iterates)
  .& (if (reads     <=== (no_transacts   .+ fx)) then blocked else unblocked_writes)
  .& (if (writes    <=== (no_transacts   .+ fx)) then blocked else unblocked_reads .& unblocked_writes)
  .& (if (throws    <=== (no_imperatives .+ fx)) then blocked else unblocked_imperatives)
  .& (if (suspends  <=== (no_imperatives .+ fx)) then blocked else unblocked_imperatives)
  .& (if (interacts <=== (no_imperatives .+ fx)) then blocked else unblocked_imperatives)

-- Instead of rule FxWeaken we do subeffect matching when checking effects.
isEffect :: Effect -> Effect -> Bool
isEffect effectFromAssumption effectFromRule = effectFromAssumption <=== effectFromRule

data Atom
  = AtomRational Q
  | AtomPointer Pointer
  | AtomPath Path
  | AtomUnit
  | AtomPrimitive String -- Print, operator'^', prefix'set'
  | AtomEffect Effect
  | AtomOpenWorld OpenWorldSpecifier
  deriving (Eq, Ord, Show, Data)

pattern AtomInteger :: Integer -> Atom
pattern AtomInteger i <- AtomRational (ratInteger -> Just i)
  where AtomInteger i = AtomRational (toRational i)

ratInteger :: Rational -> Maybe Integer
ratInteger q | denominator q == 1 = Just (numerator q)
             | otherwise = Nothing

instance Pretty Atom where
  pPrintPrec l p (AtomInteger i) = pPrintPrec l p i
  pPrintPrec l p (AtomRational q) = pPrintPrec l p (numerator q) <> text "/" <> pPrintPrec l p (denominator q)
  pPrintPrec l p (AtomPointer ptr) = pPrintPrec l p ptr
  pPrintPrec l p (AtomPath path) = pPrintPrec l p path
  pPrintPrec _ _ (AtomUnit) = text "()"
  pPrintPrec _ _ (AtomPrimitive s) = text s
  pPrintPrec l p (AtomEffect e) = pPrintPrec l p e
  pPrintPrec l p (AtomOpenWorld v) = pPrintPrec l p v

data Lambda = Lambda { lambda_arg              :: Vertex              -- u
                     , lambda_range_input      :: Variable            -- i
                     , lambda_range_output     :: Variable            -- x
                     , lambda_range_operation  :: Operation           -- op
                     }
  deriving (Eq, Ord, Show, Data)

instance Pretty Lambda where
  pPrint (Lambda u i x op) =
    text "lambda" <> parens (pPrint u) <+> pPrint i <+> pPrint x <+>  braces (pPrint op)


data Head
  = HeadAtom Atom
  | HeadLambda Lambda
  | HeadTuple [Vertex]
  deriving (Eq, Ord, Show, Data)

-- Does not have to be symmetric, since the ProgramUnifyOp handles symmetry.
-- This is used in the UnifyFails rule.
distinctHeads :: Head -> Head -> Bool
distinctHeads (HeadAtom a) (HeadAtom a') = a /= a'
distinctHeads (HeadAtom _) (HeadTuple _) = True
distinctHeads (HeadAtom _) (HeadLambda _) = True
distinctHeads (HeadTuple vs) (HeadTuple vs') = length vs /= length vs'
--distinctHeads (HeadTuple _) _ = True
distinctHeads _ _ = False

instance Pretty Head where
  pPrint (HeadAtom a) = pPrint a
  pPrint (HeadLambda l) = pPrint l
  pPrint (HeadTuple us) = text "tuple" <> braces (fsep (punctuate (text ",") (map pPrint us)))

type KnownValue = Head  -- Nested tuples are always KnownValue

data Vertex
  = VertexVariable Variable                           -- x
  | VertexHead Head                                   -- head
  deriving (Eq, Ord, Show, Data)

pattern VTuple :: [Vertex] -> Vertex
pattern VTuple vs = VertexHead (HeadTuple vs)
pattern VInteger :: Integer -> Vertex
pattern VInteger i = VertexHead (HeadAtom (AtomInteger i))
pattern VLambda :: Vertex -> Variable -> Variable -> Operation -> Vertex
pattern VLambda u i x op = VertexHead (HeadLambda (Lambda u i x op))

instance Pretty Vertex where
  pPrint (VertexVariable x) = pPrint x
  pPrint (VertexHead h) = pPrint h

data Operation
  = OpUnify Vertex Vertex                                         -- u=v
  | OpCall Vertex Vertex Vertex                                   -- u=v(p)
  | OpSeq Operation Operation                                     -- op0; op1
  | OpChoice Operation Operation                                  -- op0|op1
  | OpExists [Vertex]                                             -- exists u0 ...
  | OpIterate Vertex Variable Context Operation {-then-} Variable Operation {-else-} Operation
                                                                  -- iterate(u0) x0 c0 {op0} then x1 {op1} else {op2}
  | OpCast Vertex Vertex [RHS]                                    -- cast(u) {head_0 => {op_0}; ...}

  -- OpMetaVar is not a real Op, it's just for pretty printing a context
  | OpMetaVar String
  deriving (Eq, Ord, Show, Data)

pattern OpNoop :: Operation
pattern OpNoop = OpExists []

infixr 0 +>
(+>) :: Operation -> Operation -> Operation
(+>) = OpSeq

opSeqs :: [Operation] -> Operation
opSeqs = foldr1 OpSeq

data RHS = RHS HeadPattern Operation
  deriving (Eq, Ord, Show, Data)

instance Pretty Operation where -- XXX precedence
  pPrint (OpUnify u v) = xsep [pPrint u <+> text "=", pPrint v]
  pPrint (OpCall u v p) = pPrint u <+> text "=" <+> pPrint v <> parens (pPrint p)
  pPrint (OpSeq op0 op1) = xsep [pPrint op0 <> text ";", pPrint op1]
  pPrint (OpChoice op0 op1) = pPrint op0 <+> text "|" <+> pPrint op1
  pPrint OpNoop = text "nop"
  pPrint (OpExists xs) = hsep (text "exists" : map pPrint xs)
  pPrint (OpIterate u0 x0 c op0 x1 op1 op2) =
    text "iterate" <> parens (pPrint u0) <+> pPrint x0 <+> pPrint c <+> braces (pPrint op0) <+>
    text "then" <+> pPrint x1 <+> braces (pPrint op1) <+>
    text "else" <+> braces (pPrint op2)
  pPrint (OpCast u v rhss) = sep $ text "cast" <> parens (pPrint (u, v)) : map (nest 2 . pPrint) rhss

  pPrint (OpMetaVar s) = text s

instance Pretty RHS where
  pPrint (RHS pat op) = pPrint pat <+> text "=>" <+> pPrint op

data HeadPattern = PatHead Head  -- | ...
  deriving (Eq, Ord, Show, Data)

instance Pretty HeadPattern where
  pPrintPrec l p (PatHead h) = pPrintPrec l p h

opChoices :: [Operation] -> Operation
opChoices = foldr1 OpChoice

-- The Program has the output variable as the first argument
data Program = Program Vertex Context Operation | ProgramDone Vertex | ProgramFail
  deriving (Eq, Ord, Show, Data)

instance Pretty Program where
  pPrint (Program x c op) = sep [text "program" <> parens (pPrint x) <+> pPrint c, nest 2 (braces (pPrint op))]
  pPrint (ProgramDone v) = text "DONE" <> parens (pPrint v)
  pPrint ProgramFail = text "FAIL"

------------------------------------------------------

listCtx :: [a] -> [(a -> [a], a)]
listCtx [] = []
listCtx (a : as) = (\ a' -> a':as, a) : [ (\ b' -> a : ctx b', b) | (ctx, b) <- listCtx as ]

class ContextStartUp a where
  contextStartOp :: a -> [(Context -> Operation -> a, Context, Operation)]

instance ContextStartUp Program where
  contextStartOp (Program x c op) =
    [(Program x, c, op)]
  contextStartOp _ =
    []

instance ContextStartUp Operation where
  contextStartOp (OpIterate u0 x0 c op0 x1 op1 op2) =
    [(\ c' op0' -> OpIterate u0 x0 c' op0' x1 op1 op2, c, op0)]
  contextStartOp _ = []

contextSpan :: Operation -> [(Operation -> Operation, Operation)]
contextSpan aop =
  [(id, aop)] ++
  case aop of
    OpSeq aop0 aop1 ->
         do (ctx, op0) <- contextSpan aop0; pure (\ op0' -> ctx (OpSeq op0' aop1), op0)
      ++ do (ctx, op1) <- contextSpan aop1; pure (\ op1' -> ctx (OpSeq aop0 op1'), op1)
    _ -> []

programOp :: ContextStartUp a =>
             a -> [(Context -> Operation -> a, Context, Operation)]
programOp a =
  (do
     (ctx, c, aop) <- contextStartOp a
     (ctx', op) <- contextSpan aop
     pure (\ c' op' -> ctx c' (ctx' op'), c, op)
  ) ++
  (do
     (ctx, d, aop) <- contextStartOp a
     (ctx', c, op) <- programOp aop
     pure (\ c' op' -> ctx d (ctx' c' op'), c, op)
  )

-- XXX not symmetric between match and construct.
-- This is because we cannot guarentee consistensy between u,v,op in construction.
programUnifyOp :: ContextStartUp a =>
                  a -> [(Context -> Operation -> a, Context, Vertex, Vertex, Operation)]
programUnifyOp a = do
  (ctx, c, op@(OpUnify u v)) <- programOp a
  [ (\ c' op' -> ctx c' op', c, u, v, op)
   ,(\ c' op' -> ctx c' op', c, v, u, op) ]

programFlexible :: ContextStartUp a =>
                   a -> [(Context -> Vertex -> a, Context, Vertex)]
programFlexible a = do
  (ctx, c, OpExists us) <- programOp a
  (ctx', u) <- listCtx us
  (++)
    (pure (\ c' u' -> ctx c' (OpExists (ctx' u')), c, u))
    (do
        VTuple vs <- [u]
        (ctx'', v) <- listCtx vs
        pure (\ c' v' -> ctx c' (OpExists (ctx' (VTuple (ctx'' v')))), c, v)
    )

------------------------------------------------------------------

data Assumption
  = AEffect Effect Operation Context                                      -- fx{op}@c
  | AReadPointer Pointer KnownValue Context                               -- P^:=kv@c
  -- For debugging
  | AComment String
  deriving (Eq, Ord, Show, Data)

instance Pretty Assumption where
  pPrint (AEffect fx op c) = pPrint fx <> braces (pPrint op) <> text "@" <> pPrint c
  pPrint (AReadPointer p kv c) = pPrint p <> text "^:=" <> pPrint kv <> text "@" <> pPrint c
  pPrint (AComment s) = text "#" <+> text s

data AssumptionSet = A [Assumption]
  deriving (Eq, Ord, Show, Data)

instance Pretty AssumptionSet where
  pPrint (A []) = text "empty"
  pPrint (A as) = sep (punctuate (text ",") (map pPrint (sort as)))

{-
pPrint' x | prettyShow x == "<unblocked>{5 = 5}@c" = text (show x)
          | otherwise = pPrint x
-}

data Config = AssumptionSet :|- Program
  deriving (Eq, Ord, Show, Data)

instance Pretty Config where
  pPrint (g :|- pg) = xsep [pPrint g, text "|-", pPrint pg]

instance Pretty (Operation -> Program) where
  pPrint f = pPrint (f (OpMetaVar "OP"))

instance Pretty (Context -> Operation -> Program) where
  pPrint f = pPrint (f (Context (Ident "SC")) (OpMetaVar "OP"))

-- Add asms to g, but don't add existing (or weaker) assumptions.
-- Also, make sure to weed out any weaker assumption from g.
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
{-
gAdd asms ag =
  let g = filter (not . isComment) ag
      isComment (AComment _) = True
      isComment _ = False
      asms' = filter (`notElem` g) asms
  in  if null asms' then
        []
      else
        [A $ asms' ++ filter (`notElem` asms') g]
-}
---------------------------------------------

class NewIdents i o where
  newIdents :: (Data a) => a -> i -> o
instance NewIdents String Ident where
  newIdents x s = is !! 0  where is = identsNotIn x [Ident s]
instance NewIdents String OperationVariable where
  newIdents x s = OperationVariable $ newIdents x s
instance NewIdents String Context where
  newIdents x s = Context $ newIdents x s
instance NewIdents String Variable where
  newIdents x s = Variable $ newIdents x s
instance NewIdents String Vertex where
  newIdents x s = VertexVariable $ newIdents x s

instance NewIdents (String, String) (Ident, Ident) where
  newIdents x (s1, s2) = (is !! 0, is !! 1)  where is = identsNotIn x [Ident s1, Ident s2]
instance NewIdents (String, String) (OperationVariable, OperationVariable) where
  newIdents x ss = OperationVariable *** OperationVariable $ newIdents x ss
instance NewIdents (String, String) (Context, Context) where
  newIdents x ss = Context *** Context $ newIdents x ss
instance NewIdents (String, String) (Variable, Variable) where
  newIdents x ss = Variable *** Variable $ newIdents x ss
instance NewIdents (String, String) (Vertex, Vertex) where
  newIdents x ss = VertexVariable *** VertexVariable $ newIdents x ss

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

hrun :: Handle -> Operation -> IO ()
hrun h op = do
  hppx h op
  hPutStrLn h "---ProgramIntro--->"
  hpptr h $ startConfig op

frun :: FilePath -> Operation -> IO ()
frun fn op = do
  h <- openFile fn WriteMode
  hrun h op
  hClose h

run :: Operation -> IO ()
run = hrun stdout

---------------------------------------------

allRules :: Rule Config
allRules =
       programRules
  P.<> effectRules
  P.<> simpleOpsRules
  P.<> unificationRules
  P.<> callingRules

---------------------------------------------

-----
-- Program:
programRules :: Rule Config
programRules _ (A g :|- pg@(Program x c op)) =
  -- programIntro is startConfig
  "ProgramUnblock" `name`
  do
    g' <- gAdd [AEffect unblocked op c] g
    pure $ g' :|- pg
 ++
  "ProgramElim" `name`
  do
    AEffect fx op' c' <- g
    guard $ op == op' && c == c'
    guard $ isEffect fx succeeds_done -- only_succeeds
    pure $ A g :|- ProgramDone x
 ++
  -- Extra rule to get a nice failure
  "ProgramElimFail" `name`
  do
    AEffect fx op' c' <- g
    guard $ op == op' && c == c'
    guard $ isEffect fx fails_done
    pure $ A g :|- ProgramFail
programRules _ _ = []

-----
-- Effects:
effectRules :: Rule Config
effectRules _ (A g :|- pg) =
  "OpIntro" `name`
  do
    (_ctx, c, op) <- programOp pg
    g' <- gAdd [AEffect effects op c] g
    pure $ g' :|- pg
  -- FxWeaken is done with isEffect
 ++
  "FxStrengthen" `name`
  do
    let as = [ AEffect (fx0 .& fx1) op c
             | AEffect fx0 op c : rest <- tails g, AEffect fx1 op' c' <- rest
             , op == op', c == c' ]
    g' <- gAdd as g
    pure $ g' :|- pg

-----
-- Simple Ops:
simpleOpsRules :: Rule Config
simpleOpsRules _ (A g :|- pg) =
  "ExistsIntro" `name`
  do
    (_ctx, c, e@OpExists{}) <- programOp pg
    g' <- gAdd [AEffect only_succeeds e c] g
    pure $ g' :|- pg
 ++
  "SequenceIntro" `name`
  do
    AEffect fx2 op01@(OpSeq op0 op1) c <- g
    AEffect fx0 op0' c' <- g
    guard $ op0 == op0' && c == c'
    AEffect fx1 op1' c'' <- g
    guard $ op1 == op1' && c == c''
    let sFx = sequenceFx fx0 fx1
        dFx = downFx fx2
        aFx = afterFx fx0
    g' <- gAdd [AEffect sFx op01 c, AEffect dFx op0 c, AEffect aFx op1 c] g
    let g'' = g' -- addComment g' $ "SequenceFx"++prettyShow(fx0,fx1)++"="++prettyShow sFx++", DownFx("++prettyShow fx2++")="++prettyShow dFx++", AfterFx("++prettyShow fx0++")="++prettyShow aFx
    pure $ g'' :|- pg

addComment :: AssumptionSet -> String -> AssumptionSet
addComment (A as) s = A (AComment s : as)

-----
-- Unification:
unificationRules :: Rule Config
unificationRules _ (A g :|- pg) =
  -- UnifyIntro ???
  "UnifyAtomSucceeds" `name`
  do
    (_ctx, c, op@(OpUnify (VertexHead (HeadAtom a)) (VertexHead (HeadAtom a')))) <- programOp pg
    guard $ a == a'
    g' <- gAdd [AEffect succeeds op c] g
    pure $ g' :|- pg
 ++
  "UnifyFails" `name`
  do
    (_ctx, c, VertexHead h, VertexHead h', op) <- programUnifyOp pg
    guard $ distinctHeads h h'
    g' <- gAdd [AEffect fails op c] g
    pure $ g' :|- pg
 ++
  "UnifySubstitute" `name`
  do
    (ctx, c, VertexVariable x, u, _op) <- programUnifyOp pg
    (_ctx, c', VertexVariable x') <- programFlexible pg
    guard $ x == x' && c == c'
    let pg' = ctx c OpNoop
    pure $ remAsmDup $ subst u x (A g :|- pg')
 ++
  "UnifyTuples" `name`
  do
    (ctx, c, OpUnify (VTuple vs) (VTuple us)) <- programOp pg
    guard $ length vs == length us
    let ops = if null vs then OpNoop else opSeqs $ zipWith OpUnify vs us
    pure $ A g :|- ctx c ops

-----
-- Calling:
callingRules :: Rule Config
callingRules _ (A g :|- pg) =
{-
  "CallTupleSucceeds" `name`
  do
    (ctx, c, OpCall u (VTuple vs) (VInteger im)) <- programOp pg
    let n = length vs
        m = fromInteger im
    guard $ 0 <= m && m < n
    pure $ A g :|- ctx c (OpUnify u (vs !! m))
 ++
  "CallTupleFails" `name`
-}
  do
    (ctx, c, op@(OpCall u (VTuple vs) (VertexHead h))) <- programOp pg
    case h of
      HeadAtom (AtomInteger m) | 0 <= m && m < toInteger (length vs) ->
        pure ("CallTupleSucceeds",
              A g :|- ctx c (OpUnify u (vs !! fromInteger m)))
      _ -> do
        g' <- gAdd [AEffect fails op c] g
        pure ("CallTupleFails",
              g' :|- pg)
 ++
  "CallLambdaElim" `name`
  do
    (ctx, c, OpCall v (VLambda _u i x op) p) <- programOp pg
    let op' = freshen pg op
        op'' = subst p i $ subst v x op'
    pure $ A g :|- ctx c op''

---------------------------------------------

-- Substitution e[u/x]
-- Also removes x from existentials.
-- Assumes all variables are distinct.
subst :: (Data a) => Vertex -> Variable -> a -> a
subst u x = transformBi f . transformBi g
  where f (VertexVariable x') | x' == x   = u
        f z = z
        g (OpExists us) = OpExists (filter (/= VertexVariable x) us)
        g o = o

-- Remove duplicate assumptions, needed after subst.
remAsmDup :: Config -> Config
remAsmDup (A g :|- pg) = A (nub g) :|- pg

-- Make all bound variables fresh.
freshen :: Data a => a -> Operation -> Operation
freshen a aop = transformBi sub aop
  where
    sub :: Ident -> Ident
    sub x | Just x' <- lookup x isub = x'
        | otherwise = x
    isub :: [(Ident, Ident)]
    isub = zip ids (newIdents a ids)
    ids = execWriter (transformBiM opids aop >> transformBiM lamids aop)
    opids :: Operation -> Writer [Ident] Operation
    opids op@(OpExists vs) = do tell [x | VertexVariable (Variable x) <- vs]; return op
    opids op@(OpIterate _u0 (Variable x0) (Context c0) _op0 (Variable x1) _op1 _op2) = do tell [x0, c0, x1]; return op
    opids _op@(OpCast _ _ _) = undefined
    opids op = return op
    lamids :: Lambda -> Writer [Ident] Lambda
    lamids lam@(Lambda _u (Variable i) (Variable x) _op) = do tell [i, x]; return lam

---------------------------------------------

-- XXX Output variable is always 'x'
startConfig :: Operation -> Config
startConfig s = A [] :|- Program (newIdents () "x") (newIdents s "c") s

-- example1:  5=5
-- Hand-desugared
example1 :: Operation
example1 = OpUnify a5 a5
  where a5 = VInteger 5

-- example2:  5=3
-- Hand-desugared
example2 :: Operation
example2 = OpUnify a5 a3
  where a5 = VInteger 5
        a3 = VInteger 3

-- example3:  5
example3 :: Operation
example3 = opSeqs [ OpExists [vi, vx], OpUnify vi a5, OpUnify vx a5]
  where a5 = VInteger 5
        (vi, vx) = newIdents () ("i", "x")

-- example4: a:=5; a
example4 :: Operation
example4 = opSeqs [ OpExists [vi, vx], OpExists [vi', vx'], (OpUnify vi' a5 +> OpUnify vx' a5), (OpUnify vi vx' +> OpUnify vx vx')]
  where a5 = VInteger 5
        (vi, vx) = newIdents () ("i", "x")
        (vi', vx') = newIdents () ("i'", "x'")

-- example5:  5=3
example5 :: Operation
example5 = opSeqs [ OpExists [vi, vx],
                    (OpUnify vi a5 +> OpUnify vx a5) +> (OpUnify vi a3 +> OpUnify vx a3)
                  ]
  where a5 = VInteger 5
        a3 = VInteger 3
        (vi, vx) = newIdents () ("i", "x")

-- example6: a:=5; a=3
example6 :: Operation
example6 = opSeqs [ OpExists [vi, vx],
                    OpExists [vi', vx'],
                    (OpUnify vi' a5 +> OpUnify vx' a5),
                    (OpUnify vi vx' +> OpUnify vx vx') +> (OpUnify vi' a3 +> OpUnify vx' a3)
                  ]
  where a5 = VInteger 5
        a3 = VInteger 3
        (vi, vx) = newIdents () ("i", "x")
        (vi', vx') = newIdents () ("i'", "x'")

-- example7: <>=<>
-- Hand-desugared
example7 :: Operation
example7 = OpUnify (VTuple []) (VTuple [])

-- example8: ex p q . <3,p>=<q,5>
-- Hand-desugared
example8 :: Operation
example8 = OpExists [vp,vq] +> OpUnify (VTuple [a3,vp]) (VTuple [vq,a5])
  where a5 = VInteger 5
        a3 = VInteger 3
        (vp, vq) = newIdents () ("p", "q")

-- example9: ex x p q . <3,p>=<q,5>; x = <p,q>
-- Hand-desugared
example9 :: Operation
example9 = OpExists [vx,vp,vq] +> OpUnify (VTuple [a3,vp]) (VTuple [vq,a5]) +> OpUnify vx (VTuple [vp,vq])
  where a5 = VInteger 5
        a3 = VInteger 3
        (vp, vq) = newIdents () ("p", "q")
        vx = newIdents () "x"

-- example10: ex x . x=<3,5>(1)
-- Hand-desugared
example10 :: Operation
example10 = OpExists [vx] +> OpCall vx (VTuple [a3,a5]) a1
  where a5 = VInteger 5
        a3 = VInteger 3
        a1 = VInteger 1
        vx = newIdents () "x"

-- example11: ex x . x=<3,5>(3)
-- Hand-desugared
example11 :: Operation
example11 = OpExists [vx] +> OpCall vx (VTuple [a3,a5]) a3
  where a5 = VInteger 5
        a3 = VInteger 3
        vx = newIdents () "x"

-- example12: ex x . x=(lambda(_) i o (o = <i,i>))(3)
-- Hand-desugared
example12 :: Operation
example12 = OpExists [vx] +> OpCall vx lam a3
  where a3 = VInteger 3
        vx = newIdents () "x"
        (i, o) = newIdents () ("i", "o")
        vi = VertexVariable i; vo = VertexVariable o
        lam = VLambda (VInteger 0) i o (OpUnify vo (VTuple [vi, vi]))

main :: IO ()
main = do
  fpptr "ut" $ startConfig example5
