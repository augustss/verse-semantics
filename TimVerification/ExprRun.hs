--
-- from TimNotes/VerseRuntime-2024-Jun-09.txt
--
{-# OPTIONS_GHC -Wall -Wno-unused-imports -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Main where
import Prelude hiding ((<>), reads)
import qualified Prelude as P
import Control.Arrow(second)
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
  pPrint (Eff c t i b) =
    text "<" <> fsep (punctuate (text ",") (pPrint c : ppt ++ ppi ++ ppb)) <> text ">"
    where ppt | t == P_Transacts = [text "transacts"]
              | otherwise = map pPrint $ unSet t
          ppi | i == P_Imperatives = [text "imperatives"]
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
afterFx fx = (unblocked .& fx)
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

instance Pretty Atom where
  pPrintPrec l p (AtomRational q) | denominator q == 1 = pPrintPrec l p (numerator q)
                                  | otherwise = pPrintPrec l p (numerator q) <> text "/" <> pPrintPrec l p (denominator q)
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

instance Pretty Head where
  pPrint (HeadAtom a) = pPrint a
  pPrint (HeadLambda l) = pPrint l
  pPrint (HeadTuple us) = parens $ text "tuple" <> parens (pPrint us)

type KnownValue = Head  -- Nested tuples are always KnownValue

data Vertex
  = VertexVariable Variable                           -- x
  | VertexHead Head                                   -- head
  deriving (Eq, Ord, Show, Data)

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
                                                                  -- iterate(x0) c0 {op0} then c1 x1 {op1} else {op2}
  | OpCast Vertex Vertex [RHS]                                    -- cast(u) {head_0 => {op_0}; ...}

  -- OpMetaVar is not a real Op, it's just for pretty printing a context
  | OpMetaVar String
  deriving (Eq, Ord, Show, Data)

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

data Program = Program Context Operation | ProgramDone
  deriving (Eq, Ord, Show, Data)

instance Pretty Program where
  pPrint (Program c op) = sep [text "program" <+> pPrint c, nest 2 (pPrint op)]
  pPrint ProgramDone = text "DONE"

------------------------------------------------------

listCtx :: [a] -> [(a -> [a], a)]
listCtx [] = []
listCtx (a : as) = (\ a' -> a':as, a) : [ (\ b' -> a : ctx b', b) | (ctx, b) <- listCtx as ]

class ContextStartUp a where
  contextStartOp :: a -> [(Context -> Operation -> a, Context, Operation)]

instance ContextStartUp Program where
  contextStartOp (Program c op) =
    [(Program, c, op)]
  contextStartOp ProgramDone =
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

programUnifyOp :: ContextStartUp a =>
                  a -> [(Context -> Vertex -> Vertex -> Operation -> a, Context, Vertex, Vertex, Operation)]
programUnifyOp a = do
  (ctx, c, op@(OpUnify u v)) <- programOp a
  [ (\ c' _u' _v' op' -> ctx c' op', c, u, v, op)
   ,(\ c' _u' _v' op' -> ctx c' op', c, v, u, op) ]

programFlexible :: ContextStartUp a =>
                   a -> [(Context -> Vertex -> a, Context, Vertex)]
programFlexible a = do
  (ctx, c, OpExists us) <- programOp a
  (ctx', u) <- listCtx us
  (++)
    (pure (\ c' u' -> ctx c' (OpExists (ctx' u')), c, u))
    (do
        VertexHead (HeadTuple vs) <- [u]
        (ctx'', v) <- listCtx vs
        pure (\ c' v' -> ctx c' (OpExists (ctx' (VertexHead (HeadTuple (ctx'' v'))))), c, v)
    )

------------------------------------------------------------------

data Assumption
  = AEffect Effect Operation Context                                      -- fx{op}@c
  | AReadPointer Pointer KnownValue Context                               -- P^:=kv@c
  deriving (Eq, Ord, Show, Data)

instance Pretty Assumption where
  pPrint (AEffect fx op c) = pPrint fx <> braces (pPrint op) <> text "@" <> pPrint c
  pPrint (AReadPointer p kv c) = pPrint p <> text "^:=" <> pPrint kv <> text "@" <> pPrint c

data AssumptionSet = A [Assumption]
  deriving (Eq, Ord, Show, Data)

instance Pretty AssumptionSet where
  pPrint (A []) = text "empty"
  pPrint (A as) = sep (punctuate (text ",") (map pPrint (sort as)))

data Config = AssumptionSet :|- Program
  deriving (Eq, Ord, Show, Data)

instance Pretty Config where
  pPrint (g :|- pg) = xsep [pPrint g, text "|-", pPrint pg]

instance Pretty (Operation -> Program) where
  pPrint f = pPrint (f (OpMetaVar "OP"))

instance Pretty (Context -> Operation -> Program) where
  pPrint f = pPrint (f (Context (Ident "SC")) (OpMetaVar "OP"))

instance Pretty (Context -> Context -> Operation -> Program) where
  pPrint f = pPrint (f (Context (Ident "FC")) (Context (Ident "SC")) (OpMetaVar "OP"))

-- Add asms to g, but don't add existing (or weaker) assumptions.
-- Also, make sure to weed out any weker assumption from g.
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


---------------------------------------------

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
instance NewIdents (String, String) (Variable, Variable) where
  newIdents x (s1, s2) = (Variable (is !! 0), Variable (is !! 1))  where is = identsNotIn x [Ident s1, Ident s2]

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

---------------------------------------------

allRules :: Rule Config
allRules =
       programRules
  P.<> effectRules
  P.<> simpleOpsRules

---------------------------------------------

-----
-- Program:
programRules :: Rule Config
programRules _ (A g :|- pg@(Program c op)) =
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
    guard $ isEffect fx only_succeeds
    pure $ A g :|- ProgramDone
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
    g' <- gAdd [AEffect (sequenceFx fx0 fx1) op01 c, AEffect (downFx fx2) op0 c, AEffect (afterFx fx0) op1 c] g
    pure $ g' :|- pg

---------------------------------------------

startConfig :: Operation -> Config
startConfig s = A [] :|- Program (Context (newIdents s "c")) s

-- example1:  5
example1 :: Operation
example1 = opSeqs [ OpExists [vi, vx], OpUnify vi h, OpUnify vx h]
  where h = VertexHead $ HeadAtom $ AtomRational 5
        (i, x) = newIdents () ("i", "x")
        vi = VertexVariable i; vx = VertexVariable x

main :: IO ()
main = do
  pptr $ startConfig example1
