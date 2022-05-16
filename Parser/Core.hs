{-# LANGUAGE PatternSynonyms #-}
module Core(
  Core(..),
  pattern COne, pattern CAll, pattern CSucceeds, pattern CFail,
  Value(..),
  HNF(..),
  compos, composOp,
  exprToCore,
  coreToRedex) where
import Prelude hiding ((<>))
import Control.Monad.Identity
import Control.Monad.State.Strict

import Print
import Expr hiding (compos, composOp)
import Desugar(predefs, getVisible)
import Error
import SExp
--import Debug.Trace

data Core
  = CValue Value
  | CUnify Core Core
  | CSeq [Core]
  | CApply Core Core
  | CBar [Core]
  | CMacro Ident Core
  | CDef Heap Core
  deriving (Show)

type Heap = [Ident]

data Value = Var Ident | HNF HNF
  deriving (Show)

data HNF
  = HInt Integer
  | HRat Rational
  | HPrim String
  | HArray [Value]
  | HLam Ident Core
  | HRec Ident Core
  | HType Value      -- really a lambda
  deriving (Show)

{-
pattern CInt :: Integer -> Core
pattern CInt x = CValue (HNF (HInt x))
pattern CRat :: Rational -> Core
pattern CRat x = CValue (HNF (HRat x))
pattern CVar :: Ident -> Core
pattern CVar x = CValue (Var x)
-}
pattern COne :: Core -> Core
pattern COne c <- CMacro (Ident _ "one") c
  where COne e = CMacro (Ident noLoc "one") e
pattern CAll :: Core -> Core
pattern CAll c <- CMacro (Ident _ "one") c
  where CAll e = CMacro (Ident noLoc "one") e
pattern CSucceeds :: Core -> Core
pattern CSucceeds c <- CMacro (Ident _ "one") c
  where CSucceeds e = CMacro (Ident noLoc "one") e
pattern CFail :: Core
pattern CFail = CBar []

cEmpty :: Core
cEmpty = CValue $ HNF $ HArray []

type C = State Int

seqC :: [Core] -> Core
seqC acs =
  case concatMap flat acs of
    [] -> impossible acs
    [c] -> c
    cs -> CSeq cs
  where
    flat (CSeq cs) = concatMap flat cs
    flat c = [c]

newTmp :: C Ident
newTmp = do
  n <- get
  let i = Ident noLoc ("$c" ++ show n)
  put $! n+1
  pure i

exprToCore :: Expr -> Core
exprToCore = flip evalState 1 . coreD

core :: Expr -> C Core
core e@LitInt{} = val e
core e@LitRat{} = val e
core e@Variable{} = val e
core e@Array{} = val e
core (Seq es) = seqC <$> mapM core es
core (ApplyS e1 e2) = CSucceeds <$> core (ApplyD e1 e2)
core (ApplyD e1 e2) = CApply <$> core e1 <*> core e2
core (Unify e AnyT) = core e  -- XXX add a core simplification pass
core (Unify e1 e2) = CUnify <$> core e1 <*> core e2
core e@Typedef{} = val e
core e@Choice{} = CBar <$> mapM coreD (flat e)
  where flat (Choice e1 e2) = flat e1 ++ flat e2
        flat ee = [ee]
core (Define i e) = core $ Unify (Variable i) e
core (Range e) = core $ ApplyD (eVar "range") e
core e@Any = val e
core Fail = pure $ CBar []
core (For2 e1 e2) = do
  e2' <- thunk e2
--  traceM $ show (e2, e2', seqE [e1, e2'])
  CAll <$> core (seqE [e1, e2'])
core (If3 e1 e2 e3) = do
  e2' <- thunk e2
  e3' <- thunk e3
  l <- core (seqE [e1, e2'])
  r <- core e3'
  let fn = COne $ CBar [l, r]
  pure $ CApply fn cEmpty
core e@Function{} = val e
core e = impossible e

coreD :: Expr -> C Core
coreD e | null is = core e
        | otherwise = CDef is <$> core e
  where is = getVisible e

val :: Expr -> C Core
val e = CValue <$> value e

value :: Expr -> C Value
value (LitInt i) = pure (HNF $ HInt i)
value (LitRat i) = pure (HNF $ HRat i)
value (Variable i@(Ident _ s)) | i `elem` predefs = pure (HNF $ HPrim s)
                               | otherwise = pure (Var i)
value (Array es) = HNF . HArray <$> mapM value es
value (Typedef e) = do
  i <- newTmp
  HNF . HType . HNF . HLam i <$> coreD (Unify (Variable i) e)
value (Function (Define x AnyT) fs b) = HNF . HLam x . attr <$> coreD b
  where attr ae = foldr CMacro ae fs
value Any = pure (HNF $ HPrim "any")
value e = internalErrorMsg $ "value: not a value\n" ++ show e
  
eVar :: String -> Expr
eVar = Variable . Ident noLoc

thunk :: Expr -> C Expr
thunk e = do
--  i <- newTmp
  i <- pure $ Ident noLoc "_"
  pure $ Function (Define i AnyT) [] e

------

instance Pretty Core where
  pPrintPrec l p (CValue v) = pPrintPrec l p v
  pPrintPrec l p (CUnify c1 c2) = maybeParens (p > 6) $ pPrintPrec l 6 c1 <+> text "=" <+> pPrintPrec l 6 c2
  pPrintPrec l p (CSeq cs) = maybeParens (p > 0) $ vcat $ punctuate (text ";") $ map (pPrintPrec l 0) cs
  pPrintPrec l _ (CApply c1 c2) = pPrintPrec l 10 c1 <> brackets (pPrintPrec l 0 c2)
  pPrintPrec _ _ CFail = text "fail"
  pPrintPrec l p (CBar cs) = maybeParens (p > 7) $ fsep (punctuate (text " |") (map (pPrintPrec l 7) cs))
  pPrintPrec l _ (CMacro (Ident _ s) e) = text s <> braces (pPrintPrec l 0 e)
  pPrintPrec l p (CDef is e) =
    maybeParens (p > 0) $ fsep [text "def" <+> commaSep l 0 is, text "in" <+> pPrintPrec l 0 e]

instance Pretty Value where
  pPrintPrec l p (Var i) = pPrintPrec l p i
  pPrintPrec l p (HNF v) = pPrintPrec l p v

instance Pretty HNF where
  pPrintPrec l p (HInt i) = pPrintPrec l p i
  pPrintPrec _ _ (HRat _) = undefined -- pPrintPrec l p r
  pPrintPrec _ _ (HPrim s) = text s
  pPrintPrec l _ (HArray vs) = parens $ commaSep l 0 vs
  pPrintPrec l p (HLam i c) = maybeParens (p > 2) $ pPrintPrec l 0 i <+> text "=>" <+> pPrintPrec l 0 c
  pPrintPrec l p (HRec i c) = maybeParens (p > 2) $ text "rec" <+> pPrintPrec l 0 i <+> pPrintPrec l 0 c
  pPrintPrec l _ (HType v) = text "type" <> braces (pPrintPrec l 0 v)

------

coreToRedex :: Core -> SExp
coreToRedex = undefined

------

compos :: (Applicative f) => (Core -> f Core) -> Core -> f Core
compos f (CValue v) = CValue <$> appV f v
compos f (CUnify e1 e2) = CUnify <$> f e1 <*> f e2
compos f (CSeq es) = CSeq <$> traverse f es
compos f (CApply e1 e2) = CApply <$> f e1 <*> f e2
compos f (CBar es) = CBar <$> traverse f es
compos f (CMacro i e) = CMacro i <$> f e
compos f (CDef h e) = CDef h <$> f e

appV :: (Applicative f) => (Core -> f Core) -> Value -> f Value
appV _ v@Var{} = pure v
appV f (HNF v) = HNF <$> appH f v

appH :: (Applicative f) => (Core -> f Core) -> HNF -> f HNF
appH _ v@HInt{} = pure v
appH _ v@HRat{} = pure v
appH _ v@HPrim{} = pure v
appH f (HArray vs) = HArray <$> traverse (appV f) vs
appH f (HLam i e) = HLam i <$> f e
appH f (HRec i e) = HRec i <$> f e
appH f (HType v) = HType <$> appV f v

composOp :: (Core -> Core) -> Core -> Core
composOp f = runIdentity . compos (pure . f)
