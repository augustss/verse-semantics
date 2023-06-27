{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module FrontEnd.Core(
  Core(..),
  pattern COne, pattern CAll, pattern CSucceeds, pattern CDecides,
  Store(..),
  Ident(..), noLoc,
  exprToCore,
  pCoreFile, pCore,
  ) where
import Prelude hiding ((<>))
import Control.Monad(void)
import Data.Data(Data)
import qualified Data.IntMap as IM
import Data.Maybe
import Epic.Print
import FrontEnd.Expr hiding (compos, composOp)
import FrontEnd.Flags

import Text.Megaparsec(sepBy, sepBy1, many, eof, choice, some, optional, (<|>))
import FrontEnd.Parse(P, pOp, pParens, skip, pLiteral, pIdent, pMacroName, pBraces, try, pKeyword, lexeme, string)
import FrontEnd.Desugar(dsScope)

data Core
  = CVar Ident
  | CInt Integer
  | CRat Rational
  | CPrim String
  | CPtr Ptr
  | CArray [Value]
  | CLam Ident Core
  | CUnify Core Core
  | CSeq [Core]
  | CApply Core Core
  | CBar Core Core
  | CFail
  | CMacro Ident Core
  | CDef Heap Core
  | CWrong String
  | CSplit Core Value Value
  | CIf Core Core Core  -- Only for verification
  -- Store primitives
  | CStore Store Core
  deriving (Show, Eq, Data)

pattern COne :: Core -> Core
pattern COne c <- CMacro (Ident _ "one") c
  where COne e = CMacro (Ident noLoc "one") e
pattern CAll :: Core -> Core
pattern CAll c <- CMacro (Ident _ "all") c
  where CAll e = CMacro (Ident noLoc "all") e
pattern CSucceeds :: Core -> Core
pattern CSucceeds c <- CMacro (Ident _ "succeeds") c
  where CSucceeds e = CMacro (Ident noLoc "succeeds") e
pattern CDecides :: Core -> Core
pattern CDecides c <- CMacro (Ident _ "decides") c
  where CDecides e = CMacro (Ident noLoc "decides") e

type Value = Core

type Heap = [Ident]

data Store = Store { refMap :: IM.IntMap Value, outputs :: [Core] }
  deriving (Show, Eq, Data)
type Ptr = Int

instance Pretty Core where
  pPrintPrec l p (CVar i) = pPrintPrec l p i
  pPrintPrec l p (CInt i) = pPrintPrec l p i
  pPrintPrec _ _ (CRat _) = undefined -- pPrintPrec l p r
  pPrintPrec _ _ (CPrim s) = text s
  pPrintPrec _ _ (CPtr p) = text ("R#" ++ show p)
  pPrintPrec l _ (CArray [v]) = text "array" <> braces (pPrintPrec l 0 v)
  pPrintPrec l _ (CArray vs) = parens $ commaSep l vs
  pPrintPrec l p (CLam i c) = maybeParens (p > 2) $ pPrintPrec l 0 i <+> text "=>" <+> pPrintPrec l 0 c
  pPrintPrec l p (CUnify c1 c2) = maybeParens (p > 6) $ pPrintPrec l 6 c1 <+> text "=" <+> pPrintPrec l 6 c2
  pPrintPrec l p (CSeq cs) = maybeParens (p > 0) $ vcat $ punctuate (text ";") $ map (pPrintPrec l 1) cs
  pPrintPrec l p (CApply (CVar (Ident _ "~")) (CArray [c1, c2])) =
                  maybeParens (p > 6) $ pPrintPrec l 6 c1 <+> text "~" <+> pPrintPrec l 6 c2
  pPrintPrec l _ (CApply c1 c2) = pPrintPrec l 10 c1 <> brackets (pPrintPrec l 0 c2)
  pPrintPrec _ _ CFail = text "fail"
  pPrintPrec l p (CBar c1 c2) = maybeParens (p > 7) $ pPrintPrec l 7 c1 <+> text "|" <+> pPrintPrec l 7 c2
  pPrintPrec l _ (CMacro (Ident _ s) e) = text s <> braces (pPrintPrec l 0 e)
  pPrintPrec l p (CDef is e) =
    maybeParens (p > 0) $ fsep [text "ex" <+> hsep (map (pPrintPrec l 0) is) <> text ".", pPrintPrec l 0 e]
  pPrintPrec _ _ (CWrong s) = text $ "wrong(" ++ show s ++ ")"
  pPrintPrec l _ (CSplit e f g) =
    text "split" <> braces (sep [pPrintPrec l 0 e <> text ",",
                                 pPrintPrec l 0 f <> text ",",
                                 pPrintPrec l 0 g <> text ","])
  pPrintPrec l p (CIf e1 e2 e3) = maybeParens (p > 0) $ text "if" <+> pPrintPrec l 11 e1 <+> pPrintPrec l 11 e2 <+> pPrintPrec l 11 e3
  pPrintPrec l p (CStore s e) =
    maybeParens (p > 0) $ fsep [text "store"<+> pPrintPrec l p s <+> text "in", indent $ braces (pPrintPrec l 0 e)]

instance Pretty Store where
  pPrintPrec l _ (Store m _) = commaSep l (IM.toList m) -- XXX

exprToCore :: Flags -> Expr -> Core
--exprToCore _ e | trace ("exprToCore: " ++ prettyShow e) False = undefined
exprToCore _flg = core
  where core ee =
          case ee of
            Variable i -> CVar i
            LitInt i -> CInt i
            --LitRat i -> CRat i
            EPrim p -> CPrim p
            Array es -> CArray (map core es)
            Lam i e -> CLam i (core e)
            Unify e1 e2 -> CUnify (core e1) (core e2)
            Seq es -> CSeq (map core es)
            ApplyD e1 e2 -> CApply (core e1) (core e2)
            Choice e1 e2 -> CBar (core e1) (core e2)
            Fail -> CFail
            Macro1 m [] e -> CMacro m (core e)
            Exists is e -> CDef is (core e)
            Wrong s -> CWrong s
            Split e1 e2 e3 -> CSplit (core e1) (core e2) (core e3)
            If3 e1 e2 e3 -> CIf (core e1) (core e2) (core e3)
            e -> error $ "exprToCore: " ++ prettyShow e

{-
module FrontEnd.Core(
  Core(..),
  Store(..),
  pattern HNF, pattern CValue,
  pattern COne, pattern CAll, pattern CSucceeds, pattern CDecides,
  Value,
  getValue, getHNF,
  compos, composOp,
  exprToCore,
  cSeq, cDef, cBar,
  isCValue,
  fvs, cfvs, fvsS,
  subst,
  alphaConvert,
  pCore, pCoreFile,
  Ident(..), noLoc,
  storeAlloc, storeRead, storeWrite, storeEmpty, storePrint,
  cAssume,
  ) where
import Prelude hiding ((<>))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Data(Data)
import qualified Data.IntMap as IM
import Data.List
import Data.Maybe
import GHC.Stack(HasCallStack)
import Text.Megaparsec(sepBy, sepBy1, many, eof, choice, some, optional, (<|>))

import Epic.Print
import FrontEnd.Expr hiding (compos, composOp)
import FrontEnd.Desugar(primOps, covariantId, dsScope)
import FrontEnd.Error(unimplemented, impossible, internalError)
import FrontEnd.Flags
import FrontEnd.Parse(P, pOp, pParens, skip, pLiteral, pIdent, pMacroName, pBraces, try, pKeyword, lexeme, string)
--import Debug.Trace

data Core
  = CVar Ident
  | CInt Integer
  | CRat Rational
  | CPrim String
  | CPtr Ptr
  | CArray [Value]
  | CLam Ident Core
  | CUnify Core Core
  | CSeq [Core]
  | CApply Core Core
  | CBar Core Core
  | CFail
  | CMacro Ident Core
  | CDef Heap Core
  | CWrong String
  | CSplit Core Value Value
  | CIf Core Core Core  -- Only for verification
  -- Store primitives
  | CStore Store Core
  deriving (Show, Eq, Data)

type Value = Core

type Heap = [Ident]

data Store = Store { refMap :: IM.IntMap Value, outputs :: [Core] }
  deriving (Show, Eq, Data)
type Ptr = Int

pattern COne :: Core -> Core
pattern COne c <- CMacro (Ident _ "one") c
  where COne e = CMacro (Ident noLoc "one") e
pattern CAll :: Core -> Core
pattern CAll c <- CMacro (Ident _ "all") c
  where CAll e = CMacro (Ident noLoc "all") e
pattern CSucceeds :: Core -> Core
pattern CSucceeds c <- CMacro (Ident _ "succeeds") c
  where CSucceeds e = CMacro (Ident noLoc "succeeds") e
pattern CDecides :: Core -> Core
pattern CDecides c <- CMacro (Ident _ "decides") c
  where CDecides e = CMacro (Ident noLoc "decides") e

pattern HNF :: Core -> Core
pattern HNF e <- (getHNF -> Just e)

pattern CValue :: Core -> Value
pattern CValue e <- (getValue -> Just e)
  where CValue e = e
--x          | isValue e = e
--x          | otherwise = error "pattern CValue"

cSeq :: [Core] -> Core
cSeq [] = internalError
cSeq [e] = e
cSeq es = CSeq es

cDef :: [Ident] -> Core -> Core
cDef [] e = e
cDef is e = CDef is e

cBar :: [Core] -> Core
cBar [] = CFail
cBar [e] = e
cBar (e:es) = CBar e $ cBar es

getHNF :: Core -> Maybe Core
getHNF e@CInt{} = Just e
getHNF e@CRat{} = Just e
getHNF e@CArray{} = Just e
getHNF e@CPrim{} = Just e
getHNF e@CPtr{} = Just e
getHNF e@CLam{} = Just e
getHNF _ = Nothing

getValue :: Core -> Maybe Core
getValue e@CVar{} = Just e
getValue e = getHNF e

isCValue :: Core -> Bool
isCValue e = isJust (getValue e)

{-
isValue' :: Core -> Bool
isValue' (CSeq [e]) = isValue' e
isValue' (CSeq (e:es)) = isValue' e && isValue' (CSeq es)
isValue' e = isValue e
-}

vEmpty :: Value
vEmpty = CArray []

type C = ReaderT Flags (State Int)

seqC :: (HasCallStack) => [Core] -> Core
seqC acs =
  case {-concatMap flat-} acs of
    [] -> CArray []
    [c] -> c
    cs -> CSeq cs
{-
  where
    flat (CSeq cs) = concatMap flat cs
    flat c = [c]
-}

newTmp :: C Ident
newTmp = do
  n <- get
  let i = Ident noLoc ("$c" ++ show n)
  put $! n+1
  pure i

exprToCore :: Flags -> Expr -> Core
--exprToCore _ e | trace ("exprToCore: " ++ prettyShow e) False = undefined
exprToCore flg e = flip evalState 1 . flip runReaderT flg . core $ e

core :: HasCallStack => Expr -> C Core
core (LitInt i) = pure (CInt i)
core (LitRat i _) = pure (CRat $ toRational i)
core (Variable i@(Ident _ s)) | i `elem` primOps = pure (CPrim s)
                              | otherwise = pure (CVar i)
core (Array es) = CArray <$> mapM core es
core (Wrong s) = pure $ CWrong s
core (Seq es) = seqC <$> mapM core es
core (ApplyS e1 e2) = cSucceeds =<< core (ApplyD e1 e2)
core (ApplyD e1 e2) = CApply <$> core e1 <*> core e2
core (ApplyEff rs e) = coreEffs rs =<< core e
core (Unify e1 e2) = cUnify <$> core e1 <*> core e2
core e@Typedef{} = core e
core e@Choice{} = cBar <$> mapM core (flat e)
  where flat (Choice e1 e2) = flat e1 ++ flat e2
        flat ee = [ee]
core Fail = pure $ CFail
core (For2 e1@(Exists (i:is) (Unify (Variable i') e)) e2@(Variable i'')) | i == i' && i == i'' = do
  useSplit <- asks fSplit
  if useSplit then
    forSplit e1 e2
   else
    cAll =<< core (Exists is e)
core (For2 e1 e2) = do
  useSplit <- asks fSplit
  if useSplit then
    forSplit e1 e2
   else do
--  traceM $ show (e2, e2', seqE [e1, e2'])
    ee <- coreBind e1 e2
    ea <- cAll ee
    xa <- newTmp
    pure $ CDef [xa] $ CSeq [cUnify (CVar xa) ea, CApply (CPrim "mapAp$") (CVar xa)]
core (If3 e1 e2 e3) = do
  noLambdaIf <- asks fNoLambdaIf
  useSplit <- asks fSplit
  verif <- asks fVerify
  if verif then
    let Exists is e = e1
    in  CIf <$> (CDef is <$> core e) <*> core e2 <*> core e3
   else if noLambdaIf then
    coreIf e1 e2 e3
   else if useSplit then
    ifSplit e1 e2 e3
   else do
    e3' <- thunk e3
    l <- coreBind e1 e2
    r <- core e3'
    fn <- cOne $ CBar l r
    i <- newTmp
    pure $ CDef [i] $ seqC [cUnify (CVar i) fn, CApply (CVar i) vEmpty]
core (Macro1 (Ident _ "all") [] e) = cAll =<< core e
core (Macro1 (Ident _ "one") [] e) = cOne =<< core e
core (Macro1 (Ident _ "succeeds") [] e) = cSucceeds =<< core e
core (Macro1 (Ident _ "decides") [] e) = cDecides =<< core e
core (Exists is e) = cDef is <$> core e
core (TLam i rs e1 e2 me3) = do
  --traceM $ "Lambda:\n" ++ prettyShow eee ++ "\n+++++\n"
  verif <- asks fVerify
  let covariant = covariantId `elem` rs  || True -- XXX
  if verif then
    lamFuncVerify covariant i e1 e2 me3
   else
    lamFunc covariant i e1 (maybe e2 (HasType e2) me3)
core (HasType e t) = do
  verif <- asks fVerify
  e' <- core e
  t' <- core t
  if verif then do
    x <- newTmp
    pure $ CSeq [
      cVerify $ cAssert $ CApply t' e',
      CDef [x] $ CApply t' (CVar x)
      ]
   else do
      cSucceeds (CApply t' e')
core (Macro1 (Ident _ "assume") [] e) = cAssume <$> core e
core e = impossible e

coreBind :: Expr -> Expr -> C Core
coreBind (Exists is e1) e2 = do
  e2' <- thunk e2
  core (Exists is (seqE [e1, e2']))
coreBind e _ = error $ "coreBind: " ++ prettyShow e

coreIf :: Expr -> Expr -> Expr -> C Core
coreIf (Exists is e1) e2 e3 = do
  y <- newTmp
  let vy = Variable y
      one e = Macro1 (Ident noLoc "one") [] e
  core $ Exists [y] $
         Seq [
              Unify vy
              (one $ Exists is (Seq [e1, Array (map Variable is)])
                     `Choice`
                     LitInt 0
              ),
              Exists is (Seq [Unify vy $ Array (map Variable is), e2])
              `Choice`
              (Seq [Unify vy (LitInt 0), e3])
         ]
coreIf _ _ _ = undefined

lamFunc :: HasCallStack => Bool -> Ident -> Expr -> Expr -> C Core
--lamFunc _ _ e _ | trace ("lamFunc: " ++ prettyShow e) False = undefined
lamFunc cov i (Exists is e1) e2 = do
  b <- core $
    if null is && e1 == Array [] then
      e2
    else
      if cov then
        Exists is (Seq [e1, e2])
      else
        If3 (Exists is e1) e2 (Wrong "outside domain")
  pure $ CLam i b
lamFunc _ _ e _ = error $ "lamFunc: " ++ prettyShow e

lamFuncVerify :: HasCallStack => Bool -> Ident -> Expr -> Expr -> Maybe Expr -> C Core
--lamFunc _ _ e _ | trace ("lamFunc: " ++ prettyShow e) False = undefined
lamFuncVerify _cov i (Exists is e1) e2 me3 = do
  e1' <- core e1
  (e2', e2'') <-
    case me3 of
      -- No return type
      Nothing -> do
        e' <- core e2
        pure (e', e')
      -- Return type t: verify it and use an existential for uses
      Just t -> do
        e' <- core $ ApplyD t e2
        e'' <- do x <- newTmp
                  core $ Exists [x] $ ApplyD t (Variable x)
        pure (e', e'')
  pure $ CSeq [
    cVerify $ CLam i $ CDef is $ CSeq [cAssume e1', cAssert e2'],
              CLam i $ CDef is $ CSeq [        e1', cAssume e2'']
    ]
lamFuncVerify _ _ e _ _ = error $ "lamFunc: " ++ prettyShow e

coreEffs :: [Ident] -> Core -> C Core
coreEffs [] e = pure e
coreEffs [Ident _ "decides"] e = cDecides e
coreEffs [Ident _ "succeeds"] e = cSucceeds e
coreEffs rs _ = unimplemented $ "effects: " ++ prettyShow rs

ifSplit :: Expr -> Expr -> Expr -> C Core
ifSplit (Exists vs e1) e2 e3 = do
  e1' <- core (Exists vs $ Seq [e1, Array $ map Variable vs])
  e2' <- core e2
  e3' <- core e3
  x <- newTmp
  let f = CLam underscore e3'
      g = CLam x $ CLam underscore $ CLam underscore $ cDef vs $ cSeq [
             CUnify (CVar x) (CArray $ map CVar vs),
             e2' ]
  pure $ CSplit e1' f g
ifSplit e1 e2 e3 = impossible (e1,e2,e3)

forSplit :: Expr -> Expr -> C Core
forSplit (Exists vs e1) e2 = do
  h <- newTmp
  u <- newTmp
  x <- newTmp
  y <- newTmp
  a <- newTmp
  b <- newTmp
  e1' <- core (Exists vs $ Seq [e1, Array $ map Variable vs])
  e2' <- core e2
  let fe = CLam u $ CArray []
      ge = CLam x $ CLam y $ CLam h $ CDef (a:b:vs) $ cSeq [
             CUnify (CVar x) (CArray $ map CVar vs),
             CUnify (CVar a) e2',
             CUnify (CVar b) (CSplit (CApply (CVar y) (CArray [])) fe (CVar h)),
             CApply (CPrim "cons$") (CArray [CVar a, CVar b])
             ]
  pure $ CSplit e1' fe ge
forSplit e1 e2 = impossible (e1,e2)

cOne :: Core -> C Core
cOne e = do
 useSplit <- asks fSplit
 if not useSplit then pure $ COne e
 else do
  v <- newTmp
  pure $ CSplit e (CLam underscore CFail) (CLam v $ CLam underscore $ CLam underscore $ CVar v)

underscore :: Ident
underscore = Ident noLoc "_"

cAll :: Core -> C Core
cAll e = do
 useSplit <- asks fSplit
 if not useSplit then pure $ CAll e
 else do
  h <- newTmp
  v <- newTmp
  r <- newTmp
  x <- newTmp
  let fe = CLam underscore $ CArray []
      ge = CLam v $ CLam r $ CLam h $
             CDef [x] $ CSeq [
               CUnify (CVar x) (CSplit (CApply (CVar r) (CArray [])) fe (CVar h)),
               CApply (CPrim "cons$") (CArray [CVar v, CVar x])
             ]
  pure $ CSplit e fe ge

cSucceeds :: Core -> C Core
cSucceeds e = do
 useSplit <- asks fSplit
 if not useSplit then do
   verif <- asks fVerify
   if verif then pure $ cAssert e
   else pure $ CSucceeds e
 else do
  u1 <- newTmp
  u2 <- newTmp
  u3 <- newTmp
  u4 <- newTmp
  x <- newTmp
  y <- newTmp
  pure $ CSplit e
                (CLam u1 (CWrong "succeeds-fail"))
                (CLam x $ CLam y $ CLam underscore $
                          CSplit (CApply (CVar y) (CArray []))
                                 (CLam u2 (CVar x))
                                 (CLam u3 $ CLam u4 $ CLam underscore $ CWrong "succeed-many")
                )

cDecides :: Core -> C Core
cDecides e = do
 useSplit <- asks fSplit
 if not useSplit then pure $ CDecides e
 else do
  u1 <- newTmp
  u2 <- newTmp
  u3 <- newTmp
  u4 <- newTmp
  x <- newTmp
  y <- newTmp
  pure $ CSplit e
                (CLam u1 CFail)
                (CLam x $ CLam y $ CSplit (CApply (CVar y) (CArray []))
                                          (CLam u2 (CVar x))
                                          (CLam u3 $ CLam u4 $ CLam underscore $ CWrong "succeed-many")
                )

cVerify :: Core -> Core
cVerify e = CMacro (Ident noLoc "verify") e
cAssert :: Core -> Core
cAssert e = CMacro (Ident noLoc "assert") e
cAssume :: Core -> Core
cAssume e = CMacro (Ident noLoc "assume") e

-- A small optimization to get smaller examples.
cUnify :: Core -> Core -> Core
cUnify e (CPrim ":any") = e
cUnify e1 e2 = CUnify e1 e2

thunk :: Expr -> C Expr
thunk e = do
--  i <- newTmp
  i <- pure $ Ident noLoc "_"
  pure $ TLam i [] (Exists [] $ Array []) e Nothing

------

storeAlloc :: Value -> Store -> (Store, Ptr)
storeAlloc v s@Store{refMap = m} =
  let p = maybe 0 (succ . fst) $ IM.lookupMax m
  in  (s{refMap = IM.insert p v m}, p)

storeRead :: Ptr -> Store -> Value
storeRead p Store{refMap = m } =
  fromMaybe (error $ "Ptr not in store: " ++ show p) $ IM.lookup p m

storeWrite :: Ptr -> Value -> Store -> Store
storeWrite p v s@Store{ refMap = m } = s{ refMap = IM.insert p v m }

storeEmpty :: Store
storeEmpty = Store { refMap = IM.empty, outputs = [] }

storePrint :: Core -> Store -> Store
storePrint h s = s{ outputs = outputs s ++ [h] }

storeMap :: (Value -> Value) -> Store -> Store
storeMap f s = s{ refMap = IM.map f (refMap s) }

storeMapA :: (Applicative a) => (Value -> a Value) -> Store -> a Store
storeMapA f s = Store <$> sequenceA (IM.map f (refMap s)) <*> pure (outputs s)

storeValues :: Store -> [Value]
storeValues s = map snd $ IM.toList $ refMap s

------

instance Pretty Core where
  pPrintPrec l p (CVar i) = pPrintPrec l p i
  pPrintPrec l p (CInt i) = pPrintPrec l p i
  pPrintPrec _ _ (CRat _) = undefined -- pPrintPrec l p r
  pPrintPrec _ _ (CPrim s) = text s
  pPrintPrec _ _ (CPtr p) = text ("R#" ++ show p)
  pPrintPrec l _ (CArray [v]) = text "array" <> braces (pPrintPrec l 0 v)
  pPrintPrec l _ (CArray vs) = parens $ commaSep l vs
  pPrintPrec l p (CLam i c) = maybeParens (p > 2) $ pPrintPrec l 0 i <+> text "=>" <+> pPrintPrec l 0 c
  pPrintPrec l p (CUnify c1 c2) = maybeParens (p > 6) $ pPrintPrec l 6 c1 <+> text "=" <+> pPrintPrec l 6 c2
  pPrintPrec l p (CSeq cs) = maybeParens (p > 0) $ vcat $ punctuate (text ";") $ map (pPrintPrec l 1) cs
  pPrintPrec l p (CApply (CVar (Ident _ "~")) (CArray [c1, c2])) =
                  maybeParens (p > 6) $ pPrintPrec l 6 c1 <+> text "~" <+> pPrintPrec l 6 c2
  pPrintPrec l _ (CApply c1 c2) = pPrintPrec l 10 c1 <> brackets (pPrintPrec l 0 c2)
  pPrintPrec _ _ CFail = text "fail"
  pPrintPrec l p (CBar c1 c2) = maybeParens (p > 7) $ pPrintPrec l 7 c1 <+> text "|" <+> pPrintPrec l 7 c2
  pPrintPrec l _ (CMacro (Ident _ s) e) = text s <> braces (pPrintPrec l 0 e)
  pPrintPrec l p (CDef is e) =
    maybeParens (p > 0) $ fsep [text "ex" <+> hsep (map (pPrintPrec l 0) is) <> text ".", pPrintPrec l 0 e]
  pPrintPrec _ _ (CWrong s) = text $ "wrong(" ++ show s ++ ")"
  pPrintPrec l _ (CSplit e f g) =
    text "split" <> braces (sep [pPrintPrec l 0 e <> text ",",
                                 pPrintPrec l 0 f <> text ",",
                                 pPrintPrec l 0 g <> text ","])
  pPrintPrec l p (CIf e1 e2 e3) = maybeParens (p > 0) $ text "if" <+> pPrintPrec l 11 e1 <+> pPrintPrec l 11 e2 <+> pPrintPrec l 11 e3
  pPrintPrec l p (CStore s e) =
    maybeParens (p > 0) $ fsep [text "store"<+> pPrintPrec l p s <+> text "in", indent $ braces (pPrintPrec l 0 e)]

instance Pretty Store where
  pPrintPrec l _ (Store m _) = commaSep l (IM.toList m) -- XXX

------

compos :: (Applicative f) => (Core -> f Core) -> Core -> f Core
compos _ v@CVar{} = pure v
compos _ v@CInt{} = pure v
compos _ v@CRat{} = pure v
compos _ v@CPrim{} = pure v
compos _ v@CPtr{} = pure v
compos f (CArray vs) = CArray <$> traverse f vs
compos f (CLam i e) = CLam i <$> f e
compos f (CUnify e1 e2) = CUnify <$> f e1 <*> f e2
compos f (CSeq es) = CSeq <$> traverse f es
compos f (CApply e1 e2) = CApply <$> f e1 <*> f e2
compos f (CBar e1 e2) = CBar <$> f e1 <*> f e2
compos _ CFail = pure CFail
compos f (CMacro i e) = CMacro i <$> f e
compos f (CDef h e) = CDef h <$> f e
compos _ e@CWrong{} = pure e
compos f (CSplit e n g) = CSplit <$> f e <*> f n <*> f g
compos f (CIf e1 e2 e3) = CIf <$> f e1 <*> f e2 <*> f e3
compos f (CStore s e) = CStore <$> storeMapA f s <*> f e

composOp :: (Core -> Core) -> Core -> Core
composOp f = runIdentity . compos (pure . f)

-- Unique free variables
fvs :: Core -> [Ident]
fvs = nub . cfvs

fvsS :: Store -> [Ident]
fvsS = nub . cfvsS

-- Occurrences of free variables
cfvs :: Core -> [Ident]
cfvs (CVar v) = [v]
cfvs CInt{} = []
cfvs CRat{} = []
cfvs CPrim{} = []
cfvs CPtr{} = []
cfvs (CArray vs) = concatMap cfvs vs
cfvs (CLam i e) = filter (/= i) $ cfvs e
cfvs (CUnify e1 e2) = cfvs e1 ++ cfvs e2
cfvs (CSeq es) = concatMap cfvs es
cfvs (CApply e1 e2) = cfvs e1 ++ cfvs e2
cfvs (CBar e1 e2) = cfvs e1 ++ cfvs e2
cfvs CFail = []
cfvs (CMacro _ e) = cfvs e
cfvs (CDef is e) = filter (`notElem` is) $ cfvs e
cfvs (CSplit e f g) = cfvs e ++ cfvs f ++ cfvs g
cfvs CWrong{} = []
cfvs (CIf e1@(CDef is _) e2 e3) = cfvs e1 ++ cfvs (CDef is e2) ++ cfvs e3
cfvs CIf{} = undefined
cfvs (CStore s e) = cfvsS s ++ cfvs e

cfvsS :: Store -> [Ident]
cfvsS = concatMap cfvs . storeValues

------

-- Replace x by b in e
-- Do an occurs check and alpha-conversion when necessary
subst :: Ident -> Value -> Core -> Core
subst x b ae | x `elem` bs = impossible "subst occur check"
             | otherwise = sub ae
  where
    bs = fvs b
    sub v@(CVar i) | i == x = b
                  | otherwise = v
    sub e@CInt{} = e
    sub e@CRat{} = e
    sub e@CPrim{} = e
    sub e@CPtr{} = e
    sub (CArray vs) = CArray $ map sub vs
    sub a@(CLam i e) | x == i = a
                     | i `notElem` bs = CLam i $ sub e
                     | otherwise = sub $ alphaConvert bs a
    sub (CUnify e1 e2) = CUnify (sub e1) (sub e2)
    sub (CSeq es) = CSeq $ map sub es
    sub (CApply e1 e2) = CApply (sub e1) (sub e2)
    sub (CBar e1 e2) = CBar (sub e1) (sub e2)
    sub CFail = CFail
    sub (CMacro i e) = CMacro i $ sub e
    sub a@(CDef h e) | x `elem` h = a
                     | null (intersect bs h) = CDef h $ sub e
                     | otherwise = sub $ alphaConvert bs a
    sub e@CWrong{} = e
    sub (CSplit e f g) = CSplit (sub e) (sub f) (sub g)
    sub (CIf (CDef is e1) e2 e3) =
      let CDef is' (CSeq [e1',e2']) = sub (CDef is (CSeq [e1, e2]))
      in  CIf (CDef is' e1') e2' (sub e3)
    sub CIf{} = undefined
    sub (CStore s e) = CStore (storeMap sub s) (sub e)

-- Alpha convert a term, avoiding vs as the names for bound
-- variables.
alphaConvert :: [Ident] -> Core -> Core
alphaConvert vs = alpha []
  where
    alpha m (CVar i) = CVar $ fromMaybe i $ lookup i m
    alpha _ e@CInt{} = e
    alpha _ e@CRat{} = e
    alpha _ e@CPrim{} = e
    alpha _ e@CPtr{} = e
    alpha m (CArray es) = CArray (map (alpha m) es)
    alpha m (CLam i e) = CLam i' $ alpha (add (i, i') m) e where i' = fresh i
    alpha m (CUnify e1 e2) = CUnify (alpha m e1) (alpha m e2)
    alpha m (CSeq es) = CSeq (map (alpha m) es)
    alpha m (CApply e1 e2) = CApply (alpha m e1) (alpha m e2)
    alpha m (CBar e1 e2) = CBar (alpha m e1) (alpha m e2)
    alpha _ CFail = CFail
    alpha m (CMacro i e) = CMacro i (alpha m e)
    alpha m (CDef h e) = CDef h' (alpha m' e)
      where h' = map fresh h
            m' = foldr add m $ zip h h'
    alpha _ e@CWrong{} = e
    alpha m (CSplit e f g) = CSplit (alpha m e) (alpha m f) (alpha m g)
    alpha m (CIf (CDef h e1) e2 e3) =
      let CDef h' (CSeq [e1', e2']) = alpha m (CDef h (CSeq [e1, e2]))
      in  CIf (CDef h' e1') e2' (alpha m e3)
    alpha _ CIf{} = undefined
    alpha m (CStore s e) = CStore (storeMap (alpha m) s) (alpha m e)

    add ii@(i, i') m | i == i' = m
                     | otherwise = ii : m

    fresh i@(Ident l s) | i `notElem` vs = i
                        | otherwise = fresh $ Ident l (s ++ "'")

-----------------------------------
-}

-- Parse Core
pCoreFile :: P Core
pCoreFile = skip *> pCore <* eof

pCore :: P Core
pCore = exprToCore flg . dsScope <$> pSeq
  where flg = defaultFlags{ fSplit = False }

-- XXX pDef, pLam
-- XXX primops

pExists :: P Expr
pExists = exists <$> (pQuant *> some pIdent <* pOp ".") <*> pSeq
  where
    exists :: [Ident] -> Expr -> Expr
    exists is e = Exists is e
    pQuant = pKeyword "exists" <|> pKeyword "exi" <|> pKeyword "ex" <|> pKeyword "E"
      -- <|> void (pOp "∃")

pLam :: P Expr
pLam = lam <$> (pLambda *> some pIdent <* pOp ".") <*> pSeq
  where
    lam :: [Ident] -> Expr -> Expr
    lam is e = foldr Lam e is
    pLambda = pKeyword "lam" <|> pKeyword "lambda" <|> void (pOp "\\")
      -- <|> pKeyword "λ"

pSeq :: P Expr
pSeq = choice [ pLam, pExists, cons <$> pEqu <*> optional (pOp ";" *> pSeq) ]
  where
    cons e Nothing = e
    cons e (Just e') = Seq [e, e']

pEqu :: P Expr
pEqu = try (DefineE <$> (pIdent <* pOp ":=") <*> pChoice)
       <|>
       foldr1 Unify <$> sepBy1 pChoice (pOp "=")

pChoice :: P Expr
pChoice = foldr1 Choice <$> sepBy1 pApply (pOp "|")

pApply :: P Expr
pApply = do
  e1 <- pAtom
  let app f [] = f
      app f (a:as) = app (ApplyD f a) as
      pCall :: P Expr
      pCall = app e1 <$> many pTuple
      pBinOp = do i <- pOper; e2 <- pAtom; pure (ApplyD (Variable i) (Array [e1, e2]))
  pBinOp <|> pCall

pOper :: P Ident
pOper = choice $ map (\ o -> const (Ident noLoc ("in'" ++ o ++ "'")) <$> pOp o)
  [ ">=", "<=", "<>", "+", "-", "*", "/" ]

pTuple :: P Expr
pTuple = try (pParens (pure (Array [])))
         <|>
         pParens pComma

pComma :: P Expr
pComma = try (arr <$> pEqu <*> some (pOp "," *> pEqu))
         <|>
         pSeq
  where arr x xs = Array (x:xs)

pAtom :: P Expr
pAtom = choice [pTuple, pLiteral, pName, pMacro, pArray]

pName :: P Expr
pName = do
  i@(Ident l s) <- pIdent
  let ops = [ ("fail", xFail)
            , ("gt", vi "in'>'")
            , ("lt", vi "in'<'")
            , ("add", vi "in'+'")
            , ("addto", vi "in'+='")
            , ("isInt", vi "isInt$")
            ]
      vi = Variable . Ident l
      xFail = ApplyD (Array []) (LitInt 0)
  pure $ fromMaybe (Variable i) $ lookup s ops

pArray :: P Expr
pArray =
    Array <$> (pKeyword "array" *> pBraces (sepBy pSeq (pOp ",")))
  <|>
    pLT *> (Array <$> sepBy pSeq (pOp ",")) <* pGT
 where pLT = lexeme (string "<")
       pGT = lexeme (string ">")

pMacro :: P Expr
pMacro = mac <$> pMacroName <*> pBraces pSeq
  where
    mac i@(Ident _ s) e | s `elem` macros = Macro1 i [] e
    mac i _ = error $ "Unknown macro " ++ prettyShow i
    macros = ["one", "all", "succeeds", "decides"]
