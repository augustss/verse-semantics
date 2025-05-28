{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
#define DESUGAR_WITH_H 1
module CExp(CExp(..), CBlk(..), syntax, dI, cexpb) where
import Control.Monad
import Control.Monad.State.Strict
import Data.Data
import qualified Data.List as L
import Exp

-- Mostly MiniVerse
data CExp
  = CVar Ident
  | CInt Integer
  | CApp CExp CExp
  | CEqu CExp CExp
  | CSeq CExp CExp
  | CChoice CBlk CBlk
  | CFail
  | CExi Ident
  | CPrim Op
  | CTup [CExp]
--  | CWhere CExp CExp
  | CIf CBlk CBlk CBlk
  | CLam OC Ident CBlk CBlk CBlk (Maybe (Ident, CExp))
  | COfType CExp CExp
  | CFor CBlk CBlk
  | CAll CBlk
  | COne CBlk
  | CDef Ident CExp
  | CUChoice CBlk CBlk
  | CBlock CBlk
  | CLHS                          -- HACK for fast lambda evaluation
  deriving (Eq, Ord, Data)

data CBlk = CBlk [CExp]
  deriving (Eq, Ord, Data)

cexpb :: CBlk -> CExp
cexpb (CBlk es) = foldl1 CSeq es

#if 1
instance Show CExp where
  showsPrec _ (CVar s) = showString s
  showsPrec p (CInt i) = showsPrec p i
  showsPrec _ (CPrim o) = showString (drop 1 $ show o)
  showsPrec _ (CTup es) = showString "<" . showString (L.intercalate "," $ map show es) . showString ">"
  showsPrec _ (CApp e1 e2) = showsPrec 11 e1 . showString "[" . showsPrec 0 e2 . showString "]"
  showsPrec p (CEqu e1 e2) = showParen (p > 5) $ showsPrec 6 e1 . showString " = " . showsPrec 6 e2
  showsPrec p (CSeq e1 e2) = showParen (True || p > 3) $ showsPrec 3 e1 . showString "; " . showsPrec 3 e2
--  showsPrec p (CWhere e1 e2) = showParen (p > 1) $ showsPrec 3 e1 . showString " where " . showsPrec 3 e2
  showsPrec _ (CExi i) = showString "exi " . showString i
  showsPrec _ CFail = showString "fail"
  showsPrec _ (CIf e1 e2 e3) = showString "if " . showParen True (showsPrec 0 $ cexpb e1) . showsPrec 0 e2 . showsPrec 0 e3
  showsPrec _ (CLam q i e1 e2 e3 me4) =
    showString ("lam" ++ [show q !! 0]) .
    showParen True (showString i) .
    showParen True (showsPrec 0 $ cexpb e1) .
    showParen True (showsPrec 0 $ cexpb e2) .
    showsPrec 0 e3 .
    maybe (showString "") (\ e4 -> showString " M=" . showsPrec 11 e4) me4
  showsPrec p (COfType e1 e2) = showParen (p > 3) $ showsPrec 4 e1 . showString " |> " . showsPrec 4 e2
  showsPrec p (CChoice e1 e2) = showParen (p > 4) $ showsPrec 5 (cexpb e1) . showString " | " . showsPrec 5 (cexpb e2)
  showsPrec p (CUChoice e1 e2) = showParen (p > 4) $ showsPrec 5 (cexpb e1) . showString " ||| " . showsPrec 5 (cexpb e2)
  showsPrec _ (CAll e) = showString "all" .  (showsPrec 0 e)
  showsPrec _ (CFor e1 e2) = showString "for" . showParen True (showsPrec 0 $ cexpb e1) . (showsPrec 0 e2)
  showsPrec p (CDef x e) = showParen (p > 5) $ showString x . showString " := " . showsPrec 6 e
  showsPrec p (CBlock e) = showsPrec p e
  showsPrec _ CLHS = showString "LHS"

instance Show CBlk where
  showsPrec _ (CBlk es) = showBraces $ foldr (.) id (L.intersperse (showString "; ") (map (showsPrec 3) es))
#else
deriving instance Show CExp
deriving instance Show CBlk
#endif

dI :: CExp -> [Ident]
dI (CApp e1 e2) = dI e1 `L.union` dI e2
dI (CEqu e1 e2) = dI e1 `L.union` dI e2
dI (CSeq e1 e2) = dI e1 `L.union` dI e2
--dI (CWhere e1 e2) = dI e1 `L.union` dI e2
dI (COfType e1 e2) = dI e1 `L.union` dI e2
dI (CTup es) = foldr L.union [] (map dI es)
dI (CDef _ e) = dI e
dI (CExi x) = [x]
dI _ = []

----------------------------------

-- Monad for generating new names
type N a = State Int a

newVar :: String -> N Ident
newVar s = do
  i <- get
  put (i+1)
  return $ s ++ "_" ++ show i

newVars :: Int -> String -> N [Ident]
newVars n s = replicateM n (newVar s)

cblocks :: [CExp] -> CBlk
cblocks = CBlk . concatMap flat
  where flat (CSeq e1 e2) = flat e1 ++ flat e2
        flat e = [e]

cblock :: CExp -> CBlk
cblock e = cblocks [e]

cseqs :: [CExp] -> CExp
cseqs [] = undefined
cseqs [e] = e
cseqs (CVar _:es@(_:_)) = cseqs es
cseqs (e:es) = e `CSeq` cseqs es

syntax :: Ident -> Exp -> CExp
syntax u e = CBlock $ cblock $ evalState (syntaxN u e) 1

syntaxNB :: Ident -> Exp -> N CBlk
syntaxNB u e = cblock <$> syntaxN u e

syntaxN :: Ident -> Exp -> N CExp
syntaxN u (Int k) = pure $ u =.= CInt k
syntaxN u (Var x) = pure $ u =.= CVar x
syntaxN u (Prim p) = pure $ u =.= CPrim p
syntaxN _ Fail = pure CFail
syntaxN u (App e0 e1) = (u =.=) <$> (CApp <$> syntaxN "_" e0 <*> syntaxN "_" e1)
syntaxN u (Equ e0 e1) = CEqu <$> syntaxN u e0 <*> syntaxN u e1
syntaxN u (Choice e0 e1) = CChoice <$> syntaxNB u e0 <*> syntaxNB u e1
syntaxN u (UChoice e0 e1) = CUChoice <$> syntaxNB u e0 <*> syntaxNB u e1
syntaxN u (Seq e0 e1) = CSeq <$> syntaxN "_" e0 <*> syntaxN u e1
--syntaxN u (Where e0 e1) = CWhere <$> syntaxN u e0 <*> syntaxN "_" e1
syntaxN u (Where e0 e1) = do
  a <- newVar "a"
  c0 <- syntaxN u e0
  c1 <- syntaxN "_" e1
  pure $ cseqs [ CDef a c0, c1, CVar a ]
syntaxN "_" (Def x (Colon (Var "any"))) = pure $ CExi x `CSeq` CVar x   -- hack for x:any
syntaxN u (Def x e) = do
  c <- syntaxN u e
  pure $ cseqs [CExi x, CVar x `CEqu` c]
syntaxN u (DefI x e) = do
  c <- syntaxN x e
  pure $ cseqs [u =.= CVar x, c]  
syntaxN u (Def2 x y e) = do
  c <- syntaxN x e
  pure $ cseqs [CExi x, u =.= CVar x, CExi y, CVar y `CEqu` c]
syntaxN "_" (Exi x) = pure $ CExi x
syntaxN u (Exi x) = pure $ cseqs [CExi x, u =.= CVar x]
{-
syntaxN "_" (Colon e) = do
  x <- newVar "x"
  e' <- syntaxN "_" e
  pure $ COfType (CExi x `cSeq` CVar x) e'
syntaxN u (Colon e) =
  COfType <$> pure (CVar u) <*> syntaxN "_" e
-}
syntaxN u (Colon e) = syntaxN "_" (App e (Var u))

-- Chk
syntaxN u (OfType e0 e1) = (u =.=) <$> (COfType <$> syntaxN "_" e0 <*> syntaxN "_" e1)
--syntaxN u (OfType e0 e1) = syntaxN u (App e1 e0)
syntaxN "_" (Tup es) = CTup <$> mapM (syntaxN "_") es
syntaxN u (Tup es) = do
  us <- newVars (length es) "u"
  cs <- zipWithM syntaxN us es
  pure $ cseqs $ map CExi us ++ [u =.= CTup (map CVar us)] ++ [CTup cs]
syntaxN u (If e0 e1 e2) = do
  c0 <- syntaxN "_" e0
  CIf (cblocks [c0, CLHS]) <$> syntaxNB u e1 <*> syntaxNB u e2
syntaxN u (For e0 e1) = (u =.=) <$> (CFor <$> syntaxNB "_" e0 <*> syntaxNB "_" e1)
syntaxN u (All e) = (u =.=) <$> (CAll <$> syntaxNB "_" e)
#if !DESUGAR_WITH_H
syntaxN "_" (Fun q e0 e1) = do
  i <- newVar "i"
  c0 <- syntaxN i e0
  CLam q i (cblocks [c0, CLHS]) <$> syntaxNB "_" e1 <*> pure Nothing
#else
syntaxN "_" f@Fun{} = do
  u <- newVar "h"
  CSeq (CExi u) <$> syntaxN u f
#endif
syntaxN u (Fun q e0 e1) = do
  i <- newVar "i"
  x <- newVar "x"
  k <- newVar "k"
  c0 <- syntaxN i e0
  c1 <- syntaxN k e1
  cq <- checkQ q u e0
  pure $ CLam q i (cblocks [CDef x c0, CLHS])
                  (cblocks [CDef k $ CApp (CVar u) (CVar x)])
                  (cblocks [c1])
                  cq
syntaxN u (Block e) = CBlock <$> syntaxNB u e

checkQ :: OC -> Ident -> Exp -> N (Maybe (Ident, CExp))
checkQ Open _ _ = pure Nothing
checkQ Closed f e = do
    e' <- syntaxN "_" e
    pure (Just (f, e'))

infix 4 =.=

(=.=) :: Ident -> CExp -> CExp
"_" =.= c                 = c
u   =.= c                 = CVar u `CEqu` c

