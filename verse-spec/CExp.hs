module CExp(CExp(..), syntax, dI) where
import Control.Monad
import Control.Monad.State.Strict
import Data.Data
import qualified Data.List as L
import Exp

data CExp
  = CVar Ident
  | CInt Integer
  | CApp CExp CExp
  | CEqu CExp CExp
  | CSeq CExp CExp
  | CChoice CExp CExp
  | CFail
  | CExi Ident
  | CBlock CExp
  | CPrim Op
  | CTup [CExp]
  | CWhere CExp CExp
  | CIf CExp CExp CExp
  | CLam OC Ident CExp CExp (Maybe (Ident, CExp))
  | COfType CExp CExp
  | CFor CExp CExp
  | CAll CExp
  | CDef Ident CExp
  | CUChoice CExp CExp
  deriving (Eq, Ord, Data)

instance Show CExp where
  showsPrec _ (CVar s) = showString s
  showsPrec p (CInt i) = showsPrec p i
  showsPrec _ (CPrim o) = showString (drop 1 $ show o)
  showsPrec _ (CTup es) = showString "<" . showString (L.intercalate "," $ map show es) . showString ">"
  showsPrec _ (CApp e1 e2) = showsPrec 11 e1 . showString "[" . showsPrec 0 e2 . showString "]"
  showsPrec p (CEqu e1 e2) = showParen (p > 5) $ showsPrec 6 e1 . showString " = " . showsPrec 6 e2
  showsPrec p (CSeq e1 e2) = showParen (True || p > 3) $ showsPrec 3 e1 . showString "; " . showsPrec 3 e2
  showsPrec p (CWhere e1 e2) = showParen (p > 1) $ showsPrec 3 e1 . showString " where " . showsPrec 3 e2
  showsPrec _ (CExi i) = showString "exi " . showString i
  showsPrec _ CFail = showString "fail"
  showsPrec _ (CIf e1 e2 e3) = showString "if " . showParen True (showsPrec 0 e1) .
                              showBraces (showsPrec 0 e2) .
                              showBraces (showsPrec 0 e3)
  showsPrec _ (CLam q i e1 e2 me3) = showString ("lam" ++ [show q !! 0]) .
                              showParen True (showString i) . showParen True (showsPrec 0 e1) .
                              showBraces (showsPrec 0 e2) .
                              showBraces (maybe (showString "") (showsPrec 0) me3)
  showsPrec p (COfType e1 e2) = showParen (p > 3) $ showsPrec 4 e1 . showString " |> " . showsPrec 4 e2
  showsPrec p (CChoice e1 e2) = showParen (p > 4) $ showsPrec 5 e1 . showString " | " . showsPrec 5 e2
  showsPrec p (CUChoice e1 e2) = showParen (p > 4) $ showsPrec 5 e1 . showString " || " . showsPrec 5 e2
  showsPrec _ (CAll e) = showString "all" . showBraces (showsPrec 0 e)
  showsPrec _ (CFor e1 e2) = showString "for" . showParen True (showsPrec 0 e1) . showBraces (showsPrec 0 e2)
  showsPrec p (CDef x e) = showParen (p > 5) $ showString x . showString " := " . showsPrec 6 e
  showsPrec _ (CBlock e) = showString "block" . showBraces (showsPrec 0 e)

dI :: CExp -> [Ident]
dI (CApp e1 e2) = dI e1 `L.union` dI e2
dI (CEqu e1 e2) = dI e1 `L.union` dI e2
dI (CSeq e1 e2) = dI e1 `L.union` dI e2
dI (CWhere e1 e2) = dI e1 `L.union` dI e2
dI (COfType e1 e2) = dI e1 `L.union` dI e2
dI (CTup es) = foldr L.union [] (map dI es)
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

cseqs :: [CExp] -> CExp
cseqs [] = undefined
cseqs [c] = c
cseqs (c:cs) = c `CSeq` cseqs cs

syntax :: Ident -> Exp -> CExp
syntax u e = evalState (syntaxN u e) 1

syntaxN :: Ident -> Exp -> N CExp
syntaxN u (Int k) = pure $ u =.= CInt k
syntaxN u (Var x) = pure $ u =.= CVar x
syntaxN u (Prim p) = pure $ u =.= CPrim p
syntaxN _ Fail = pure CFail
syntaxN u (App e0 e1) = (u =.=) <$> (CApp <$> syntaxN "_" e0 <*> syntaxN "_" e1)
syntaxN u (Equ e0 e1) = CEqu <$> syntaxN u e0 <*> syntaxN u e1
syntaxN u (Choice e0 e1) = CChoice <$> syntaxN u e0 <*> syntaxN u e1
syntaxN u (UChoice e0 e1) = CUChoice <$> syntaxN u e0 <*> syntaxN u e1
syntaxN u (Seq e0 e1) = CSeq <$> syntaxN "_" e0 <*> syntaxN u e1
syntaxN u (Where e0 e1) = CWhere <$> syntaxN u e0 <*> syntaxN "_" e1
syntaxN "_" (Def x (Colon (Var "any"))) = pure $ CExi x `CSeq` CVar x   -- hack for x:any
syntaxN u (Def x e) = do
  c <- syntaxN u e
  pure $ cseqs [CExi x, CVar x `CEqu` c]
syntaxN u (Def2 x y e) = do
  c <- syntaxN u e
  pure $ cseqs [CExi x, u =.= CVar x, CExi y, CVar y `CEqu` c]
{-
syntaxN "_" (Colon e) = do
  x <- newVar "x"
  e' <- syntaxN "_" e
  pure $ COfType (CExi x `CSeq` CVar x) e'
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
syntaxN u (If e0 e1 e2) = CIf <$> syntaxN "_" e0 <*> syntaxN u e1 <*> syntaxN u e2
syntaxN u (For e0 e1) = (u =.=) <$> (CFor <$> syntaxN "_" e0 <*> syntaxN "_" e1)
syntaxN u (All e) = (u =.=) <$> (CAll <$> syntaxN "_" e)
syntaxN "_" (Fun q e0 e1) = do
  i <- newVar "i"
  CLam q i <$> syntaxN i e0 <*> syntaxN "_" e1 <*> pure Nothing
syntaxN u (Fun q e0 e1) = do
  i <- newVar "i"
  x <- newVar "x"
  k <- newVar "k"
  c0 <- syntaxN i e0
  c1 <- syntaxN k e1
  cq <- checkQ q u e0
  pure $ CLam q i (cseqs [ CExi x, CVar x `CEqu` c0 ]) (cseqs [CExi k `CSeq` (k =.= CApp (CVar u) (CVar x)), c1 ]) cq
syntaxN u (Block e) = CBlock <$> syntaxN u e

checkQ :: OC -> Ident -> Exp -> N (Maybe (Ident, CExp))
checkQ Open _ _ = pure Nothing
checkQ Closed f e = do
    e' <- syntaxN "_" e
    pure (Just (f, e'))

{-
mustBeVar :: Ident -> N CExp
mustBeVar "_" = do u <- newVar "u"; pure (CExi u `CSeq` CVar u)
mustBeVar u = pure (CVar u)
-}

infix 4 =.=

(=.=) :: Ident -> CExp -> CExp
"_" =.= c                 = c
u   =.= c                 = CVar u `CEqu` c

