{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-methods #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
module Main where
import Control.Arrow(first)
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.List as L
import Data.Data
--import Data.Maybe
import Exp hiding (dI)
import ValK
import SetX
import EnvK
import Examples hiding ((===))
import Debug.Trace

--------------------------------------------------

data CExp = CVar Ident | CInt Integer | CPrim Op | CTup [CExp] | CApp Ident Ident
          | CEqu CExp CExp | CSeq CExp CExp | CWhere CExp CExp | CExi Ident
          | CIf CExp CExp CExp | CLam OC Ident CExp CExp (Maybe (Ident, CExp))
          | CFail | COfType CExp CExp
          | CChoice CExp CExp | CFor CExp CExp | CAll CExp
          | CDef Ident CExp
          | CUChoice CExp CExp
  deriving (Eq, Ord, Data)

instance Show CExp where
  showsPrec _ (CVar s) = showString s
  showsPrec p (CInt i) = showsPrec p i
  showsPrec _ (CPrim o) = showString (drop 1 $ show o)
  showsPrec _ (CTup es) = showString "<" . showString (L.intercalate "," $ map show es) . showString ">"
--  showsPrec _ (CApp e1 e2) = showsPrec 11 e1 . showString "[" . showsPrec 0 e2 . showString "]"
  showsPrec _ (CApp e1 e2) = showString e1 . showString "[" . showString e2 . showString "]"
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

dI :: CExp -> [Ident]
--dI (CApp e1 e2) = dI e1 `L.union` dI e2
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
syntaxN u (App (Var f) (Var x)) = pure (u =.= CApp f x)
syntaxN  u (App e0 e1) = do
  f <- newVar "f"
  x <- newVar "x"
  sf <- syntaxN "_" (Def f e0)
  sx <- syntaxN "_" (Def x e1)
  pure (sf `CSeq` sx `CSeq` (u =.= CApp f x))
--syntaxN u (App e0 e1) = (u =.=) <$> (CApp <$> syntaxN "_" e0 <*> syntaxN "_" e1)
syntaxN u (Equ e0 e1) = CEqu <$> syntaxN u e0 <*> syntaxN u e1
syntaxN u (Choice e0 e1) = CChoice <$> syntaxN u e0 <*> syntaxN u e1
syntaxN u (UChoice e0 e1) = CUChoice <$> syntaxN u e0 <*> syntaxN u e1
syntaxN u (Seq e0 e1) = CSeq <$> syntaxN "_" e0 <*> syntaxN u e1
syntaxN u (Where e0 e1) = CWhere <$> syntaxN u e0 <*> syntaxN "_" e1
syntaxN "_" (Def x (Colon (Var "any"))) = pure $ CExi x   -- hack for x:any
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
--syntaxN u (OfType e0 e1) = (u =.=) <$> (COfType <$> syntaxN "_" e0 <*> syntaxN "_" e1)
syntaxN u (OfType e0 e1) = syntaxN u (App e1 e0)
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
  pure $ CLam q i (cseqs [ CExi x, CVar x `CEqu` c0 ]) (cseqs [CExi k `CSeq` (k =.= CApp u x), c1 ]) cq

checkQ :: OC -> Ident -> Exp -> N (Maybe (Ident, CExp))
checkQ Open _ _ = pure Nothing
checkQ Closed f e = do
    e' <- syntaxN "_" e
    pure (Just (f, e'))

mustBeVar :: Ident -> N CExp
mustBeVar "_" = do u <- newVar "u"; pure (CExi u `CSeq` CVar u)
mustBeVar u = pure (CVar u)

infix 4 =.=

(=.=) :: Ident -> CExp -> CExp
"_" =.= c                 = c
u   =.= c                 = CVar u `CEqu` c

-------------------------------------------

newtype Sem a = Sem { unSem :: SetX [a] }
  deriving Show

instance Functor Sem where
  fmap f (Sem s) = Sem (fmap (fmap f) s)

instance Applicative Sem where
  pure x = Sem (sing [x])
  (<*>) = ap

instance Monad Sem where
  s >>= k = joinSem (fmap k s)

instance Alternative Sem where
  empty = Sem empty -- $ sing []
  Sem x1 <|> Sem x2 = Sem [ s1 ++ s2 | s1 <- x1, s2 <- x2 ]

joinSem :: Sem (Sem a) -> Sem a
joinSem (Sem ss) = Sem $ joinSetSet (fmap (fmap unSem) ss)

joinSetSet :: SetX [SetX [a]] -> SetX [a]
joinSetSet set = [ s2 | s1 <- set, s2 <- flatten s1 ]

flatten :: [SetX [a]] -> SetX [a]
flatten [] = sing []
flatten (s:ss) = [ s1 ++ s2 | s1 <- s, s2 <- flatten ss ]

-------------------------------------------

type WS = Sem (Env, W)

dE :: CExp -> Sem (Env, W)
dE (CVar "_") = Sem [ [(emptyEnv,    v)] | v <- allWs ]
dE (CVar x)   = Sem [ [(singEnv x v, v)] | v <- allWs ]
dE (CInt k)   = pure (emptyEnv, VInt k)
dE (CPrim p)  = Sem $ mkSet [ [(emptyEnv, v)] | v <- dO p ]
dE (CTup es)  = fmapM (comb VTup) $ traverse dE es
dE (CApp f x) = Sem $ mkSet [ [(mkEnv [(f, fv), (x, xv)], yv)]
                            | fv <- allWsL
                            , Just xys <- [enumFcn fv]
                            , (xv, yv) <- xys
                            ]
--dE (COfType e1 e2) = undefined
dE (CEqu e1 e2) = do
  (rho1, v1) <- dE e1
  (rho2, v2) <- dE e2
--  traceM $ "Equ 1 " ++ show (rho1, v1, rho2, v2)
  guard (v1 == v2)
--  traceM $ "Equ 2 " ++ show (rho1, rho2, combEnv rho1 rho2)
  rho <- combEnv rho1 rho2
--  traceM $ "Equ 3 " ++ show (rho, v2)
  pure (rho, v2)
dE (CSeq e1 e2) = do
  (rho1, _v1) <- dE e1
  (rho2, v2) <- dE e2
  rho <- combEnv rho1 rho2
  pure (rho, v2)
dE (CWhere e1 e2) = do
  (rho1, v1) <- dE e1
  (rho2, _v2) <- dE e2
  rho <- combEnv rho1 rho2
  pure (rho, v1)
dE (CExi _) = pure (emptyEnv, VInt 99999)
dE CFail = Sem $ sing []
--dE (CIf e1 e2 e3) = undefined
--dE (CLam q i e1 e2 me3) = undefined
dE (CChoice e1 e2) = dE e1 <|> dE e2
--dE (CUChoice e1 e2) = union (dE e1) (dE e2)
--dE (CAll e) rho = undefined
dE e = error $ "unimplemented " ++ show e

dD :: CExp -> WS
dD e = fmap (first (remVars xs)) $ dE e
  where xs = dI e

combEnv :: Env -> Env -> Sem Env   -- the list is always either empty or a singleton
combEnv r1 r2 =
  case unifyEnv r1 r2 of
    Nothing -> Sem empty
    Just x  -> pure x

comb :: ([a] -> a) -> [(Env, a)] -> Maybe (Env, a)
comb f xs = do
  let (rhos, ws) = unzip xs
  rho <- unifyEnvs rhos
  pure (rho, f ws)

fmapM :: (a -> Maybe b) -> Sem a -> Sem b
fmapM f (Sem x) = Sem (SetX.mapMaybe (mapM f) x)

unifyEnvs :: [Env] -> Maybe Env
unifyEnvs [] = Just emptyEnv
unifyEnvs [rho] = Just rho
unifyEnvs (rho:rhos) = unifyEnvs rhos >>= unifyEnv rho

-------------------------------------------

den :: Exp -> WS
den e = dD ({-redef $-} syntax "_" e)

dene :: Exp -> WS
dene e = dD ({-redef $-} syntax "_" e)

dP :: Exp -> RVal
dP e =
  case toList $ unSem $ den e of
    [[(_, v)]] -> RVal v
--        | otherwise       -> Wrong $ showListWith showPretty (toList s)
    vs                    -> Wrong $ show vs

allExps :: [Example]
allExps = [exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22,
           exp23,exp24,exp25,exp26,exp27,exp28,exp29,{-WRONG exp30,exp31,-}exp32,
           exp33, exp34, exp35,
           exp36, exp37, exp38, exp39, exp40, {- UNSURE exp41, exp43, exp44, -}
           exp45, exp46, exp47, exp48, {- UNSURE exp49, exp50, -}
           exp51, exp52,
           exp53, exp54,
           exp55, exp56, exp57, {- SLOW exp58,-} exp59, exp60,
           exp61, exp62
          ]

main :: IO ()
main = do
  putStrLn "Start"
  runExamples dP allExps

