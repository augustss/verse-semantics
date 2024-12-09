{-# OPTIONS_GHC -Wall #-}
module Main where
import Control.Monad hiding (ap)
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.List
import Data.Data
--import Data.Generics.Uniplate.Data(universeBi, transform, universe)
--import Data.Maybe
import Exp hiding (dI)
import Val
import Set
import Env
import Examples
--import Debug.Trace

implies :: Bool -> Bool -> Bool
implies x y = not x || y

--------------------------------------------------

data CExp = CVar Ident | CInt Integer | CPrim Op | CTup [CExp] | CApp CExp CExp
          | CEqu CExp CExp | CSeq CExp CExp | CWhere CExp CExp | CExi Ident
          | CIf CExp CExp CExp | CLam OC Ident CExp CExp
          | CFail | COfType CExp CExp
          | CChoice CExp CExp | CFor CExp CExp | CAll CExp
          | CChkClsd
  deriving (Eq, Ord, Data)

instance Show CExp where
  showsPrec _ (CVar s) = showString s
  showsPrec p (CInt i) = showsPrec p i
  showsPrec _ (CPrim o) = showString (drop 1 $ show o)
  showsPrec _ (CTup es) = showString "<" . showString (intercalate "," $ map show es) . showString ">"
  showsPrec _ (CApp e1 e2) = showsPrec 11 e1 . showString "[" . showsPrec 0 e2 . showString "]"
  showsPrec p (CEqu e1 e2) = showParen (p > 5) $ showsPrec 6 e1 . showString " = " . showsPrec 6 e2
  showsPrec p (CSeq e1 e2) = showParen (p > 3) $ showsPrec 3 e1 . showString "; " . showsPrec 3 e2
  showsPrec p (CWhere e1 e2) = showParen (p > 1) $ showsPrec 3 e1 . showString " where " . showsPrec 3 e2
  showsPrec _ (CExi i) = showString "exi " . showString i
  showsPrec _ CFail = showString "fail"
  showsPrec _ (CIf e1 e2 e3) = showString "if " . showParen True (showsPrec 0 e1) .
                              showBraces (showsPrec 0 e2) .
                              showBraces (showsPrec 0 e3)
  showsPrec _ (CLam q i e1 e2) = showString ("lam" ++ [show q !! 0]) .
                              showParen True (showString i) . showParen True (showsPrec 0 e1) .
                              showBraces (showsPrec 0 e2)
  showsPrec p (COfType e1 e2) = showParen (p > 3) $ showsPrec 4 e1 . showString " |> " . showsPrec 4 e2
  showsPrec p (CChoice e1 e2) = showParen (p > 4) $ showsPrec 5 e1 . showString " | " . showsPrec 5 e2
  showsPrec _ (CAll e) = showString "all" . showBraces (showsPrec 0 e)
  showsPrec _ (CFor e1 e2) = showString "for" . showParen True (showsPrec 0 e1) . showBraces (showsPrec 0 e2)


dI :: CExp -> [Ident]
dI (CApp e1 e2) = dI e1 `union` dI e2
dI (CEqu e1 e2) = dI e1 `union` dI e2
dI (CSeq e1 e2) = dI e1 `union` dI e2
dI (CWhere e1 e2) = dI e1 `union` dI e2
dI (COfType e1 e2) = dI e1 `union` dI e2
dI (CTup es) = foldr union [] (map dI es)
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
syntaxN u (Seq e0 e1) = CSeq <$> syntaxN "_" e0 <*> syntaxN u e1
syntaxN u (Where e0 e1) = CWhere <$> syntaxN u e0 <*> syntaxN "_" e1
syntaxN "_" (Def x (Colon (Var "any"))) = pure $ CExi x   -- hack for x:any
syntaxN u (Def x e) = do
  c <- syntaxN u e
  pure $ cseqs [CExi x, CVar x `CEqu` c]
syntaxN u (Def2 x y e) = do
  c <- syntaxN u e
  pure $ cseqs [CExi x, u =.= CVar x, CExi y, CVar y `CEqu` c]
syntaxN "_" (Colon e) = do
  x <- newVar "x"
  e' <- syntaxN "_" e
  pure $ COfType (CExi x `CSeq` CVar x) e'
syntaxN u (Colon e) =
  COfType <$> pure (CVar u) <*> syntaxN "_" e
-- Chk
syntaxN u (OfType e0 e1) = (u =.=) <$> (COfType <$> syntaxN "_" e0 <*> syntaxN "_" e1)
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
  CLam q i <$> syntaxN i e0 <*> syntaxN "_" e1
syntaxN u (Fun q e0 e1) = do
  i <- newVar "i"
  x <- newVar "x"
  k <- newVar "k"
  c0 <- syntaxN i e0
  c1 <- syntaxN k e1
  cq <- checkQ q u e0
  pure $ CLam q i (cseqs [ CExi x, CVar x `CEqu` c0 ]) (cseqs $ cq ++ [CExi k, k =.= CApp (CVar u) (CVar x), c1 ])

checkQ :: OC -> Ident -> Exp -> N [CExp]
checkQ Open _ _ = pure []
checkQ Closed f e = do
  e' <- syntaxN "_" e
  pure [CChkClsd f e']
  {-do
  a <- newVar "a"
  e' <- syntaxN "_" e
  pure [ CLam Closed a (CApp (CVar f) (CVar a)) (CVar a `CEqu` e') ]
  -}

mustBeVar :: Ident -> N CExp
mustBeVar "_" = do u <- newVar "u"; pure (CExi u `CSeq` CVar u)
mustBeVar u = pure (CVar u)

infix 4 =.=

(=.=) :: Ident -> CExp -> CExp
"_" =.= c                 = c
u   =.= c                 = CVar u `CEqu` c

-------------------------------------------

apply :: W -> W -> WS
apply (VTup ws) (VInt k) | 0 <= k' && k' < length ws = sing (ws !! k')  where k' = fromInteger k
apply (VFcn (Fcn _ xys)) w = maybe empty sing $ M.lookup w xys
apply _ _ = empty

dE :: CExp -> Env -> WS
dE (CVar "_") _                             = error "stray _"
dE (CVar x)  rho                            = sing $ lookupEnv x rho
dE (CInt k)    _                            = sing $ VInt k
dE (CPrim p)   _                            = sing $ dO p
dE (CTup es) rho                            = mkSet $ map VTup $ sequence $ map (\ e -> unSet (dE e rho)) es
dE (CApp e1 e2)   rho                       = mkSet [ r | v1 <- unSet $ dE e1 rho, v2 <- unSet $ dE e2 rho, r <- unSet $ apply v1 v2 ]
dE (COfType e1 e2) rho                      = dE (CApp e2 e1) rho
dE (CEqu e1 e2)   rho                       = dE e1 rho `isect` dE e2 rho
dE (CSeq e1 e2)   rho | isEmpty (dE e1 rho) = empty
                      | otherwise           = dE e2 rho
dE (CWhere e1 e2) rho | isEmpty (dE e2 rho) = empty
                      | otherwise           = dE e1 rho
dE (CExi _)         _                       = sing $ VInt 99999
dE CFail            _                       = empty
dE (CIf e1 e2 e3) rho                       = do
  case [ rho' | rho' <- dX e1 rho, not (isEmpty (dE e1 rho')) ] of
    [] -> dE e3 rho
    rhos -> sUnion [ dE e2 rho' | rho' <- rhos ]  -- XX Not the correct semantics
dE (CLam q i e1 e2)   rho                     = mkSet $ close q
  [ f
  | f <- unSet allWs, function f
--  , (q == Closed) `implies` hackyDomainTest i e1 e2 rho
  , forAll allWs $ \ w ->
      forAllL (dX e1 (extend rho i w)) $ \ rho' ->
        not (isEmpty (dE e1 rho'))
        `implies`
        (w `inDomV` f  &&  apV f w `sIn` dD e2 rho')
{-
  , (q == Closed)
    `implies`
    (forAll allWs $ \ w ->
       (w `inDomV` f) `implies`
         (existsL (dX e1 rho) $ \rho' -> not (isEmpty (dE e1 (extend rho' i w)))))
-}
  ]
dE e _ = error $ "unimplemented " ++ show e

close :: OC -> [W] -> [W]
close _ [] = []
close _ [f] = [f]
close Open fs = fs
close Closed fs =
  let r = [ f | f <- fs, forAllL fs (\ f' -> domV f `lessEq` domV f') ]
  in  --trace ("close " ++ show (fs, r))
      r

{-
-- Check that e1 and e2 fail on exactly the same inputs.
hackyDomainTest :: Ident -> CExp -> CExp -> Env -> Bool
hackyDomainTest i e1 e2 rho =
  forAll allWs $ \ w ->
    forAllL (dX e1 rho) $ \ rho' ->
      isEmpty (dE e1 (extend rho' i w)) == isEmpty (dD e2 rho')

--domE :: Exp -> Env -> WS
--domE e rho = mkSet [ x | x <- unSet allWs, rho' <- unSet $ dX e rho, not (isEmpty (dM e x rho') ) ]
-}

dX :: CExp -> Env -> [Env]
dX e rho = 
  let exts = sequence $ map (\ x -> map (x,) (unSet allWs)) (dI e)
  in  map (foldr (uncurry M.insert) rho) exts

dD :: CExp -> Env -> WS
dD e rho = sUnion [ dE e rho' | rho' <- dX e rho ]

den :: Exp -> WS
den e = dD (syntax "_" e) rho0

dP :: Exp -> RVal
dP e =
  case unSet (den e) of
    [v] -> RVal v
    vs   -> Wrong $ show vs

allExps :: [Example]
allExps = [exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22, exp33, exp34, exp35, exp45, exp46, exp49, exp50
          ]

main :: IO ()
main = do
  putStrLn "Start"
  runExamples dP allExps
