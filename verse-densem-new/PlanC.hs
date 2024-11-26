--module PlanC where
import Control.Monad hiding (ap)
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.List
import Data.Data
import Data.Generics.Uniplate.Data(universeBi, transform, universe)
--import Data.Maybe
import Exp hiding (dI)
import Val
import Set
import Env
import Examples
import Debug.Trace

implies :: Bool -> Bool -> Bool
implies x y = not x || y

--------------------------------------------------

data CExp = CVal CVal | CApp CVal CVal
          | CEqu CExp CExp | CSeq CExp CExp | CWhere CExp CExp | CExi Ident
          | CIf CExp CExp CExp | CLam OC Ident CExp CExp
          | CFail
  deriving (Eq, Ord, Data)

data CVal = CVar Ident | CInt Integer | CPrim Op | CTup [CVal]
  deriving (Eq, Ord, Data)

cVar :: Ident -> CExp
cVar = CVal . CVar

cInt :: Integer -> CExp
cInt = CVal . CInt

cTup :: [CVal] -> CExp
cTup = CVal . CTup

instance Show CExp where
  showsPrec p (CVal v) = showsPrec p v
  showsPrec _ (CApp e1 e2) = showsPrec 11 e1 . showString "[" . showsPrec 0 e2 . showString "]"
  showsPrec p (CEqu e1 e2) = showParen (p > 5) $ showsPrec 6 e1 . showString " = " . showsPrec 6 e2
  showsPrec p (CSeq e1 e2) = showParen (p > 3) $ showsPrec 3 e1 . showString "; " . showsPrec 3 e2
  showsPrec p (CWhere e1 e2) = showParen (p > 1) $ showsPrec 3 e1 . showString " where " . showsPrec 3 e2
  showsPrec _ (CExi i) = showString "exi " . showString i
  showsPrec _ CFail = showString "fail"
  showsPrec _ (CIf e1 e2 e3) = showString "if " . showParen True (showsPrec 0 e1) .
                              showBraces (showsPrec 0 e2) .
                              showBraces (showsPrec 0 e3)
  showsPrec _ (CLam q i e1 e2) = showString (if q == Open then "lam_o" else "lam_c") .
                              showParen True (showString i) . showParen True (showsPrec 0 e1) .
                              showBraces (showsPrec 0 e2)

instance Show CVal where
  showsPrec _ (CVar s) = showString s
  showsPrec p (CInt i) = showsPrec p i
  showsPrec _ (CPrim o) = showString (drop 1 $ show o)
  showsPrec _ (CTup es) = showString "<" . showString (intercalate "," $ map show es) . showString ">"

dI :: CExp -> [Ident]
dI (CEqu e1 e2) = dI e1 `union` dI e2
dI (CSeq e1 e2) = dI e1 `union` dI e2
dI (CWhere e1 e2) = dI e1 `union` dI e2
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
syntaxN u (Int k) = pure $ u =.= cInt k
syntaxN u (Var x) = pure $ u =.= cVar x
syntaxN u (Prim p) = pure $ u =.= CVal (CPrim p)
syntaxN "_" (Tup es) = do
  (css, xs) <- unzip <$> mapM (syntaxNVal "t") es
  pure $ cseqs $ concat css ++ [cTup xs]
syntaxN u (Tup es) = do
  xs <- newVars (length es) "x"
  us <- newVars (length es) "u"
  cs <- zipWithM syntaxN us es
  let cs' = zipWith (\ x c -> cVar x `CEqu` c) xs cs
  pure $ cseqs $ map CExi us ++ [u =.= cTup (map CVar us)] ++ cs' ++ [cTup $ map CVar xs]
syntaxN u (App e0 e1) = do
  (c0, f) <- syntaxNVal "f" e0
  (c1, x) <- syntaxNVal "x" e1
  pure $ cseqs $ c0 ++ c1 ++ [u =.= CApp f x]
syntaxN u (Equ e0 e1) = CEqu <$> syntaxN u e0 <*> syntaxN u e1
syntaxN u (Def x e) = do
  c <- syntaxN u e
  pure $ cseqs [CExi x, cVar x `CEqu` c]
syntaxN u (Colon e) = do
  (c, f) <- syntaxNVal "f" e
  pure $ cseqs $ c ++ [CApp f (CVar u)]
syntaxN u (Seq e0 e1) = CSeq <$> syntaxN "_" e0 <*> syntaxN u e1
syntaxN u (Where e0 e1) = CWhere <$> syntaxN u e0 <*> syntaxN "_" e1
syntaxN u (If e0 e1 e2) = CIf <$> syntaxN "_" e0 <*> syntaxN u e1 <*> syntaxN u e2
syntaxN u (Fun q e0 e1) = do
  i <- newVar "i"
  x <- newVar "x"
  k <- newVar "k"
  c0 <- syntaxN i e0
  c1 <- syntaxN k e1
  pure $ CLam q i (cseqs [ CExi x, cVar x `CEqu` c0 ]) (cseqs [CExi k, k =.= CApp (CVar u) (CVar x), c1 ])
syntaxN _ Fail = pure CFail
syntaxN _ Choice{} = undefined
syntaxN _ All{} = undefined
syntaxN _ For{} = undefined

-- Put the expression in a variable.
-- The special cases vastly reduce the number of existentials.
syntaxNVal :: String -> Exp -> N ([CExp], CVal)
syntaxNVal _ (Int k) = pure ([], CInt k)
syntaxNVal _ (Var x) = pure ([], CVar x)
syntaxNVal _ (Prim p) = pure ([], CPrim p)
syntaxNVal s (Tup es) = do
  (css, xs) <- unzip <$> mapM (syntaxNVal s) es
  return (concat css, CTup xs)
syntaxNVal s e = do
  x <- newVar s
  c <- syntaxN "_" e
  return ([CExi x, cVar x `CEqu` c], CVar x)

--mustBeVar :: Ident -> N CExp
--mustBeVar "_" = do u <- newVar "u"; pure (CExi u `CSeq` CVar u)
--mustBeVar u = pure (CVar u)

infix 4 =.=

(=.=) :: Ident -> CExp -> CExp
"_" =.= c                 = c
u   =.= CApp (CVar "_") _ = cVar u
u   =.= c                 = cVar u `CEqu` c

-- Remove some nonsense
cleanup :: CExp -> CExp
cleanup =
  seqAssoc .
  removeExis .
  seqAssoc

-- Flatten Seq into its right associative form, remove values to the left of ;
seqAssoc :: CExp -> CExp
seqAssoc (CEqu e1 e2) = CEqu (seqAssoc e1) (seqAssoc e2)
seqAssoc (CSeq e1 e2) = seqApp (seqAssoc e1) (seqAssoc e2)
  where seqApp (CSeq s1 s2) s3 = xSeq s1 (seqApp s2 s3)
        seqApp s1 s2 = xSeq s1 s2
        xSeq (CVal _) s = s
        xSeq s1 s2 = CSeq s1 s2
seqAssoc (CWhere e1 e2) = CWhere (seqAssoc e1) (seqAssoc e2)
seqAssoc (CIf e1 e2 e3) = CIf (seqAssoc e1) (seqAssoc e2) (seqAssoc e3)
seqAssoc (CLam q x e1 e2) = CLam q x (seqAssoc e1) (seqAssoc e2)
seqAssoc e = e

-- Turn 'CExi x; ...; x = e' into '...; e'
-- if those are the only two occurences of x.
removeExis :: CExp -> CExp
removeExis ae =
  let allxs = allVariables ae
      exixs = [ i | CExi i <- universe ae ]
      lhsxs = [ i | CVal (CVar i) `CEqu` _ <- universeBi ae ]
      remxs = [ i | i <- lhsxs, i `elem` exixs, length (filter (== i) allxs) == 2 ]
      remvar (CExi x) | x `elem` remxs = CVal (CInt 99)          -- make it harmless constant
      remvar (CVal (CVar x) `CEqu` e) | x `elem` remxs = e
      remvar e = e
  in  --trace ("removeExis " ++ show remxs) $
      transform remvar ae

allVariables :: CExp -> [Ident]
allVariables e =
  [ i | CVar i <- universeBi e ] ++
  [ i | CExi i <- universeBi e ] ++
  [ i | CLam _ i _ _ <- universeBi e ]

-------------------------------------------

apply :: W -> W -> WS
apply (VTup ws) (VInt k) | 0 <= k' && k' < length ws = sing (ws !! k')  where k' = fromInteger k
apply (VFcn (Fcn _ xys)) w = maybe empty sing $ M.lookup w xys
apply _ _ = empty

dV :: CVal -> Env -> W
dV (CVar x)  rho = lookupEnv x rho
dV (CInt k)    _ = VInt k
dV (CPrim p)   _ = dO p
dV (CTup es) rho = VTup (map (flip dV rho) es)

dE :: CExp -> Env -> WS
dE (CVal v)       rho                       = sing (dV v rho)
dE (CApp e1 e2)   rho                       = apply (dV e1 rho) (dV e2 rho)
dE (CEqu e1 e2)   rho                       = dE e1 rho `isect` dE e2 rho
dE (CSeq e1 e2)   rho | isEmpty (dE e1 rho) = empty
                      | otherwise           = dE e2 rho
dE (CWhere e1 e2) rho | isEmpty (dE e2 rho) = empty
                      | otherwise           = dE e1 rho
dE (CExi _)         _                       = sing $ VInt 99999
dE CFail            _                       = empty
dE (CIf _e1 _e2 _e3)   _                       = error "CIf"
dE (CLam q i e1 e2)   rho                   = mkSet
  [ VFcn f
  | VFcn f <- unSet allWs
  , forAll allWs $ \ w ->
      forAllL (dX e1 rho) $ \ rho' ->
        not (isEmpty (dE e1 (extend rho' i w)))
        `implies`
        (w `inDom` f  &&  ap f w `sIn` dD e2 rho')
  , (q == Closed)
    `implies`
    (forAll allWs $ \ w ->
       (w `inDom` f) `implies`
         (existsL (dX e1 rho) $ \rho' -> not (isEmpty (dE e1 (extend rho' i w)))))
  ]

dX :: CExp -> Env -> [Env]
dX e rho = 
  let exts = sequence $ map (\ x -> map (x,) (unSet allWs)) (dI e)
  in  map (foldr (uncurry M.insert) rho) exts

dD :: CExp -> Env -> WS
dD e rho = sUnion [ dE e rho' | rho' <- dX e rho ]

den :: Exp -> WS
den e = dD (cleanup $ syntax "_" e) rho0

dP :: Exp -> RVal
dP e =
  case unSet (den e) of
    [v] -> RVal v
    _   -> Wrong ""

xx1, xx2 :: Exp
xx1 = Fun Closed (Def "x" (Int 1)) (Var "x")
xx2 = Fun Closed (Def "x" (Colon (Var "int"))) (Var "x" `Equ` Int 1)

{-
xx3 :: CExp
xx3 = CLam Closed "i1" $ cseqs [cVar "i1" `CEqu` cInt 1]

xx4 :: CExp
xx4 = CLam Closed "i1" $ cseqs [cVar "i1" `CEqu` CVal (CPrim Oint), cVar "i1" `CEqu` cInt 1]
-}

allExps :: [Example]
allExps = [exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22, exp33, exp34, exp35
          ]

main :: IO ()
main = do
  putStrLn "Start"
  runExamples dP allExps
