{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
module FrontEnd.Tim(dsProg, simpProg, Prog) where
import Prelude hiding ((<>))
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map as M
import GHC.Stack
import Debug.Trace

import Epic.List
import Epic.Print
import FrontEnd.Expr

notYet :: HasCallStack => a
notYet = error "not yet"

pattern Ef :: String -> Eff
pattern Ef s <- Ident _ s
  where Ef s = Ident noLoc s

data Id = Id
  { idName :: !String
  , idNo   :: !Int
  }
  deriving (Show, Eq, Ord)

{-
instance Ord Id where
  compare (Id _ k1) (Id _ k2) = compare k1 k2

instance Eq Id where
  Id _ k1 == Id _ k2  =  k1 == k2
-}

--type Set a = [a]

type OEff = String

data Opc
  = Id :=: Exp
  | ChoiceOp Opc Opc
  | ScopeOp Ops
  | ExistsOp [Id] Ops
  | VerifyOp OEff Opc
  deriving (Show, Eq)
type Ops = [Opc]

data Exp
  = Int Integer
  | Var Id
  | App Id Id
  | Arr [Id]
  | Lambd Id Id Ops Id Id Ops
  deriving (Show, Eq)

data Prog = Prog Id Opc
  deriving (Show, Eq)

unusedId :: Id
unusedId = Id "_" 0

instance Pretty Id where
  pPrintPrec _ _ i | i == unusedId = text "_"
  pPrintPrec _ _ (Id n k) = text $ n ++ "#" ++ show k

instance Pretty Opc where
  pPrintPrec l p (i :=: e) = maybeParens (p > 1) $ pPrintPrec l 1 i <+> text "=" <+> pPrintPrec l 1 e
  pPrintPrec l p (ChoiceOp op1 op2) =
    maybeParens (p > 1) $ sep [text "choice", indent $ pPrintPrec l 5 op1, indent $ pPrintPrec l 5 op2]
  pPrintPrec l _ (ScopeOp ops) = sep [text "scope", indent $ ppOps l ops]
  pPrintPrec l _ (ExistsOp is ops) = sep [text "exists" <+> sep (map (pPrintPrec l 0) is), indent $ ppOps l ops ]
  pPrintPrec l _ (VerifyOp e op) = text "verify<" <> text e <> text ">" <> ppOps l [op]

ppOps :: Pretty a => PrettyLevel -> [a] -> Doc
ppOps l ops = braces $ sep (punctuate (text ";") (map (pPrintPrec l 0) ops))

instance Pretty Exp where
  pPrintPrec l p (Int i) = pPrintPrec l p i
  pPrintPrec l p (Var i) = pPrintPrec l p i
  pPrintPrec l _ (App f a) = pPrintPrec l 0 f <> brackets (pPrintPrec l 0 a)
  pPrintPrec l _ (Arr [x]) = text "array" <> braces (pPrintPrec l 0 x)
  pPrintPrec l _ (Arr xs) = parens $ fsep $ punctuate (text ",") $ map (pPrintPrec l 0) xs
  pPrintPrec l p (Lambd x y d z w r) = maybeParens (p>0) $
    text "lambda" $$
    indent (text "domain" <+> pPrintPrec l 0 x <+> pPrintPrec l 0 y <> text "." <+> ppOps l d) $$
    indent (text "range " <+> pPrintPrec l 0 z <+> pPrintPrec l 0 w <> text "." <+> ppOps l r)

instance Pretty Prog where
  pPrintPrec l _ (Prog r o) = (pPrintPrec l 0 r <+> text "from") $$ pPrintPrec l 0 o

--------------------------------------------

data ScId = ScId Id | ScDerefId Id
  deriving (Show)

data Scope = Scope {
  bound :: M.Map Ident [ScId],
  fx :: OEff
  }
  deriving (Show)

data DState = DState {
  uniq :: !Int,
  env  :: [(Ident, ScId)]
  }
  deriving (Show)

type D = State DState

newId :: String -> D Id
newId n = do
  k <- gets uniq
  modify $ \ s -> s{ uniq = k+1 }
  pure $ Id n k

newIds :: [String] -> D [Id]
newIds = mapM newId

addEnv :: Ident -> ScId -> D ()
addEnv i si = modify $ \ s -> s{ env = (i, si) : env s }

prelude :: [String]
prelude =
     ["int", "any", "false"]
  ++ map toOperator binOps
  ++ map toPostfix ["^", "?"]
  ++ map toPrefix ["-","+","^","?","[]"]

binOps :: [String]
binOps = ["+","-","*", "/", "<", "<=", ">", ">="]

toOperator :: String -> String
toOperator op = "in'" ++ op ++ "'"

toPostfix :: String -> String
toPostfix op = "post'" ++ op ++ "'"

toPrefix :: String -> String
toPrefix op = "pre'" ++ op ++ "'"

findPrim :: String -> Scope -> Id
findPrim s sc =
  case M.lookup (Ident noLoc s) (bound sc) of
    Just [ScId i] -> i
    _ -> error $ "findPrim: " ++ s

dsProg :: Expr -> Prog
dsProg e = evalState ds DState{ uniq = 1, env = [] }
  where
    ds = do
      ~[i, x] <- newIds ["i", "res"]
      ps <- newIds prelude
      let bnd = M.fromList $ zipWith (\ s p -> (Ident noLoc s, [ScId p])) prelude ps
      op <- scope Scope{ bound = bnd, fx = "???" } i x e
      let p = Prog x $ ExistsOp [i] [op]
      pure $ seq (p==p) p   -- force evaluation for better error messages

dsExpr :: Scope -> Id -> Id -> Expr -> D Ops
dsExpr _ i x (LitInt k) = pure [ i :=: n, x :=: n ] where n = Int k
dsExpr _ _ _ (LitRat _ _) = notYet
dsExpr _ _ _ (LitChar _) = notYet
dsExpr _ _ _ (LitStr _) = notYet
dsExpr _ i x (Variable (Ident _ "_")) = pure [ i :=: Var x ]
dsExpr sc i x (Variable v) = do
  let (y, e) =
        case M.lookup v (bound sc) of
          Nothing -> error $ "N00 " ++ prettyShow v
          Just [ScId a] -> (a, Var a)
          Just [ScDerefId a] -> (a, App (findPrim (toPostfix "^") sc) a)
          Just _ -> error $ "N03 " ++ prettyShow v
  pure [ i :=: Var y, x :=: e ]
dsExpr _ _ _ (QualVariable _ _) = notYet
dsExpr sc i x (Array ss) = do
  ~[j, y] <- newIds ["j", "y"]
  js <- newIds ["j" ++ show k | k <- [1..length ss] ]
  ys <- newIds ["y" ++ show k | k <- [1..length ss] ]
  opss <- sequence $ zipWith3 (dsExpr sc) js ys ss
  pure [ExistsOp (j: y: js ++ ys) $ [j :=: Arr js, y :=: Arr ys, i :=: Var j, x :=: Var y] ++ concat opss]
dsExpr sc i x (ApplyS s1 s2) = apply sc i x (VerifyOp "succeeds") s1 s2
dsExpr sc i x (ApplyD s1 s2) = apply sc i x                    id s1 s2
dsExpr _ _ _ (ApplyEff _ _) = undefined
dsExpr _ _ _ (EffAttr _ _) = undefined
dsExpr sc i x (PrefixOp (Op ":") s0) = do
  ~[h, f] <- newIds ["h", "f"]
  ops0 <- dsExpr sc h f s0
  pure [ExistsOp [h, f] $ ops0 ++ [ x :=: App f i ]] -- XXX no effects?
dsExpr _ _ _ (PrefixOp _ _) = undefined
dsExpr _ _ _ (PostfixOp _ _) = undefined
dsExpr sc i x (InfixOp s0 (Op "=") s1) = (++) <$> dsExpr sc i x s0 <*> dsExpr sc i x s1
dsExpr sc i x (InfixOp s0 (Op "|") s1) = do
  op0 <- scope sc i x s0
  op1 <- scope sc i x s1
  pure [ChoiceOp op0 op1]
dsExpr sc i x (InfixOp as0 op@(Op ":=") as1) =
  case as0 of
    Variable v -> do addEnv v (ScId x); dsExpr sc i x as1
    EffAttr (Variable v) (Ef "var") -> do addEnv v (ScDerefId x); dsExpr sc i x as1
    EffAttr (Variable v) (Ef "ref") -> do addEnv v (ScDerefId x); dsExpr sc i x as1
    InfixOp s0 (Op ".") (Variable (Ident _ v)) ->
      dsExpr sc i x $ InfixOp (ApplyS (Variable (Ident noLoc ("operator'." ++ v ++ "'"))) s0) op as1
    ApplyS s0 s1 -> dsExpr sc i x $ InfixOp s0 op $ Function [(s1, [Ef "succeeds", Ef "transacts"])] as1
    -- XXX many missing
    _ -> error $ "Bad LHS of := " ++ prettyShow as0
dsExpr sc i x (InfixOp s0 (Op "where") s1) = do
  ~[j, y] <- newIds ["j", "y"]
  ops0 <- dsExpr sc i x s0
  ops1 <- dsExpr sc j y s1
  pure [ExistsOp [j, y] $ ops0 ++ ops1]
dsExpr sc i x (InfixOp s0 (Op ":") s1) = dsExpr sc i x (InfixOp s0 (Op ":=") (PrefixOp (Op ":") s1))
dsExpr sc i x (InfixOp s0 (Op op) s1) | op `elem` binOps =
  dsExpr sc i x (ApplyD (Variable f) (Array [s0, s1])) where f = Ident noLoc $ toOperator op
dsExpr sc i x (InfixOp s0 (Op "=>") s1) = dsExpr sc i x $ Function [(s0, [])] s1
{-
dsExpr _ _ _ (InfixOp _ _ _) = undefined
dsExpr _ _ _ (If1 _) = undefined
dsExpr _ _ _ (If2 _ _) = undefined
dsExpr _ _ _ (If2E _ _) = undefined
dsExpr _ _ _ (If3 _ _ _) = undefined
dsExpr _ _ _ (For1 _) = undefined
dsExpr _ _ _ (For2 _ _) = undefined
dsExpr _ _ _ (Let _ _) = undefined
dsExpr _ _ _ (Do _) = undefined
dsExpr _ _ _ (Case1 _) = undefined
dsExpr _ _ _ (Case2 _ _) = undefined
-}
dsExpr sc h f (Function [(s0, _fx)] s1) = do
  ~[x, y, w, q] <- newIds ["x", "y", "w", "q"]
  (sc0, op0) <- scope' sc x y s0
  op1 <- scope sc0 q w s1
  pure [f :=: Lambd x y [op0] y w [ExistsOp [q] $ [q :=: App h y] ++ [op1]]]
dsExpr _ _ _ e@(Function _ _) = error $ "Function " ++ show e
dsExpr sc i x (Block es) = dsExprs sc i x es
dsExpr sc i x (Seq es) = dsExprs sc i x es
--dsExpr _ _ _ (Option _) = undefined
dsExpr sc i x (Parens e) = dsExpr sc i x e
dsExpr sc i x (Typedef s0) = dsExpr sc i x $ Function [(InfixOp y (Op ":=") s0, [Ef "closed"])] y
  where y = Variable (Ident noLoc "y$")
{-
dsExpr _ _ _ (Set _ _ _) = undefined
dsExpr _ _ _ (MVar _ _ _) = undefined
dsExpr _ _ _ (MRef _ _ _) = undefined
dsExpr _ _ _ (MAlias _ _ _) = undefined
dsExpr _ _ _ (Macro1 _ _ _) = undefined
dsExpr _ _ _ (Macro2 _ _ _) = undefined
dsExpr _ _ _ (Return _) = undefined
-}
dsExpr _ _ _ e = error $ "dsExpr: unimplemented " ++ show e

dsExprs :: Scope -> Id -> Id -> [Expr] -> D Ops
dsExprs sc i x [] = dsExpr sc i x (Array [])
dsExprs sc i x [e] = dsExpr sc i x e
dsExprs sc i x (e:es) = do
  ~[ii, xx] <- newIds ["i", "x"]
  ops1 <- dsExpr sc ii xx e
  ops2 <- dsExprs sc i x es
  pure $ [ExistsOp [ii, xx] $ ops1 ++ ops2]


apply :: Scope -> Id -> Id -> (Opc -> Opc) -> Expr -> Expr -> D Ops
apply sc i x aeff s0 s1 = do
  ~[h, g, j, y, z] <- newIds ["h", "g", "j", "y", "z"]
  ops1 <- dsExpr sc h g s0
  ops2 <- dsExpr sc j y s1
  let op3 = ExistsOp [h,g,j,y,z] $ ops1 ++ ops2 ++ [aeff (z :=: App g y), i :=: Var z, x :=: Var z]
  pure [op3]
  
scope :: Scope -> Id -> Id -> Expr -> D Opc
scope sc i x e = snd <$> scope' sc i x e

scope' :: Scope -> Id -> Id -> Expr -> D (Scope, Opc)
scope' sc i x e = do
  u <- gets uniq
  let (ops, DState{ uniq = u', env = ies }) = runState (dsExpr sc' i x e) ds
      ies' = [(ii, [ee]) | (ii, ee) <- ies]
      ds = DState{ uniq = u, env = [] }
      bnd = if anySame (map fst ies) then error "N03" else foldr (uncurry M.insert) (bound sc) ies'
      sc' = sc{ bound = bnd }
  modify $ \ s -> s{ uniq = u' }
  pure (sc', ScopeOp ops)


simpProg :: Prog -> Prog
simpProg (Prog r o) = Prog r (simpOpc o)

simpOpc :: Opc -> Opc
simpOpc (i :=: e) = i :=: simpExp e
simpOpc (ChoiceOp op1 op2) = ChoiceOp (simpOpc op1) (simpOpc op2)
simpOpc (ScopeOp ops) = ScopeOp [simpExists [] ops]
simpOpc (ExistsOp is ops) = simpExists is ops
simpOpc (VerifyOp r op) = VerifyOp r (simpOpc op)

simpExists :: [Id] -> [Opc] -> Opc
simpExists is ops = removeUnusedExists (is ++ is') ops'
  where
    (is', ops') = findExists (map simpOpc ops)

findExists :: Ops -> ([Id], Ops)
findExists [] = ([], [])
findExists (ExistsOp vs os : ops) = (vs ++ vs', os ++ ops') where (vs', ops') = findExists ops
findExists (op : ops) = (vs, op : ops') where (vs, ops') = findExists ops

removeUnusedExists :: [Id] -> [Opc] -> Opc
removeUnusedExists is ops =
  let unused = singleOcc (getUsed ops) `intersect` is
      f (i :=: e) = (if i `notElem` unused then i else unusedId) :=: e
      f op = op
      ops' = filter (not . useless) $ map f ops
      used = getUsed ops'
      is' = filter (`elem` used) is
      ops'' = doInline is' ops'
  in
      (if False then trace ("***\n" ++ prettyShow (ExistsOp is ops, is, used)) else id) $
      if is == is' && ops == ops'' then
          ExistsOp is' ops''
      else
          removeUnusedExists is' ops''

useless :: Opc -> Bool
useless (i :=: e) = i == unusedId && isValue e
useless _ = False

isValue :: Exp -> Bool
isValue Int{} = True
isValue Arr{} = True
isValue Var{} = True
isValue _ = False

doInline :: [Id] -> Ops -> Ops
doInline is ops =
  case [ (i, i') | i :=: Var i' <- ops, i `elem` is ] of
    (i, i') : _ -> filter (not . isRefl) $ substId i i' ops
    _ -> ops

isRefl :: Opc -> Bool
isRefl (i :=: Var i') = i == i'
isRefl _ = False

simpExp :: Exp -> Exp
simpExp (Lambd i1 i2 d i3 i4 r) = Lambd i1 i2 (map simpOpc d) i3 i4 (map simpOpc r)
simpExp e = e

class GetUsed a where
  getUsed :: a -> [Id]

instance (GetUsed a) => GetUsed [a] where
  getUsed = concatMap getUsed

instance GetUsed Opc where
  getUsed (i :=: e) = i : getUsed e
  getUsed (ChoiceOp o1 o2) = getUsed o1 ++ getUsed o2
  getUsed (ScopeOp os) = getUsed os
  getUsed (ExistsOp is o) = filter (`notElem` is) $ getUsed o
  getUsed (VerifyOp _ o) = getUsed o

instance GetUsed Exp where
  getUsed Int{} = []
  getUsed (Var i) = [i]
  getUsed (App i1 i2) = [i1, i2]
  getUsed (Arr is) = is
  getUsed (Lambd i1 i2 d i3 i4 r) = filter (`notElem` [i1,i2]) (getUsed d) ++ filter (`notElem` [i3,i4]) (getUsed r)

singleOcc :: (Ord a) => [a] -> [a]
singleOcc = concat . filter ((== 1) . length) . group . sort

class SubstId a where
  substId :: Id -> Id -> a -> a

instance (SubstId a) => SubstId [a] where
  substId i i' = map (substId i i')

instance SubstId Id where
  substId i i' v | i == v = i'
                 | otherwise = v

instance SubstId Opc where
  substId i i' (v :=: e) = substId i i' v :=: substId i i' e
  substId i i' (ChoiceOp op1 op2) = ChoiceOp (substId i i' op1) (substId i i' op2)
  substId i i' (ScopeOp ops) = ScopeOp (substId i i' ops)
  substId i i' (ExistsOp is o) = ExistsOp is (substId i i' o)  -- all ids are unique
  substId i i' (VerifyOp r o) = VerifyOp r (substId i i' o) 

instance SubstId Exp where
  substId _ _ e@Int{} = e
  substId i i' (Var v) = Var (substId i i' v)
  substId i i' (App i1 i2) = App (substId i i' i1) (substId i i' i2)
  substId i i' (Arr is) = Arr (substId i i' is)
  substId i i' (Lambd i1 i2 d i3 i4 r) = Lambd i1 i2 (substId i i' d) i3 i4 (substId i i' r)

{-

Constant Reductions:
*    V(sc,i,x,Num)                  ---> i=Num; j=Num (for Num sans units)
                                               x=Num

Expression Reductions:
*    V(sc,i,x,_)                    ---> i=x
*    V(sc,i,x,Ident)                ---> i=y; x=ei; ... where ei is the ith expression successfully looked up by Ident
        Here, ei is either y or y^ for some variable y looked up successfully by y.
        While no lookups have succeeded, holds N00 if sc contains no V-terms else N01
        What are R-terms? Tim: I meant V-terms. Also, a list of N0x error would be useful.
        Tim: Updating the whole doc to include all verifier errors.
        While one lookup has succeeded, holds nothing if sc contains no R-terms else N02
        While multiple lookups have succeeded, holds N03
    V(sc,i,x,e0.Ident)             ---> exists j y. V(sc,j,y,e0); head N05 y {...}
                                        Ident unused,  i unused, x unused, what is y?
                                        Tim: We're evaluating e0 into y and then "..." needs to be expanded
                                        with cases to handle struct members, class members, union members.
*    V(sc,i,x,s0[s1])               ---> exists h g j y z. V(sc,h,g,s0); V(sc,j,y,s1); z=g[y]; i=z; x=z
                                        What does sc{...} mean? Tim: Deleted vestigal effects check.
                                        Now this desugaring just translates syntax into expressions,
                                        and another spec will say what we do with expressions.
                                        Beta reduction will occur entirely at the expression level, not here.
*    V(sc,i,x,s0(s1))               ---> exists h g j y z. V(sc,h,g,s0); V(sc,j,y,s1); verify<succeeds>{z=g[y]}; sc.fx{i=z}; sx.fx{x=z}
                                        Now verify<fx>{expr} is an expr that checks whether expr has at most
                                        the effects fx.
        !!same
*    V(sc,i,x,s0=s1)                ---> V(sc,i,x,s0); V(sc,i,x,s1)
*    V(sc,i,x,s0|s1)                ---> (scope sc1. V(sc1,i,x,s0)) | (scope sc2. V(sc2,i,x,s1))
                                        What happened to sc? Inside sc, "scope sc1. expr" creates a new scope
                                        sc1 that sees everything in sc, but puts new symbols in sc1 
                                        invisible to sc. Here, the "forking" from ICFP occurs at the expression
                                        level, and desugaring just decomposes the syntax and delegates forking
                                        to the expression it reduces to.
Some form of sequential composition would be useful, perhaps
*    V(sc,i,x,block{s0;s1;…;sn})    ---> exists i0 x0 … i1 x1 . V(sc,i0,x0,s0); V(sc,i1,x1,s1); … V(sx,i,x,sn)

Abstraction Reductions:
*    V(sc,i,x,:s0)                  ---> exists h f. V(sc,h,f,s0); x=f[i]
        !! sans abstraction
    V(sc,i,x,:s0<fx>=s1)           ---> exists h f. V(sc,h,f,s0); assume fx c j y.
                                           i=y; x=y; enter(c){y=f[j]}
                                           weaken(c){exists k z w. V(sc,k,z,s1); w=f[z]}
                                        Should the x be an h? Tim: I think this is right as-is.
                                        It says i=x=y and then assumes y=f[j].
                                        In other words, the relation between i and x is an identity relation.
        !! scope for s1
        !! sans abstraction
    V(sc,i,x,:s0=s1)               ---> V(sc,i,x,:s0<sc.fx>=s1)
        !! scope for s1

Definition Reductions:
*    V(sc,i,x,Ident0:=s0)           ---> V(sc,i,x,s0);     add unique Ident0=>x  to sc[0]; while multiple lookups have succeeded holds N04.
*    V(sc,i,x,Ident0<var>:=s0)      ---> V(sc,i,x,s0);     add unique Ident0=>x^ to sc[0]; while multiple lookups have succeeded holds N04.
*    V(sc,i,x,Ident0<ref>:=s0)      ---> V(sc,i,x,ref s0); add unique Ident0=>x^ to sc[0]; while multiple lookups have succeeded holds N04.
        !!break down specifiers and scope for specifiers
*    V(sc,i,x,s0.Ident:=s1)         ---> V(sc,i,x,operator'.Ident'(s0):=s1)
*    V(sc,i,x,s0(s1):=s2)           ---> V(sc,i,x,s0:=function(s1)<succeeds><transacts>{s2})
    V(sc,i,x,s0[s1]:=s2)           ---> ..
    V(sc,i,x,(s0,..):=en)          ---> V(sc,i,x,en); exists w0.. x0..:
                                        when i&x both have head [n]any then i=(w0,..); x=(x0,..)
    V(sc,i,x,s0&..:=sn)            ---> V(sc,i,x,s0:=expect<pure>{sn}); ..
    V(sc,i,x,s0->s1:...s2)         ---> exists j. V(sc,j,x,s1:=...s2[s0:=i])
        What is the ... ?
        Tim: "..." stands for zero or more ":".
        If you delete both ..., you get the simple rule for i->x:xs.
        But with the ..., we support i->j->x::xss.
        But error if there aren't an equal number of '->' and ':', to avoid input gap.
        In V(sc,i,x,s0->s1:...s2), i must bind to innermost xs to capture function input.
        !!sans abstraction
    V(sc,i,x,s0?:=s1)              ---> V(sc,i,x,s0:=option{s1})
    V(,sc,i,x,?s0:=s1)             ---> ..
    V(sc,i,x,s0^:s1)               ---> V(sc,i,x,s0:new(s1))
    V(sc,i,x,s0^:s1=s2)            ---> V(sc,i,x,s0:new(s1)=s2)
    V(sc,i,x,var s0:s1)            ---> V(sc,i,x,s0<var>:new(s1))
    V(sc,i,x,var s0:s1=s2)         ---> V(sc,i,x,s0<var>:new(s1)=s2) 
    V(sc,i,x,ref s0)               ---> V(sc,i,x,ref{s0})
    V(sc,i,x,ref s0:s1)            ---> V(sc,i,x,s0<var>:^s1)
    V(sc,i,x,ref s0:s1=s2)         ---> V(sc,i,x,s0<var>:^s1=s2)
    V(sc,i,x,alias s0:=s1)         ---> V(sc,i,x,s0<var>:=s1 ref)
                                        What is s2?  Tim: Sorry, should be s1.
                                        What does 's2 ref' mean? Tim: It's just syntax like s1& in C.
                                        It means: require s1 is an l-expression and give me its pointer.
                                        This stuff is all about desugaring var|ref|alias into normal IORef stuff.
    V(sc,i,x,alias s0)             ---> V(sc,i,x,s0<alias>)
    
    PRINCIPLE: Definition left-hand-side topmost expression determines whether it captures or propagates failure.
        So (a,b)?:int captures failure, (a?,b?) propagates failure

Do we have a case for "f(a:A):B := e" or "x:t := e"?
Tim: All functions are handled by the "s0(s1)<fx>:=s2" rule below, which recursively reduces any "a:A" nested inside.
Tim: x:t:=e, equivalent to x:t=e, is handled in the "Abstraction Reductions" section above.

Definition Specifier Reductions:
    V(sc,i,x,(s0,..)<spec>:=s1)    ---> V(s0<spec>,..:=s1)
    V(sc,i,x,(s0&..)<spec>:=s1)    ---> V(s0<spec>&..:=s1)
    V(sc,i,x,(s0->s1)<spec>:...s2) ---> V(sc,i,x,s0->s1<spec>:...s2)
    V(sc,i,x,s0(s1)<fx>:=s2)       ---> V(sc,i,x,s0:=function(s1)<fx>{s2})
    V(sc,i,x,s0[s1]<fx>:=s2)       ---> ...
        OLD: Sans <fx> is complete map or array type or value; with <decides> is sparse finite map type.

Intrinsic Macro Reductions:
*    V(sc,i,x,array{s0,..})         ---> exists j y j0.. y0.. . j=<j0,..>; y=<y0,..>; i=j; x=y; V(sc,j0,y0,s0); ...
    V(sc,i,x,let(s0){s1}           ---> scope sc1. exists j y. V(sc,j,y,s0); scope sc2. V(sc,i,x,s1)
*    V(sc,i,x,s0 where s1           ---> exists j y. V(sc,i,x,s0); V(sc,j,y,s1)
    V(sc,i,x,if(s0){s1}else{e2})   ---> iterate sc1 y.
                                           domain   {exists j z. V(sc1,j,y,s0)}
                                           Tim: I made a mess of this.
                                           What we mean is: in the nested scope, we allow whichever
                                           <transacts> effects are allowed in the outside, plus we 
                                           allow <iterates> effects.
                                           succeeds {V(sc,i,x,s1)}
                                           fails    {V(sc,i,x,s2)}
    V(sc,i,x,if(s0){s1})           ---> V(sc,i,x,if(s0){s1}else{})
        !!scope for s0&s1
    V(sc,i,x,if{s0})               ---> V(sc,i,x,if(exists y. y=s0){y}else{})
                                        s1 should be y? Yes.
    V(sc,i,x,find(s0){s1})         ---> ...
        !!scope for s0&s1
    V(sc,i,x,find{s0})             ---> V(sc,i,x,find(exists y. y=s0){y}else{})
    V(sc,i,x,for(s0){s1})          ---> i=<..>; j=<..>;
                                       iterate c i 0
                                           domain.     exists j z. V(sc.fx\/transacts|iterates,j,y,s0)
                                           succeeds k. exists j y. j=i[k]; y=x[k]; V(sc,j,y,s1); next c (k+1)}
                                           fails    k. exists i1..ik. i=<i0,..,ik>; exists x1..xk. x=<x1,..,xk>}
        !!scope for s0&s1
    V(sc,i,x,for{s0})              ---> V(sc,i,x,for(exists y. y=s0){y}else{})
                                        Remove else 
    V(sc,h,f,function(s0)<fx>{s1}) ---> f=lambda
                                           domain x y. V(sc+{sc1},x,y,s0)
                                           range  y w. exists q. q=h[y]; V(sc+{sc1,sc2},q,w,s1)
                                        Will fix. We mean to say: the domain has a new scope,
                                        and the range has a new scope seeing the domain scope.
        !!specifiers and scope for specifiers
*    V(sc,i,x,operator'=>'(s0){s1}) ---> V(sc,i,x,function(s0){s1})
    V(sc,i,x,s0?}                  ---> exists j y. V(sc,j,y,s0); s0[_]
    V(sc,i,x,s0<>s1)               ---> V(sc,i,x,s0 where for(y:=s1). not x=y)
*    V(sc,i,x,type{s0})             ---> V(sc,i,x,function(y:=s0)<closed>. y)

Macro Reductions For Testing
    V(sc,i,x,assume<fx>{s0})       ---> assume fx c j y. sc.fx{i=j}; sx.fx{x=y}; enter(c){V(sc.fx?!!,j,y,s0)}
    V(sc,i,x,assume{s0})           ---> scope sc1. V(sc1,i,x,assume<succeeds>{s0})
        !!needs work
    V(sc,i,x,test(err){s0})        ---> scope sc1. ...
    V(sc,i,x,reject{expr})         ---> scope sc1. ...
    TODO: verify, allow, expect
    TODO: case,function,not,type,assert,operator'or',operator'and'

Syntactic Equivalences:
    Num Units     <--> units'Units'[Num]
    s0(s1). e2    <--> s0(s1){e2} <--> s0(s1) do e2
    s0:s1         <--> s0:=:s1
    s0:s1=s2      <--> s0:=(:s1:=s2)
    s0:s1<fx>=s2  <--> s0:=(:s1<fx>:=s2)
    s0=>s1        <--> function(s0){s1}
    s0^           <--> operator'^'[s0]     # Same for ^ ?
    s0&s1         <--> operator'&'{s0,s1}  # Same for & -> to | <> = and or += -= *= /=
    s0+s1         <--> operator'*'[s0,s1]  # Same for * / + - >= > <= < .. 
    :s0           <--> prefix'-'{s0}       # Same for : not
    -s0           <--> prefix'-'[s0]       # Same for ^ ? + - * []
    -{s0}         <--> ...                 # Same for ^ ? + - * []
    [s0]s1        <--> operator'[]'[s0,s1]
    [s0]{s1}      <--> ...
    s0=>cl0       <--> operator'=>'(s0){cl0}
    s0 where cl0  <--> operator'where'(s0){cl0}
    s0            <--> (s0)                # Commas and semicolons are in clauses, not syntax.

-}
