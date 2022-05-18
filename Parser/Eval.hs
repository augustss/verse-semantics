-- XXX
-- reduction rule bugs
--  X lacks succeeds

{-# LANGUAGE PatternSynonyms #-}
module Eval(eval) where
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Debug.Trace

import Expr(Ident(..), noLoc)
import Core
import Error
import Print hiding (float)

pattern VArray :: [Value] -> Value
pattern VArray vs = HNF (HArray vs)

pattern CArray :: [Value] -> Core
pattern CArray vs = CValue (VArray vs)

pattern CUnOp :: String -> Value -> Core
pattern CUnOp op v <- CApply (CPrim op) (CValue v@HNF{})

pattern CBinOp :: String -> Value -> Value -> Core
pattern CBinOp op v1 v2 <- CApply (CPrim op) (CArray [v1@HNF{}, v2@HNF{}])

pattern VLam :: Ident -> Core -> Value
pattern VLam i e = HNF (HLam i e)

pattern CLam :: Ident -> Core -> Core
pattern CLam i e = CValue (VLam i e)

pattern VRec :: Ident -> Core -> Value
pattern VRec i e = HNF (HRec i e)

pattern CType :: Value -> Core
pattern CType e = CValue (HNF (HType e))

pattern VInt :: Integer -> Value
pattern VInt i = HNF (HInt i)

pattern CInt :: Integer -> Core
pattern CInt i = CValue (VInt i)

cSeq :: [Core] -> Core
cSeq [] = internalError
cSeq [e] = e
cSeq es = CSeq es

cDef :: [Ident] -> Core -> Core
cDef [] e = e
cDef is e = CDef is e

isValue :: Core -> Bool
isValue CValue{} = True
isValue _ = False

isVar :: Core -> Bool
isVar CVar{} = True
isVar _ = False

{-
isTy :: Core -> Bool
isTy (CValue (HNF (HPrim t))) = t `elem` ["int", "nat", "any", "false"]
isTy (CValue (HNF (HType _))) = True
isTy _ = False
-}

cWrongs :: [String] -> Core
cWrongs [] = internalError
cWrongs ws = CWrong $ intercalate ";" ws

getWrong :: Core -> Maybe String
getWrong (CWrong s) = Just s
getWrong _ = Nothing

getValue :: Core -> Maybe Core
getValue e@CValue{} = Just e
getValue _ = Nothing

-------------

doTrace :: Bool
doTrace = True

evalTrace :: String -> (Core -> Core) -> Core -> Core
evalTrace s f e | not doTrace = e
                | otherwise =
  let e' = f e
  in  if e == e' then
        e -- trace s e
      else
        trace (s ++ ":\n" ++ prettyShow e') e'

-------------

-- Reduce until we reach HNF
eval :: Core -> Core
eval ea = loop 50 $ replacePrelude $ evalTrace "eval" (const ea) (CWrong"")
  where
    loop :: Int -> Core -> Core
    loop 0 e = e
    loop n e | isHNF e = e
             | otherwise = loop (n-1) $ evalSteps e

isHNF :: Core -> Bool
isHNF (CValue (HNF _)) = True
isHNF CWrong{} = True  -- Not really a HNF, but cannot reduce
isHNF _ = False

isX :: Core -> Bool
isX CUnify{} = True
isX CApply{} = True
isX CSeq{} = True
isX _ = False

-- Unhandled rules:
--  CHOOSE
--  

-- Take some reduction steps.
evalSteps :: Core -> Core
evalSteps =
  evalDefFloat . evalAll .
  evalWrong . evalFail . evalUnify . evalBind . evalDef .
  evalOne . evalApp . evalSeq . evalBar . evalSucceeds . evalPrimOps

-- Handle CDef floating
--  DEF-FLOAT
evalDefFloat :: Core -> Core
evalDefFloat = evalTrace "evalDefFloat" f
  where
    f e | isX e = float e
    f e = composOp f e

    float e =
      case runWriter (g e) of
        (e', hs) -> cDef (concat hs) e'

    -- Follows the X context
    g e | isX e = compos g e
    g (CDef h e) = do tell [h]; pure e  -- XXX maybe use f in e
    g e = pure e   -- XXX maybe use f in e

-- Handle CWrong propagation
--  WRONG
evalWrong :: Core -> Core
evalWrong = evalTrace "evalWrong" f
  where
    f e | ws@(_:_) <- getWrongs e = cWrongs ws
    f e = composOp f e

    -- Follows the X context
    getWrongs (CWrong s) = [s]
    getWrongs (CUnify e1 e2) = getWrongs e1 ++ getWrongs e2
    getWrongs (CSeq es) = concatMap getWrongs es
    getWrongs (CApply e1 e2) = getWrongs e1 ++ getWrongs e2
    getWrongs _ = []

-- Handle CFail propagation
--  FAIL
evalFail :: Core -> Core
evalFail = evalTrace "evalFail" f
  where
    f e | hasFail e = CFail
    f e = composOp f e

    -- Follows the X context
    hasFail CFail = True
    hasFail (CUnify e1 e2) = hasFail e1 || hasFail e2
    hasFail (CSeq es) = any hasFail es
    hasFail (CApply e1 e2) = hasFail e1 || hasFail e2
    hasFail _ = False

-- Handle unification
--  SWAP, UTYPE, UCON, UARR, UX*
evalUnify :: Core -> Core
evalUnify = evalTrace "evalUnify" f
  where
    f (CUnify e x@CVar{}) | not (isVar e) = f $ CUnify x e
    f (CUnify (CValue v1@HNF{}) (CValue v2@HNF{})) = unifyV v1 v2
    f e = composOp f e

    unifyV v@(VInt i1) (VInt i2) | i1 == i2 = CValue v
                               | otherwise = CFail
    unifyV VInt{} _ = CFail
    unifyV _ VInt{} = CFail

    unifyV v@(VArray vs1) (VArray vs2) | length vs1 == length vs2 =
                                           cSeq $ zipWith unifyV vs1 vs2 ++ [CValue v]
                                       | otherwise = CFail
    unifyV VArray{} _ = CFail
    unifyV _ VArray{} = CFail

    unifyV _ _ = CWrong "unifyV"


-- Handle BIND rule
evalBind :: Core -> Core
evalBind = evalTrace "evalBind" f
  where
    f (CDef h e) | b@(x,_) : _ <- coll h e =
      let e' = dropB b e
      in  cDef (h \\ [x]) $ subst b e'
    f e = composOp f e

    coll h (CUnify (CVar x) (CValue v)) | elem x h = [(x, v)]
    coll h (CUnify e1 e2) = coll h e1 ++ coll h e2
    coll h (CApply e1 e2) = coll h e1 ++ coll h e2
    coll h (CSeq es) = concatMap (coll h) es
    coll h (CSucceeds e) = coll h e
    coll _ _ = []

    dropB b (CUnify (CVar x) e@(CValue v)) | (x, v) == b = e
    dropB b (CUnify e1 e2) = CUnify (dropB b e1) (dropB b e2)
    dropB b (CApply e1 e2) = CApply (dropB b e1) (dropB b e2)
    dropB b (CSeq es) = CSeq $ map (dropB b) es
    dropB b (CSucceeds e) = CSucceeds $ dropB b e
    dropB _ e = e


-- Handle simple 'def'
--  FAIL-DEF, DEF-ELIM, DEF-WRONG, DEF-MERGE
evalDef :: Core -> Core
evalDef = evalTrace "evalDef" f
  where
    f (CDef _ CFail) = CFail
    f (CDef [] e) = f e
    f (CDef (_:_) CValue{}) = CWrong "def-wrong"
    f (CDef h1 (CDef h2 e)) =
      assert (null (intersect h1 h2)) ("DEF-MERGE: " ++ show (h1,h2)) $
      f $ CDef (h1 ++ h2) e
    f e = composOp f e

-- Handle 'all'
--  ALL-0, ALL-N, ALL-WRONG
evalAll :: Core -> Core
evalAll = evalTrace "evalAll" f
  where
    f (CAll e@CValue{}) = mkArr [e]
    f (CAll e@CWrong{}) = e
    f (CAll (CBar es)) | ws@(_:_) <- mapMaybe getWrong es = cWrongs ws
                       | Just vs  <- traverse getValue es = mkArr vs
    f e = composOp f e

    mkArr :: [Core] -> Core
    mkArr es = undefined

-- Handle 'one'
--  ONE-VALUE, ONE-CHOICE, ONE-FAIL, ONE-WRONG
evalOne :: Core -> Core
evalOne = evalTrace "evalOne" f
  where
    f (COne e@CValue{}) = f e
    f (COne CFail) = CFail
    f (COne (CBar (e@CValue{} : _))) = f e
    f (COne e@CWrong{}) = e
    f e = composOp f e

-- Handle non-primop applications
--  APP-LAM, APP-TYPE, APP-REC, APP-ARR
--  APP-CONST-WRONG
evalApp :: Core -> Core
evalApp = evalTrace "evalApp" f
  where
    f (CApply (CLam i e1) e2) = f $ CDef [i'] $ CSeq [CUnify (CVar i') e2, e1']
      where (i', e1') | i `elem` fvs e2 = unimplemented
                      | otherwise = (i, e1)
    f (CApply (CValue v@(VRec i e1)) e2) = CApply e1' e2
      where e1' = subst (i, v) e1
    f (CApply (CArray vs) e@(CValue _)) = CBar $ zipWith g [0..] vs
      where g i v = CSeq [CUnify e (CInt i), CValue v]
    f (CApply (CType v1) e2) = f $ CApply (CValue v1) e2
    f (CApply CInt{} _) = CWrong "APP-CONST-WRONG"
    f e = composOp f e

-- Handle CSeq in odd places
--  SEQ, APP-SEQL, APP-SEQR, UNIFY-SEQL, UNIFY-SEQR
evalSeq :: Core -> Core
evalSeq = evalTrace "evalSeq" f
  where
    f (CSeq es) = cSeq $ filter (not . isValue) (init es') ++ [last es']
      where es' = concatMap (flat . f) es
    f (CApply (CSeq es) e2) = CSeq $ init es ++ [CApply (last es) e2]
    f (CUnify (CSeq es) e2) = CSeq $ init es ++ [CUnify (last es) e2]
    f (CApply e1@CValue{} (CSeq es)) = CSeq $ init es ++ [CApply e1 (last es)]
    f (CUnify e1@CValue{} (CSeq es)) = CSeq $ init es ++ [CUnify e1 (last es)]
    f e = composOp f e
    flat (CSeq es) = es
    flat e = [e]

-- Handle CBar associativity and fail elimination
-- FAIL-L, FAIL-R, ASSOC-CHOICE
evalBar :: Core -> Core
evalBar = evalTrace "evalBar" f
  where
    f (CBar es) = CBar $ concatMap (flat . f) es
    f e = composOp f e
    flat (CBar es) = es
    flat e = [e]

-- succeeds{v}  -->  v
-- SUCCEEDS-VALUE, SUCCEEDS-FAIL, SUCCEEDS-CHOICE, SUCCEEDS-WRONG
evalSucceeds :: Core -> Core
evalSucceeds = evalTrace "evalSucceeds" f
  where
    f (CSucceeds e@CValue{}) = f e
    f (CSucceeds CFail) = CWrong "succeeds-fail"
    f (CSucceeds (CBar (CValue{} : _))) = CWrong "succeeds-many"
    f (CSucceeds e@CWrong{}) = e
    f e = composOp f e

-- Reduce applications of primops
-- P-* rules
evalPrimOps :: Core -> Core
evalPrimOps = evalTrace "evalPrimOps" f
  where
    --- any, nat, false need not be primitives
    f (CUnOp  "any" v) = CValue v
    f (CUnOp  "nat" v) | VInt i <- v, i >= 0 = CValue v
                       | otherwise   = CFail
    f (CUnOp  "false" _) = CFail
    -- real primitives
    f (CUnOp  "int#" v) | VInt _ <- v = CValue v
                        | otherwise   = CFail
    -- float#, string#
    f (CUnOp  "pre'-'" (VInt i)) = CInt $ -i
    f (CBinOp "in'+'"  v1 v2) = arith (+) v1 v2
    f (CBinOp "in'-'"  v1 v2) = arith (-) v1 v2
    f (CBinOp "in'*'"  v1 v2) = arith (*) v1 v2
    f (CBinOp "in'/'"  (VInt i1) (VInt i2)) | i2 == 0 = CFail
                                            | otherwise = CInt $ i1 - i2
    f (CBinOp "in'<'"   v1 v2) = cmp (<)  v1 v2
    f (CBinOp "in'<='"  v1 v2) = cmp (<=) v1 v2
    f (CBinOp "in'>'"   v1 v2) = cmp (>)  v1 v2
    f (CBinOp "in'>='"  v1 v2) = cmp (>=) v1 v2
    f e = composOp f e

    arith op (VInt i1) (VInt i2) = CInt $ op i1 i2
    arith _ _ _ = CFail  -- CWrong?
    
    cmp op (VInt i1) (VInt i2) | op i1 i2 = CInt i1
                               | otherwise = CFail
    cmp _ _ _ = CFail   -- CWrong?

{-
getInverse :: Core -> Maybe Core
getInverse e@(CPrim s) | s `elem` ["int", "nat", "any", "false"] = Just e
getInverse e@(CType _) = Just e
getInverse _ = Nothing
-}

-------------------

-- Until we get a proper prelude, just hack it.
replacePrelude :: Core -> Core
replacePrelude = evalTrace "replacePrelude" f
  where
    f (CVar (Ident _ i)) | Just e <- lookup i prelude = e
    f e = composOp f e

-- Functions that should be in a prelude.
prelude :: [(String, Core)]
prelude =
  [("any", typ [])                                         -- x => x
  ,("nat", typ [app "int#" vx, app2 "in'>='" vx (VInt 0)]) -- x => int#[x]; x>=0; x
  ,("int", typ [app "int#" vx])                            -- x => int#[x]; x
  ,("false", CArray [])                                    -- ()
  ,("float", undefined)
  ,("string", undefined)
  ]
  where typ s = CType $ VLam ix $ cSeq $ s ++ [x]
        ix = Ident noLoc "q"
        x = CVar ix
        vx = Var ix
        app f v = CApply (CPrim f) (CValue v)
        app2 f v1 v2 = CApply (CPrim f) (CArray [v1, v2])
