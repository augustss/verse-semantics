{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- XXX
-- reduction rule bugs
--  X lacks succeeds

-- TODO:
--  Broken tests
--    succ(n : int) := n + 1; f(m : succ) := m; f(5)
--      FIX: deugaring bug

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
module Eval(eval) where
import Control.Monad.State.Strict
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
pattern CUnOp op v <- CApply (VPrim op) v@HNF{}

pattern CBinOp :: String -> Value -> Value -> Core
pattern CBinOp op v1 v2 <- CApply (VPrim op) (VArray [v1@HNF{}, v2@HNF{}])

pattern VLam :: Ident -> Core -> Value
pattern VLam i e = HNF (HLam i e)

pattern VType :: Value -> Value
pattern VType e = HNF (HType e)

pattern VInt :: Integer -> Value
pattern VInt i = HNF (HInt i)

pattern CInt :: Integer -> Core
pattern CInt i = CValue (VInt i)

pattern CDecides :: Core -> Core
pattern CDecides c <- CMacro (Ident _ "decides") c
  where CDecides e = CMacro (Ident noLoc "decides") e

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

cWrongs :: [String] -> Core
cWrongs [] = internalError
cWrongs ws = CWrong $ intercalate ";" ws

getWrong :: Core -> Maybe String
getWrong (CWrong s) = Just s
getWrong _ = Nothing

getValue :: Core -> Maybe Value
getValue (CValue v) = Just v
getValue _ = Nothing

-- Infinite list of variables not in vs
newVars :: String -> [Ident] -> [Ident]
newVars s vs = [ Ident noLoc $ "$" ++ s ++ show n | n <- [0::Int ..] ] \\ vs

-------------

type EvalCore = Bool -> Core -> Core

evalTrace :: String -> (Core -> Core) -> EvalCore
evalTrace s f trc e | not trc = e'
                    | e == e'     = e'
                    | otherwise   = trace (s ++ ":\n" ++ prettyShow e') e'
  where e' = f e

-------------

-- Reduce until we reach HNF
eval :: EvalCore
eval trc ea = loop 50 $ replacePrelude trc $ evalTrace "eval" (const ea) trc (CWrong"")
  where
    loop :: Int -> Core -> Core
    loop 0 e = e
    loop n e | isIrred e = e
             | otherwise = loop (n-1) $ evalSteps trc e

-- Irreducible term
isIrred :: Core -> Bool
isIrred (CValue (HNF _)) = True
isIrred CWrong{} = True  -- Not really a HNF, but cannot reduce
isIrred _ = False

isX :: Core -> Bool
isX CUnify{} = True
isX CApply{} = True
isX CSeq{} = True
isX _ = False

-- Take some reduction steps.
evalSteps :: EvalCore
evalSteps t =
  evalDefFloat t . evalAll t . evalChoice t .
  evalWrong t . evalFail t . evalUnify t . evalBind t . evalDef t .
  evalOne t . evalApp t . evalSeq t . evalBar t . evalSucceeds t . evalPrimOps t

-- Handle CBar
--  CHOOSE
-- First locate an anchor point and then try to find CX hole with a CBar.
-- XXX What are the anchor points?
evalChoice :: EvalCore
evalChoice = evalTrace "evalChoice" t
  where
    -- Top-level anchor point
    t (CBar [e]) = e
    t e = choice e

    -- Find more anchor points
    f (COne e) = COne $ choice e
    f (CAll e) = CAll $ choice e
    f (CSucceeds e) = CSucceeds $ choice e
    f (CDecides e) = CDecides $ choice e
    f e = composOp f e

    choice (CBar es) = CBar $ map choice es  -- look for nested choices
    choice e =
      case runState (findC e) Nothing of
        (_, Nothing) -> f e  -- no choice found, look deeper down
        (e', Just es) -> CBar $ map e' es
    -- Find the leftmost choice.
    -- Return a function representing the CX context.
    findC e = do
      me <- get
      if isJust me then
        pure $ const e  -- Already found, just keep going
       else
        case e of
          CUnify e1 e2 -> do
            e1' <- findC e1
            e2' <- findC e2
            pure $ \ x -> CUnify (e1' x) (e2' x)
          CSeq es -> do
            es' <- mapM findC es
            pure $ \ x -> CSeq (map ($ x) es')
          CDef h b -> do
            b' <- findC b
            pure $ \ x -> CDef h (b' x)
          CBar es -> do
            put $ Just es
            pure id
          _ -> pure $ const e

-- Handle CDef floating
--  DEF-FLOAT
evalDefFloat :: EvalCore
evalDefFloat = evalTrace "evalDefFloat" f
  where
    f e | isX e = float e
    f e = composOp f e

    float e =
      case runState (findD e) Nothing of
        (_, Nothing) -> e
        (e', Just d) ->
          case alphaConvert (fvs (e' (CArray []))) d of
            CDef h b -> CDef h (e' b)
            x -> impossible x

    findD e = do
      me <- get
      if isJust me then
        pure $ const e  -- Already found, just keep going
       else
        case e of
          CUnify e1 e2 -> do
            e1' <- findD e1
            e2' <- findD e2
            pure $ \ x -> CUnify (e1' x) (e2' x)
          CSeq es -> do
            es' <- mapM findD es
            pure $ \ x -> CSeq (map ($ x) es')
          CDef _ _ -> do
            put $ Just e
            pure id
          _ -> pure $ const e


-- Handle CWrong propagation
--  WRONG
evalWrong :: EvalCore
evalWrong = evalTrace "evalWrong" f
  where
    f e | ws@(_:_) <- getWrongs e = cWrongs ws
    f e = composOp f e

    getWrongs (CWrong s) = [s]
    getWrongs (CUnify e1 e2) = getWrongs e1 ++ getWrongs e2
    getWrongs (CSeq es) = concatMap getWrongs es
    getWrongs (CDef _ e) = getWrongs e
    getWrongs (CSucceeds e) = getWrongs e
    getWrongs (CDecides e) = getWrongs e
    getWrongs _ = []

-- Handle CFail propagation
--  FAIL-*
evalFail :: EvalCore
evalFail = evalTrace "evalFail" f
  where
    f e | hasFail e = CFail
    f e = composOp f e

    -- Follows the X context
    hasFail CFail = True
    hasFail (CUnify e1 e2) = hasFail e1 || hasFail e2
    hasFail (CSeq es) = any hasFail es
    hasFail (CDef _ e) = hasFail e
    hasFail _ = False

-- Handle unification
--  SWAP, UTYPE, UCON, UARR, UREC, UX*
evalUnify :: EvalCore
evalUnify = evalTrace "evalUnify" f
  where
{-
    f ue@(CUnify ex@(CVar x) (CLam y e)) | x `elem` fvs e =
      trace ("UREC " ++ prettyShow (x, y, fvs e, e)) $
      CUnify ex $ CLam y $ CDef [x] $ CSeq [ue, e]
-}
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


--CWrong ("occurs check: " ++ prettyShow (x, bs))

-- Handle BIND rules
evalBind :: EvalCore
evalBind = evalTrace "evalBind" f
  where
    f (CDef h e) | Just d <- bind h e = d
    f e = composOp f e

    bind h e =
      case runState (findB h e) Nothing of
        (e', Just (x, v))
            | Just v' <- occursCheck x v -> Just $ cDef (h \\ [x]) (subst x v' e')
            | otherwise                  -> Just $ CWrong $ "occurs check " ++ prettyShow x
        _ -> Nothing

    -- Combines occurs check and UREC
    occursCheck :: Ident -> Value -> Maybe Value
    occursCheck x v0 = go v0
      where
        go :: Value -> Maybe Value
        go v@(Var y)
            | x == y    = Nothing -- occurs check
            | otherwise = Just v
        go v@(VInt {})       = Just v
        go v@(VLam _ b)
            | x `elem` fvs b =
              let VLam y' b' = alphaConvertV [x] v  -- Make sure y doesn't clash with x
              in  Just $ VLam y' $ CDef [x] $ CSeq [CUnify (CVar x) (CValue v0), b']
            | otherwise      = Just v
        go (VArray vs)       = VArray <$> mapM go vs
        go _ = internalError

    -- Find the leftmost BIND.
    -- Return a function representing the CX context.
    findB h e = do
      me <- get
      if isJust me then
        pure e  -- Already found, just keep going
       else
        case e of
          CUnify ex@(CVar x) ev@(CValue v) | elem x h, ex /= ev -> do
            put $ Just (x, v)
            pure ev
          CUnify e1 e2 -> CUnify <$> findB h e1 <*> findB h e2
          CSeq es -> CSeq <$> mapM (findB h) es
          CSucceeds b -> CSucceeds <$> findB h b
          _ -> pure e


-- Handle simple 'def'
--  FAIL-DEF, DEF-ELIM, DEF-WRONG, DEF-MERGE
evalDef :: EvalCore
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
evalAll :: EvalCore
evalAll = evalTrace "evalAll" f
  where
    f (CAll (CValue v)) = mkArr [v]
    f (CAll e@CWrong{}) = e
    f (CAll (CBar es)) | ws@(_:_) <- mapMaybe getWrong es = cWrongs ws
                       | Just vs  <- traverse getValue es = mkArr vs
    f e = composOp f e

    mkArr :: [Value] -> Core
    mkArr vs =
      let xs = take (length vs) $ newVars "x" $ fvsV (VArray vs)
          unit = VArray []
      in  CDef xs $ cSeq $ zipWith (\ x v -> CUnify (CVar x) $ CApply v unit) xs vs ++ [CArray $ map Var xs]

-- Handle 'one'
--  ONE-VALUE, ONE-CHOICE, ONE-FAIL, ONE-WRONG
evalOne :: EvalCore
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
evalApp :: EvalCore
evalApp = evalTrace "evalApp" f
  where
    f (CApply (VLam i e1) v2) = f $ subst i' v2 e1'
      where (i', e1') | i `elem` vs = (x, subst i (Var x) e1)
                      | otherwise = (i, e1)
            vs = fvsV v2
            x = head $ newVars "i" vs
    f (CApply (VArray vs) vi) = CBar $ zipWith g [0..] vs
      where g i v = CSeq [CUnify ei (CInt i), CValue v]
            ei = CValue vi
    f (CApply (VType v1) e2) = f $ CApply v1 e2
    f (CApply VInt{} _) = CWrong "APP-CONST-WRONG"
    f e = composOp f e

-- Handle CSeq in odd places
--  SEQ, APP-SEQL, APP-SEQR, UNIFY-SEQL, UNIFY-SEQR
evalSeq :: EvalCore
evalSeq = evalTrace "evalSeq" f
  where
    f (CSeq es) = cSeq $ filter (not . isValue) (init es') ++ [last es']
      where es' = concatMap (flat . f) es
    f (CUnify (CSeq es) e2) = CSeq $ init es ++ [CUnify (last es) e2]
    f (CUnify e1@CValue{} (CSeq es)) = CSeq $ init es ++ [CUnify e1 (last es)]
    f e = composOp f e
    flat (CSeq es) = es
    flat e = [e]

-- Handle CBar associativity and fail elimination
-- FAIL-L, FAIL-R, ASSOC-CHOICE
evalBar :: EvalCore
evalBar = evalTrace "evalBar" f
  where
    f (CBar es) = CBar $ concatMap (flat . f) es
    f e = composOp f e
    flat (CBar es) = es
    flat e = [e]

-- succeeds{v}  -->  v
-- SUCCEEDS-VALUE, SUCCEEDS-FAIL, SUCCEEDS-CHOICE, SUCCEEDS-WRONG
-- DECIDES-*
evalSucceeds :: EvalCore
evalSucceeds = evalTrace "evalSucceeds" f
  where
    f (CSucceeds e@CValue{}) = f e
    f (CSucceeds CFail) = CWrong "succeeds-fail"
    f (CSucceeds (CBar (CValue{} : _))) = CWrong "succeeds-many"
    f (CSucceeds e@CWrong{}) = e
    f (CDecides e@CValue{}) = f e
    f (CDecides CFail) = CFail
    f (CDecides (CBar (CValue{} : _))) = CWrong "decides-many"
    f (CDecides e@CWrong{}) = e
    f e = composOp f e

-- Reduce applications of primops
-- P-* rules
evalPrimOps :: EvalCore
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

-------------------

-- Until we get a proper prelude, just hack it.
-- XXX This doesn't really work, it only replaces in applications.
-- Could just put the prelude as a prefix to the program.
replacePrelude :: EvalCore
replacePrelude = evalTrace "replacePrelude" f
  where
    f (CApply (Var (Ident _ i)) v) | Just p <- lookup i prelude = CApply p v
    f e = composOp f e

-- Functions that should be in a prelude.
prelude :: [(String, Value)]
prelude =
  [("any", typ [])                                         -- x => x
  ,("nat", typ [app "int#" vx, app2 "in'>='" vx (VInt 0)]) -- x => int#[x]; x>=0; x
  ,("int", typ [app "int#" vx])                            -- x => int#[x]; x
  ,("false", VArray [])                                    -- ()
  ,("float", undefined)
  ,("string", undefined)
  ]
  where typ s = VType $ VLam ix $ cSeq $ s ++ [x]
        ix = Ident noLoc "q"
        x = CVar ix
        vx = Var ix
        app f v = CApply (VPrim f) v
        app2 f v1 v2 = CApply (VPrim f) (VArray [v1, v2])
