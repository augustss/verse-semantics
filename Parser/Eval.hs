{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- XXX
-- reduction rule bugs
--  X lacks succeeds

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
module Eval(
  eval,
  evalSeq,
  Flags(..)
  ) where
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Debug.Trace

import Expr(Ident(..), noLoc)
import Core
import Error
import Print hiding (float)
import Misc

pattern VArray :: [Value] -> Value
pattern VArray vs = HNF (HArray vs)

pattern CArray :: [Value] -> Core
pattern CArray vs = CValue (VArray vs)

pattern CUnOp :: String -> Value -> Core
pattern CUnOp op v <- CApply (VPrim op) v@HNF{}

pattern CBinOp :: String -> Value -> Value -> Core
pattern CBinOp op v1 v2 <- CApply (VPrim op) (VArray [v1@HNF{}, v2@HNF{}])

pattern VInt :: Integer -> Value
pattern VInt i = HNF (HInt i)

pattern CInt :: Integer -> Core
pattern CInt i = CValue (VInt i)

pattern CDecides :: Core -> Core
pattern CDecides c <- CMacro (Ident _ "decides") c
  where CDecides e = CMacro (Ident noLoc "decides") e

pattern CUnit :: Core
pattern CUnit = CArray []

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

data Flags = Flags
  { traceEval   :: !Bool
  , underLambda :: !Bool
  }
  deriving (Show)

type EvalCore = Flags -> Core -> Core

evalTrace :: String -> (Core -> Core) -> EvalCore
evalTrace s f flg e | not (traceEval flg) = e'
                    | e == e'     = e'
                    | otherwise   = trace (s ++ ":\n" ++ prettyShow e') e'
  where e' = f e

-------------

-- Reduce until we reach HNF
eval :: EvalCore
eval trc ea = loop 1000 $ evalTrace "eval" (const ea) trc (CWrong"")
  where
    -- Loop until convergence or timeout
    loop :: Int -> Core -> Core
    loop 0 e = trace "Reduction did not reach a normal form, use :eval to reduce more."
               e
    loop n e =
      let e' = evalSteps trc e
      in  if e == e' then
            e'
          else
            loop (n-1) e'

isX :: Core -> Bool
isX CUnify{} = True
isX CSeq{} = True
isX _ = False

-- Take some reduction steps.
evalSteps :: EvalCore
evalSteps t =
  evalDefFloat t . evalAll t . evalChoice t .
  evalWrong t . evalFail t . evalUnify t . evalUnused t . evalSubst t . evalDef t .
  evalOne t . evalApp t . evalSeq t . evalBar t . evalSucceeds t . evalPrimOps t .
  replacePrelude t

-- Handle CBar
--  CHOOSE
-- First locate an anchor point and then try to find CX hole with a CBar.
-- XXX What are the anchor points?
evalChoice :: EvalCore
evalChoice flg = evalTrace "evalChoice" t flg
  where
    -- Top-level anchor point
    t (CBar [e]) = e
    t e = choice e

    -- Find more anchor points
    f e@CLam{} | not (underLambda flg) = e
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
evalDefFloat flg = evalTrace "evalDefFloat" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
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
evalUnify flg = evalTrace "evalUnify" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
    f (CUnify e@(CValue HNF{}) x@CVar{}) = f $ CUnify x e
    f (CUnify (CValue v1@HNF{}) (CValue v2@HNF{})) = unifyV v1 v2
    f e = composOp f e

    unifyV v@(VInt i1) (VInt i2) | i1 == i2 = CValue v
                               | otherwise = CFail
    unifyV VInt{} _ = CFail
    unifyV _ VInt{} = CFail

    unifyV v@(VArray vs1) (VArray vs2) | length vs1 == length vs2 =
      cSeq $ zipWith (\ v1 v2 -> CUnify (CValue v1) (CValue v2)) vs1 vs2 ++ [CValue v]
                                       | otherwise = CFail
    unifyV VArray{} _ = CFail
    unifyV _ VArray{} = CFail

    unifyV _ _ = CWrong "unifyV"

{-
-- Handle BIND rules
evalBind :: EvalCore
evalBind = evalTrace "evalBind" f
  where
    f e@CLam{} | not (underLambda flg) = e
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
        go v = internalErrorMsg (show v)

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
-}

-- Handle
--  DEF-UNUSED
evalUnused :: EvalCore
evalUnused flg = evalTrace "evalUnused" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
    f (CDef h e) | Just d <- bind h e = d
    f e = composOp f e

    bind h e =
      case runState (findB (cfvs e) h e) [] of
        (_, []) -> Nothing
        (e', xs)-> Just $ cDef (h \\ xs) e'

    -- Find bindings that are unused.
    findB vs h e = do
      case e of
        CUnify (CVar x) ev@(CValue _)
          | elem x h                 -- in this heap
          , x `notElem` delete x vs  -- this is the only mention
          -> do
            modify $ (x:)            -- remember variable
            pure ev                  -- replace with value
        CUnify e1 e2 -> CUnify <$> findB vs h e1 <*> findB vs h e2
        CSeq es -> CSeq <$> mapM (findB vs h) es
        --CSucceeds b -> CSucceeds <$> findB vs h b
        _ -> pure e


-- Handle
--  SUBST
--  SUBST-REC
evalSubst :: EvalCore
evalSubst flg = evalTrace "evalSubst" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
    f e@CSeq{} | Just d <- bind e = d
    f (CUnify (CVar x) (CValue v)) | Just c <- substRec x v = c
    f e@CUnify{} | Just d <- bind e = d
    f e = composOp f e

    bind e =
      case runState (findB (cfvs e) e) Nothing of
        (cx, Just (x, v)) ->
          -- We need to substitute v for x in cx, and then plug the hole
          -- with x=v.
          Just $ substDummy (CUnify (CVar x) (CValue v)) $ subst x v cx
--        (_, Just (x, v)) | x `elem` fvsV v, Var x /= v ->
--          Just $ CWrong $ "occurs check " ++ prettyShow x
        _ -> Nothing

    dummy = Ident noLoc "***"  -- Identifier not used anywhere else

    -- XXX This may find the same binding over and over when there are others that could
    --     succeed.  Must include the fvs cx test!
    -- Find the leftmost binding.
    -- Return with the binding replaced by dummy.
    findB vs e = do
      me <- get
      if isJust me then
        pure e  -- Already found, just keep going
       else
        case e of
          CUnify (CVar x) (CValue v)
            | x `notElem` fvsV v     -- occurs check
            , x `elem` delete x vs   -- there is another free occurrence
            -> do
              put $ Just (x, v)
              pure $ CVar dummy
          CUnify e1 e2 -> CUnify <$> findB vs e1 <*> findB vs e2
          CSeq es -> CSeq <$> mapM (findB vs) es
          --CSucceeds b -> CSucceeds <$> findB h b
          _ -> pure e

    -- Replace dummy by an expression.
    substDummy e = sub
      where
        sub (CVar i) | i == dummy = e
        sub (CSeq es) = CSeq (map sub es)
        sub (CUnify e1 e2) = CUnify (sub e1) (sub e2)
        sub c = c

    -- Recognize and execute the SUBST-REC rule
    substRec :: Ident -> Value -> Maybe Core
    substRec x vv | x `notElem` fvsV vv = Nothing
                  | otherwise =
      case runState (findLam vv) Nothing of
        (v', Just (y, e)) ->
          let lam = VLam y $ CDef [x] $ CSeq [CUnify (CVar x) (CValue vv), e]
          in  Just $ CUnify (CVar x) $ CValue $ substDummyV lam v'
        _ -> Just $ CWrong $ "occurs check " ++ prettyShow x
      where
        findLam :: Value -> State (Maybe (Ident, Core)) Value
        findLam v = do
          mv <- get
          if isJust mv then
            pure v
           else
            case v of
              VLam y e | x `elem` fvs e, x /= y -> do
                put $ Just (y, e)
                pure $ Var dummy
              VArray vs -> VArray <$> mapM findLam vs
              _ -> pure v

    substDummyV vv = sub
      where
        sub (Var i) | i == dummy = vv
        sub (VArray vs) = VArray $ map sub vs
        sub v = v

-- Handle simple 'def'
--  FAIL-DEF, DEF-ELIM, DEF-WRONG, DEF-MERGE
evalDef :: EvalCore
evalDef flg = evalTrace "evalDef" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
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
evalAll flg = evalTrace "evalAll" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
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
evalOne flg = evalTrace "evalOne" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
    f (COne e@CValue{}) = f e
    f (COne CFail) = CFail
    f (COne (CBar (e@CValue{} : _))) = f e
    f (COne e@CWrong{}) = e
    f e = composOp f e

-- Handle non-primop applications
--  APP-LAM, APP-TYPE, APP-REC, APP-ARR
--  APP-CONST-WRONG
evalApp :: EvalCore
evalApp flg = evalTrace "evalApp" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
    f (CApply (VLam i e1) v2) = f $ subst i' v2 e1'
      where (i', e1') | i `elem` vs = (x, subst i (Var x) e1)
                      | otherwise = (i, e1)
            vs = fvsV v2
            x = head $ newVars "i" vs
    f (CApply (VArray vs) vi) = CBar $ zipWith g [0..] vs
      where g i v = CSeq [CUnify ei (CInt i), CValue v]
            ei = CValue vi
    f (CApply VInt{} _) = CWrong "APP-CONST-WRONG"
    f e = composOp f e

-- Handle CSeq in odd places
--  SEQ, APP-SEQL, APP-SEQR, UNIFY-SEQL, UNIFY-SEQR
evalSeq :: EvalCore
evalSeq flg = evalTrace "evalSeq" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
    f (CSeq es) = cSeq $ Snoc (filter (not . isValue) es') e'
      where Snoc es' e' = concatMap (flat . f) es
    f (CUnify (CSeq (Snoc es e)) e2) = CSeq $ es ++ [CUnify e e2]
    f (CUnify e1@CValue{} (CSeq (Snoc es e))) = CSeq $ es ++ [CUnify e1 e]
    f e = composOp f e
    flat (CSeq es) = es
    flat e = [e]

-- Handle CBar associativity and fail elimination
-- FAIL-L, FAIL-R, ASSOC-CHOICE
evalBar :: EvalCore
evalBar flg = evalTrace "evalBar" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
    f (CBar es) = CBar $ concatMap (flat . f) es
    f e = composOp f e
    flat (CBar es) = es
    flat e = [e]

-- succeeds{v}  -->  v
-- SUCCEEDS-VALUE, SUCCEEDS-FAIL, SUCCEEDS-CHOICE, SUCCEEDS-WRONG
-- DECIDES-*
evalSucceeds :: EvalCore
evalSucceeds flg = evalTrace "evalSucceeds" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
    f (CSucceeds e@CValue{}) = f e
    f (CSucceeds CFail) = CWrong "succeeds-fail"
    f (CSucceeds (CBar [e@CValue{}])) = f e
    f (CSucceeds (CBar (CValue{} : _ : _))) = CWrong "succeeds-many"
    f (CSucceeds e@CWrong{}) = e
    f (CDecides e@CValue{}) = f e
    f (CDecides CFail) = CFail
    f (CDecides (CBar [e@CValue{}])) = f e
    f (CDecides (CBar (CValue{} : _ : _))) = CWrong "decides-many"
    f (CDecides e@CWrong{}) = e
    f e = composOp f e

-- Reduce applications of primops
-- P-* rules
evalPrimOps :: EvalCore
evalPrimOps flg = evalTrace "evalPrimOps" f flg
  where
    f e@CLam{} | not (underLambda flg) = e
    -- real primitives
    f (CUnOp  "isInt#" v) | VInt{} <- v = CUnit
                          | otherwise   = CFail
    -- float#, string#
    f (CUnOp  "isArr#" v) | VArray{} <- v = CUnit
                          | otherwise   = CFail
    f (CUnOp  "isFcn#" v) | VLam{} <- v = CUnit
                          | otherwise   = CFail

    --
    f (CUnOp  "pre'-'" (VInt i)) = CInt $ -i
    f (CBinOp "in'+'"  v1 v2) = arith (+) v1 v2
    f (CBinOp "in'-'"  v1 v2) = arith (-) v1 v2
    f (CBinOp "in'*'"  v1 v2) = arith (*) v1 v2
    f (CBinOp "in'/'"  (VInt i1) (VInt i2)) | i2 == 0 = CFail
                                            | otherwise = CInt $ i1 `div` i2
    f (CBinOp "intLT#"  v1 v2) = cmpU (<)  v1 v2
    f (CBinOp "intLE#"  v1 v2) = cmpU (<=) v1 v2
    f (CBinOp "intGT#"  v1 v2) = cmpU (>)  v1 v2
    f (CBinOp "intGE#"  v1 v2) = cmpU (>=) v1 v2
    f (CBinOp "in'<>'"  v1 v2) = cmp  (/=) v1 v2

    f (CUnOp  "concat#" (VArray as)) | all isHNF as =
      case () of
        _ | Just vss <- traverse getA as -> CValue $ VArray $ concat vss
        _ ->  CWrong $ "concat#"
        where getA (VArray vs) = Just vs
              getA _ = Nothing
    f (CBinOp op  (VInt ni) (VArray vs)) | op `elem` ["takeL#", "dropL#", "takeR#", "dropR#"]
                                         , let n = fromInteger ni
                                         , n >= 0 && n <= length vs =
      case op of
        "takeL#" -> CArray $ take n vs
        "dropL#" -> CArray $ drop n vs
        "takeR#" -> CArray $ revTake n vs
        "dropR#" -> CArray $ revDrop n vs
        _ -> impossible "take/drop"

    f e@(CUnOp "new#" _) = e  -- XXX Just leave it alone for now
    f e@(CUnOp "pre'[]'" _) = e  -- XXX Just leave it alone for now

    -- Fully evaluated, and still no match
    f (CApply (VPrim op) a) | isNF a = unimplemented $ show (op, a)
    f e = composOp f e

    arith op (VInt i1) (VInt i2) = CInt $ op i1 i2
    arith _ _ _ = CFail  -- CWrong?
    
    cmp op (VInt i1) (VInt i2) | op i1 i2  = CInt i1
                               | otherwise = CFail
    cmp _ _ _ = CFail   -- CWrong?
    cmpU op (VInt i1) (VInt i2) | op i1 i2  = CUnit
                                | otherwise = CFail
    cmpU _ _ _ = CFail   -- CWrong?

isNF :: Value -> Bool
isNF (Var _) = False
isNF (HNF (HArray vs)) = all isNF vs
isNF (HNF _) = True

isHNF :: Value -> Bool
isHNF HNF{} = True
isHNF _ = False

-------------------

-- Until we get a proper prelude, just hack it.
-- XXX This doesn't really work, it only replaces in applications.
-- Could just put the prelude as a prefix to the program.
replacePrelude :: EvalCore
replacePrelude = evalTrace "replacePrelude" f
  where
    f (CApply (Var (Ident _ i)) v) | Just p <- lookup i prelude = CApply p v
    f (CVar (Ident _ i)) | Just p <- lookup i prelude = CValue p
    f e = composOp f e

-- Functions that should be in a prelude.
prelude :: [(String, Value)]
prelude =
  [("any", typ [])                                           -- x => x
  ,("nat", typ [app "isInt#" vx, app2 "in'>='" vx (VInt 0)]) -- x => int#[x]; x>=0; x
  ,("int", typ [app "isInt#" vx])                            -- x => int#[x]; x
  ,("in'->'", arrowV)
  ,("false", VArray [])                                      -- ()
  ,("in'>'",  cmpV "intGT#")
  ,("in'>='", cmpV "intGE#")
  ,("in'<'",  cmpV "intLT#")
  ,("in'<='", cmpV "intLE#")
  ,("new", newV)
  ,("float", undefined)
  ,("string", undefined)
--  ,("pre'[]'", unimplemented "pre []")
--  ,("pre'^'", unimplemented "pre ^")
  ]
  where typ is = VLam x $ cSeq $ is ++ [CVar x]
        vx = Var x
        app f v = CApply (VPrim f) v
        app2 f v1 v2 = CApply (VPrim f) (VArray [v1, v2])

        arrowV =
          VLam st $
            CDef [s, t] $
            CSeq [
              CUnify (CArray [Var s, Var t]) (CVar st),
              CLam g $ CLam y $
                CDef [sy, gsy] $
                CSeq [
                  app "isFcn#" (Var g),
                  CUnify (CVar sy) (CApply (Var s) (Var y)),
                  CUnify (CVar gsy) (CApply (Var g) (Var sy)),
                  CApply (Var t) (Var gsy)
                  ]
              ]
        [st, s, t, g, y, sy, gsy, x, xy] =
           map (Ident noLoc) ["st","s","t","g","y","sy","gsy","x", "xy"]

        cmpV op =
          VLam xy $
            CDef [x, y] $
            CSeq [
              CUnify (CArray [Var x, Var y]) (CVar xy),
              app op (Var xy),
              CVar x
              ]

        newV =
          VLam t $ CLam x $
            CDef [y] $
            CSeq [
              CUnify (CVar y) (CApply (Var t) (Var x)),
              app "new#" (Var y)
              ]
