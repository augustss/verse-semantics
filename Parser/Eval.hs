{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
module Eval(
  eval,
  evalSeq,
  Flags(..)
  ) where
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Debug.Trace

import Expr(Ident(..), noLoc)
import Core
import Error
import Print hiding (float)
import Misc

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

cWrongs :: [String] -> Core
cWrongs [] = internalError
cWrongs ws = CWrong $ intercalate ";" ws

{-
getWrong :: Core -> Maybe String
getWrong (CWrong s) = Just s
getWrong _ = Nothing
-}

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
    -- HACK: Recognizer when we have loaded the prelude.verse file
    hasPRELUDE = case ea of CDef (Ident _ "PRELUDE" : _) _ -> True; _ -> False

    -- Loop until convergence or timeout
    loop :: Int -> Core -> Core
    loop 0 e = trace "Reduction did not reach a normal form, use :eval to reduce more."
               e
    loop n e =
      let e' = evalSteps hasPRELUDE trc e
      in  if e == e' then
            let e'' = evalKnown trc e'
            in  if e'' == e' then
                  e''
                else
                  loop (n-1) e''
          else
            loop (n-1) e'

isX :: Core -> Bool
isX CUnify{} = True
isX CSeq{} = True
isX _ = False

-- Take some reduction steps.
evalSteps :: Bool -> EvalCore
evalSteps hasPRELUDE t =
  evalWrong t .
  evalDefFloat t . evalAll t . evalChoice t . evalSplit t .
  evalFail t . evalUnify t . evalUnused t . evalSubst t . evalDef t .
  evalOne t . evalApp t . evalSeq t . evalBar t . evalSucceeds t . evalPrimOps t .
  (if hasPRELUDE then id else replacePrelude t)

-- Handle CBar
--  CHOOSE
-- First locate an anchor point and then try to find CX hole with a CBar.
-- XXX This probably doesn't implement the SX/CX reduction correctly.
-- We need to try every SX context, not just the first one.
evalChoice :: EvalCore
evalChoice flg = evalTrace "evalChoice" t flg
  where
    -- Top-level anchor point
    t e = choice e

    -- Find more anchor points
    f (COne e) = COne $ choice e
    f (CAll e) = CAll $ choice e
    f (CSucceeds e) = CSucceeds $ choice e
    f (CDecides e) = CDecides $ choice e
    f (CSplit e n g) = CSplit (choice e) n g
    f e = composOpLam (underLambda flg) f e

    -- Is choice free expression?
    isCE :: Core -> Bool
    isCE CValue{} = True
    isCE (CUnify e1 e2) = isCE e1 && isCE e2
    isCE (CSeq es) = all isCE es
    isCE COne{} = True
    isCE CAll{} = True
    isCE CSucceeds{} = True
    isCE CDecides{} = True
    isCE (CSplit _ (VLam _ n) (VLam _ (CLam _ g))) = isCE n && isCE g
    isCE (CApply (VPrim p) _) = isCEPrim p
    isCE CFail = True
    isCE CWrong{} = True
    isCE _ = False

    isCEPrim _ = True

    choice (CBar es) = CBar $ map choice es
--    choice e | trace ("\nchoice ***\n" ++ prettyShow e++"\n---") False = undefined
    choice e =
      case runState (findC e) Nothing of
        (_, Nothing) -> f e  -- no choice found, look deeper down
        (e', Just es) -> CBar $ map e' es
    -- Find the leftmost choice.
    -- Return a function representing the CX context.
    findC :: Core -> State (Maybe [Core]) (Core -> Core)
    findC e = do
      me <- get
      if isJust me then
        pure $ const e  -- Already found, just keep going
       else
        case e of
          CUnify e1 e2 -> do
            e1' <- findC e1
            e2' <- if isCE e1 then findC e2 else pure $ const e2
            pure $ \ x -> CUnify (e1' x) (e2' x)
          CSeq es -> do
            let loop [] = pure []
                loop (x:xs) = do
                  x' <- findC x
                  xs' <- if isCE x then loop xs else pure $ map const xs
                  pure $ x' : xs'
            es' <- loop es
            pure $ \ x -> CSeq $ map ($ x) es'
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
    f e | isX e = float e
    f e = composOpLam (underLambda flg) f e

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
    f (CDef (_:_) CValue{}) = CWrong "def-wrong"
    f e | ws@(_:_) <- getWrongs e = cWrongs ws
    f e = composOp f e

    getWrongs (CWrong s) = [s]
    getWrongs (CUnify e1 e2) = getWrongs e1 ++ getWrongs e2
    getWrongs (CSeq es) = concatMap getWrongs es
    getWrongs (CDef _ e) = getWrongs e
    getWrongs (CSucceeds e) = getWrongs e
    getWrongs (CDecides e) = getWrongs e
    getWrongs (CSplit (CWrong s) _ _) = [s]
    getWrongs (CSplit (CBar (CWrong s : _)) _ _) = [s]
    getWrongs (COne (CWrong s)) = [s]
    getWrongs (COne (CBar (CWrong s : _))) = [s]
    getWrongs (CAll (CWrong s)) = [s]
    getWrongs (CAll (CBar es)) = concatMap getWrongs es
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
--  SWAP, UCON, UARR, UX*
evalUnify :: EvalCore
evalUnify flg = evalTrace "evalUnify" f flg
  where
    f (CUnify (CValue v@HNF{}) (CValue x@Var{})) = f $ CUnify (CValue x) (CValue v)
    f (CUnify (CValue v1@HNF{}) (CValue v2@HNF{})) = unifyV v1 v2
    f e = composOpLam (underLambda flg) f e

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

-- Handle
--  DEF-UNUSED
evalUnused :: EvalCore
evalUnused flg = evalTrace "evalUnused" f flg
  where
    f (CDef h e) | Just d <- bind h e = d
    f e = composOpLam (underLambda flg) f e

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
    f e@CSeq{} | Just d <- bind e = d
    f (CUnify (CVar x) (CValue v)) | Just c <- substRec x v = c
    f e@CUnify{} | Just d <- bind e = d
    f e = composOpLam (underLambda flg) f e

    bind e =
      case runState (findB (cfvs e) e) Nothing of
        (cx, Just (x, v)) ->
          -- We need to substitute v for x in cx, and then plug the hole
          -- with x=v.
          Just $ substHole (CUnify (CVar x) (CValue v)) $ subst x v cx
--        (_, Just (x, v)) | x `elem` fvsV v, Var x /= v ->
--          Just $ CWrong $ "occurs check " ++ prettyShow x
        _ -> Nothing

    hole = Ident noLoc "***"  -- Identifier not used anywhere else

    -- XXX This may find the same binding over and over when there are others that could
    --     succeed.  Must include the fvs cx test!
    -- Find the leftmost binding.
    -- Return with the binding replaced by hole.
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
              pure $ CVar hole
          CUnify e1 e2 -> CUnify e1 <$> findB vs e2
          CSeq es -> CSeq <$> mapM (findB vs) es
          --CSucceeds b -> CSucceeds <$> findB h b
          _ -> pure e

    -- Replace hole by an expression.
    substHole e = sub
      where
        sub (CVar i) | i == hole = e
        sub (CSeq es) = CSeq (map sub es)
        sub (CUnify e1 e2) = CUnify (sub e1) (sub e2)
        sub c = c

    -- Recognize and execute the SUBST-REC rule
    substRec :: Ident -> Value -> Maybe Core
    substRec x vv | x `notElem` fvsV vv = Nothing
                  | Var x == vv = Nothing
                  | otherwise =
      case runState (findLam vv) Nothing of
        (v', Just (y, e)) ->
          let lam = VLam y $ CDef [x] $ CSeq [CUnify (CVar x) (CValue vv), e]
          in  Just $ CUnify (CVar x) $ CValue $ substHoleV lam v'
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
                pure $ Var hole
              VArray vs -> VArray <$> mapM findLam vs
              _ -> pure v

    substHoleV vv = sub
      where
        sub (Var i) | i == hole = vv
        sub (VArray vs) = VArray $ map sub vs
        sub v = v

-- Handle simple 'def'
--  FAIL-DEF, DEF-ELIM, DEF-WRONG, DEF-MERGE
evalDef :: EvalCore
evalDef flg = evalTrace "evalDef" f flg
  where
    f (CDef _ CFail) = CFail
    f (CDef [] e) = f e
    f (CDef h1 (CDef h2 e)) =
      assert (null (intersect h1 h2)) ("DEF-MERGE: " ++ show (h1,h2)) $
      f $ CDef (h1 ++ h2) e
    f e = composOpLam (underLambda flg) f e

-- Handle 'split'
--  SPLIT-*
evalSplit :: EvalCore
evalSplit flg = evalTrace "evalSplit" f flg
  where
    f (CSplit CFail n _) = CApply n (VArray [])
    f e@(CSplit (CValue v) _ g) = val e g v (VLam dummy CFail)
    f e@(CSplit (CBar (CValue v : es)) _ g) = val e g v (VLam dummy $ CBar es)
    f e = composOpLam (underLambda flg) f e
    val e g v r =
      let y : _ = newVars "a" (fvs e)
      in  CDef [y] $ CSeq [CUnify (CVar y) (CApply g v), CApply (Var y) r]

dummy :: Ident
dummy = Ident noLoc "_"

-- Handle 'all'
--  ALL-0, ALL-N
evalAll :: EvalCore
evalAll flg = evalTrace "evalAll" f flg
  where
    f (CAll (CValue v)) = CArray [v]
    f (CAll (CBar es)) | Just vs  <- traverse getValue es = CArray vs
    f e = composOpLam (underLambda flg) f e

-- Handle 'one'
--  ONE-VALUE, ONE-CHOICE, ONE-FAIL
evalOne :: EvalCore
evalOne flg = evalTrace "evalOne" f flg
  where
    f (COne e@CValue{}) = f e
    f (COne CFail) = CFail
    f (COne (CBar (e@CValue{} : _))) = f e
    f e = composOpLam (underLambda flg) f e

-- Handle non-primop applications
--  APP-LAM, APP-TYPE, APP-REC, APP-ARR
--  APP-CONST-WRONG
evalApp :: EvalCore
evalApp flg = evalTrace "evalApp" f flg
  where
    f (CApply (VLam i e1) v2) = f $ subst i' v2 e1'
      where (i', e1') | i `elem` vs = (x, subst i (Var x) e1)
                      | otherwise = (i, e1)
            vs = fvsV v2
            x = head $ newVars "i" vs
    f (CApply (VArray vs) vi) = CBar $ zipWith g [0..] vs
      where g i v = CSeq [CUnify (CValue vi) (CInt i), CValue v]
    f e@(CApply VPrim{} _) = composOpLam (underLambda flg) f e
    f (CApply HNF{} _) = CWrong "APP-CONST-WRONG"
    f e = composOpLam (underLambda flg) f e

-- Handle CSeq in odd places
--  SEQ, APP-SEQL, APP-SEQR, UNIFY-SEQL, UNIFY-SEQR
--  UNIFY-UNIFYL, UNIFY-UNIFYR
evalSeq :: EvalCore
evalSeq flg = evalTrace "evalSeq" f flg
  where
    f (CSeq es) = cSeq $ Snoc (filter (not . isValue) es') e'
      where Snoc es' e' = concatMap (flat . f) es
    f (CUnify (CSeq (Snoc es e)) e2) = CSeq $ es ++ [CUnify e e2]
    f (CUnify e1@CValue{} (CSeq (Snoc es e))) = CSeq $ es ++ [CUnify e1 e]
    f (CUnify e1@(CUnify CValue{} v2@CValue{}) e2) =
      CSeq [e1, CUnify v2 e2]
    f (CUnify e1 e2@(CUnify CValue{} v2@CValue{})) =
      CSeq [e2, CUnify e1 v2]
    f e = composOpLam (underLambda flg) f e
    flat (CSeq es) = es
    flat e = [e]

-- Handle CBar associativity and fail elimination
-- FAIL-L, FAIL-R, ASSOC-CHOICE
evalBar :: EvalCore
evalBar flg = evalTrace "evalBar" f flg
  where
    f (CBar [e]) = e
    f (CBar es) = CBar $ concatMap (flat . f) es
    f e = composOpLam (underLambda flg) f e
    flat (CBar es) = es
    flat e = [e]

-- succeeds{v}  -->  v
-- SUCCEEDS-VALUE, SUCCEEDS-FAIL, SUCCEEDS-CHOICE, SUCCEEDS-WRONG
-- DECIDES-*
evalSucceeds :: EvalCore
evalSucceeds flg = evalTrace "evalSucceeds" f flg
  where
    f (CSucceeds e@CValue{}) = f e
    f (CSucceeds CFail) = CWrong "succeeds-fail"
    f (CSucceeds (CBar [e@CValue{}])) = f e
    f (CSucceeds (CBar (CValue{} : _ : _))) = CWrong "succeeds-many"
    f (CDecides e@CValue{}) = f e
    f (CDecides CFail) = CFail
    f (CDecides (CBar [e@CValue{}])) = f e
    f (CDecides (CBar (CValue{} : _ : _))) = CWrong "decides-many"
    f e = composOpLam (underLambda flg) f e

-- Reduce applications of primops
-- P-* rules
evalPrimOps :: EvalCore
evalPrimOps flg = evalTrace "evalPrimOps" f flg
  where
    -- real primitives
    f (CUnOp  "isInt$" v) | VInt{} <- v = CUnit
                          | otherwise   = CFail
    -- float#, string#
    f (CUnOp  "isArr$" v) | VArray{} <- v = CUnit
                          | otherwise   = CFail
    f (CUnOp  "isFcn$" v) | VLam{} <- v = CUnit
                          | otherwise   = CFail

    --
    f (CUnOp  "pre'-'" (VInt i)) = CInt $ -i
    f (CBinOp "in'+'"  v1 v2) = arith (+) v1 v2
    f (CBinOp "in'-'"  v1 v2) = arith (-) v1 v2
    f (CBinOp "in'*'"  v1 v2) = arith (*) v1 v2
    f (CBinOp "in'/'"  (VInt i1) (VInt i2)) | i2 == 0 = CFail
                                            | otherwise = CInt $ i1 `div` i2
    f (CBinOp "intLT$"  v1 v2) = cmpU (<)  v1 v2
    f (CBinOp "intLE$"  v1 v2) = cmpU (<=) v1 v2
    f (CBinOp "intGT$"  v1 v2) = cmpU (>)  v1 v2
    f (CBinOp "intGE$"  v1 v2) = cmpU (>=) v1 v2
    f (CBinOp "in'<>'"  v1 v2) = cmp  (/=) v1 v2

    f (CBinOp "in'..'"  v1 v2) = enum v1 v2

    f (CUnOp  "concat$" (VArray as)) | all isHNF as =
      case () of
        _ | Just vss <- traverse getA as -> CValue $ VArray $ concat vss
        _ ->  CWrong $ "concat$"
        where getA (VArray vs) = Just vs
              getA _ = Nothing
    f (CBinOp op  (VInt ni) (VArray vs)) | op `elem` ["takeL$", "dropL$", "takeR$", "dropR$"] =
      let n = fromInteger ni in
      if n >= 0 && n <= length vs then
        case op of
          "takeL$" -> CArray $ take n vs
          "dropL$" -> CArray $ drop n vs
          "takeR$" -> CArray $ revTake n vs
          "dropR$" -> CArray $ revDrop n vs
          _ -> impossible "take/drop"
       else
        CFail

    f (CUnOp  "length" v) | VArray as <- v = CInt $ toInteger $ length as
                          | otherwise = CFail

    -- Use in 'all' desugaring.  mapAp = map ($())
    f (CUnOp  "mapAp" (VArray vs)) = mkArr vs

    -- XXX Stricter than necessary?
    f (CApply (VPrim "cons$") v) | VArray [v1, VArray vs] <- v = CArray $ v1 : vs
                                 | VArray [_, va] <- v, isHNF va = CFail
--x                                 | isHNF v = CFail

    f e@(CUnOp "known$" _) = e  -- XXX Just leave it alone for now
    f e@(CUnOp "new$" _) = e  -- XXX Just leave it alone for now
    f e@(CUnOp "pre'[]'" _) = e  -- XXX Just leave it alone for now

    -- Fully evaluated, and still no match
    f (CApply (VPrim op) a) | isNF a = unimplemented $ show (op, a)
    f e = composOpLam (underLambda flg) f e

    arith op (VInt i1) (VInt i2) = CInt $ op i1 i2
    arith _ _ _ = CFail  -- CWrong?
    
    cmp op (VInt i1) (VInt i2) | op i1 i2  = CInt i1
                               | otherwise = CFail
    cmp _ _ _ = CFail   -- CWrong?
    cmpU op (VInt i1) (VInt i2) | op i1 i2  = CUnit
                                | otherwise = CFail
    cmpU _ _ _ = CFail   -- CWrong?

    enum (VInt lo) (VInt hi) = CBar [CInt i | i <- [lo .. hi ]]
    enum _ _ = CFail


isNF :: Value -> Bool
isNF (Var _) = False
isNF (HNF (HArray vs)) = all isNF vs
isNF (HNF _) = True

isHNF :: Value -> Bool
isHNF HNF{} = True
isHNF _ = False

mkArr :: [Value] -> Core
mkArr vs =
  let xs = take (length vs) $ newVars "x" $ fvsV (VArray vs)
      unit = VArray []
  in  CDef xs $ cSeq $ zipWith (\ x v -> CUnify (CVar x) $ CApply v unit) xs vs ++ [CArray $ map Var xs]

-- A gruesome hack to test if something is an uninstantiated logical variable.
evalKnown :: EvalCore
evalKnown flg = evalTrace "evalKnown" f flg
  where
    f (CApply (VPrim "known$") v) | isHNF v   = CUnit
                                  | otherwise = CFail
    f e = composOp f e

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
  ,("nat", typ [app "isInt$" vx, app2 "in'>='" vx (VInt 0)]) -- x => int#[x]; x>=0; x
  ,("int", typ [app "isInt$" vx])                            -- x => int#[x]; x
  ,("in'->'", arrowV)
  ,("false", VArray [])                                      -- ()
  ,("in'>'",  cmpV "intGT$")
  ,("in'>='", cmpV "intGE$")
  ,("in'<'",  cmpV "intLT$")
  ,("in'<='", cmpV "intLE$")
  ,("new", newV)
--  ,("pre'[]'", unimplemented "pre []")
--  ,("pre'^'", unimplemented "pre ^")
  ,("post'^'", VLam x $ CApply (VPrim "read$") vx)
  ,("in'.='", VLam x $ CApply (VPrim "write$") vx)
  ]
  where typ is = VLam x $ cSeq $ is ++ [CVar x]
        vx = Var x
        app f v = CApply (VPrim f) v
        app2 f v1 v2 = CApply (VPrim f) (VArray [v1, v2])

        arrowV =
          VLam st $
            CDef [s, t] $
            CSeq [
              CUnify (CValue (VArray [Var s, Var t])) (CVar st),
              CLam g $ CLam y $
                CDef [sy, gsy] $
                CSeq [
                  app "isFcn$" (Var g),
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
              CUnify (CValue (VArray [Var x, Var y])) (CVar xy),
              app op (Var xy),
              CVar x
              ]

        newV =
          VLam t $ CLam x $
            CDef [y] $
            CSeq [
              CUnify (CVar y) (CApply (Var t) (Var x)),
              app "alloc$" (Var y)
              ]

-- A special purpose composOp that can avoid going under lambda.
composOpLam :: Bool -> (Core -> Core) -> Core -> Core
composOpLam underLam f = runIdentity . composC fc fv fh
  where
    fc = pure . f
    fv = composV fc fv fh
    fh h@HLam{} | not underLam = pure h
    fh h = composH fc fv fh h
    
