{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module FrontEnd.Eval(
  eval,
  evalSeq,
  replacePrelude,
  EFlags(..)
  ) where
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Debug.Trace

import FrontEnd.Core
import FrontEnd.Error
import Epic.Print hiding (float)
import Epic.List
import Epic.Uniplate(universe)

pattern CUnOp :: String -> Value -> Core
pattern CUnOp op v <- CApply (CPrim op) v@HNF{}

pattern CBinOp :: String -> Value -> Value -> Core
pattern CBinOp op v1 v2 <- CApply (CPrim op) (CArray [v1@HNF{}, v2@HNF{}])

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

-- Infinite list of variables not in vs
newVars :: String -> [Ident] -> [Ident]
newVars s vs = [ Ident noLoc $ "$" ++ s ++ show n | n <- [0::Int ..] ] \\ vs

-------------

data EFlags = EFlags
  { traceEval   :: !Bool
  , underLambda :: !Bool
  , steps       :: !Int
  }
  deriving (Show)

type EvalCore = EFlags -> Core -> Core

evalTrace :: String -> (Core -> Core) -> EvalCore
evalTrace s f flg e | not (traceEval flg) = e'
                    | e == e'     = e'
                    | otherwise   = trace (s ++ ":\n" ++ prettyShow e') e'
  where e' = f e

-------------

-- Reduce until we reach HNF
eval :: EvalCore
eval trc ea = unStore $ loop (steps trc) $ evalTrace "eval" (const ea'') trc (CWrong"")
  where
    ea' = anf ea
    ea'' = if noStoreIn ea && hasStoreOp ea then CStore storeEmpty ea' else ea'
    unStore (CStore _ e) | not (hasStoreOp e) = e
    unStore e = e

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
  evalRef t . (if False then checkStoreConsistency t else id) . evalStore t .
  evalDefFloat t . evalAll t . evalChoice t . evalSplit t .
  evalFail t . evalUnify t . evalUnused t . evalSubst t . evalDef t .
  evalOne t . evalApp t . evalSeq t . evalBar t . evalSucceeds t . evalPrimOps t .
  (if hasPRELUDE then id else replPrelude t)

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
--    f e | trace ("+++ evalChoice " ++ prettyShow e) False = undefined
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
    isCE (CSplit _ (CLam _ n) g) = isCE n && isCELam g
    isCE (CApply (CPrim p) (CValue _)) = not (isStorePrim p)
    isCE CFail = True
    isCE CWrong{} = True
    isCE (CDef _ e) = isCE e
    isCE _ = False
    -- The g of CSplit is recursive, so it might get a CDef in front of the lambda.
    --isCELam e | trace ("isCELam " ++ prettyShow e) False = undefined
    isCELam (CLam _ (CLam _ e)) = isCE e
    isCELam (CLam _ (CDef [_] (CSeq [_, CLam _ e]))) = isCE e
    isCELam (CVar _) = True -- happens for recursive functions
    isCELam _ = False

    choice (CStore s e) = CStore s (choice e)
    choice (CBar e1 e2) = CBar (choice e1) (choice e2)
--    choice e | trace ("\nchoice ***\n" ++ prettyShow e++"\n---") False = undefined
    choice e | noStoreIn e =
--      trace ("*** choice " ++ prettyShow (e, snd (runState (findC e) Nothing))) $
      case runState (findC e) Nothing of
        (_, Nothing) -> f e  -- no choice found, look deeper down
        (e', Just es) ->
--          trace ("%%% dup " ++ prettyShow (map e' es)) $
          cBar $ map e' es
    choice e = f e
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
          CBar e1 e2 -> do
            put $ Just [e1, e2]
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
    getWrongs (CSplit (CBar (CWrong s) _) _ _) = [s]
    getWrongs (COne (CWrong s)) = [s]
    getWrongs (COne (CBar (CWrong s) _)) = [s]
    getWrongs (CAll (CWrong s)) = [s]
    getWrongs (CAll (CBar e1 e2)) = getWrongs e1 ++ getWrongs e2
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
    hasFail (CStore _ e) = hasFail e
    hasFail _ = False

-- Handle unification
--  SWAP, UCON, UARR, UX*
evalUnify :: EvalCore
evalUnify flg = evalTrace "evalUnify" f flg
  where
--    f e | trace ("f " ++ show e) False = undefined
--    f (CUnify e1 e2) | trace ("f " ++ show (e1, e2, getHNF e1, getHNF e2)) False = undefined
    f (CUnify v@HNF{} x@CVar{}) = f $ CUnify x v
    f (CUnify v1@HNF{} v2@HNF{}) = unifyV v1 v2
    f e = composOpLam (underLambda flg) f e

--    unifyV v1 v2 | trace ("unifyV " ++ show (v1, v2)) False = undefined
    unifyV v@(CInt i1) (CInt i2) | i1 == i2 = CValue v
                                 | otherwise = CFail
    unifyV CInt{} _ = CFail
    unifyV _ CInt{} = CFail

    unifyV v@(CArray vs1) (CArray vs2) | length vs1 == length vs2 =
      cSeq $ zipWith (\ v1 v2 -> CUnify (CValue v1) (CValue v2)) vs1 vs2 ++ [CValue v]
                                       | otherwise = CFail
    unifyV CArray{} _ = CFail
    unifyV _ CArray{} = CFail

    unifyV v1@CPrim{} v2 | v1 == v2 = CValue v1  -- Compatible with PLDI rules

    unifyV v@(CPtr i1) (CPtr i2) | i1 == i2 = CValue v
                                 | otherwise = CFail
    unifyV CPtr{} _ = CFail
    unifyV _ CPtr{} = CFail

    unifyV _ _ = CFail -- Compatible with PLDI rule CWrong "unifyV"

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
--        (_, Just (x, v)) | x `elem` fvs v, Var x /= v ->
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
            | x `notElem` fvs v     -- occurs check
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
    substRec x vv | x `notElem` fvs vv = Nothing
                  | CVar x == vv = Nothing
                  | otherwise =
      case runState (findLam vv) Nothing of
        (v', Just (y, e)) ->
          let lam = CLam y $ CDef [x] $ CSeq [CUnify (CVar x) (CValue vv), e]
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
              CLam y e | x `elem` fvs e, x /= y -> do
                put $ Just (y, e)
                pure $ CVar hole
              CArray vs -> CArray <$> mapM findLam vs
              _ -> pure v

    substHoleV vv = sub
      where
        sub (CVar i) | i == hole = vv
        sub (CArray vs) = CArray $ map sub vs
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
    f (CSplit CFail n _) = CApply n (CArray [])
    f e@(CSplit (CValue v) _ g) = val e g v (CLam dummy CFail)
    f e@(CSplit (CBar (CValue v) e2) _ g) = val e g v (CLam dummy e2)
    f e = composOpLam (underLambda flg) f e
    val e g v r =
      let y : _ = newVars "a" (fvs e)
      in  CDef [y] $ CSeq [CUnify (CVar y) (CApply g v), CApply (CVar y) r]

dummy :: Ident
dummy = Ident noLoc "_"

-- Handle 'all'
--  ALL-0, ALL-N
evalAll :: EvalCore
evalAll flg = evalTrace "evalAll" f flg
  where
    f (CAll e) | Just vs  <- getValues e = CArray vs
    f e = composOpLam (underLambda flg) f e
    getValues (CBar e1 e2) = (++) <$> getValues e1 <*> getValues e2
    getValues CFail = Just []
    getValues (CValue v) = Just [v]
    getValues _ = Nothing

-- Handle 'one'
--  ONE-VALUE, ONE-CHOICE, ONE-FAIL
evalOne :: EvalCore
evalOne flg = evalTrace "evalOne" f flg
  where
    f (COne e@CValue{}) = f e
    f (COne CFail) = CFail
    f (COne (CBar e@CValue{} _)) = f e
    f e = composOpLam (underLambda flg) f e

-- Handle non-primop applications
--  APP-LAM, APP-TYPE, APP-REC, APP-ARR
--  APP-CONST-WRONG
evalApp :: EvalCore
evalApp flg = evalTrace "evalApp" f flg
  where
    f (CApply (CLam i e1) (CValue v2)) = f $ subst i' v2 e1'
      where (i', e1') | i `elem` vs = (x, subst i (CVar x) e1)
                      | otherwise = (i, e1)
            vs = fvs v2
            x = head $ newVars "i" vs
    f (CApply (CArray vs) (CValue vi)) = cBar $ zipWith g [0..] vs
      where g i v = CSeq [CUnify (CValue vi) (CInt i), CValue v]
    f e@(CApply CPrim{} _) = composOpLam (underLambda flg) f e
    f (CApply HNF{} CValue{}) = CWrong "APP-CONST-WRONG"
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
    f (CBar CFail e) = f e
    f e@CBar{} = cBar $ map f $ flat e
    f e = composOpLam (underLambda flg) f e
    flat (CBar e1 e2) = flat e1 ++ flat e2
    flat e = [e]

-- succeeds{v}  -->  v
-- SUCCEEDS-VALUE, SUCCEEDS-FAIL, SUCCEEDS-CHOICE, SUCCEEDS-WRONG
-- DECIDES-*
evalSucceeds :: EvalCore
evalSucceeds flg = evalTrace "evalSucceeds" f flg
  where
    f (CSucceeds e@CValue{}) = f e
    f (CSucceeds CFail) = CWrong "succeeds-fail"
    f (CSucceeds (CBar CValue{} _)) = CWrong "succeeds-many"
    f (CDecides e@CValue{}) = f e
    f (CDecides CFail) = CFail
    f (CDecides (CBar CValue{} (CBar CValue{} _))) = CWrong "decides-many"
    f e = composOpLam (underLambda flg) f e

-- Reduce applications of primops
-- P-* rules
evalPrimOps :: EvalCore
evalPrimOps flg = evalTrace "evalPrimOps" f flg
  where
    -- real primitives
    f (CUnOp  "isInt$" v) | CInt{} <- v = v -- CUnit
                          | otherwise   = CFail
    -- float#, string#
    f (CUnOp  "isArr$" v) | CArray{} <- v = v -- CUnit
                          | otherwise   = CFail
    f (CUnOp  "isFcn$" v) | CLam{} <- v = v -- CUnit
                          | otherwise   = CFail

    --
    f (CUnOp  "pre'-'" (CInt i)) = CInt $ -i
    f (CUnOp  "pre'+'" v) | CInt i <- v = CInt i
                          | otherwise   = CFail
    f (CBinOp "in'+'"  v1 v2) = arith (+) v1 v2
    f (CBinOp "in'-'"  v1 v2) = arith (-) v1 v2
    f (CBinOp "in'*'"  v1 v2) = arith (*) v1 v2
    f (CBinOp "in'/'"  (CInt i1) (CInt i2)) | i2 == 0 = CFail
                                            | otherwise = CInt $ i1 `div` i2
{-
    f (CBinOp "intLT$"  v1 v2) = cmpU (<)  v1 v2
    f (CBinOp "intLE$"  v1 v2) = cmpU (<=) v1 v2
    f (CBinOp "intGT$"  v1 v2) = cmpU (>)  v1 v2
    f (CBinOp "intGE$"  v1 v2) = cmpU (>=) v1 v2
-}
    f (CBinOp "in'<'"   v1 v2) = cmp  (<)  v1 v2
    f (CBinOp "in'<='"  v1 v2) = cmp  (<=) v1 v2
    f (CBinOp "in'>'"   v1 v2) = cmp  (>)  v1 v2
    f (CBinOp "in'>='"  v1 v2) = cmp  (>=) v1 v2
    f (CBinOp "in'<>'"  v1 v2) = cmp  (/=) v1 v2

    f (CBinOp "in'..'"  v1 v2) = enum v1 v2

    f (CUnOp  "concat$" (CArray as)) | all isHNF as =
      case () of
        _ | Just vss <- traverse getA as -> CValue $ CArray $ concat vss
        _ ->  CWrong $ "concat$"
        where getA (CArray vs) = Just vs
              getA _ = Nothing
    f (CBinOp op  (CInt ni) (CArray vs)) | op `elem` ["takeL$", "dropL$", "takeR$", "dropR$"] =
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

    f (CUnOp  "length" v) | CArray as <- v = CInt $ toInteger $ length as
                          | otherwise = CFail

    -- Use in 'all' desugaring.  mapAp = map ($())
    f (CUnOp  "mapAp$" (CArray vs)) = mkArr vs

    -- XXX Stricter than necessary?
    f (CApply (CPrim "cons$") v) | CArray [v1, CArray vs] <- v = CArray $ v1 : vs
                                   | CArray [_, va] <- v, isHNF va = CFail
--x                                 | isHNF v = CFail

    f e@(CUnOp "known$" _) = e  -- XXX Just leave it alone for now
    f e@(CUnOp "pre'[]'" _) = e  -- XXX Just leave it alone for now

    f e@(CUnOp "alloc$" _) = e  -- Just leave it alone
    f e@(CUnOp "read$" _)  = e  -- Just leave it alone
    f e@(CUnOp "write$" _) = e  -- Just leave it alone
    f e@(CUnOp "print$" _) = e  -- Just leave it alone
    f e@(CBinOp "in'+='" _ _)  = e
    f e@(CBinOp "in'-='" _ _)  = e
    f e@(CBinOp "in'*='" _ _)  = e
    f e@(CBinOp "in'/='" _ _)  = e

    -- Fully evaluated, and still no match
    f (CApply (CPrim op) a) | isNF a = unimplemented $ show (op, a)
    f e = composOpLam (underLambda flg) f e

    arith op (CInt i1) (CInt i2) = CInt $ op i1 i2
    arith _ _ _ = CFail  -- CWrong?
    
    cmp op (CInt i1) (CInt i2) | op i1 i2  = CInt i1
                               | otherwise = CFail
    cmp _ _ _ = CFail   -- CWrong?
{-
    cmpU op (CInt i1) (CInt i2) | op i1 i2  = CUnit
                                | otherwise = CFail
    cmpU _ _ _ = CFail   -- CWrong?
-}
    enum (CInt lo) (CInt hi) = cBar [CInt i | i <- [lo .. hi ]]
    enum _ _ = CFail


isNF :: Value -> Bool
isNF (CVar _) = False
isNF (HNF (CArray vs)) = all isNF vs
isNF (HNF _) = True
isNF _ = False

isHNF :: Value -> Bool
isHNF HNF{} = True
isHNF _ = False

mkArr :: [Value] -> Core
mkArr vs =
  let xs = take (length vs) $ newVars "x" $ fvs (CArray vs)
  in  CDef xs $ cSeq $ zipWith (\ x v -> CUnify (CVar x) $ CApply v CUnit) xs vs ++ [CArray $ map CVar xs]

-- A gruesome hack to test if something is an uninstantiated logical variable.
evalKnown :: EvalCore
evalKnown flg = evalTrace "evalKnown" f flg
  where
    f (CApply (CPrim "known$") v) | isHNF v   = CUnit
                                  | otherwise = CFail
    f e = composOp f e

-------------------

-- Until we get a proper prelude, just hack it.
-- XXX This doesn't really work, it only replaces in applications.
-- Could just put the prelude as a prefix to the program.
replPrelude :: EvalCore
replPrelude = evalTrace "replacePrelude" replacePrelude

replacePrelude :: Core -> Core
replacePrelude = f
  where
    f (CApply (CVar (Ident _ i)) v) | Just p <- lookup i prelude = f (CApply p v)
    f (CVar (Ident _ i)) | Just p <- lookup i prelude = CValue p
    f e = composOp f e

-- Functions that should be in a prelude.
prelude :: [(String, Value)]
prelude =
  [("any", typ [])                                           -- x => x
  ,("nat", typ [app "isInt$" vx, app2 "in'>='" vx (CInt 0)]) -- x => int#[x]; x>=0; x
  ,("int", typ [app "isInt$" vx])                            -- x => int#[x]; x
--  ,("int", CPrim "isInt$")
  ,("in'->'", arrowV)
  ,("false", CArray [])                                      -- ()
{-
  ,("in'>'",  cmpV "intGT$")
  ,("in'>='", cmpV "intGE$")
  ,("in'<'",  cmpV "intLT$")
  ,("in'<='", cmpV "intLE$")
-}
  ,("new", newV)
--  ,("pre'[]'", unimplemented "pre []")
--  ,("pre'^'", unimplemented "pre ^")
  ,("post'^'", CPrim "read$")
  ,("in'.='", CPrim "write$")
  ]
  where typ is = CLam x $ cSeq $ is ++ [CVar x]
        vx = CVar x
        app f v = CApply (CPrim f) v
        app2 f v1 v2 = CApply (CPrim f) (CArray [v1, v2])

        arrowV =
          CLam st $
            CDef [s, t] $
            CSeq [
              CUnify (CValue (CArray [CVar s, CVar t])) (CVar st),
              CLam g $ CLam y $
                CDef [sy, gsy] $
                CSeq [
                  app "isFcn$" (CVar g),
                  CUnify (CVar sy) (CApply (CVar s) (CVar y)),
                  CUnify (CVar gsy) (CApply (CVar g) (CVar sy)),
                  CApply (CVar t) (CVar gsy)
                  ]
              ]
        [st, s, t, g, y, sy, gsy, x, _xy] =
           map (Ident noLoc) ["st","s","t","g","y","sy","gsy","x", "xy"]

{-
        cmpV op =
          CLam xy $
            CDef [x, y] $
            CSeq [
              CUnify (CValue (CArray [CVar x, CVar y])) (CVar xy),
              app op (CVar xy),
              CVar x
              ]
-}
        newV =
          CLam t $ CLam x $
            CDef [y] $
            CSeq [
              CUnify (CVar y) (CApply (CVar t) (CVar x)),
              app "alloc$" (CVar y)
              ]

-- A special purpose composOp that can avoid going under lambda.
composOpLam :: Bool -> (Core -> Core) -> Core -> Core
composOpLam underLam f = composOp f'
  where
    f' e@CLam{} | not underLam = e
    f' e = f e

anf :: Core -> Core
anf = flip evalState (1::Int) . expr
  where
    expr e@CArray{} = val e
    expr (CApply e1 e2) = do
      (es1, v1) <- value e1
      (es2, v2) <- value e2
      defs (es1 ++ es2) (CApply v1 v2)
    expr e = compos expr e
    val e = do
      (es, v) <- value e
      defs es v
    value (CArray es) = do
      (ess, vs) <- unzip <$> mapM value es
      pure (concat ess, CArray vs)
    value e@CLam{} = ([],) <$> expr e
    value (CValue v) = pure ([], v)
    value e = do
      e' <- expr e
      i <- newIdent "b"
      pure ([(i, e')], CVar i)
    newIdent s = do
      i <- get
      put $! i+1
      pure $ Ident noLoc $ s ++ show i
    defs ies r =
      pure $ cDef (map fst ies) $ cSeq $ [ CUnify (CVar i) e | (i, e) <- ies ] ++ [r]

--------------------


-- Handles
--  REF-NEW REF-READ REF-WRITE
evalRef :: EvalCore
evalRef flg = evalTrace "evalRef" f flg
  where
    f (CStore s e) = CStore s' (f e') where (s', e') = st s e
    f e = composOp f e

    -- Reference ops
    st s (CApply (CPrim "alloc$") (HNF v)) = (s', CPtr p)  -- XXX HNF or CValue?
      where (s', p) = storeAlloc v s
    st s (CApply (CPrim "read$") (CPtr p)) = (s, CValue $ storeRead p s)
    st s (CApply (CPrim "write$") (CArray [CPtr p, HNF v])) = (s', CArray [])  -- XXX HNF or CValue?
      where s' = storeWrite p v s
    st s (CApply (CPrim "in'+='") (CArray [CPtr p, CInt j])) = asgOp s p (+) j
    st s (CApply (CPrim "in'-='") (CArray [CPtr p, CInt j])) = asgOp s p (-) j
    st s (CApply (CPrim "in'*='") (CArray [CPtr p, CInt j])) = asgOp s p (*) j
    st s (CApply (CPrim "in'/='") (CArray [CPtr p, CInt j])) | j == 0 = (s, CFail)
                                                             | otherwise = asgOp s p div j
    -- HACK
    st s (CApply (CPrim "print$") (HNF h)) =
       trace ("print$: " ++ prettyShow h) $
       (storePrint h s, CArray [])

    -- Recurse
    st s (CUnify e1 e2) =
      let (s', e1') = st s e1
          (s'', e2') = st s' e2
      in  if isSE e1' then
            (s'', CUnify e1' e2')
          else
            (s', CUnify e1' e2)
    st s (CDef h e) = (s', CDef h e') where (s', e') = st s e
    st as (CSeq aes) = loop as [] aes
      where loop s rs [] = (s, CSeq $ reverse rs)
            loop s rs (e:es) =
              let (s', e') = st s e
              in  if isSE e' then loop s' (e':rs) es else (s', CSeq $ reverse rs ++ e' : es)
    st s e = (s, e)

    asgOp s p op j =
      case storeRead p s of
        CInt i ->
          let v = CInt (i `op` j)
              s' = storeWrite p v s
          in  (s', CValue v)
        e -> error $ "asgOP " ++ show e

isSE :: Core -> Bool
isSE (CValue _) = True
isSE (CUnify e1 e2) = isSE e1 && isSE e2
isSE (CSeq es) = all isSE es
isSE (CDef _ e) = isSE e
isSE (CApply (CPrim p) (CValue _)) = not (isStorePrim p)
isSE _ = False

evalStore :: EvalCore
evalStore flg = evalTrace "evalStore" f flg
  where
    f (CStore s e) = CStore s' (f e') where (s', e') = st s e
    f e = composOp f e

    -- Duplicate store
    st s (CSplit e n g) | not (isDone e) = (s, CSplit (CStore s e) n g)
    st s (CBar e1 e2) | not (isDone e1) = (s, CBar (CStore s e1) e2)
    st _ COne{} = error "evalStore: found one{}"
    st _ CAll{} = error "evalStore: found all{}"
    st _ CSucceeds{} = error "evalStore: found succeeds{}"

    -- Commit store
    st _ (CSplit (CStore s e) n g) | isRes e = (s, CSplit e n g)
    st _ (CBar (CStore s e1) e2) | isRes e1 = (s, CBar e1 e2)

    -- Recurse
    st s (CUnify e1 e2) =
      let (s', e1') = st s e1
          (s'', e2') = st s' e2
      in  if isSE e1' then
            (s'', CUnify e1' e2')
          else
            (s', CUnify e1' e2)
    st s (CDef h e) = (s', CDef h e') where (s', e') = st s e
    st as (CSeq aes) = loop as [] aes
      where loop s rs [] = (s, CSeq $ reverse rs)
            loop s rs (e:es) =
              let (s', e') = st s e
              in  if isSE e' then loop s' (e':rs) es else (s', CSeq $ reverse rs ++ e' : es)
    st s e = (s, e)

    isRes (CValue _) = True
    isRes (CBar (CValue _) _) = True
    isRes _ = False

    isDone CFail = True
    isDone CStore{} = True
    isDone e = isRes e

isStorePrim :: String -> Bool
isStorePrim p = p `elem`
  [
   "alloc$", "read$", "write$"
  ,"in'+='" ,"in'-='" ,"in'*='" ,"in'/='"
  ,"print$"
  ,"in'..'"  -- XXX not really
  ,"mapAp"   -- XXX unknown
  ]

isStore :: Core -> Bool
isStore CStore{}  = True
isStore _ = False

noStoreIn :: Core -> Bool
noStoreIn = not . any isStore . universe

hasStoreOp :: Core -> Bool
hasStoreOp e = not $ null [ () | CApply (CPrim p) _ <- universe e, isStorePrim p ]

checkStoreConsistency :: EvalCore
checkStoreConsistency _ _ee = undefined
{-
  case execState (count ee) (0::Int,0::Int,0::Int) of
    c@(cStore, cSplitE, cBarE) | (cStore == 1 || cStore == 0 && cSplitE > 0) &&
                                 (cSplitE == 0 && cBarE == 0 || cSplitE > 0) -> ee
                               | otherwise -> error $ "checkStoreConsistency " ++ show c ++ "\n" ++ prettyShow ee
  where
    count (CStore s e) = do
      modify (\ (t,p,b) -> (t+1,p,b))
      CStore s <$> count e
    count (CSplitE s e f g) = do
      modify (\ (t,p,b) -> (t,p+1,b))
      CSplitE s <$> count e <*> count f <*> count g
    count (CBarE s e1 e2) = do
      modify (\ (t,p,b) -> (t,p,b+1))
      CBarE s <$> count e1 <*> count e2
    count e = compos count e
-}
