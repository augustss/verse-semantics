{-# LANGUAGE MultiWayIf #-}

module Rules.TRS2024 (
     evalRules, evalStep, recStep
   , blocked, choiceFree
   , name, nameWith, iff
   , skolValue
 ) where

import Prelude

import TRS.Bind
import Rules.Core
import Epic.Print hiding ( (<>) )
import FrontEnd.Error

import Control.Monad( guard )
import Data.List( (\\) )

--------------------------------------------------------------------------------
--
--            The rules themselves
--
--------------------------------------------------------------------------------

evalRules :: Rule
evalRules = everywhere evalStep <> everywhere recStep

-- NB: (everywhere (evalStep <> recStep) does not work.
-- Because evalStep tries top-level single step; we don't want to
-- go off into recStep just becuase there is nothing to do at outermost
-- level.    eg.  exists f. (f = \x. ..f..); f[3]

evalStep :: Rule
-- Runtime evauation rules
evalStep = applicationStep
           <> arrayOpStep
           <> unificationStep
           <> existentialStep
           <> normalizationStep
           <> choiceStep
           <> oneAndAllStep
           <> checkStep

-- currently:  everywhere (evalRulesNoRec `tryBefore` rulesRec)
-- better:    (everywhere evalRulesNoRec) `tryBefore` (everywhere rulesRec)

--------------------------------------------------------------------------------
applicationStep :: Rule
applicationStep _env lhs =
  "APP-ADD" `name`
  do Op Add :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     pure (LitInt (k1+k2))
 ++
  "APP-SUB" `name`
  do Op Sub :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     pure (LitInt (k1-k2))
 ++
  "APP-MUL" `name`
  do Op Mul :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     pure (LitInt (k1*k2))
 ++
  "APP-DIV" `name`
  do Op Div :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k2 /= 0)
     pure (LitInt (k1 `div` k2))
 ++
  "APP-NEG" `name`
  do Op Neg :@: LitInt k <- [lhs]
     pure (LitInt (- k))
 ++
  "APP-GT" `name`
  do Op Gt :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k1 > k2)
     pure (LitInt k1)
 ++
  "APP-GT-FAIL" `name`
  do Op Gt :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (not (k1 > k2))
     pure Fail
 ++
  "APP-LT" `name`
  do Op Lt :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k1 < k2)
     pure (LitInt k1)
 ++
  "APP-LT-FAIL" `name`
  do Op Lt :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (not (k1 < k2))
     pure Fail
 ++
  "APP-LE" `name`
  do Op LEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k1 <= k2)
     pure (LitInt k1)
 ++
  "APP-LE-FAIL" `name`
  do Op LEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (not (k1 <= k2))
     pure Fail
 ++
  "APP-GE" `name`
  do Op GEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k1 >= k2)
     pure (LitInt k1)
 ++
  "APP-GE-FAIL" `name`
  do Op GEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (not (k1 >= k2))
     pure Fail
 ++
  "APP-NE" `name`
  do Op NEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k1 /= k2)
     pure (LitInt k1)
 ++
  "APP-NE-FAIL" `name`
  do Op NEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (not (k1 /= k2))
     pure Fail
 ++
  "APP-ISINT" `name`
  do Op IsInt :@: a <- [lhs]
     case a of
       Lit (LInt _)  -> pure a
       _ | isHNF a   -> pure Fail  -- Lambda, tuples, floats etc all fail
         | otherwise -> []
 ++
  "APP-ISSTR" `name`
  do Op IsStr :@: a <- [lhs]
     case a of
       Lit (LStr _)  -> pure a
       _ | isHNF a   -> pure Fail  -- Lambda, tuples, floats etc all fail
         | otherwise -> []
 ++
  "APP-ISCHAR" `name`
  do Op IsChar :@: a <- [lhs]
     case a of
       Lit (LChar _) -> pure a
       _ | isHNF a   -> pure Fail  -- Lambda, tuples, floats etc all fail
         | otherwise -> []
 ++
  "APP-ISARR" `name`
  do Op IsArr :@: a <- [lhs]
     case a of
       Arr {}        -> pure a
       _ | isHNF a   -> pure Fail  -- Lambda, ints, floats etc all fail
         | otherwise -> []
 ++
  "APP-ISCOMP" `name`
  do Op IsComp :@: a <- [lhs]
     if | isComparable a -> pure a
        | isHNF a        -> pure Fail
        | otherwise      -> []
 ++
  "APP-ISTRU" `name`
  do Op IsTru :@: a <- [lhs]
     case a of
       Tru {}        -> pure a
       _ | isHNF a   -> pure Fail  -- Lambda, ints, floats etc all fail
         | otherwise -> []
 ++
  "APP-LAM" `name`
  do Lam bnd :@: v <- [lhs]
     guard (isVal v)
     let (x,e) = alphaRename (free v) bnd
         body = (Var x :=: v) :>: e
     pure (if isUnderscore x
           then body
           else Exi (bind x body))

arrayOpStep :: Rule
arrayOpStep _env lhs =
  "APP-TUPK" `name`   -- This rule isn't needed, but it makes the reduction sequence much shorter
                      -- when indexing with a constant
  do Arr vs :@: Lit (LInt i) <- [lhs]
     guard (all isVal vs)
     let i' = fromInteger i
     if 0 <= i' && i' < length vs then
       pure (vs !! i')
      else
       pure Fail
 ++
  "APP-TUP" `name`
  do Arr vs@(_:_) :@: v <- [lhs]
     guard (isVal v && all isVal vs)
     pure (foldr1 (:|:) [ (v :=: LitInt i) :>: vi | (i,vi) <- [0..] `zip` vs ])
 ++
  "APP-TRU" `name`
  do Tru a :@: v <- [lhs]
     guard (isVal v && isVal a)
     pure ((v :=: a) :>: a)
 ++
  "APP-TUP-0" `name`
  do Arr [] :@: v <- [lhs]
     guard (isVal v)
     pure Fail
 ++
  "APP-LENGTH" `name`
  do Op ArrLen :@: Arr xs <- [lhs]
     pure (LitInt (fromIntegral (length xs)))
 ++
  "APP-DOTDOT" `nameWith`
  do Op DotDot :@: Arr [Lit (LInt k1), Lit (LInt k2)] <- [lhs]
     pure (pPrint (k1,k2), foldr ((:|:) . Lit . LInt) Fail [k1..k2])
 ++
  "APP-ARRAPP" `name`
  do { Op ArrApp :@: Arr [e1,e2,res] <- [lhs]
     ; (do { Arr vs1 <- [e1]; Arr vs2 <- [e2]; pure $ equateArr res (vs1++vs2) })
     ++
       (do { Just (ls,vs2) <- [dropEqualPrefix e1 res]; pure $ foldr (:>:) (equateArr e2 vs2) ls })
     ++
       (do { Just (ls,vs1) <- [dropEqualSuffix e2 res]; pure $ foldr (:>:) (equateArr e1 vs1) ls }) }

equateArr :: Expr -> [Val] -> Expr
-- (equateArr e vs)  returns  (Arr vs = e; Arr vs)
-- It duplicates (Arr vs), but the rewrite engine will
-- do that anyway even if we exi-bind it here
equateArr e vs = (Arr vs :=: e) :>: Arr vs

dropEqualPrefix :: Expr -> Expr -> Maybe ([Expr],[Val])
dropEqualPrefix (Arr vs1) (Arr vs2) = drop_prefix vs1 vs2
dropEqualPrefix _         _         = Nothing

dropEqualSuffix :: Expr -> Expr -> Maybe ([Expr],[Val])
dropEqualSuffix (Arr vs1) (Arr vs2) = case (drop_prefix (reverse vs1) (reverse vs2)) of
                                        Nothing      -> Nothing
                                        Just (ls,vs) -> Just (reverse ls, reverse vs)
dropEqualSuffix _         _         = Nothing

drop_prefix :: [Val] -> [Val] -> Maybe ([Expr],[Val])
-- (drop_prefix xs (ys+zs)) =  (x1=y1; ..; xn=yn), zs
-- Strips an initial prefix of xs from ys
drop_prefix []     ys     = Just ([], ys)
drop_prefix (x:xs) (y:ys) = fmap (\(ls,vs) -> (x:=:y : ls, vs)) $
                            drop_prefix xs ys
drop_prefix (_:_)  []     = Nothing

--------------------------------------------------------------------------------
unificationStep :: Rule
unificationStep _env lhs =
  "U-LIT" `name`
  do (Lit l1 :=: Lit l2) :>: e <- [lhs]
     guard (l1 == l2)
     pure e
 ++
  "U-TUP" `name`
  do (Arr vs :=: Arr vs') :>: e <- [lhs]
     guard (length vs == length vs')
     pure (foldr (:>:) e [ v :=: v' | (v,v') <- vs `zip` vs' ])
 ++
  "U-TRU" `name`
  do (Tru v :=: Tru v') :>: e <- [lhs]
     pure (( v :=: v') :>: e)
 ++
  "U-FAIL" `name`
  do (a1 :=: a2) :>: _ <- [lhs]
     guard (isHNF a1 && isHNF a2)
     guard $
       case (a1, a2) of
         (Lit l1, Lit l2)  -> l1 /= l2
         (Arr vs, Arr vs') -> length vs /= length vs'
         (Tru _,  Tru _)   -> False
         (_,      _)       -> True
     pure Fail
 ++
  "U-OCCURS" `name`
  do (x@(Var _) :=: v) :>: _ <- [lhs]
     guard (v /= x)
     (_ctx, e) <- valueCtx v
     guard (e == x)
     pure Fail
 ++
  "U-SWAP" `name`
  do (a :=: Var x) :>: e <- [lhs]
     guard (isHNF a)
     pure ((Var x :=: a) :>: e)

--------------------------------------------------------------------------------
existentialStep :: Rule
existentialStep _env lhs =
  "UNDERSCORE-ELIM" `name`
  do { (Var u :=: v) :>: e <- [lhs]
     ; guard (isUnderscore u)
     ; guard (isVal v)
     ; pure e }
 ++
  "EXI-ELIM" `nameWith`
  do (exis,x,e) <- matchExi_alphaRename [] lhs
     guard (x `notElem` free e)
     pure (pPrint x, exis <@ e)
 ++
  "EXI-FLOAT" `nameWith`
  do (v :=: exi_x_e1) :>: e2 <- [lhs]
     (exis,x,e1) <- matchExi_alphaRename (free (v,e2)) exi_x_e1
     pure (pPrint x, Exi (bind x ((v:=:(exis <@ e1)):>:e2)))


 ++  -- EXI-PUSH is necessary to let us do
     --    exi x. f[y]; x=3; 3+1; blah
     -- Here we want to substitute for x despite the intervening f[y]
  "EXI-PUSH" `nameWith`
  do (exis,x,(v :=: e1) :>: e2) <- matchExi_alphaRename [] lhs
     guard (x `notElem` free (v,e1))
     pure (pPrint x, exis <@ ((v :=: e1) :>: Exi (bind x e2)))

  ++
  -- Do this last: most complex and expensive
  "EXI-SUBST" `nameWith`
  do (exis, ctx, x_eq_v :>: e) <- evalCtxLift (free lhs) lhs
     (Var x,v) <- matchEq x_eq_v
     guard (isVal v)
     guard (x `elem` exis)
     guard (x `notElem` free v)
     guard (blkd (LX { exi_flexi = exis, exi_rigid = [] }) ctx)
     pure ( pPrint x <+> text ":=" <+> pPrintSmallExpr v
          , wrapExis (exis \\ [x]) $
            subst [(x,v)] (ctx <@ e) )

--------------------------------------------------------------------------------
normalizationStep :: Rule
normalizationStep _env lhs =
  "SEQ-ASSOC" `name`
  do (v2 :=: ((v1 :=: e1) :>: e2)) :>: e3 <- [lhs]
     pure ((v1 :=: e1) :>: ((v2 :=: e2) :>: e3))

--------------------------------------------------------------------------------
choiceStep :: Rule
choiceStep _env lhs =
{-
  "CHOICE-ASSOC" `name`
  do (e1 :|: e2) :|: e3 <- [lhs]
     pure (e1 :|: (e2 :|: e3))
 ++
-}
  -- CHOICE-FAIL-L and CHOICE-FAIL-R should not be needed,
  -- but some dubious verification tests don't pass without them.
  "CHOICE-FAIL-L" `name`
  do Fail :|: e <- [lhs]
     pure e
 ++
  "CHOICE-FAIL-R" `name`
  do e :|: Fail <- [lhs]
     pure e
 ++
  "CHOICE" `name`
  do (ctx, e1 :|: e2) <- evalCtx [] lhs
     guard (ctx /= HOLE)
     guard (choiceFree ctx)
     guard (blocked ctx)
     pure ((ctx <@ e1) :|: (ctx <@ e2))
 ++
  "FAIL" `name`
  do (ctx, Fail) <- evalCtx [] lhs
     guard (ctx /= HOLE)
     guard (blocked ctx)
     pure Fail

--------------------------------------------------------------------------------
oneAndAllStep :: Rule
oneAndAllStep _env lhs =
  -- iter(fail,    u){f, g}  -->  g u
  "ITER-FAIL" `name`
  do Iter Fail u _ g <- [lhs]
     guard (isVal u)            -- XXX Maybe bind 'u' if it's not a value?
     pure (g :@: u)
 ++
  -- iter(v,       u){f, g}  -->  f u v g
  "ITER-VALUE" `name`
  do Iter v u f g <- [lhs]
     guard (isVal v)
     guard (isVal u)            -- XXX Maybe bind 'u' if it's not a value?
     let f1 = identNotIn $ free lhs
         f2 = identNotIn $ f1 : free lhs
         res = Exi $ bind f1 $
               Exi $ bind f2 $
               (Var f1 :=: (f :@: u)) :>:
               ((Var f2 :=: (Var f1 :@: v)) :>:
                (Var f2 :@: g))
     pure res
 ++
  -- iter(e1 | e2, u){f, g}  -->  iter(e1, u){f, \ x . iter(e2, x){f, g} }
  "ITER-CHOICE" `name`
  do Iter (e1 :|: e2) u f g <- [lhs]
     let x = identNotIn $ free lhs
         res = Iter e1 u f (Lam $ bind x $ Iter e2 (Var x) f g)
     pure res

recStep :: Rule
-- x=V[\y.body]  --> x = V[\y. exists x. x=V[\y.body]; body]
--   if x/=y, and x free in body
recStep _env lhs =
  "REC" `name`
  do Var x :=: v <- [lhs]
     (ctx, Lam bnd) <- valueCtx v
     let (y,body) = alphaRename [x] bnd
     guard (x `elem` free body)
     pure (Var x :=: ctx <@ (Lam $ bind y $
                             Exi $ bind x $
                             (Var x :=: v) :>: body) )

--------------------------------------------------------------------------------
checkStep :: Rule
checkStep env lhs =
   "CHECK-SUC" `name`
   do Check eff v <- [lhs]
      guard (skolValue (skolVars env) v)
      guard (canSucceed eff)
      pure v
   ++
   "CHECK-FAIL" `name`
   do Check eff Fail <- [lhs]
      guard (canFail eff)
      pure Fail

skolValue :: [SkolIdent] -> Expr -> Bool
-- A value whose only free vars are skolems
skolValue rs e = isVal e && null (free e \\ rs)

--------------------------------------------------------------------------------
--
--            Auxiliary functions
--
--------------------------------------------------------------------------------

name :: String -> [Expr] -> [(String,Expr)]
name s es = [ (s,e) | e <- es ]

-- This is used to give rules names.
nameWith :: String -> [(Doc, a)] -> [(String, a)]
nameWith rulename as = [(rulename ++ render (parens doc), a) | (doc, a) <- as]

iff :: [Bool] -> [()]
iff conds = [()| and conds]


--------------------------------------------------------------------------------
--
--            Value contexts
--
--------------------------------------------------------------------------------

{-
isV :: Expr -> Expr -> Bool
-- (isV x e) returns True if  e = < ..., < ..., x, ...>, ... >
isV x e = x==e || case e of
                    Arr es -> any (isV x) es
                    _      -> False
-}
valueCtx :: Expr -> [(Context, Expr)]
-- V ::= HOLE | <e1,..,V,..en>
-- Moreover we only return pairs whose Expr is /not/ a tuple
valueCtx e
  = [(HOLE,e)]
  ++
   do Arr es <- [e]
      i <- [0..(length es - 1)]
      let ei = es !! i
      (ctx,h) <- valueCtx ei
      pure (Arr (take i es ++ [ctx] ++ drop (i+1) es), h)
  ++
   do Tru a <- [e]
      (ctx,h) <- valueCtx a
      pure (Tru ctx, h)

--------------------------------------------------------------------------------
--
--            Evaluation contexts
--
--------------------------------------------------------------------------------

evalCtx :: [Ident] -> Expr -> [(Context, Expr)]
evalCtx zs lhs =
  do pure (HOLE, lhs)
 ++
  do Exi bnd <- [lhs]
     let (x,e) = alphaRename zs bnd
     (ctx, h) <- evalCtx (x:zs) e
     pure (Exi (bind x ctx), h)
 ++
  do (v :=: e1) :>: e2 <- [lhs]
     (ctx, h) <- evalCtx zs e1
     pure ((v :=: ctx) :>: e2, h)
 ++
  do (v :=: e1) :>: e2 <- [lhs]
     (ctx, h) <- evalCtx zs e2
     pure ((v :=: e1) :>: ctx, h)

evalCtxLift :: [Ident] -> Expr
            -> [( [Ident]  -- All the 'exists x' bits
                , Context  -- All the other bits
                , Expr)]   -- The expression in the middle
-- E.g.   evalCtxtLift (exi x. x=3; exi y. y=5; x+y)
--        returns  ( [x,y]
--                 , x=3; y=5; HOLE
--                 , x+y )
evalCtxLift zs lhs =
  do pure ([], HOLE, lhs)
 ++
  do Exi bnd <- [lhs]
     let (x,e) = alphaRename zs bnd
     (exis, ctx, h) <- evalCtxLift (x:zs) e
     pure (x:exis, ctx, h)
 ++
  do (v :=: e1) :>: e2 <- [lhs]
     (exis, ctx, h) <- evalCtxLift zs e1
     pure (exis, (v :=: ctx) :>: e2, h)
 ++
  do (v :=: e1) :>: e2 <- [lhs]
     (exis, ctx, h) <- evalCtxLift zs e2
     pure (exis, (v :=: e1) :>: ctx, h)

wrapExis :: [Ident] -> Expr -> Expr
wrapExis xs orig_e = foldr wrap orig_e xs
  where
    wrap x e = Exi (bind x e)


--------------------------------------------
--
--            The 'blocked' and 'choice-free' predicates
--
--------------------------------------------------------------------------------

{- Note [Blocked]
~~~~~~~~~~~~~~~~~
"blocked(e)" means "blocked on a local existential"
   * e is not a HNF, AND
   * e can only take a reduction step
       to the left of the HOLE (if any)
       by giving a value to at least one of the local existentials.

OR (see MV7 Simon/Koen 9 Aug 24)
   * e cannot loop, or unify anything
       to the left to the HOLE
       except giving a value to at least one of the local existentials.

  exists x. x>3; y>3; x=2

Be careful! "or unify anything"
  exists x. x>3; y=3; x=2
  if(y=3) then loop() else (); exists x. x>3; y=3; x=2

---- Examples -----

Imagine HOLE is filled with (x=2) and we are considering substituing
that (x=2) throughout

E1:  exists x. x>3; HOLE                  (x>3) blocked because local exi x is free
E1a: exists x. y>3; HOLE                  (y>3) NOT blocked because y is not "local exi"
     where y is bound "outside"
     by lamba, or an existential

E2: exists x. (if(x=3) then e1 else e2);  (x=3) is blocked because local exi x
              HOLE                        is rigid under the 'if'

E2a: exists x. (if(y=3) then e1 else e2);  (y=3) is NOT blocked because we are waiting
               HOLE                        for the "outside" to give us a value of y

E2b: exists x. x=3; HOLE                   (x=3) is NOT blocked because local exi x

E2c: exists x. (if(x>3) then e1 else e2);  (x>3) is blocked because local exi x
              HOLE                         is rigid under the 'if'

E3: exists x. x>3; 7; HOLE                 7 is blocked; it's fine to substitute
                                           across the 7
    NB: in more complicated cases the "7" might not go away, eg
        exists x. all{ x>0; 7 }; x=2

E3a.  exists x. x>3; 1=2; x=2

E4: exists x. if (x>1) then loop(); fail; HOLE    'fail' is not blocked;
                                                  don't substitute across it

E5: exists x. if (x>1) then loop(x); y>3; HOLE     (y>3) is not blocked, because the "outside"
    where y is bound outside,                      may give it a value; then we might fail
          by lambda or existential                 instead of calling loop(2)

E6: exists x. x>3; if (y>1) then loop(x); HOLE     (y>1) is not blocked, because the "outside"
    where y is bound outside,                      may give it a value; then we might fail
          by lambda or existential                 instead of calling loop(2)

-----------------
Question (with Koen): could we simplify `blocked` by moving existentials around
-}

type Expr_or_Context = Expr

blocked :: Expr_or_Context -> Bool
blocked ec = blkd (LX { exi_flexi = [], exi_rigid = []}) ec

data LocalExis = LX { exi_flexi :: [Ident]   -- Flexible existentials
                    , exi_rigid :: [Ident]   -- Rigid existentials
                    }

allExis :: LocalExis -> [Ident]
allExis (LX { exi_flexi = flexi, exi_rigid = rigid }) = rigid ++ flexi


isLocal :: LocalExis -> Ident -> Bool
isLocal lx x = isFlexiLocal lx x || isRigidLocal lx x

isFlexiLocal :: LocalExis -> Ident -> Bool
isFlexiLocal (LX { exi_flexi = flexi }) x = x `elem` flexi

isRigidLocal :: LocalExis -> Ident -> Bool
isRigidLocal (LX { exi_rigid = rigid }) x = x `elem` rigid

addFlexi :: LocalExis -> Ident -> LocalExis
addFlexi (LX { exi_flexi = flexi, exi_rigid = rigid }) x
  = LX { exi_flexi = x:flexi, exi_rigid = rigid }

makeRigid :: LocalExis -> LocalExis
makeRigid (LX { exi_flexi = flexi, exi_rigid = rigid })
 = LX { exi_flexi = [], exi_rigid = rigid ++ flexi }

blkd :: LocalExis -> Expr_or_Context -> Bool
-- See Note [Blocked] for what this function means
-- In the Context case (i.e. Expr has a HOLE), look only to the left of the HOLE
-- SLPJ: need to update the document to reflect this function
blkd _  HOLE        = True

blkd _  e | isVal e = False   -- See (E3)

blkd lx (Var x :=: Var y) | x == y
                          = isFlexiLocal lx x
blkd lx (Var x :=: e)     = isRigidLocal lx x -- See (E2)
                          || blkd lx e      -- Blocked if *either* side is blocked
blkd lx (hnf :=: e)       = assert (isHNF hnf) (show hnf) $
                            blkd lx e

blkd lx (e1 :>: e2)
  | isVal e2        = blkd lx e1
  | otherwise       = blkd lx e1 && (isContext e1 || blkd lx e2)  -- If HOLE is in e1, ignore e2
blkd lx (e1 :|: e2) = blkd lx e1 && (isContext e1 || blkd lx e2)  -- SLPJ: check

blkd lx (Exi bnd)   = blkd (addFlexi lx x) e where (x,e) = alphaRename (allExis lx) bnd
blkd lx (v1 :@: v2) = case v1 of
                        Var f -> isLocal lx f                -- Needed for (E2)!
                        Op {} -> any (isLocal lx) (free v2)  -- See (E1)
                        _     -> False

blkd _  (Verify _)  = True
blkd lx (Check _ e) = blkd lx e
blkd lx (Some v)    = any (isLocal lx) (free v)
blkd lx (v :>>: _)  = any (isLocal lx) (free v)

blkd _  Fail        = False

blkd lx (Iter e1 _e2 _ _) = blkd (makeRigid lx) e1 -- && blkd lx e2

blkd _ e = errorMessage ("Uncovered case in blkd " ++ show e)

---------------------
choiceFree :: Expr_or_Context -> Bool
-- (choiceFree ctx) means no choices to the left of the HOLE
-- or, if no HOLE, anywhere
choiceFree = choiceFree' []

-- The first argument to choiceFree' are functions known to be choice free.
-- This is used for the iter construct.  In the case where iter(e){u;f;g}
-- calls f, the continuation argument will have the same effects as e&f&g
-- could have.  We can safely assume that the continuation is choice free,
-- because if it's not this will already show up in the bodies of f and/or g.
choiceFree' :: [Ident] -> Expr_or_Context -> Bool
choiceFree' _  (_ :|: _)           = False
choiceFree' fs ((_ :=: e1) :>: e2) = choiceFree' fs e1 && (isContext e1 || choiceFree' fs e2)
choiceFree' fs (_ :>>: e)          = choiceFree' fs e
choiceFree' fs (Exi bnd)           = choiceFree' fs e where (_,e) = unsafeUnbind bnd
choiceFree' fs (v1 :@: _)          = case v1 of
                                       Op DotDot -> False
                                       Op _      -> True  -- all other ops are choice-free
                                       Var f     -> f `elem` fs
                                       _         -> False -- may or may not be choice free
choiceFree' fs e@Iter{} | Just (_, _, (_, _, c, f), (_, g)) <- unIter e
                                   = choiceFree' (c:fs) f && choiceFree' fs g
choiceFree' _  Iter{}              = error "Malformed Iter"
choiceFree' _  _                   = True
