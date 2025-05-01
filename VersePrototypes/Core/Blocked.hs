{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE RecordWildCards,MultiWayIf #-}

module Core.Blocked
  ( blocked, blkd, choiceFreeLH
  , LocalExis(..), makeRigid, addFlexi, allExis
  )
 where

import Core.Expr
import Core.Bind
import Epic.Print hiding ( (<>), empty )
import FrontEnd.Error( errorMessage )

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

OR (alternative defn, see MV7 Simon/Koen 9 Aug 24)
   * e cannot loop, or unify anything
       to the left to the HOLE
       except giving a value to at least one of the local existentials.

  exists x. x>3; y>3; x=2

Be careful! "or unify anything".  Must substitue y=3, not x=2 here:
  exists y. if(y=3) then loop() else (); exists x. x>3; y=3; x=2

---- Examples -----

Imagine HOLE is filled with (x=2) and we are considering substituing
that (x=2) throughout

E1:  exists x. x>3; HOLE                  (x>3) blocked because (x>3) is stuck on local exi x
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

data LocalExis = LX { exi_flexi :: [Ident]   -- Flexible existentials
                    , exi_rigid :: [Ident]   -- Rigid existentials
                    }

instance Pretty LocalExis where
  pPrint (LX { .. } ) = text "LX" <> braces( sep [ text "flx=" <> pPrint exi_flexi
                                                 , text "rig=" <> pPrint exi_rigid ])

allExis :: LocalExis -> [Ident]
allExis (LX { exi_flexi = flexi, exi_rigid = rigid }) = rigid ++ flexi


isLocal :: LocalExis -> Ident -> Bool
isLocal lx x = isFlexiLocal lx x || isRigidLocal lx x

isFlexiLocal :: LocalExis -> Ident -> Bool
isFlexiLocal (LX { exi_flexi = flexi }) x = isUnderscore x || x `elem` flexi

isRigidLocal :: LocalExis -> Ident -> Bool
isRigidLocal (LX { exi_rigid = rigid }) x = x `elem` rigid

addFlexi :: LocalExis -> Ident -> LocalExis
addFlexi (LX { exi_flexi = flexi, exi_rigid = rigid }) x
  = LX { exi_flexi = x:flexi, exi_rigid = rigid }

makeRigid :: LocalExis -> LocalExis
makeRigid (LX { exi_flexi = flexi, exi_rigid = rigid })
 = LX { exi_flexi = [], exi_rigid = rigid ++ flexi }

---------------------
data Status
  = SomethingToDo
    -- Something to do to the left of the hole, including calling
    -- functions, unifying variables from outside, failure, choice, loop
    -- Also stuck terms like 3[4]

  | BlockedOnExi HolePresent
    -- Everything to the left of the hole is stuck,
    -- awaiting the value of one of the LocalExis

  | NothingToDo  HolePresent
    -- Value(s) optionally with a hole to the right
    -- Key cases  status HOLE   = NothingToDo HasHole
    --            status <val>  = NothingToDo NoHole

-- Possible alternative; instead of (NothingToDo HolePresent), have
-- two constructors:   HoleStatus and ValueStatus, corresponding to
-- "two key cases" above.  Downside of that: would allow fewer reductions
--     exists x. x>3; 43; x=7
-- We probably don't really care about this.

data HolePresent = HasHole | NoHole

instance Pretty Status where
  pPrint SomethingToDo     = text "SomethingToDo"
  pPrint (BlockedOnExi hp) = text "BlockedOnExi" <> parens (pPrint hp)
  pPrint (NothingToDo hp)  = text "NothingToDo"  <> parens (pPrint hp)

instance Pretty HolePresent where
  pPrint HasHole = text "HasHole"
  pPrint NoHole  = text "NoHole"

blockedStatus, valueStatus :: Status
blockedStatus = BlockedOnExi NoHole
valueStatus   = NothingToDo  NoHole

andStatus :: Status -> Status -> Status
-- `andStatus` tries hard to be lazy in its second argument
andStatus SomethingToDo          _               = SomethingToDo
andStatus (NothingToDo HasHole)  _               = NothingToDo HasHole
andStatus (NothingToDo NoHole)   s               = s
andStatus (BlockedOnExi HasHole) _               = BlockedOnExi HasHole
andStatus (BlockedOnExi NoHole) (NothingToDo hp) = BlockedOnExi hp
andStatus (BlockedOnExi NoHole) s                = s

addSomethingToDo :: Status -> Status
-- If the inner thing has nothing further to do, and is not blocked,
-- and does not contain a HOLE, then we can do something;
-- otherwis just pass on the inner status
addSomethingToDo (NothingToDo NoHole) = SomethingToDo
addSomethingToDo s                    = s

addUnify :: Ident -> Status -> Status
addUnify x s
  | isUnderscore x = s                  -- Unifying with "_" is a no-op
  | otherwise      = addSomethingToDo s -- Otherwise substitute

---------------------------------------------------
blocked :: Expr_or_Context -> Bool
-- Returns True if everything to the left of the HOLE is stuck,
-- so the expression in the HOLE is really the next thing to do.
blocked = blkd (LX { exi_flexi = [], exi_rigid = []})

blkd :: LocalExis -> Expr_or_Context -> Bool
blkd lx e = case status lx e of
               BlockedOnExi _ -> True
               NothingToDo _  -> True
               SomethingToDo  -> False

---------------------------------------------------
status :: LocalExis -> Expr_or_Context -> Status
-- This is the main workhorse function
-- It returns the status of the expression /to the left of the HOLE/
-- (or all of it if there is no hole)

status _  HOLE = NothingToDo HasHole
  -- We want (exi x. x=3; blah) to substitute right away!

status _  e | isVal e = valueStatus

status _ (_ :=: (_ :>: _)) = SomethingToDo

status lx (Var x :=: Var y)  -- (x=x) is blocked pending getting a value for x
  | x == y, isLocal lx x
  = blockedStatus

status lx (Var x :=: rhs)
  | Var y <- rhs
  , isFlexiLocal lx y
  = addUnify y (NothingToDo NoHole)

  | isFlexiLocal lx x
  = addUnify x (status lx rhs)

  | isRigidLocal lx x
  = blockedStatus  -- See (E2)

  -- Otherwise the case (Var x :=: rhs), where x is not local,
  -- falls through to the following (val :=: rhs) case

status lx (_val :=: rhs)     -- e.g. x=blah, where x is bound "outside", or hnf=blah
  = addSomethingToDo (status lx rhs)

status lx (e1 :>: e2) = status lx e1 `andStatus` status lx e2

-- this should be removed once we remove All
--status lx (All body) = status lx (mkAll body)

status lx (Exi bnd) = status (addFlexi lx x) e
  where
    (x,e) = alphaRename (allExis lx) bnd

status lx (Op {} :@: arg)
  | blocked_on_local arg = blockedStatus
  | otherwise            = SomethingToDo
  where
    blocked_on_local :: Val -> Bool
    -- True if the value (a primop argument) mentions a locally-bound existential,
    -- but /not/ if that existential is only mentioned under a lambda
    blocked_on_local (Var x)  = isLocal lx x
    blocked_on_local (Tup vs) = any blocked_on_local vs
    blocked_on_local (Tru v)  = blocked_on_local v
    blocked_on_local _        = False   -- In particular do not look inside lambdas

status lx (Var f :@: _arg)
  | isLocal lx f = blockedStatus   -- exists x. x[3]; x=(\y.y)
  | otherwise    = SomethingToDo

status _lx (_hnf :@: _arg)
  = SomethingToDo

status _  Fail        = SomethingToDo
status _  (Err _)     = SomethingToDo
status _  (e1 :|: e2) = BlockedOnExi (if isContext (e1 :|: e2) then HasHole else NoHole)
  -- status lx e1 `andStatus` status lx e2
  -- We must skolemise in verify(){check<succeeds>{ (x=some{t}; blah) | more-blah }}
  --               and in verify(){check<succeeds>{ v | (x=some{t}; blah) }}

status lx (Iter _ e _)
  | isVal e   = SomethingToDo
  | otherwise =
  case status (makeRigid lx) e of
    SomethingToDo             -> SomethingToDo
    _ | choicy e == Just True -> SomethingToDo
    NothingToDo hasHole       -> NothingToDo hasHole -- or BlockedOnExi??
    BlockedOnExi hasHole      -> BlockedOnExi hasHole

status _ (Verify {})
  = NothingToDo NoHole   -- There should be no HOLE inside a verify{}
  -- We need this for
  --    exi f.  verify{ ...f...}; f = \x.some(int)
  --
  -- Earlier version looked inside verify{}
  --    (_, (_,e)) = alphaRenameVerify (allExis lx) bl

--status lx (Check _ e) = SomethingToDo
status _  (Choose {}) = NothingToDo NoHole
--status lx (Size _ e)  = status lx e
status lx (Some v) | any (isLocal lx) (free v) = blockedStatus
                   | otherwise                 = SomethingToDo
status lx (v :>>: e) | any (isLocal lx) (free v) = blockedStatus
                     | otherwise                 = status lx e

status _ e = errorMessage ("Uncovered case in status " ++ show e)


-- choicy e returns Just True  if e = C[e1|e2] and C is choicefreeLH
-- choicy e returns Nothing    if e = C[e1|e2] and C is NOT choicefreeLH
-- choicy e returns Just False if e /= C[e1|e2]
choicy :: Expr -> Maybe Bool
choicy (Op{} :@: _) = Just False
choicy (_ :@: _)    = Nothing
choicy (_ :|: _)    = Just True
choicy (Exi bnd)    = choicy e where (_,e) = unsafeUnbind bnd
choicy (_ :=: e)    = choicy e
choicy (e1 :>: e2)
  | isContext e1    = choicy e1
  | otherwise       = choicy e1 >>= \c -> if c then Just True else choicy e2
choicy _            = Just False

{-
---------------------
-- Koen: this function is not used anywhere AFAIK

choiceAndFailureFree :: Expr_or_Context -> Bool
-- No choices or failure anyhere, to the left or to
-- the right of the hole
choiceAndFailureFree orig_e = go [] [orig_e]
  where
    -- all OK
    go _  []                 = True
    go vs (v : es) | isVal v = go vs es
    go vs ((e1 :>: e2)  :es) = go vs (e1:e2:es)
    go vs ((_ :>>: e)   :es) = go vs (e:es)
    go vs (Some {}      :es) = go vs es
    go vs (Verify {}    :es) = go vs es
    go vs (HOLE         :es) = go vs es

    -- analyze
    go vs (Exi bnd      :es) = go (x:vs) (e:es) where (x,e) = alphaRename (free es) bnd
    go vs ((Var x :=: e):es) = x `elem` vs && x `notElem` free e && go (vs \\ [x]) (e:es)
    go vs ((Op op :@: _):es) = not (primOpCanFail op) && go vs es
    go vs (Iter f _ e0  :es) = iterChoiceFree f && go vs (e0:es)

    -- all potentially bad
    go _  ((_ :=: _)    :_ ) = False
    go _  ((_ :@: _)    :_ ) = False
    go _  (Choose {}    :_ ) = False
    go _  ((_ :|: _)    :_ ) = False
    go _  (Fail         :_ ) = False

    -- impossible
    go _ e = error ("impossible: choiceAndFailureFree " ++ show e) 
-}

-- TODO: This function can be simplified now, the extra "fs" in choiceFree'
-- is not used anymore
choiceFreeLH :: Expr_or_Context -> Bool
-- (choiceFree ctx) means no choices to the left of the HOLE
-- or, if no HOLE, anywhere
choiceFreeLH = choiceFree' []

-- The first argument to choiceFree' are functions (thunks) known to be choice free.
-- This is used for the iter construct.  In the case where iter(e){f;g}
-- calls f, the thunk argument will have the same effects as e&f&g
-- could have.  We can safely assume that the thunk is choice free,
-- because if it's not this will already show up in the bodies of f and/or g.
choiceFree' :: [Ident]           -- These functions are known to be choice-free
            -> Expr_or_Context
            -> Bool
choiceFree' _  (_ :|: _)           = False
choiceFree' fs ((_ :=: e1) :>: e2) = choiceFree' fs e1 && (isContext e1 || choiceFree' fs e2)
choiceFree' fs (_ :>>: e)          = choiceFree' fs e
choiceFree' fs (Exi bnd)           = choiceFree' fs e where (_,e) = unsafeUnbind bnd
choiceFree' fs (v1 :@: _)          = case v1 of
                                       Op DotDot -> False
                                       Op ArrMap -> False
                                       Op _      -> True  -- all other ops are choice-free
                                       Var f     -> f `elem` fs
                                       _         -> False -- may or may not be choice free
choiceFree' fs (Iter f e e0)       = iterChoiceFree f && choiceFree' fs e && choiceFree' fs e0
choiceFree' _ (Some {})            = True
--choiceFree' _ (All {})             = True
choiceFree' _ (Arr {})             = True
--choiceFree' _ (Size {})            = True
choiceFree' _ (Choose {})          = False
choiceFree' _  _                   = True
