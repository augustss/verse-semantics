{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Rules.Verifier  where
import TRS.Bind
import TRS.TRS
import Rules.Core hiding (Wrong)
import Rules.ICFP (allSystemsICFP)
-- import Epic.Print
-- import qualified Verifier.Verify as V
-- import Control.Monad (guard, filterM)
-- import Data.List (intersect)
-- import qualified Rules.ICFP as ICFP
-- import qualified Verifier.FOL as FOL
-- import Data.Maybe (maybeToList)

trivVerifier :: TRSystem Expr
trivVerifier = icfpVerifier
  {
    rules  = rules icfpVerifier Prelude.<> contextFreeRules,
    rules2 = contextSensitiveRules
  }

icfpVerifier :: TRSystem Expr
icfpVerifier = base'
  where
    base     = head allSystemsICFP
    base'    = base { rules = rules base }


--------------------------------------------------------------------------------------
-- | The "Context" in which a subsumption must hold; Tim's "G" -- set of "known facts"
--------------------------------------------------------------------------------------

data QContext
  = QDef Ident QContext   -- ^ x; G
  | QAsm Expr  QContext   -- ^ e; G
  | QEmp                  -- ^ EMPTY
  deriving (Show)

--------------------------------------------------------------------------------
-- | Abstract Rules
--------------------------------------------------------------------------------
type VRule = Rule Expr

contextFreeRules :: VRule
contextFreeRules _ lhs = undefined
{-
  "A-EQN-ELIM" `TRSS.name`  -- duplicate of EQN-ELIM to go under EXI
  do EXI x a <- [lhs]
     (ctx, _, (Var x' :=: Vval _) :>: e) <- ICFP.execBX a
     guard (x == x')
     guard (x `notElem` free (ctx e))
     pure (ctx e, mempty)

  ++
  "A-LIT" `TRSS.name`
  do Int k <- [lhs]
     pure (aval (sngINT k), mempty)
  ++
  "A-LAM" `TRSS.name`
  do LAM x v@(AVAL _) <- [lhs]
     pure (TLAM x (aval aANY) v, mempty)
  ++
  "A-UNIFY" `TRSS.name`
  do (Vval (Base _p1 ixs1) :=: Vval (Base _p2 ixs2)) :>: e <- [lhs]
     let unifies = not (null (ixs1 `intersect` ixs2))
     let grd = case (ixs1, ixs2) of
                _ | unifies  -> TRUE
                (i1:_, i2:_) -> i1 .=. i2
                (_, _)       -> decides
     -- let grd | unifies  = TRUE
     --         | i1 <- i
     --         | otherwise = decides
     pure (Vasm grd :>: e, mempty)
  ++
  "A-ASM-TRUE" `TRSS.name`
  do Vasm TRUE :>: e <- [lhs]
     pure (e, mempty)
  ++
  "A-IF-TRUE" `TRSS.name`
  do Vif TRUE e <- [lhs]
     pure (e, mempty)
  ++
  -- "A-ASM-DEC" `TRSS.name`
  -- do Vasm p :>: Vval t <- [lhs]
  --    -- guard (p /= TRUE && p /= decides)
  --    guard (null (free p `intersect` free t))
  --    pure (Vasm decides :>: Vval t, mempty)
  -- ++
  "A-CHOOSE" `TRSS.name`
  do (AbsVal p1 t1 :|: AbsVal p2 t2) <- [lhs]
     pure (join (p1, t1) (p2, t2), mempty)
  ++
  "A-ITE" `TRSS.name`
  do If _ (AbsVal p1 t1) (AbsVal p2 t2) <- [lhs]
     pure (join (p1, t1) (p2, t2), mempty)
  ++
  "A-SUBST-TLAM" `TRSS.name`
  do TLAM x v@(AVAL _) e <- [lhs]
     let freeX = free e
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     pure (TLAM x v $ subst sub e, mempty)

{-
    [APP]   (\x. e) v   --> ex x. x = v; e

            (\(x:Pre). asm{p}; Post)

            1. CHECK v : Pre

            2. exist x. x = v; asm{p}; Post

-}

-}
-- NOTE: APP-RULE THIS IS FOR "round-bracket" application (which MUST succeed)

-- e is T ---> Te is T

contextSensitiveRules :: VRule
contextSensitiveRules _env lhs = undefined

{-
 "A-APP" `TRSS.name`
  do (ctx, g, (AFUN x t1 p t2) :@: AVAL t) <- execEX lhs
     let freeT = free t
     if x `notElem` freeT then
        -- no name clash
        pure (ctx (absBeta x t p t2), subsumes g t t1)
     else do
        -- The x has to be renamed to avoid capture
        let freeE = free t1 ++ free p ++ free t2
            x' = identNotIn (freeT ++ freeE)
            (t1', p', t2') = substI x x' (t1, p, t2)
        pure (ctx (absBeta x' t p' t2'), subsumes g t t1')
  ++
  "A-CONSEQ-R" `TRSS.name`
  do (ctx, g, (Vval t) `Vis` t') <- execEX lhs
     pure (ctx (aval t'), subsumes g (ABase t) t')

-}

{-
absBeta :: Ident -> AVal -> Form -> AVal -> Expr
absBeta x t p t2 = EXI x ((Var x :=: aval t) :>: (Vasm p :>: aval t2))

join :: (Form, AVal) -> (Form, AVal) -> Expr
join (p1, t1)   (p2, t2)   = Vasm (joinForm p1 p2) :>: aval (joinAVal t1 t2)

joinForm :: Form -> Form -> Form
joinForm TRUE  _     = TRUE
joinForm _     TRUE  = TRUE
joinForm FALSE p     = p
joinForm p     FALSE = p
joinForm p1    p2    = if p1 == p2 then p1 else p1 :||: p2

mergeForm :: Form -> Form -> Form
mergeForm TRUE  p     = p
mergeForm p     TRUE  = p
mergeForm FALSE _     = FALSE
mergeForm _     FALSE = FALSE
mergeForm p1    p2    = p1 :||: p2

joinAVal :: AVal -> AVal -> AVal
joinAVal (ABase (Base (Bind a1 p1) ixs1)) (ABase (Base (Bind a2 p2) ixs2))
  | a1 == a2 = aBase (Bind a1 (joinForm p1 p2)) (ixs1 `intersect` ixs2) -- TODO: name shift shenanigans
joinAVal (AFun (Bind a1 (s1, p1, t1))) (AFun (Bind a2 (s2, p2, t2)))
  | a1 == a2 = AFun (Bind a1 (joinAVal s1 s2, mergeForm p1 p2, joinAVal t1 t2))
joinAVal  _ _ = error "todo: joinAVal"

tLam :: Ident -> Expr -> Form -> Expr -> Expr
tLam x e1 TRUE e2 = TLam (Bind x (e1, e2))
tLam x e1 p    e2 = TLam (Bind x (e1, Vasm p :>: e2))

-}


----------------------------------------------------------------------
-- | Expression Contexts
-----------------------------------------------------------------------

-- scope contexts

execEX, execEX1 :: Expr -> [(EContext, QContext, Expr)]
-- E context
execEX lhs = execEX1 lhs ++ [(id, QEmp, lhs)]
-- X context, X /= hole
execEX1 lhs =
  do v :=: x     <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (\ a -> v :=: ctx a, g, hole)
 ++
  do x :>: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:>: e) . ctx, qAsm e g, hole)
 ++
  do e :>: x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((e :>:) . ctx, qAsm e g, hole)
 ++
  do EXI y x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (EXI y . ctx, QDef y g, hole)
 ++
--   do Vif p x <- [lhs]
--      (ctx, g, hole) <- execEX x
--      pure (Vif p . ctx, g, hole)
--  ++
--   do Vis x t <- [lhs]
--      (ctx, g, hole) <- execEX x
--      pure ((`Vis` t) . ctx, g, hole)
--  ++
  do x :@: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:@: e) . ctx, g, hole)
 ++
  do One x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (One . ctx, g, hole)
 ++
  do All x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (All . ctx, g, hole)
 ++
  do x :|: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:|: e) . ctx, g, hole)
 ++
  do e :|: x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((e :|:) . ctx, g, hole)
 ++
  do Lam (Bind y x) <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (Lam . Bind y . ctx, QDef y g, hole)
 ++
  do x :@: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:@: e) . ctx, g, hole)
 ++
  do e :@: x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((e :@:) . ctx, g, hole)

qAsm :: Expr -> QContext -> QContext
qAsm (Assume e)  g = QAsm e g
qAsm (e1 :>: e2) g = qAsm e1 (qAsm e2 g)
qAsm _           g = g
