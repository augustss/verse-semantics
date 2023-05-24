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
import qualified TRS.TRSS as TRSS
import qualified TRS.TRS as TRS
import Rules.Core hiding (Wrong)
import Rules.ICFP (allSystemsICFP)
import Epic.Print
import qualified Verifier.Verify as V
import Control.Monad (guard, filterM)
import Data.List (intersect)
import qualified Rules.ICFP as ICFP
import qualified Verifier.FOL as FOL
import Data.Maybe (maybeToList)

trivVerifier :: TRSS.TRSystem VC Expr
trivVerifier = icfpVerifier
  {
    TRSS.rules  = TRSS.rules icfpVerifier Prelude.<> contextFreeRules,
    TRSS.rules2 = contextSensitiveRules
  }

icfpVerifier :: TRSS.TRSystem VC Expr
icfpVerifier = liftSystem base'
  where
    base     = head allSystemsICFP
    base'    = base { TRS.rules = TRS.rules base }

--------------------------------------------------------------------------------
-- | Abstract Rules
--------------------------------------------------------------------------------
type VC    = ()
type VRule = TRSS.Rule VC Expr

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
  do Vif p x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (Vif p . ctx, g, hole)
 ++
  do Vis x t <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((`Vis` t) . ctx, g, hole)
 ++
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
 ++
  do TLAM y e1 x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (TLAM y e1 . ctx, qAsm (Var y :=: e1) g, hole)
 ++
  do If x e2 e3 <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (\e1 ->If (ctx e1) e2 e3, g, hole)
 ++
  do If e1 x e3 <- [lhs]
     (ctx, g, hole) <- execEX x
     (g',_) <- qAsm' g e1
     pure (\e2 -> If e1 (ctx e2) e3, g', hole)
 ++
  do If e1 e2 x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (\e3 ->If e1 e2 (ctx e3), g, hole)


qAsm' :: QContext -> Expr -> [(QContext, AVal)]
qAsm' = go []
  where
    -- go :: [Ident] -> QContext -> Expr -> Maybe (QContext, AVal)
    go _  g (Vasm TRUE)        = pure (g, aANY)
    go _  g (Vasm p)           = pure (QAsm p g, aANY)
    go xs g (Var x :=: Vval b)
      | x `elem` xs            = pure (qAsmVal x (ABase b) g, aANY)  -- TODO: should ONLY be a "flexible" / "local" variable?
      | otherwise              = []
    go xs g (EXI x e)          = do { (g', v) <- go (x:xs) g e; pure (QDef x g', v) }
    go xs g (e1 :>: e2)        = do { (g', v) <- go xs g e2; (g'', _) <- go xs g' e1; pure (g'', v) }
    go _  g e                  = do { v <- maybeToList (getAVal e); pure (g, v) }

qAsm :: Expr -> QContext -> QContext
qAsm (Vasm p)           g = QAsm p g
qAsm (Var x :=: Vval b) g = qAsmVal x (ABase b) g
qAsm (e1 :>: e2)        g = qAsm e1 (qAsm e2 g)
qAsm _                  g = g

qAsmVal :: Ident -> AVal -> QContext -> QContext
qAsmVal x (ABase (Base (Bind y f) ts)) g = QAsm (conjoin (fp : tps)) g
  where fp  = substI y x f
        tps = [ Vr x .=. t | t <- ts ]
qAsmVal _ _                     g = g

pattern AbsVal :: Form -> AVal -> Expr
pattern AbsVal p t <- (getAbsVal -> Just (p, t))

getAbsVal :: Expr -> Maybe (Form, AVal)
getAbsVal (Vasm p :>: e) = (p,)    <$> getAVal e
-- getAbsVal e              = (TRUE,) <$> getAVal e
getAbsVal e              = case getAbsVal' e of
                              [(p, t)] -> Just (p, t)
                              _        -> Nothing

getAbsVal' :: Expr -> [(Form, AVal)]
getAbsVal' e = do
  (_, v) <- qAsm' QEmp e
  guard (null (free v))
  pure (TRUE, v)

---------------------------------------------------------------------------------------
-- | Substitution with just Idents
---------------------------------------------------------------------------------------

class SubstIdent a where
  substI :: Ident -> Ident -> a -> a

instance SubstIdent FTerm where
  substI x y t@(Vr v)
    | x == v           = Vr y
    | otherwise        = t
  substI x y (Ap f ts) = Ap f (substI x y <$> ts)

instance SubstIdent a => SubstIdent (Bind a) where
  substI x y b@(Bind z a)
    | z == x    = b
    | otherwise = Bind z (substI x y a)

instance SubstIdent Form where
  substI _ _ FALSE       = FALSE
  substI _ _ TRUE        = TRUE
  substI x y (Pred p ts) = Pred p (substI x y <$> ts)
  substI x y (Not f)     = Not (substI x y f)
  substI x y (f :&&: g)  = substI x y f :&&: substI x y g
  substI x y (f :||: g)  = substI x y f :||: substI x y g
  substI x y (Forall bf) = Forall (substI x y bf)
  substI x y (Exists bf) = Exists (substI x y bf)


instance SubstIdent ABase where
  substI x y (Base f ts) = Base (substI x y f) (substI x y <$> ts)

instance SubstIdent AVal where
  substI x y a@(AFun (Bind z s))
    | x == z           = a
    | otherwise        = AFun (Bind z (substI x y s))
  substI x y (ABase b) = ABase (substI x y b)

instance (SubstIdent a, SubstIdent b) => SubstIdent (a, b) where
  substI x y (a, b) = (substI x y a, substI x y b)

instance (SubstIdent a, SubstIdent b, SubstIdent c) => SubstIdent (a, b, c) where
  substI x y (a, b, c) = (substI x y a, substI x y b, substI x y c)

--------------------------------------------------------------------------------
-- | Primitives to construct expressions
--------------------------------------------------------------------------------
getBase :: AVal -> ABase
getBase (ABase b) = b
getBase _ = error "getBase: not a base"

aBase :: Bind Form -> [FTerm] -> AVal
aBase b xs = ABase (Base b xs)

aANY :: AVal
aANY = aBase (Bind v TRUE) [] where v = ident "a"

aINT :: AVal
aINT = aBase (Bind v (isINT (Vr v))) [] where v = ident "a"

aRAT :: AVal
aRAT = aBase (Bind v (isRAT (Vr v))) [] where v = ident "a"

isRAT :: FTerm -> Form
isRAT v = Pred (ident "isRat$") [v]

isINT :: FTerm -> Form
isINT v = Pred (ident "isInt$") [v]

decides :: Form
decides = Pred (ident "decides$") []

singleton :: ABase -> FTerm -> ABase
singleton (Base p ixs) x = Base p (x:ixs)
-- singleton (ABase (Bind v p) ) x = ABase (Bind v (p :&&: (Vr v .=. x) ))
-- singleton _            _ = error "singleton: not a base"

class Term a where
  mkTerm :: a -> FTerm

instance Term Integer where
   mkTerm = V.term . Int

instance Term Int where
   mkTerm = V.term . Int . fromIntegral

instance Term Ident where
  mkTerm = V.term . Var

($==) :: (Term a, Term b) => a -> b -> Form
x $== y = mkTerm x .=. mkTerm y



sng :: (Term a) => AVal -> a -> AVal
sng a x = ABase (singleton (getBase a) (mkTerm x))

sngINT :: Integer -> AVal
sngINT = sng aINT

triple :: Form -> Expr -> AVal -> Expr
triple p e t = Vif p (Vis e t)

class Exp a where
  mkExpr :: a -> Expr

instance Exp Expr where
  mkExpr = id

instance Exp Integer where
  mkExpr = Int

instance Exp Ident where
  mkExpr = Var

bin :: (Exp a, Exp b) => Ident -> a -> b -> Expr
bin f x y = (Var f :@: mkExpr x) :@: mkExpr y


------------------------------------------------------------------------------------

solveVC :: VC -> IO [Query]
solveVC (VC qs) = filterM (fmap not . solve1) qs

trivial :: ABase -> ABase -> Bool
trivial (Base _ _) (Base (Bind _ TRUE) _)
  = True
trivial (Base (Bind _ FALSE) _) (Base _ _)
  = True
trivial (Base (Bind a1 f1) _) (Base (Bind a2 f2) _)
  | a1 == a2 && f1 == f2
  = True
trivial _ _
  = False

solve1 :: Query -> IO Bool
solve1 (Subsumes _ (ABase b1) (ABase b2))
  | trivial b1 b2
  = return True
solve1 q = do
  case queryVC q of
    Just qf -> FOL.proveC (FOL.Config False) qf
    _       -> return False

baseVC :: QContext -> ABase -> ABase -> Form
baseVC g b1@(Base (Bind x1 _) _) b2@(Base (Bind x2 _) _)
  = withContext g (Forall (Bind x1 (a1 `implies` a2')))
  where
    a1  = avalAsm b1
    a2  = avalAsm b2
    a2' = substI x2 x1 a2

queryVC :: Query -> Maybe Form
queryVC (Subsumes g (ABase b1) (ABase b2))
  = Just (baseVC g b1 b2)
queryVC (Subsumes g (AFun (Bind x1 (s1, p1, t1))) (AFun (Bind x2 (s2, p2, t2))))
  = do let g2 = qAsmVal x2 s2 g
       q1    <- queryVC (Subsumes g s2 (substI x1 x2 s1))
       q2    <- Just $ withContext g2 (p2 `implies` substI x1 x2 p1)
       q3    <- queryVC (Subsumes g2 t1 t2)
       Just   $ conjoin [q1, q2, q3]
queryVC _
  = error "TODO: smtQuery" -- Nothing

withContext :: QContext -> Form -> Form
withContext g0 goal = go g0
  where
    go QEmp       = goal
    go (QDef x g) = Forall (Bind x (go g))
    go (QAsm p g) = p `implies` go g

implies :: Form -> Form -> Form
implies p q = Not p :||: q

conjoin :: [Form] -> Form
conjoin [p] = p
conjoin ps  = foldr (:&&:) TRUE ps

avalAsm :: ABase -> Form
avalAsm (Base (Bind x p) ts) = p :&&: sngAsm x ts

sngAsm :: Ident -> [FTerm] -> Form
sngAsm x ts = foldr (:&&:) TRUE [ Vr x .=. t | t <- ts ]
