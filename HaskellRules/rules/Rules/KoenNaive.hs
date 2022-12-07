{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Rules.KoenNaive(allSystemsKoen) where

import TRS.Bind
import TRS.System
import TRS.TRS
import Rules.Core
import Control.Monad( guard )

--------------------------------------------------------------------------------

allSystemsKoen :: [TRSystem Expr]
allSystemsKoen = [ systemKoen ]

systemKoen :: TRSystem Expr
systemKoen = TRSystem
  { sname               = "Koen"
  , description         = "Koen's very naive rules"
  , ruleEnv             = defaultTRSFlags
  , preProcess          = check validE . expr
  , postProcess         = id
  , rules               = allRules
  , rulesHaveStructural = True
  , confluenceRules     = \_ _ -> []
  , validExpr           = validE
  }

-- Turn an expression into the subset of the Koen grammar
expr :: Expr -> Expr
expr (e1 :|: e2)      = expr e1 :|: expr e2
expr (Def (Bind x e)) = Def (Bind x (expr e))
expr (One e)          = One (expr e)
expr (All e)          = All (expr e)
expr (Split a b c)    = Split (expr a) (expr b) (expr c)
expr e                = defseq e

defseq :: Expr -> Expr
defseq (Var x)          = Var x
defseq (e1 :=: e2)      = letExpr (expr e1) (\x -> (x :=: expr e2) :>: x)
defseq (e1 :>: e2)      = letExpr (expr e1) (\_ -> defseq e2)
defseq (Def (Bind x e)) = Def (Bind x (defseq e))
defseq (Arr es)         = letExprs (map expr es) (\xs -> letExpr (Arr xs) id)
defseq (fe :@: e)       = letExprs [expr fe,expr e] (\[f,x] -> letExpr (f :@: x) id)
defseq (Lam (Bind x e)) = letExpr (Lam (Bind x (expr e))) id
defseq e                = letExpr e id

letExpr :: Expr -> (Expr -> Expr) -> Expr
letExpr e@(Var _)     f = f e
letExpr (Var x :=: e) f = (Var x :=: e) :>: f (Var x)
letExpr e             f = Def (Bind x ((Var x :=: e) :>: f (Var x)))
 where
  fx0 = f (Var (ident ""))
  x   = identNotIn (free fx0)

letExprs :: [Expr] -> ([Expr] -> Expr) -> Expr
letExprs []     f = f []
letExprs (e:es) f = letExpr e (\x -> letExprs es (\xs -> f (x:xs)))

data OK = EQ | Yes | No deriving ( Eq, Ord, Show )

-- Check that an expression is in the subset defined by the grammar.
validE :: Expr -> Bool
validE = ok
 where
  ok (Arr xs)         = all isVar xs
  ok (Lam (Bind _ e)) = ok e
  ok (Def (Bind _ e)) = ok e
  ok (_ :=: _)        = False
  ok (e1 :>: e2)      = eq e1 && sequ e2
  ok (f :@: x)        = isVar f && isVar x
  ok (One e)          = ok e
  ok (All e)          = ok e
  ok (Split x y z)    = ok x && ok y && ok z  
  ok _                = True

  eq (x :=: e) = isVar x && ok e
  eq _         = False
  
  sequ (e1 :>: e2)      = eq e1 && sequ e2
  sequ (Def (Bind _ e)) = sequ e
  sequ (Var _)          = True
  sequ _                = False
  
  isVar (Var _) = True
  isVar _       = False

--------------------------------------------------------------------------------

allRules :: ERule
allRules =
     rulesStructural
  <> rulesUnification
  <> rulesEquality
  <> rulesChoice
  <> rulesApplication
  <> rulesGarbageCollect
  <> rulesOne
  <> rulesAll

--------------------------------------------------------------------------------

rulesStructural :: ERule
rulesStructural _ lhs =
  "STRUCT-DEF-MOVE" `name`
  do (x :=: e1) :>: (y :=: Val v) :>: r <- [lhs]
     pure ((y :=: v) :>: (x :=: e1) :>: r)
 ++
  "STRUCT-DEF-VAR-SWAP" `name`
  do Var x :=: Var y <- [lhs]
     pure (Var y :=: Var x)
 ++
  "STRUCT-DEF-BORROW" `name`
  do (Var x :=: Var y) :>: (Var x' :=: e) :>: r <- [lhs]
     guard (x == x')
     pure ((Var x :=: Var y) :>: (Var y :=: e) :>: r)
 ++
  "STRUCT-VAR-VAR-SWAP" `name`
  do Def (Bind x (Def (Bind y e))) <- [lhs]
     pure (Def (Bind y (Def (Bind x e))))

--------------------------------------------------------------------------------

rulesUnification :: ERule
rulesUnification = rulesUnificationProductive
                <> rulesUnificationFail

rulesUnificationProductive :: ERule
rulesUnificationProductive _ lhs =
  "UNIF-SAME" `name`
  do (x :=: Val v) :>: (x' :=: Val v') :>: r <- [lhs]
     guard (x == x' && v == v' && not (isLambda v))
     pure ((x :=: v) :>: r)
 ++
  "UNIF-ARR" `name`
  do (x :=: Arr xs) :>: (x' :=: Arr ys) :>: r <- [lhs]
     guard (x == x' && length xs == length ys)
     pure (foldr (:>:) ((x :=: Arr xs) :>: r) (zipWith (:=:) xs ys))
 ++
  "UNIF-X-X" `name`
  do (x :=: x') :>: r <- [lhs]
     guard (x == x')
     pure r
 where
  isLambda (Lam _) = True
  isLambda _       = False

rulesUnificationFail :: ERule
rulesUnificationFail _ lhs =
  "UNIF-DIFF-CONST" `name`
  do (x :=: Int k1) :>: (x' :=: Int k2) :>: _ <- [lhs]
     guard (x == x' && k1 /= k2)
     pure Fail
 ++
  "UNIF-DIFF-ARR" `name`
  do (x :=: Arr xs) :>: (x' :=: Arr ys) :>: _ <- [lhs]
     guard (x == x' && length xs /= length ys)
     pure Fail
 ++
  "UNIF-LAMBDA" `name`
  do (x :=: Lam _) :>: (x' :=: Lam _) :>: _ <- [lhs]
     guard (x == x')
     pure Fail
 ++
  "UNIF-DIFF-LAMBDA-CONST" `name`
  do (x :=: Lam _) :>: (x' :=: Int _) :>: _ <- [lhs]
     guard (x == x')
     pure Fail
 ++
  "UNIF-DIFF-CONST-LAMBDA" `name`
  do (x :=: Int _) :>: (x' :=: Lam _) :>: _ <- [lhs]
     guard (x == x')
     pure Fail
 ++
  "UNIF-DIFF-LAMBDA-ARR" `name`
  do (x :=: Lam _) :>: (x' :=: Arr _) :>: _ <- [lhs]
     guard (x == x')
     pure Fail
 ++
  "UNIF-DIFF-ARR-LAMBDA" `name`
  do (x :=: Arr _) :>: (x' :=: Lam _) :>: _ <- [lhs]
     guard (x == x')
     pure Fail
 ++
  "UNIF-DIFF-CONST-ARR" `name`
  do (x :=: Int _) :>: (x' :=: Arr _) :>: _ <- [lhs]
     guard (x == x')
     pure Fail
 ++
  "UNIF-DIFF-ARR-CONST" `name`
  do (x :=: Arr _) :>: (x' :=: Int _) :>: _ <- [lhs]
     guard (x == x')
     pure Fail

--------------------------------------------------------------------------------

rulesEquality :: ERule
rulesEquality _ lhs =
  "EQ-EQ" `name`
  do (x :=: ((y :=: e) :>: r1)) :>: r2 <- [lhs]
     pure ((y :=: e) :>: (x :=: r1) :>: r2)
 ++
  "EQ-EXISTS" `name`
  do (Var x :=: Def bnd) :>: r <- [lhs]
     let Bind y e = alphaRename (x:free r) bnd
     pure (Def (Bind y ((Var x :=: e) :>: r)))
 ++
  "SEQ-EXISTS" `name`
  do x_eq_e1 :>: Def bnd <- [lhs]
     let Bind y e2 = alphaRename (free x_eq_e1) bnd
     pure (Def (Bind y (x_eq_e1 :>: e2)))
 ++
  "EQ-FAIL" `name`
  do (_ :=: Fail) :>: r <- [lhs]
     pure Fail
 ++
  "SEQ-VAR-INTRO" `name`
  do x_eq_e1 :>: e2 <- [lhs]
     guard (isRelevant e2)
     let y = identNotIn (free e2)
     pure (x_eq_e1 :>: Def (Bind y ((Var y :=: e2) :>: Var y)))
 where
  isRelevant (Var _)   = False
  isRelevant (Def _)   = False
  isRelevant (_ :=: _) = False
  isRelevant _         = True

--------------------------------------------------------------------------------

rulesChoice :: ERule
rulesChoice _ lhs =
  "CHOICE-ASSOC" `name`
  do (e1 :|: e2) :|: e3 <- [lhs]
     pure (e1 :|: (e2 :|: e3))
 ++
  "CHOICE-FAIL-L" `name`
  do Fail :|: e <- [lhs]
     pure e
 ++
  "CHOICE-FAIL-R" `name`
  do e :|: Fail <- [lhs]
     pure e
 ++
  "EXISTS-CHOICE" `name`
  do Def (Bind x (e1 :|: e2)) <- [lhs]
     pure (Def (Bind x e1) :|: Def (Bind x e2))
 ++
  "CHOICE-MOVE" `name`
  do (Var x :=: e) :>: (Var y :=: (e1 :|: e2)) :>: e3 <- [lhs]
     guard (isChoiceFree e)
     pure ((Var y :=: (e1 :|: e2)) :>: (Var x :=: e) :>: e3)
 ++
  "CHOICE-EXPAND" `name`
  do (ctx, (x :=: (e1 :|: e2)) :>: e3) <- choiceContext lhs
     pure (ctx (((x :=: e1) :>: e3) :|: ((x :=: e2) :>: e3)))

isChoiceFree :: Expr -> Bool
isChoiceFree (One _)     = True
isChoiceFree (All _)     = True
isChoiceFree (Val _)     = True
isChoiceFree Fail        = True
isChoiceFree (e1 :>: e2) = isChoiceFree e1 && isChoiceFree e2
isChoiceFree (e1 :=: e2) = isChoiceFree e1 && isChoiceFree e2
isChoiceFree _           = False

choiceContext :: Expr -> [(Expr -> Expr, Expr)]
choiceContext lhs =
  do One e <- [lhs]
     defns One e
 ++
  do All e <- [lhs]
     defns All e
 ++
  do e1 :|: e2 <- [lhs]
     defns (:|: e2) e1 ++ defns (e1 :|:) e2
 where
  defns ctx e = (ctx, e) : case e of
                             Def (Bind x e) -> defns (ctx . Def . Bind x) e
                             _              -> []

--------------------------------------------------------------------------------

rulesApplication :: ERule
rulesApplication _ lhs =
  "APP-LAMBDA" `name`
  do (f :=: Lam bnd) :>: (y :=: (f' :@: v)) :>: r <- [lhs]
     guard (f == f')
     let Bind x e = alphaRename (free (y, (f, (v, r)))) bnd
     pure ((f :=: Lam bnd) :>: Def (Bind x ((Var x :=: v) :>: (y :=: e) :>: r)))
 ++
  "APP-ARRAY" `name`
  do (a :=: Arr xs) :>: (y :=: (a' :@: v)) :>: r <- [lhs]
     guard (a == a')
     let i = identNotIn (free (a, (xs, (y, (v, r)))))
     pure ( (a :=: Arr xs)
        :>: Def (Bind i ( (Var i :=: v)
                      :>: (y :=: foldr
                                 (\(x,j) e -> ((Var i :=: Int j) :>: x) :|: e)
                                 Fail
                                 (xs `zip` [0..]))
                      :>: r
                        ))
          )

--------------------------------------------------------------------------------

rulesGarbageCollect :: ERule
rulesGarbageCollect _ lhs =
  "VAR-UNUSED" `name`
  do Def (Bind x e) <- [lhs]
     guard (x `notElem` free e)
     pure e
 ++
  "VAR-DEF-UNUSED" `name`
  do Def (Bind x ((Var x' :=: Val v) :>: r)) <- [lhs]
     guard (x == x' && x `notElem` free v)
     let (ys,r') = takeXEqs x r
     guard (x `notElem` free r')
     pure (foldr (:>:) r' (zipWith (:=:) (map Var ys) (v : repeat (Var (head ys)))))
 where
  takeXEqs x ((Var x' :=: Var y) :>: r) | x == x' = (y:ys,r') where (ys,r') = takeXEqs x r
  takeXEqs x r                                    = ([],r)

--------------------------------------------------------------------------------

rulesOne :: ERule
rulesOne _ lhs = []

rulesAll :: ERule
rulesAll _ lhs = []

--------------------------------------------------------------------------------

