{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches #-}
module Rules.Eval where

import TRS.Bind
import Rules.Core
import Rules.TRS2024( choiceFreeLH )

choicefree :: Expr -> Bool
choicefree = choiceFreeLH

--------------------------------------------------------------------------------

data Result
  = FAIL
  | VAL Val
  | BLKD Expr      -- BLKD e: blkd e == True
  | Expr :||: Expr
  | SUBST [Ident] (Ident,Val) Expr
 deriving ( Eq, Ord, Show )

-- a Result can be interpreted as an expression
toExpr :: Result -> Expr
toExpr FAIL               = Fail
toExpr (VAL v)            = v
toExpr (BLKD e)           = e -- where blkd e == True
toExpr (e1 :||: e2)       = e1 :|: e2
toExpr (SUBST zs (x,v) e) = exis zs ((Var x :=: v) :>: e)

exis :: [Ident] -> Expr -> Expr
exis []     e = e
exis (z:zs) e = Exi (bind z (exis zs e))

--------------------------------------------------------------------------------

-- eval inCoicefreeC flexis e = r:
-- 1. e === toExpr r
-- 2. if r = SUBST zs (x,v) e', then:
--    - x is in flexis
--    - zs are exi-bound somewhere in the original e
-- 3. inChoiceFreeC says if we are operating inside a choicefree context
--    (important for if we can apply the CHOICE rule or not)
eval :: Bool -> [Ident] -> Expr -> Result
eval inChoicefreeC flexis Fail        = FAIL
eval inChoicefreeC flexis v | isVal v = VAL v
eval inChoicefreeC flexis (e1 :|: e2) = e1 :||: e2

eval inChoicefreeC flexis e@(f :@: v) =
  case (f, v) of
    -- (\x.a)b --> exi x.x=a; b
    (Lam bnd, a) ->
      let (x,b) = alphaRename flexis bnd in
        eval inChoicefreeC flexis (Exi (bind x ((Var x :=: a) :>: b)))

    -- a+b --> "a+b"
    (Op Add, Tup [Lit (LInt a),Lit (LInt b)]) ->
      VAL (Lit (LInt (a+b)))

    -- a<b --> a OR fail
    (Op Lt, Tup [Lit (LInt a),Lit (LInt b)])
      | a < b     -> VAL (Lit (LInt a))
      | otherwise -> FAIL

    _ -> BLKD e

eval inChoicefreeC flexis (Exi bnd) =
  case eval inChoicefreeC (x:flexis) e of
    SUBST zs (y,w) e'
      -- exi x . Exi zs . x=w; e' --> Exi zs . e'{w/x}
      | y==x             -> eval inChoicefreeC flexis (exis zs (subst [(x,w)] e'))
      -- exi x . Exi zs . y=w; e' --> Exi x,zs . y=w; e'
      | otherwise        -> SUBST (x:zs) (y,w) e'
    
    -- exi x.(e1|e2) --> (exi x.e1)|(exi x.e2)
    eL :||: eR           -> Exi (bind x eL) :||: Exi (bind x eR)

    -- do EXI-ELIM?
    r | x `elem` free e' -> BLKD (Exi (bind x e'))
      | otherwise        -> BLKD e'
     where
      e' = toExpr r
 where
  (x, e) = alphaRename flexis bnd

eval inChoicefreeC flexis (Iter e cons nil) =
  case eval False [] e of
    -- iter(cons,nil){fail} --> <>
    FAIL ->
      eval inChoicefreeC flexis (nil :@: Tup [])

    -- iter(cons,nil){v} --> exi f. f=cons(v); f(nil)
    VAL v ->
      eval inChoicefreeC flexis $
        Exi $ bind f $
          (Var f :=: (cons :@: v)) :>: (Var f :@: nil)
     where
      f = identNotIn (free e ++ free cons ++ free nil)
    
    -- iter(cons,nil){e1|e2} --> iter(cons,\_.iter(cons,nil){e2}){e1}
    e1 :||: e2 ->
      eval inChoicefreeC flexis $
        Iter e1 cons $
          Lam $ bind underscore $
            Iter e2 cons nil

    r ->
      BLKD (Iter (toExpr r) cons nil)

eval inChoicefreeC flexis ((v :=: e1) :>: e2) =
  case (v, eval inChoicefreeC flexis e1) of
    -- v=fail; e2 --> fail
    (v, FAIL) ->
      FAIL

    -- v=(Exi zs. y=w;e1'); e2 --> Exi zs. y=w; v=e1'; e2
    (v, SUBST zs (y,w) e1') ->
      SUBST zs (y,w) ((v :=: e1') :>: e2)

    -- x=v1; e2 == x=v1; e2
    (Var x, VAL v1) | x `elem` flexis ->
      substCheck x v1 e2

    -- v=y; e2 --> y=v; e2
    (v, VAL (Var y)) | y `elem` flexis ->
      substCheck y v e2

    -- hnf1=hnf2; e2 --> --DO-THE-UNIFICATION--
    (hnf1, VAL hnf2) | isHNF hnf1 && isHNF hnf2 ->
      case unify hnf1 hnf2 e2 of
        Nothing -> FAIL
        Just e  -> eval inChoicefreeC flexis e
    
    -- v=(eL|eR);e2 --> (v=eL;e2)|(v=eR;e2)
    (v, eL :||: eR)
      | inChoicefreeC ->
        eval inChoicefreeC flexis (((v :=: eL) :>: e2) :|: ((v :=: eR) :>: e2))
      
      | otherwise ->
        case eval False [] eL of
          FAIL -> eval False flexis ((v :=: eR) :>: e2)
          rL   -> case eval False [] eR of
                    FAIL -> eval False flexis ((v :=: toExpr rL) :>: e2)
                    rR   -> evalSeqBlkd False flexis (v, toExpr rL :|: toExpr rR) e2

    (v, r1) -> evalSeqBlkd inChoicefreeC flexis (v, toExpr r1) e2

eval _ _ e =
  error ("eval unimplemented for " ++ show e)

-- eval for (v:=:e1'):>:e2, where we know that e1 is blkd
evalSeqBlkd :: Bool -> [Ident] -> (Val,Expr) -> Expr -> Result
evalSeqBlkd inChoicefreeC flexis (v, e1') e2 =
  case eval (inChoicefreeC && choicefree_e1') flexis e2 of
    -- v=e1';Exi zs. y=w;e2' --> Exi zs. y=w;v=e1';e2'   [v=e1' is blkd, so this is OK]
    SUBST zs (y,w) e2'          -> SUBST zs (y,w) ((v :=: e1') :>: e2')

    -- v=e1';fail --> fail
    FAIL                        -> FAIL

    -- v=e1';(eL|eR) --> (v=e1';eL)|(v=e1';eR)  when e1' is blkd&choicefree
    eL :||: eR | choicefree_e1' -> ((v:=:e1'):>:eL) :||: ((v:=:e1'):>:eR)

    -- v=e1';e2' == v=e1';e2'
    r2                          -> BLKD ((v :=: e1') :>: toExpr r2)
 where
  choicefree_e1' = choicefree e1'

-- SUBST, but with occurs check
substCheck :: Ident -> Val -> Expr -> Result
substCheck x v e
  | occurs x v = FAIL
  | otherwise  = SUBST [] (x,v) e
 where
  occurs x (Var y)  = x==y
  occurs x (Tup vs) = any (occurs x) vs
  occurs x (Tru t)  = occurs x t
  occurs x _        = False

-- unifying HNF values
unify :: Val -> Val -> Expr -> Maybe Expr
unify (Lit a)  (Lit b)  e | a == b = Just e
unify (Tru a)  (Tru b)  e | a == b = Just e
unify (Tup vs) (Tup ws) e | length vs == length ws =
  Just (foldr (:>:) e (zipWith (:=:) vs ws))
unify _        _        _          = Nothing

--------------------------------------------------------------------------------


