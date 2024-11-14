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

data Env
  = Env
  { inChoiceFreeC :: Bool    -- are we in a choiceFree context?
  , rigids        :: [Ident] -- what rigid variables are in scope (only used for alphaRename)
  , flexis        :: [Ident] -- what flexible variables are in scope
  }

-- starting Env
env0 :: Env
env0 = Env True [] []

-- what variables are in scope
vars :: Env -> [Ident]
vars env = flexis env ++ rigids env

-- start a new scope; reset choicefree-ness and make all flexis rigid
newScope :: Env -> Env
newScope env = env{ inChoiceFreeC = True, rigids = vars env, flexis = [] }

-- add a flexible variable
flexi :: Ident -> Env -> Env
flexi x env = env{ flexis = x : flexis env }

-- update choicefree-ness
(/\) :: Bool -> Env -> Env
chf /\ env = env{ inChoiceFreeC = chf && inChoiceFreeC env }

--------------------------------------------------------------------------------

-- eval env e = r:
-- 1. e === toExpr r
-- 2. if r = SUBST zs (x,v) e', then:
--    - x is in flexis env
--    - zs are exi-bound somewhere in the original e
-- 3. inChoiceFreeC env says if we are operating inside a choicefree context
--    (important for if we can apply the CHOICE rule or not)
eval :: Env -> Expr -> Result
eval env Fail        = FAIL
eval env v | isVal v = VAL v
eval env (e1 :|: e2) = e1 :||: e2

eval env e@(f :@: v) =
  case (f, v) of
    -- (\x.b)a --> exi x.x=a; b
    (Lam bnd, a) ->
      let (x,b) = alphaRename (vars env) bnd in
        eval env (Exi (bind x ((Var x :=: a) :>: b)))

    -- a+b --> "a+b"
    (Op Add, Tup [Lit (LInt a),Lit (LInt b)]) ->
      VAL (Lit (LInt (a+b)))

    -- a<b --> a OR fail
    (Op Lt, Tup [Lit (LInt a),Lit (LInt b)])
      | a < b     -> VAL (Lit (LInt a))
      | otherwise -> FAIL

    _ -> BLKD e

eval env (Exi bnd) =
  case eval (flexi x env) e of
    SUBST zs (y,w) e'
      -- exi x . Exi zs . x=w; e' --> Exi zs . e'{w/x}
      | y==x             -> eval env (exis zs (subst [(x,w)] e'))
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
  (x, e) = alphaRename (vars env) bnd

eval env (All e) = evalAll [e]
 where
  evalAll [] =
    VAL (Tup [])
  
  evalAll (e:es) =
    case eval (newScope env) e of
      FAIL ->
        evalAll es

      VAL v ->
        case evalAll es of
          VAL (Tup vs) -> VAL (Tup (v:vs))
          r            -> BLKD (All (v :|: toExpr r))

      e1 :||: e2 ->
        evalAll (e1:e2:es)

      r ->
        BLKD (All (foldr1 (:|:) (toExpr r : es)))

eval env (Iter e cons nil) =
  case eval (newScope env) e of
    -- iter(cons,nil){fail} --> <>
    FAIL ->
      eval env (nil :@: Tup [])

    -- iter(cons,nil){v} --> exi f. f=cons(v); f(nil)
    VAL v ->
      eval env $
        Exi $ bind f $
          (Var f :=: (cons :@: v)) :>: (Var f :@: nil)
     where
      f = identNotIn (free e ++ free cons ++ free nil)
    
    -- iter(cons,nil){e1|e2} --> iter(cons,\_.iter(cons,nil){e2}){e1}
    e1 :||: e2 ->
      eval env $
        Iter e1 cons $
          Lam $ bind underscore $
            Iter e2 cons nil

    r ->
      BLKD (Iter (toExpr r) cons nil)

eval env ((v :=: e1) :>: e2) =
  case (v, eval env e1) of
    -- v=fail; e2 --> fail
    (v, FAIL) ->
      FAIL

    -- v=(Exi zs. y=w;e1'); e2 --> Exi zs. y=w; v=e1'; e2
    (v, SUBST zs (y,w) e1') ->
      SUBST zs (y,w) ((v :=: e1') :>: e2)

    -- x=v1; e2 == x=v1; e2
    (Var x, VAL v1) | x `elem` flexis env ->
      substOccursCheck x v1 e2

    -- v=y; e2 --> y=v; e2
    (v, VAL (Var y)) | y `elem` flexis env ->
      substOccursCheck y v e2

    -- hnf1=hnf2; e2 --> --DO-THE-UNIFICATION--
    (hnf1, VAL hnf2) | isHNF hnf1 && isHNF hnf2 ->
      case unify hnf1 hnf2 e2 of
        Nothing -> FAIL
        Just e  -> eval env e
    
    (v, eL :||: eR)
      | inChoiceFreeC env ->
        -- v=(eL|eR);e2 --> (v=eL;e2)|(v=eR;e2)
        eval env (((v :=: eL) :>: e2) :|: ((v :=: eR) :>: e2))
      
      | otherwise ->
        -- e|fail -> e  OR  fail|e -> e
        evalChoiceTry eL eR env (\e1' -> ((v:=:e1'):>:e2))
          (\e1' -> evalSeqBlkd env (v,e1') e2)

    (v, r1) -> evalSeqBlkd env (v, toExpr r1) e2

eval _ e =
  error ("eval unimplemented for " ++ show e)

-- eval for (v:=:e1'):>:e2, where we know that e1 is blkd
evalSeqBlkd :: Env -> (Val,Expr) -> Expr -> Result
evalSeqBlkd env (v, e1') e2 =
  case eval (choicefree_e1' /\ env) e2 of
    -- v=e1';Exi zs. y=w;e2' --> Exi zs. y=w;v=e1';e2'   [v=e1' is blkd, so this is OK]
    SUBST zs (y,w) e2' -> SUBST zs (y,w) ((v :=: e1') :>: e2')

    -- v=e1';fail --> fail
    FAIL               -> FAIL

    eL :||: eR
      -- v=e1';(eL|eR) --> (v=e1';eL)|(v=e1';eR)  when e1' is blkd&choicefree
      | choicefree_e1' -> ((v:=:e1'):>:eL) :||: ((v:=:e1'):>:eR)

      | otherwise ->
        evalChoiceTry eL eR (False /\ env) (\e2' -> (v:=:e1'):>:e2')
          (\e2' -> BLKD ((v:=:e1'):>:e2'))

    -- v=e1';e2' == v=e1';e2'
    r2                 -> BLKD ((v :=: e1') :>: toExpr r2)
 where
  choicefree_e1' = choicefree e1'

-- try to find places where to use e|fail->e and fail|e->e
evalChoiceTry :: Expr -> Expr -> Env -> (Expr -> Expr) -> (Expr -> Result) -> Result
evalChoiceTry eL eR env k choicy =
  case eval (False /\ newScope env) eL of
    FAIL -> eval env (k eR)
    rL   -> case eval (False /\ newScope env) eR of
              FAIL -> eval env (k (toExpr rL))
              rR   -> choicy (toExpr rL :|: toExpr rR)

-- SUBST, REC, U-OCCURS in one step
substOccursCheck :: Ident -> Val -> Expr -> Result
substOccursCheck x v e =
  case check x v of
    Nothing -> FAIL
    Just v' -> SUBST [] (x,v') e
 where
  check x (Var y)       = Nothing
  check x (Tup vs)      = Tup `fmap` sequence [ check x v | v <- vs ]
  check x (Tru v)       = Tru `fmap` check x v
  check x (Lam bnd)
    | x `elem` free bnd = 
      let (y,e) = alphaRename (x : free v) bnd in
        Just $ Lam $ bind y $ Exi (bind x ((Var x :=: v) :>: e))
  check _ v             = Just v

-- unifying HNF values
unify :: Val -> Val -> Expr -> Maybe Expr
unify (Lit a)  (Lit b)  e | a == b = Just e
unify (Tru a)  (Tru b)  e | a == b = Just e
unify (Tup vs) (Tup ws) e | length vs == length ws =
  Just (foldr (:>:) e (zipWith (:=:) vs ws))
unify _        _        _          = Nothing

--------------------------------------------------------------------------------


