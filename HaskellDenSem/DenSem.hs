{-# OPTIONS_GHC -Wall -Wno-missing-methods #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
import Control.Arrow(first, second)
import Data.Function
import Data.List
import Data.Maybe
import Data.String
import Bind
import Debug.Trace

infixl 8 :@:
infixr 7 :|:
infix  6 :=:
infixl 5 :>:

data Exp
  = Val Val
  | Exp :=: Exp               -- ^ e1 = e2
  | Exp :>: Exp               -- ^ e1; e2
  | Exp :|: Exp               -- ^ e1 | e2
  | Exp :@: Exp               -- ^ v1(v2)
  -- To make it executable, give a list of values for Exi
  -- to iterate over.
  | Exi [W] (Bind Exp)        -- ^ ex x. e
  | One Exp                   -- ^ one { e }
  | All Exp                   -- ^ all { e }
  | Fail                      -- ^ fail
  -- This is for convenience only.
  | Def Exp (Bind Exp)        -- ^ ex x . x = e1; e2
  deriving (Show, Eq)
  
pattern ELam :: Ident -> Exp -> Exp
pattern ELam x e = Val (Lam (Bind x e))

pattern EExi :: [W] -> Ident -> Exp -> Exp
pattern EExi ws x e = Exi ws (Bind x e)

data Val
  = Var Ident
  | Int Integer
  | Opr Op
  | Arr [Val]
  | Lam (Bind Exp)
  deriving (Show, Eq)

pattern EArr :: [Val] -> Exp
pattern EArr vs = Val (Arr vs)

data Op = Add | Gt
  deriving (Show, Eq)

instance Free Exp where
  free (Val v)     = free v
  free (e1 :=: e2) = free e1 `union` free e2
  free (e1 :>: e2) = free e1 `union` free e2
  free (e1 :|: e2) = free e1 `union` free e2
  free (e1 :@: e2) = free e1 `union` free e2
  free (Exi _ b)   = free b
  free (One e)     = free e
  free (All e)     = free e
  free Fail        = []
  free (Def e b)   = free e `union` free b
  allIds (Val v)     = allIds v
  allIds (e1 :=: e2) = allIds e1 ++ allIds e2
  allIds (e1 :>: e2) = allIds e1 ++ allIds e2
  allIds (e1 :|: e2) = allIds e1 ++ allIds e2
  allIds (e1 :@: e2) = allIds e1 ++ allIds e2
  allIds (Exi _ b)   = allIds b
  allIds (One e)     = allIds e
  allIds (All e)     = allIds e
  allIds Fail        = []
  allIds (Def e b)   = allIds e ++ allIds b

instance Free Val where
  free (Var v)  = [v]
  free (Arr vs) = foldr (union . free) [] vs
  free (Lam b)  = free b
  free _        = []
  allIds (Var v)  = [v]
  allIds (Arr vs) = foldr ((++) . allIds) [] vs
  allIds (Lam b)  = allIds b
  allIds _        = []

----------------

data W
  = WInt Integer
  | WArr [W]
  | WLam Fcn
  deriving (Show, Eq)

instance Num W where
  fromInteger i = WInt i

data Fcn = Fcn String (W -> Wstar)

-- This function comparison is a lie, of course.
-- But it's good enough for simple experiments.
instance Eq Fcn where
  Fcn s _ == Fcn t _ =
--    trace (show (s, t, s == t)) $
    s == t
instance Show Fcn where
  show (Fcn s _) = "(" ++ s ++ ")"

type Env = [(Ident, W)]

extEnv :: Env -> Ident -> W -> Env
extEnv rho x w = (x, w) : rho

optable :: [(Op, W)]
optable =
  [ (Add, WLam $ Fcn "add" add)
  , (Gt,  WLam $ Fcn "gt"  gt)
  ]
  where add (WArr [WInt i, WInt j]) = unit $ WInt (i + j)
        add _                       = Wrong   -- XXX This should probably be empty
        gt  (WArr [WInt i, WInt j]) = unit $ WInt (i + j)
        gt  _                       = empty

evalE :: Env -> Exp -> Wstar
evalE rho (Val val)           = unit (evalV rho val)
evalE _   Fail                = empty
evalE rho (e1 :|: e2)         = evalE rho e1 `wunion`     evalE rho e2
evalE rho (e1 :=: e2)         = evalE rho e1 `wintersect` evalE rho e2
evalE rho (e1 :>: e2)         = evalE rho e1 `wsequence`  evalE rho e2
evalE rho (Val v1 :@: Val v2) = apply (evalV rho v1) (evalV rho v2)
evalE rho (Exi ws (Bind x e)) = wbigunion [ evalE (extEnv rho x w) e | w <- ws ]
evalE rho (One e)             = wone (evalE rho e)
evalE rho (All e)             = wall (evalE rho e)
evalE rho (Def e1 (Bind x e2))=
  case evalE rho e1 of
    Wrong -> Wrong
    P s   -> wbigunion [ P [wl] `wsequence` evalE (extEnv rho x w) e2 | wl@(_,w) <- s ]
evalE _ _ = error "evalE"

evalV :: Env -> Val -> W
evalV rho val =
  case val of
    Var x          -> fromMaybe (error $ "undefined " ++ show x) $ lookup x rho
    Int i          -> WInt i
    Opr o          -> fromMaybe (error $ "undefined " ++ show o) $ lookup o optable
    Lam (Bind x e) -> WLam $ Fcn (show val) $ \ w -> evalE (extEnv rho x w) e
    Arr vs         -> WArr (map (evalV rho) vs)

apply :: W -> W -> Wstar
apply (WArr ws) (WInt i) | 0 <= i' && i' < length ws = unit (ws !! i')
                         | otherwise                 = empty
  where i' = fromInteger i
apply (WLam (Fcn _ f)) w = f w
-- These two will make things go wrong when iterating over all possible values
--apply (WInt _) _ = Wrong
--apply (WArr _) _ = Wrong
apply _ _ = empty     -- this "works"

----------------------------------------

{-
type Wstar = Maybe [(Lbl, W)]
pattern Wrong :: Wstar
pattern Wrong = Nothing
pattern P :: [(Lbl, W)] -> Wstar
pattern P x   = Just x
-}
data Wstar = Wrong | P [(Lbl, W)]
  deriving (Show)

type Lbl = [LR]

data LR = L | R
  deriving (Show, Eq, Ord)

unit :: W -> Wstar
unit w = P [([], w)]

empty :: Wstar
empty = P []

-- Maybe wbigunion should remove Wrong?
wbigunion :: [Wstar] -> Wstar
wbigunion = foldr f empty
  where f (P s1) (P s2) = P (s1 ++ s2)
        f _ _ = Wrong

wunion :: Wstar -> Wstar -> Wstar
wunion (P s1) (P s2) = P (map (first (L:)) s1 ++ map (first (R:)) s2)
wunion _ _ = Wrong

wintersect :: Wstar -> Wstar -> Wstar
wintersect (P s1) (P s2) = P [(l1 ++ l2, w1) | (l1, w1) <- s1, (l2, w2) <- s2, w1 == w2]
wintersect _ _ = Wrong

wsequence :: Wstar -> Wstar -> Wstar
wsequence (P s1) (P s2) = P [(l1 ++ l2, w2) | (l1, _) <- s1, (l2, w2) <- s2]
wsequence _ _ = Wrong

wone :: Wstar -> Wstar
wone Wrong = Wrong
wone (P s) =
  case wsort s of
    Nothing -> Wrong
    Just [] -> empty
    Just ((l,w):_) -> P [(l,w)]    -- new
--    Just ((_,w):_) -> unit w       -- old
wone _ = error "impossible"

wall :: Wstar -> Wstar
wall Wrong = Wrong
wall (P s) =
  case wsort s of
    Nothing -> Wrong
    Just lws -> unit $ WArr (map snd lws)
wall _ = error "impossible"

wsort :: [(Lbl, W)] -> Maybe [(Lbl, W)]
wsort s = mapM chkSing $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) s
  where chkSing [x] = Just x
        chkSing _   = Nothing

--------------------------------------

iF :: Exp -> Exp -> Exp -> Exp
iF e1 e2 e3 = Def (One((e1 :>: ELam "_" e2) :|: ELam "_" e3)) (Bind x $ Val (Var x) :@: Val (Arr []))
  where x = notIn (e1, e2, e3)

def :: Ident -> Exp -> Exp -> Exp
def x e1 e2 = Def e1 (Bind x e2)

(@:)                :: Exp -> Exp -> Exp
v1@Val{} @: v2@Val{} = v1 :@: v2
v1@Val{} @: e2       = def x e2 (v1 :@: Val (Var x))  where x = notIn (v1, e2)
e1       @: e2       = def f e1 (Val (Var f) @: e2)  where f = notIn (e1, e2)

instance IsString Exp where
  fromString s = Val (Var (ident s))

instance IsString Val where
  fromString s = Var (ident s)

instance Num Exp where
  x + y = Val (Opr Add) @: arr [x, y]
  fromInteger i = Val (Int i)

instance Num Val where
  fromInteger i = Int i

arr :: [Exp] -> Exp
arr = arr' id []
  where arr' f vs [] = f (Val (Arr vs))
        arr' f vs (Val v : es) = arr' f (vs ++ [v]) es
        arr' f vs (e     : es) = arr' (f . def x e) (vs ++ [Var x]) es where x = notIn (e, es, f (Val (Int 0)))

etest1 :: Exp
etest1 = Exi ws $ Bind "x" $
                  "x" :=: 2 :>: "x" + 1
  where ws = [WInt 1, WInt 2, WInt 3]

etest2 = (1 :|: 2) + (3 :|: 4)

etest3 = All etest2
etest4 = One etest2

etest5 = def "x" (1 :|: 2) $ iF ("x" :=: 1) (40 :|: 50) 5

etest6a = All $
  EExi [0,1,2,3] "x" $
  iF ("x":=:1) (2:|:3) (4) :>:
  "x" :=: (1:|:2) :>:
  iF ("x":=:1) (5) (6:|:7)

etest6b = All $
  EExi [3,2,1,0] "x" $
  iF ("x":=:1) (2:|:3) (4) :>:
  "x" :=: (1:|:2) :>:
  iF ("x":=:1) (5) (6:|:7)

etest7 = All $
  EExi [0,1,2,3] "x" $
  "x" :=: (1:|:2) :>:
  iF ("x":=:1) (2:|:3) (4) :>:
  iF ("x":=:1) (5) (6:|:7)

etest8a = All $
  EExi [0,1,2,3] "x" $
  arr [
    iF ("x":=:1) (2:|:3) (4),
    "x" :=: (1:|:2),
    iF ("x":=:1) (5) (6:|:7)
    ]

etest8b = All $
  EExi [3,2,1,0] "x" $
  arr [
    iF ("x":=:1) (2:|:3) (4),
    "x" :=: (1:|:2),
    iF ("x":=:1) (5) (6:|:7)
    ]

etest9 = All $
  EExi [0,1,2,3] "x" $
  arr [
    "x" :=: (1:|:2),
    iF ("x":=:1) (2:|:3) (4),
    iF ("x":=:1) (5) (6:|:7)
    ]

etest10 = All $
  EExi [0, add1, add2] "f" $
  def "r" ("f" @: 1) $
    "f" :=: Val vadd1 :|: Val vadd2 :>:
    "r"
 where
    add1 = WLam $ Fcn (show vadd1) (\ (WInt i) -> unit $ WInt (i+1))
    add2 = WLam $ Fcn (show vadd2) (\ (WInt i) -> unit $ WInt (i+2))
    vadd1 = Lam $ Bind "a" $ "a" + 1
    vadd2 = Lam $ Bind "a" $ "a" + 2

-- This should be Wrong (== Nothing)
etest11 = All $
  EExi [1,2,3] "x" $ "x"
