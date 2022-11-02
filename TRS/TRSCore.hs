{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-missing-signatures -Wno-missing-pattern-synonym-signatures -Wno-name-shadowing #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module TRSCore where

import TRS
import Bind
import Test.QuickCheck
import Data.List( intercalate, union, elemIndex )
import Data.Maybe

--------------------------------------------------------------------------------

data Expr
  = Val Value                   -- ^ v
  | Expr :=: Expr               -- ^ e1 = e2
  | Expr :>: Expr               -- ^ e1; e2
  | Expr :|: Expr               -- ^ e1 | e2
  | Value :@: Value             -- ^ v1(v2)
  | Def (Bind Expr)             -- ^ ex x. e
  | One Expr                    -- ^ one { e }
  | All Expr                    -- ^ all { e }
  | Fail                        -- ^ fail
  | Wrong                       -- ^ wrong
  | Split Expr Value Value      -- ^ split { e, v1, v2 }

infixr 1 :>:
infixr 3 :|:
infixr 2 :=:
infixl 4 :@:

instance Show Expr where
  showsPrec p (Val v)          = showsPrec p v
  showsPrec p (a :|: b)        = showParen (p > 3) $ showsPrec 4 a . showString " | " . showsPrec 4 b
  showsPrec p (a :>: b)        = showParen (p > 1) $ showsPrec 2 a . showString "; "  . showsPrec 1 b
  showsPrec p (a :=: b)        = showParen (p > 2) $ showsPrec 3 a . showString " = " . showsPrec 3 b
  showsPrec p (a :@: b)        = showParen (p > 4) $ showsPrec 4 a . showString "(" . showsPrec 0 b . showString ")"
  showsPrec _ Fail             = showString "fail"
  showsPrec _ (Def (Bind x a)) = showString "def " . showsPrec 0 x . showString " in {" . showsPrec 0 a . showString "}"
  showsPrec _ (One a)          = showString "one {" . showsPrec 0 a . showString "}"
  showsPrec _ (All a)          = showString "all {" . showsPrec 0 a . showString "}"
  showsPrec _ Wrong            = showString "wrong"
  showsPrec _ (Split e v1 v2)  = showString "split {" . showsPrec 0 e . showString ", " .
                                 showsPrec 0 v1 . showString ", " . showsPrec 0 v2 . showString "}"

instance Eq Expr where
  a == b = a `compare` b == EQ

instance Ord Expr where
  a `compare` b = comp [] [] a b
   where
    -- so much code... this can probably simplified a lot
    comp xs ys Wrong Wrong = EQ
    comp xs ys Wrong _     = LT
    comp xs ys _     Wrong = GT

    comp xs ys Fail Fail = EQ
    comp xs ys Fail _    = LT
    comp xs ys _    Fail = GT

    comp xs ys (Val v) (Val w) = compV xs ys v w
    comp xs ys (Val v) _       = LT
    comp xs ys _       (Val w) = GT

    comp xs ys (a:=:b) (c:=:d) = comp xs ys a c & comp xs ys b d
    comp xs ys (a:=:b) _       = LT
    comp xs ys _       (c:=:d) = GT

    comp xs ys (a:>:b) (c:>:d) = comp xs ys a c & comp xs ys b d
    comp xs ys (a:>:b) _       = LT
    comp xs ys _       (c:>:d) = GT

    comp xs ys (a:|:b) (c:|:d) = comp xs ys a c & comp xs ys b d
    comp xs ys (a:|:b) _       = LT
    comp xs ys _       (c:|:d) = GT

    comp xs ys (a:@:b) (c:@:d) = compV xs ys a c & compV xs ys b d
    comp xs ys (a:@:b) _       = LT
    comp xs ys _       (c:@:d) = GT

    comp xs ys (One a) (One b) = comp xs ys a b
    comp xs ys (One a) _       = LT
    comp xs ys _       (One b) = GT

    comp xs ys (All a) (All b) = comp xs ys a b
    comp xs ys (All a) _       = LT
    comp xs ys _       (All b) = GT

    comp xs ys (Split e f g) (Split e' f' g') = comp xs ys e e' & compV xs ys f f' & compV xs ys g g'
    comp xs ys Split {} _ = LT
    comp xs ys _ Split {} = GT

    comp xs ys (Def (Bind x a)) (Def (Bind y b)) = comp (x:xs) (y:ys) a b

    compV xs ys (Var x) (Var y) =
      case (elemIndex x xs, elemIndex y ys) of
        (Just i, Just j)   -> i `compare` j
        (Nothing, Nothing) -> x `compare` y
        (Just _, Nothing)  -> LT
        (Nothing, Just _)  -> GT
    compV xs ys (Var _) _       = LT
    compV xs ys _       (Var _) = GT

    compV xs ys (HNF a) (HNF b) = compH xs ys a b

    compH xs ys (Arr vs) (Arr ws)
      | n == m    = head (dropWhile (==EQ) (zipWith (compV xs ys) vs ws) ++ [EQ])
      | otherwise = n `compare` m
     where
      n  = length vs
      m  = length ws
    compH xs ys (Lam (Bind x a)) (Lam (Bind y b)) = comp (x:xs) (y:ys) a b
    compH xs ys a b = a `compare` b

    EQ & c = c
    c  & _ = c

--------------------------------------------------------------------------------

data Value
  = Var Ident
  | HNF HNF
 deriving ( Eq, Ord )

data HNF
  = Int Integer
  | Op Op
  | Arr [Value]
  | Lam (Bind Expr)
 deriving ( Eq, Ord )

data Op
  = Gt
  | Ge
  | Lt
  | Le
  | Ne
  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | Plus
  | IsInt
  | MapAp
  | Cons
 deriving ( Eq, Ord )

instance Show Value where
  show (Var x) = show x
  show (HNF a) = show a

instance Show HNF where
  show (Int k)  = show k
  show (Op op)  = show op
  show (Arr vs) = "<" ++ intercalate ", " (map show vs) ++ ">"
  show (Lam (Bind x e)) = "(\\" ++ show x ++ "." ++ show e ++ ")"

instance Show Op where
  show Gt    = "gt"
  show Ge    = "ge"
  show Lt    = "lt"
  show Le    = "le"
  show Ne    = "ne"
  show Add   = "add"
  show Sub   = "sub"
  show Mul   = "mul"
  show Div   = "div"
  show Neg   = "neg"
  show Plus  = "plus"
  show IsInt = "isInt"
  show MapAp = "mapAp"
  show Cons  = "cons"

--------------------------------------------------------------------------------
-- patterns

-- Expr
pattern VAR v  = Val (Var v)
pattern INT n  = Val (VINT n)
pattern ARR vs = Val (VARR vs)
pattern LAM v e= Val (VLAM v e)
pattern EHNF h = Val (HNF h)
pattern OP o   = Val (VOP o)
pattern HVAL :: HNF -> Expr
pattern HVAL v <- Val (getH -> Just v)
  where HVAL h = Val (HNF h)
pattern SCL :: Value -> Expr
pattern SCL v <- Val (getS -> Just v)
  where SCL v = Val v

getH :: Value -> Maybe HNF
getH (HNF v@Arr{}) = Just v
getH (HNF v@Lam{}) = Just v
getH _ = Nothing

getS :: Value -> Maybe Value
getS v@Var{} = Just v
getS v@VINT{} = Just v
getS v@VOP{} = Just v
getS _ = Nothing

pattern DEF x e = Def (Bind x e)

-- Value
pattern VINT n  = HNF (Int n)
pattern VARR vs = HNF (Arr vs)
pattern VLAM v e= HNF (Lam (Bind v e))
pattern VOP o   = HNF (Op o)
pattern ADD     = HNF (Op Add)
pattern SUB     = HNF (Op Sub)
pattern MUL     = HNF (Op Mul)
pattern DIV     = HNF (Op Div)
pattern NEG     = HNF (Op Neg)
pattern PLUS    = HNF (Op Plus)
pattern GRT     = HNF (Op Gt)
pattern GRE     = HNF (Op Ge)
pattern LST     = HNF (Op Lt)
pattern LSE     = HNF (Op Le)
pattern NEQ     = HNF (Op Ne)
pattern IsINT   = HNF (Op IsInt)
pattern MAPAP   = HNF (Op MapAp)
pattern CONS    = HNF (Op Cons)

--------------------------------------------------------------------------------

instance Rec Expr where
  rec r e =
    r e ++
    structMatch r e ++
    case e of
      a :=: b ->
           [ (n, a' :=: b)  | (n,a') <- rec r a ]
        ++ [ (n, a  :=: b') | (n,b') <- rec r b ]
    
      a :|: b ->
           [ (n, a' :|: b)  | (n,a') <- rec r a ]
        ++ [ (n, a  :|: b') | (n,b') <- rec r b ]

      a :>: b ->
           [ (n, a' :>: b)  | (n,a') <- rec r a ]
        ++ [ (n, a  :>: b') | (n,b') <- rec r b ]

      Def (Bind x a) ->
           [ (n, Def (Bind x a')) | (n,a') <- rec r a ]

      f :@: a ->
           [ (n,f' :@: a)  | (n,f') <- vrec r f ]
        ++ [ (n,f  :@: a') | (n,a') <- vrec r a ]
  
      Val v ->
           [ (n,Val v') | (n,v') <- vrec r v ]
  
      One a -> [ (n, One a') | (n,a') <- rec r a ]
      All a -> [ (n, All a') | (n,a') <- rec r a ]
      Split a f g ->
           [ (n, Split a' f g) | (n,a') <- rec r a ]
        ++ [ (n, Split a f' g) | (n,f') <- vrec r f ]
        ++ [ (n, Split a f g') | (n,g') <- vrec r g ]
      _     -> []
   where
    -- recursively rewrite expressions in values
    vrec r (Var x) = []
    vrec r (HNF a) = [ (n,HNF a') | (n,a') <- hrec r a ]

    -- recursively rewrite expressions in HNFs
    hrec r (Arr as)         = [ (n,Arr (take i as ++ [a'] ++ drop (i+1) as))
                              | (i,a) <- [0..] `zip` as
                              , (n,a') <- vrec r a
                              ]
    hrec r (Lam (Bind x e)) = [ (n,Lam (Bind x e')) | (n,e') <- rec r e ]
    hrec r _                = []

  norm = structNorm

-- structural rules
-- every structural rule is implemented in 2 parts:
-- 1. make sure everything matches correctly
-- 2. make sure terms are "normalized" w.r.t. the rules

structMatch :: Rule Expr -> Rule Expr
structMatch r = struct
 where
  struct (Def (Bind x e)) =
    [ (n,ctx xe')
    | (ctx,e') <- structDefs e
    , (n,xe') <- r (Def (Bind x e'))
    ]
   where
    structDefs (Def (Bind y e)) = (Def . Bind y, e)
                                : [ (Def . Bind y . ctx, e') | (ctx,e') <- structDefs e ]
    structDefs _                = []
 
  struct ((VAR x1 :=: Val v1) :>: e) =
    [ (n,ctx ve')
    | (ctx,e') <- structSeqs e
    , (n,ve') <- r ((VAR x1 :=: Val v1) :>: e')
    ]
   where
    structSeqs ((VAR x1 :=: Val v1) :>: e) =
        (((VAR x1 :=: Val v1) :>:), e)
      : [(((VAR x1 :=: Val v1) :>:) . ctx, e') | (ctx,e') <- structSeqs e ]
    structSeqs _ = []

  struct _ = []

structNorm :: Expr -> Expr
structNorm e =
  case normalForms (rec rules) e of
    []       -> e
    (_,e'):_ -> e'
 where
  rules (Def (Bind x (Def (Bind y e)))) | x > y =
    [ ("SWAP-C", Def (Bind y (Def (Bind x e)))) ]

  rules ((VAR x1 :=: Val v1) :>: ((VAR x2 :=: Val v2) :>: e)) | (x1,v1) > (x2,v2) =
    [ ("SWAP-D", (VAR x2 :=: Val v2) :>: ((VAR x1 :=: Val v1) :>: e)) ]

  rules _ = []

--------------------------------------------------------------------------------

instance Free Expr where
  free (Val v)   = free v
  free (a :=: b) = free a `union` free b
  free (a :>: b) = free a `union` free b
  free (a :|: b) = free a `union` free b
  free (a :@: b) = free a `union` free b
  free (Def bnd) = free bnd
  free (One a)   = free a
  free (All a)   = free a
  free (Split e f g) = free e `union` free f `union` free g
  free _         = []

instance Free Value where
  free (Var x) = [x]
  free (HNF a) = free a

instance Free HNF where
  free (Arr vs)  = free vs
  free (Lam bnd) = free bnd
  free _         = []

--------------------------------------------------------------------------------

class Term a where
  subst :: Subst Value -> a -> a

instance Term Value where
  subst sub (Var x) = fromMaybe (Var x) (lookup x sub)
  subst sub (HNF a) = HNF (subst sub a)

instance Term HNF where
  subst sub (Arr vs) = Arr (map (subst sub) vs)
  subst sub (Lam bnd)= Lam (substBind Var subst sub bnd)
  subst sub a        = a

instance Term Expr where
  subst sub (Val v)   = Val (subst sub v)
  subst sub (a :=: b) = subst sub a :=: subst sub b
  subst sub (a :>: b) = subst sub a :>: subst sub b
  subst sub (a :|: b) = subst sub a :|: subst sub b
  subst sub (a :@: b) = subst sub a :@: subst sub b
  subst sub Fail      = Fail
  subst sub (Def bnd) = Def (substBind Var subst sub bnd)
  subst sub (One a)   = One (subst sub a)
  subst sub (All a)   = All (subst sub a)
  subst sub (Split e f g) = Split (subst sub e) (subst sub f) (subst sub g)
  subst sub Wrong     = Wrong

--------------------------------------------------------------------------------

instance Arbitrary Op where
  arbitrary = elements [ Add, Gt, IsInt ]

---

instance Arbitrary HNF where
  arbitrary = arbIdents >>= (sized . flip arbHNF)

  shrink (Int n)   = [ Int n' | n' <- shrink n ] ++ [ Arr [] ]
  shrink (Arr vs)  = [ Arr vs' | vs' <- shrink vs ]
  shrink (Lam bnd) = [ Arr [] ] ++ [ Lam (Bind x e') | let Bind x e = bnd, e' <- shrink e ]
  shrink _         = []

arbIdents :: Gen [Ident]
arbIdents =
  do k <- choose (1,7)
     return (take k (map ident names))
 where
  names = ["x","y","z","v","w"] ++ ["x" ++ show i | i <- [1..]]

arbHNF :: Int -> [Ident] -> Gen HNF
arbHNF n xs =
  frequency
  [ (1, Int `fmap` arbitrary)
  , (1, Op  `fmap` arbitrary)
  , (n, Arr `fmap` listOf (arbValue n2 xs))
  , (n, Lam `fmap` arbBind n1 xs)
  ]
 where
  n1 = n-1
  n2 = n `div` 2

---

instance Arbitrary Value where
  arbitrary = arbIdents >>= (sized . flip arbValue)

  shrink (Var _) = [ HNF (Int 0), HNF (Arr []) ]
  shrink (HNF a) = [ HNF a' | a' <- shrink a ]

arbValue :: Int -> [Ident] -> Gen Value
arbValue n xs =
  frequency $
  [ (1, Var `fmap` elements xs) | not (null xs) ] ++
  [ (n', HNF `fmap` arbHNF n1 xs)
  ]
 where
  n' = if null xs then 1 else n
  n1 = if n > 0 then n-1 else 0

---

instance Arbitrary Expr where
  arbitrary = sized (`arbExpr` []) -- closed by default

  shrink (Val v)   = [Val v'|v'<-shrink v] ++ [Def bnd|HNF(Lam bnd)<-[v]]
  shrink (a :=: b) = [a,b] ++ [a':=:b|a'<-shrink a] ++ [a:=:b'|b'<-shrink b]
  shrink (a :|: b) = [a,b] ++ [a':|:b|a'<-shrink a] ++ [a:|:b'|b'<-shrink b]
  shrink (a :>: b) = [a,b] ++ [a':>:b|a'<-shrink a] ++ [a:>:b'|b'<-shrink b]
  shrink (a :@: b) = [Val a,Val b] ++ [a':@:b|a'<-shrink a] ++ [a:@:b'|b'<-shrink b]
  shrink Fail      = []
  shrink (One a)   = [a] ++ [One a'| a'<-shrink a]
  shrink (All a)   = [a, One a, ARR []] ++ [All a'| a'<-shrink a]
  shrink (Def (Bind x a)) = [a |x `notElem` free a] ++ [Def (Bind x a') | a' <- shrink a]
  shrink (Split e f g) = [e, Val f, Val g] ++ [Split e' f g | e' <- shrink e]
                                           ++ [Split e f' g | f' <- shrink f]
                                           ++ [Split e f g' | g' <- shrink g]
  shrink Wrong     = []

arbExpr :: Int -> [Ident] -> Gen Expr
arbExpr n xs =
  frequency
  [ (1, Val `fmap` arbValue n xs)
  , (1, return Fail) -- maybe not have this?
  -- , (n, (:=:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  -- , (n, (\v e -> Val v :=: e) <$> arbValue n2 xs <*> arbExpr n2 xs)
  , (n, (:>:) <$> arbExprU n2 xs <*> arbExpr n2 xs)
  , (n, (:|:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (n, (:@:) <$> arbValue n2 xs <*> arbValue n2 xs)
  , (n, Def <$> arbBind n1 xs)
  , (n, One <$> arbExpr n1 xs)
  , (n, All <$> arbExpr n1 xs)
  -- , (n, Split <$> arbExpr n3 xs <*> arbValue n3 xs <*> arbValue n3 xs)
  ]
 where
  n1 = n-1
  n2 = n `div` 2
  n3 = n `div` 3

-- Either an expression or a unification
arbExprU :: Int -> [Ident] -> Gen Expr
arbExprU n xs =
  frequency
  [ (1, arbExpr n xs)
  , (1, (\v e -> Val v :=: e) <$> arbValue n2 xs <*> arbExpr n2 xs)
  ]
 where
  n2 = n `div` 2

arbBind :: Int -> [Ident] -> Gen (Bind Expr)
arbBind n xs =
  frequency $
  [ (1, do x <- elements xs
           Bind x <$> arbExpr n xs)
  | not (null xs)
  ] ++
  [ (4, do let x:_ = filter (`notElem` xs) (map Name ["x","y","z","v","w"] ++ map Prim [1..])
           Bind x <$> arbExpr n (x:xs))
  ]

--------------------------------------------------------------------------------
