{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-missing-signatures -Wno-missing-pattern-synonym-signatures -Wno-name-shadowing #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module TRSCore where

import Show
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

instance Show Expr where
  show (Val v)          = show v
  show (a :=: b)        = show' a ++ " = " ++ show' b
  show (a :>: b)        = show' a ++ "; " ++ show' b
  show (a :|: b)        = show' a ++ " | " ++ show' b
  show (a :@: b)        = show a ++ "(" ++ show b ++ ")"
  show Fail             = "fail"
  show (Def (Bind x a)) = "def " ++ show x ++ " in {" ++ show a ++ "}"
  show (One a)          = "one {" ++ show a ++ "}"
  show (All a)          = "all {" ++ show a ++ "}"
  show Wrong            = "wrong"
  show (Split e v1 v2)  = "split {" ++ show e ++ ", " ++ show v1 ++ ", " ++ show v2 ++ "}"

instance Parens Expr where
  parens (_ :=: _) = True
  parens (_ :>: _) = True
  parens (_ :|: _) = True
  parens (_ :@: _) = False
  parens _         = False

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
  show (Arr vs) = "arr{" ++ intercalate ", " (map show vs) ++ "}"
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
pattern HVAL :: HNF -> Expr
pattern HVAL v <- Val (getH -> Just v)
  where HVAL h = Val (HNF h)

getH :: Value -> Maybe HNF
getH (HNF v@Arr{}) = Just v
getH (HNF v@Lam{}) = Just v
getH _ = Nothing



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
  rec r (a :=: b) =
       [ (n, a' :=: b)  | (n,a') <- rec r a ]
    ++ [ (n, a  :=: b') | (n,b') <- rec r b ]
    
  rec r (a :|: b) =
       [ (n, a' :|: b)  | (n,a') <- rec r a ]
    ++ [ (n, a  :|: b') | (n,b') <- rec r b ]

  rec r (a :>: b) =
       [ (n, a' :>: b)  | (n,a') <- rec r a ]
    ++ [ (n, a  :>: b') | (n,b') <- rec r b ]

  rec r (Def (Bind x a)) =
       [ (n, Def (Bind x a')) | (n,a') <- r a ]

  rec r (f :@: a) =
       [ (n,f' :@: a)  | (n,f') <- vrec r f ]
    ++ [ (n,f  :@: a') | (n,a') <- vrec r a ]
  
  rec r (Val v) =
       [ (n,Val v') | (n,v') <- vrec r v ]
  
  rec r (One a) = [ (n, One a') | (n,a') <- rec r a ]
  rec r (All a) = [ (n, All a') | (n,a') <- rec r a ]
  rec r _       = []

vrec r (Var x) = []
vrec r (HNF a) = [ (n,HNF a') | (n,a') <- hrec r a ]

hrec r (Arr as)         = [ (n,Arr (take i as ++ [a'] ++ drop (i+1) as))
                          | (i,a) <- [0..] `zip` as
                          , (n,a') <- vrec r a
                          ]
hrec r (Lam (Bind x e)) = [ (n,Lam (Bind x e')) | (n,e') <- rec r e ]
hrec r _                = []

{-
  rec r (Split e f g) = [ (n,Split e' f g) | (n,e') <- r e ]



recAssoc :: (Expr -> [Expr]) -> Expr -> [Expr]
recAssoc r e =
     [ a' :=: b  | a :=: b <- es, a' <- r a ]
  ++ [ a  :=: b' | a :=: b <- es, b' <- r b ]
  ++ [ a' :>: b  | a :>: b <- es, a' <- r a ]
  ++ [ a  :>: b' | a :>: b <- es, b' <- r b ]
  ++ [ a' :|: b  | a :|: b <- es, a' <- r a ]
  ++ [ a  :|: b' | a :|: b <- es, b' <- r b ]
 where
  es = assoc e

-- normalizes associative operators on top-level
norm :: Expr -> Expr
norm ((a :=: b) :=: c) = norm (a :=: (b :=: c))
norm ((a :>: b) :>: c) = norm (a :>: (b :>: c))
norm ((a :|: b) :|: c) = norm (a :|: (b :|: c))
norm (a :=: b)         = a :=: norm b
norm (a :>: b)         = a :>: norm b
norm (a :|: b)         = a :|: norm b
norm a                 = a

-- mangles associative operators on top-level
assocs :: Expr -> [Expr]
assocs e@(a :=: (b :=: c)) = e : assocs ((a :=: b) :=: c)
assocs e@(a :>: (b :>: c)) = e : assocs ((a :>: b) :>: c)
assocs e@(a :|: (b :|: c)) = e : assocs ((a :|: b) :|: c)
assocs e                   = [e]

-- matcher to use for associative operators on top-level
assoc :: Expr -> [Expr]
assoc = assocs . norm
-}

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

{-
-- not using the "bind" trick for now
instance Binding Expr where
  binders (a :=: b) = binders a ++ binders b
  binders (a :>: b) = binders a ++ binders b
  binders (a :|: b) = binders a ++ binders b
  binders (a :@: b) = binders a ++ binders b
  binders (Def bnd) = [bnd]
  binders (One a)   = binders a
  binders (All a)   = binders a
  binders _         = []
-}

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

  shrink (Int n)  = [ Int n' | n' <- shrink n ] ++ [ Arr [] ]
  shrink (Arr vs) = [ Arr vs' | vs' <- shrink vs ]
  shrink _        = []

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
  ]
 where
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

  shrink (Val v)   = [ Val v' | v' <- shrink v ]
  shrink (a :=: b) = [a,b] ++ [a':=:b|a'<-shrink a] ++ [a:=:b'|b'<-shrink b]
  shrink (a :|: b) = [a,b] ++ [a':|:b|a'<-shrink a] ++ [a:|:b'|b'<-shrink b]
  shrink (a :>: b) = [a,b] ++ [a':>:b|a'<-shrink a] ++ [a:>:b'|b'<-shrink b]
  shrink (a :@: b) = [Val a,Val b] ++ [a':@:b|a'<-shrink a] ++ [a:@:b'|b'<-shrink b]
  shrink Fail      = []
  shrink (One a)   = [a] ++ [One a'| a'<-shrink a]
  shrink (All a)   = [a, ARR []] ++ [All a'| a'<-shrink a]
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
  , (n, (:=:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (n, (:>:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (n, (:|:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (n, (:@:) <$> arbValue n2 xs <*> arbValue n2 xs)
  , (n, Def <$> arbBind n1 xs)
  , (n, One <$> arbExpr n1 xs)
  , (n, All <$> arbExpr n1 xs)
  , (n, Split <$> arbExpr n3 xs <*> arbValue n3 xs <*> arbValue n3 xs)
  ]
 where
  n1 = n-1
  n2 = n `div` 2
  n3 = n `div` 3

arbBind :: Int -> [Ident] -> Gen (Bind Expr)
arbBind n xs =
  frequency $
  [ (1, do x <- elements xs
           Bind x <$> arbExpr n xs)
  | not (null xs)
  ] ++
  [ (4, do let x = identNotIn xs
           Bind x <$> arbExpr n (x:xs))
  ]

--------------------------------------------------------------------------------
