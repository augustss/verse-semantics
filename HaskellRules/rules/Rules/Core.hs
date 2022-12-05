{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Rules.Core(
  Expr(..), Op(..),
  Value,
  TRSFlags, RuleEnv(..), defaultTRSFlags,
  DerefPos(..),
  ERule,
  EContext,
  pattern Val,
  pattern HNF,
  pattern CON,
  isHNF,
  pattern DEF,
  pattern LAM,
  subst,
  check,
  ) where
import GHC.Stack(HasCallStack)

import TRS.Bind
import TRS.TRS
import Test.QuickCheck
import Data.List( intercalate, union, elemIndex )
import Data.Maybe

type ERule = Rule Expr
type EContext = Expr -> Expr

--------------------------------------------------------------------------------

data Expr
    -- The following 5 are the old Value type
  = Var Ident                   -- ^ x
    -- The following 4 are the old HNF type
  | Int Integer                 -- ^ k
  | Op Op                       -- ^ op
  | Arr [Expr]                  -- ^ <e1,e2,...>
  | Lam (Bind Expr)             -- ^ \ x . e
  --
  | Expr :=: Expr               -- ^ e1 = e2
  | Expr :>: Expr               -- ^ e1; e2
  | Expr :|: Expr               -- ^ e1 | e2
  | Expr :@: Expr               -- ^ v1(v2)
  | Def (Bind Expr)             -- ^ ex x. e
  | One Expr                    -- ^ one { e }
  | All Expr                    -- ^ all { e }
  | Fail                        -- ^ fail
  | Wrong                       -- ^ wrong
  | Split Expr Expr Expr        -- ^ split { e, v1, v2 }

type Value = Expr

infixr 1 :>:
infixr 3 :|:
infixr 2 :=:
infixl 4 :@:

instance Show Expr where
  showsPrec p (Var v)          = showsPrec p v
  showsPrec p (Int k)          = showsPrec p k
  showsPrec p (Op o)           = showsPrec p o
  showsPrec _ (Arr es)         = showString $ "<" ++ intercalate ", " (map show es) ++ ">"
  showsPrec p (Lam (Bind x e)) = showParen (p > 0) $ showString $ "\\" ++ show x ++ "." ++ show e
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
  compare = comp [] []
   where
    -- so much code... this can probably simplified a lot
    comp  xs  ys (Var x) (Var y) =
      case (elemIndex x xs, elemIndex y ys) of
        (Just i, Just j)   -> i `compare` j
        (Nothing, Nothing) -> x `compare` y
        (Just _, Nothing)  -> LT
        (Nothing, Just _)  -> GT
    comp _xs _ys (Var _) _       = LT
    comp _xs _ys _       (Var _) = GT

    comp _xs _ys (Int a) (Int b) = compare a b
    comp _xs _ys (Int _) _       = LT
    comp _xs _ys _       (Int _) = GT

    comp _xs _ys (Op a) (Op b) = compare a b
    comp _xs _ys (Op _) _      = LT
    comp _xs _ys _      (Op _) = GT

    comp  xs  ys (Arr vs) (Arr ws)
      | n == m    = head (dropWhile (==EQ) (zipWith (comp xs ys) vs ws) ++ [EQ])
      | otherwise = n `compare` m
     where
      n  = length vs
      m  = length ws
    comp _xs _ys (Arr _) _       = LT
    comp _xs _ys _       (Arr _) = GT

    comp  xs  ys (Lam (Bind x a)) (Lam (Bind y b)) = comp (x:xs) (y:ys) a b
    comp _xs _ys (Lam _) _       = LT
    comp _xs _ys _       (Lam _) = GT

    comp _xs _ys Wrong Wrong = EQ
    comp _xs _ys Wrong _     = LT
    comp _xs _ys _     Wrong = GT

    comp _xs _ys Fail Fail = EQ
    comp _xs _ys Fail _    = LT
    comp _xs _ys _    Fail = GT

    comp  xs  ys (a:=:b) (c:=:d) = comp xs ys a c & comp xs ys b d
    comp _xs _ys (_:=:_) _       = LT
    comp _xs _ys _       (_:=:_) = GT

    comp  xs  ys (a:>:b) (c:>:d) = comp xs ys a c & comp xs ys b d
    comp _xs _ys (_:>:_) _       = LT
    comp _xs _ys _       (_:>:_) = GT

    comp  xs  ys (a:|:b) (c:|:d) = comp xs ys a c & comp xs ys b d
    comp _xs _ys (_:|:_) _       = LT
    comp _xs _ys _       (_:|:_) = GT

    comp  xs  ys (a:@:b) (c:@:d) = comp xs ys a c & comp xs ys b d
    comp _xs _ys (_:@:_) _       = LT
    comp _xs _ys _       (_:@:_) = GT

    comp  xs  ys (One a) (One b) = comp xs ys a b
    comp _xs _ys (One _) _       = LT
    comp _xs _ys _       (One _) = GT

    comp  xs  ys (All a) (All b) = comp xs ys a b
    comp _xs _ys (All _) _       = LT
    comp _xs _ys _       (All _) = GT

    comp  xs  ys (Split e f g) (Split e' f' g') = comp xs ys e e' & comp xs ys f f' & comp xs ys g g'
    comp _xs _ys Split {} _ = LT
    comp _xs _ys _ Split {} = GT

    comp  xs  ys (Def (Bind x a)) (Def (Bind y b)) = comp (x:xs) (y:ys) a b

    EQ & c = c
    c  & _ = c

--------------------------------------------------------------------------------

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

pattern DEF :: Ident -> Expr -> Expr
pattern DEF x e = Def (Bind x e)

pattern LAM :: Ident -> Expr -> Expr
pattern LAM x e = Lam (Bind x e)

pattern Val :: Expr -> Expr
pattern Val e <- (getVal -> Just e)
  where Val e | Just _ <- getVal e = e
              | otherwise = error "pattern Val"

getVal :: Expr -> Maybe Expr
getVal e@Var{} = Just e
getVal e = getHNF e

pattern HNF :: Expr -> Expr
pattern HNF e <- (getHNF -> Just e)
--  where HNF e = e

getHNF :: Expr -> Maybe Expr
getHNF e@Int{} = Just e
getHNF e@Op{} = Just e
getHNF e@Arr{} = Just e
getHNF e@Lam{} = Just e
getHNF _ = Nothing

isHNF :: Expr -> Bool

isHNF = isJust . getHNF

pattern CON :: Expr -> Expr
pattern CON e <- (getCON -> Just e)

getCON :: Expr -> Maybe Expr
getCON e@Int{} = Just e
getCON e@Op{} = Just e
getCON _ = Nothing

--------------------------------------------------------------------------------

type TRSFlags = RuleEnv Expr

-- Where should derefA substitute?
data DerefPos
  = Consumed          -- Only in consuming positions (e.g. application)
  | ConsumedOrBarrEq  -- Consumed and in unification under barrier.
  deriving (Eq, Ord, Show)

defaultTRSFlags :: TRSFlags
defaultTRSFlags = TRSFlags { tfUnderLambda = True, tfDerefPos = Consumed }

instance Rec Expr where
  data RuleEnv Expr = TRSFlags
    { tfUnderLambda :: !Bool     -- reduce under lambda
    , tfDerefPos    :: !DerefPos -- where derefH is substituting
    }
  rec r s ae =
    r s ae ++
    case ae of
      a :=: b ->
           [ (n, a' :=: b)  | (n,a') <- rec r s a ]
        ++ [ (n, a  :=: b') | (n,b') <- rec r s b ]
    
      a :|: b ->
           [ (n, a' :|: b)  | (n,a') <- rec r s a ]
        ++ [ (n, a  :|: b') | (n,b') <- rec r s b ]

      a :>: b ->
           [ (n, a' :>: b)  | (n,a') <- rec r s a ]
        ++ [ (n, a  :>: b') | (n,b') <- rec r s b ]

      Def (Bind x a) ->
           [ (n, Def (Bind x a')) | (n,a') <- rec r s a ]

      f :@: a ->
           [ (n,f' :@: a)  | (n,f') <- rec r s f ]
        ++ [ (n,f  :@: a') | (n,a') <- rec r s a ]
  
      Arr as -> [ (n,Arr (take i as ++ [a'] ++ drop (i+1) as))
                | (i,a) <- [0..] `zip` as
                , (n,a') <- rec r s a
                ]
      Lam (Bind x e)
        | tfUnderLambda s -> [ (n,Lam (Bind x e')) | (n,e') <- rec r s e ]

      One a -> [ (n, One a') | (n,a') <- rec r s a ]
      All a -> [ (n, All a') | (n,a') <- rec r s a ]
      Split a f g ->
           [ (n, Split a' f g) | (n,a') <- rec r s a ]
        ++ [ (n, Split a f' g) | (n,f') <- rec r s f ]
        ++ [ (n, Split a f g') | (n,g') <- rec r s g ]
      _     -> []

--------------------------------------------------------------------------------

instance Free Expr where
  free (Var v)   = [v]
  free (Arr vs)  = free vs
  free (Lam bnd) = free bnd
  free (a :=: b) = free a `union` free b
  free (a :>: b) = free a `union` free b
  free (a :|: b) = free a `union` free b
  free (a :@: b) = free a `union` free b
  free (Def bnd) = free bnd
  free (One a)   = free a
  free (All a)   = free a
  free (Split e f g) = free e `union` free f `union` free g
  free _         = []

--------------------------------------------------------------------------------

class Term a where
  subst :: Subst Expr -> a -> a

instance Term Expr where
  subst sub (Var x)   = fromMaybe (Var x) (lookup x sub)
  subst _sub e@Int{}  = e
  subst _sub e@Op{}   = e
  subst sub (Arr vs)  = Arr (map (subst sub) vs)
  subst sub (Lam bnd) = Lam (substBind Var subst sub bnd)
  subst sub (a :=: b) = subst sub a :=: subst sub b
  subst sub (a :>: b) = subst sub a :>: subst sub b
  subst sub (a :|: b) = subst sub a :|: subst sub b
  subst sub (a :@: b) = subst sub a :@: subst sub b
  subst _sub Fail     = Fail
  subst sub (Def bnd) = Def (substBind Var subst sub bnd)
  subst sub (One a)   = One (subst sub a)
  subst sub (All a)   = All (subst sub a)
  subst sub (Split e f g) = Split (subst sub e) (subst sub f) (subst sub g)
  subst _sub Wrong    = Wrong

--------------------------------------------------------------------------------

instance Arbitrary Op where
  arbitrary = elements [ Add, Gt ]

{-
arbIdents :: Gen [Ident]
arbIdents =
  do k <- choose (1,7)
     return (take k (map ident names))
 where
  names = ["x","y","z","v","w"] ++ ["x" ++ show i | i <- [1::Int ..]]
-}

---

instance Arbitrary Expr where
  arbitrary = sized (`arbExpr` []) -- closed by default

  shrink (Var _)   = [ Int 0, Arr [] ]
  shrink (Int n)   = [ Int n' | n' <- shrink n ] ++ [ Arr [] ]
  shrink (Op _)    = []
  shrink (Arr vs)  = [ Arr vs' | vs' <- shrink vs ]
  shrink (Lam bnd) = [ Arr [] ] ++ [ Lam (Bind x e') | let Bind x e = bnd, e' <- shrink e ]
  shrink (a :=: b) = [a,b] ++ [a':=:b|a'<-shrink a] ++ [a:=:b'|b'<-shrink b]
  shrink (a :|: b) = [a,b] ++ [a':|:b|a'<-shrink a] ++ [a:|:b'|b'<-shrink b]
  shrink (a :>: b) = as ++ [b] ++ [a':>:b|a'<-shrink a] ++ [a:>:b'|b'<-shrink b]
    where as = case a of _ :=: _ -> []; _ -> [a]
  shrink (a :@: b) = [a, b] ++ [a':@:b|a'<-shrink a] ++ [a:@:b'|b'<-shrink b]
  shrink Fail      = []
  shrink (One a)   = [a] ++ [One a'| a'<-shrink a]
  shrink (All a)   = [a, One a, Arr []] ++ [All a'| a'<-shrink a]
  shrink (Def (Bind x a)) = [a |x `notElem` free a] ++ [Def (Bind x a') | a' <- shrink a]
  shrink (Split e f g) = [e, f, g] ++ [Split e' f g | e' <- shrink e]
                                   ++ [Split e f' g | f' <- shrink f]
                                   ++ [Split e f g' | g' <- shrink g]
  shrink Wrong     = []

arbExpr :: Int -> [Ident] -> Gen Expr
arbExpr n xs =
  frequency $
  [ (1, Var <$> elements xs) | not (null xs) ] ++
  [ (1, Int <$> arbitrary)
  , (1, Op  <$> arbitrary)
  , (n, Arr <$> scale (min 5) (listOf (arbExpr n2 xs)))
  , (n, Lam <$> arbBind n1 xs)
  , (1, return Fail) -- maybe not have this?
  , (n, (:=:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (n, (:>:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (n, (:|:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (n, (:@:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (n, Def <$> arbBind n1 xs)
  , (n, One <$> arbExpr n1 xs)
  , (n, All <$> arbExpr n1 xs)
  -- , (n, Split <$> arbExpr n3 xs <*> arbValue n3 xs <*> arbValue n3 xs)
  ]
 where
  n1 = n-1
  n2 = n `div` 2
  -- n3 = n `div` 3

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

-- XXX Move somewhere better
check :: (HasCallStack) => (Expr -> Bool) -> Expr -> Expr
check p a | p a = a
          | otherwise = error $ "check failed: " ++ show a
