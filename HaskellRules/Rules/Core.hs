{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Rules.Core(
  Expr(..), Op(..),
  Heap, Ptr(..),
  Value,
  TRSFlags, RuleEnv(..), defaultTRSFlags,
  DerefPos(..),
  ERule,
  EContext,
  pattern Val, getVal,
  pattern HNF, getHNF,
  pattern CON,
  isHNF,
  isVal,
  pattern EXI,
  pattern LAM,
  pattern Block, Eqn,
  getExis,
  exis,
  opArity,
  comp,
  subst,
  alphaRename,
  invariant,
  collect,
  allVars,
  check,
  substExp,
  BndVar(..),
  boundVars, flexVars, rigidVars,
  ) where
import qualified Epic.SIntMap as IM
import Data.Char
import Data.Data(Data)
import Data.List( union, elemIndex )
import Data.Maybe
import GHC.Stack(HasCallStack)

import TRS.Bind
import TRS.TRS
import Test.QuickCheck hiding ( collect )
import Epic.List(nub)
import Epic.Print hiding ((<>))
import qualified Epic.Print as P

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
  | Ident :~: Ident             -- ^ e1 ~ e2
  | Expr :>: Expr               -- ^ e1; e2
  | Expr :|: Expr               -- ^ e1 | e2
  | Expr :@: Expr               -- ^ v1(v2)
  | Exi (Bind Expr)             -- ^ ex x. e
  | One Expr                    -- ^ one { e }
  | All Expr                    -- ^ all { e }
  | Fail                        -- ^ fail
  | Wrong String                -- ^ wrong
  | If Expr Expr Expr           -- ^ if e1 e2 e3

  -- used for verification (experimental)
  | Assert Expr                 -- ^ assert{ e }
  | Assume Expr                 -- ^ assume{ e }
  | Verify Expr                 -- ^ verify{ e }
  | Decide Expr                 -- ^ decide{ e }

  | Split Expr Expr Expr        -- ^ split { e, v1, v2 }
  | BlockC Expr                 -- ^ same as e, but maintaining invariants
  -- only used for updatable references
  | Store Heap Expr
  | Ref Ptr
  deriving (Show, Data)

instance CoArbitrary Expr where
  coarbitrary e = coarbitrary (show e) -- cool hack!

type Value = Expr

type Heap = IM.SIntMap Ptr Value
instance Free Heap where
  free h = free (IM.elems h)

newtype Ptr = Ptr Int deriving (Show, Eq, Ord, Data, Enum)

instance Pretty Ptr where pPrintPrec _ _ (Ptr i) = text ("r" ++ show i)

infixr 1 :>:
infixr 3 :|:
infixr 2 :=:
infixl 4 :@:
infix  5 :~:

instance Pretty Expr where
  pPrintPrec l p (Var v)          = pPrintPrec l p v
  pPrintPrec l p (Int k)          = pPrintPrec l p k
  pPrintPrec l p (Op o)           = pPrintPrec l p o
  pPrintPrec l _ (Arr es)         = text "<" <> fsep (punctuate (text ",") (map (pPrintPrec l 0) es)) <> text ">"
  pPrintPrec l p (LAM x e)        = maybeParens (p > 0) $ sep [text "\\" <> pPrintPrec l 0 x <> text ".", pPrintPrec l 0 e]
  pPrintPrec l p (a :|: b)        = maybeParens (l >= prettyNormal || p > 3) $ sep [pPrintPrec l 4 a <+> text "|", pPrintPrec l 4 b]
  pPrintPrec l p e@(_ :>: _)      = maybeParens (p > 1) $ sep $ punctuate (text ";")  $ map (pPrintPrec l 2) $ ap [] e
                                    where ap r (a :>: b) = ap (r ++ [a]) b; ap r a = r ++ [a]
  pPrintPrec l p (a :=: b)        = maybeParens (l >= prettyNormal || p > 2) $ pPrintPrec l 3 a <+> text "=" <+> pPrintPrec l 3 b
  pPrintPrec l p (a :~: b)        = maybeParens (p > 5) $ pPrintPrec l 6 a <+> text "~" <+> pPrintPrec l 6 b
  pPrintPrec l p (a :@: b)        = maybeParens (p > 4) $ pPrintPrec l 4 a <> text "(" <> pPrintPrec l 0 b <> text ")"
  pPrintPrec _ _ Fail             = text "fail"
  pPrintPrec l p e@(EXI{})        = maybeParens (p > 0) $ text "ex" <+> sep [hcat (punctuate (text " ") (map (pPrintPrec l 0) xs)) P.<> text ".",
                                                                             pPrintPrec l 0 a]
                                    where (xs, a) = getExis e
  pPrintPrec l _ (One a)          = text "one {" <> pPrintPrec l 0 a <> text "}"
  pPrintPrec l _ (All a)          = text "all {" <> pPrintPrec l 0 a <> text "}"
  pPrintPrec l _ (Assume a)       = text "assume {" <> pPrintPrec l 0 a <> text "}"
  pPrintPrec l _ (Assert a)       = text "assert {" <> pPrintPrec l 0 a <> text "}"
  pPrintPrec l _ (Decide a)       = text "decide {" <> pPrintPrec l 0 a <> text "}"
  pPrintPrec l _ (Verify a)       = text "verify {" <> pPrintPrec l 0 a <> text "}"
  pPrintPrec l _ (If a b c)       = text "if" <+> parens (pPrintPrec l 0 a) <+> braces (pPrintPrec l 0 b) <+> text "else" <+> braces (pPrintPrec l 0 c)
  pPrintPrec _ _ (Wrong s)        = text $ "wrong(" ++ show s ++")"
  pPrintPrec l _ (Split e v1 v2)  = text "split {" P.<> sep [pPrintPrec l 0 e P.<> text ",",
                                                             pPrintPrec l 0 v1 P.<> text ",",
                                                             pPrintPrec l 0 v2] P.<> text "}"
  pPrintPrec l _ (BlockC e)       = text "block {" <> pPrintPrec l 0 e <> text "}"
  pPrintPrec l _ (Store h e)      = text "store {" P.<> sep [pPrintPrec l 0 (IM.toList h) P.<> text ",",
                                                             pPrintPrec l 0 e] P.<> text "}"
  pPrintPrec l p (Ref r)          = pPrintPrec l p r
  pPrintPrec _ _ _                = undefined -- GHC bug

instance Eq Expr where
  a == b = a `compare` b == EQ

instance Ord Expr where
  compare = comp [] []

-- so much code... this can probably simplified a lot
comp :: [Ident] -> [Ident] -> Expr -> Expr -> Ordering
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
  | n == m    = foldr (<>) EQ (zipWith (comp xs ys) vs ws)
  | otherwise = n `compare` m
 where
  n  = length vs
  m  = length ws
comp _xs _ys (Arr _) _       = LT
comp _xs _ys _       (Arr _) = GT

comp  xs  ys (LAM x a) (LAM y b) = comp (x:xs) (y:ys) a b
comp _xs _ys (Lam _) _       = LT
comp _xs _ys _       (Lam _) = GT

comp _xs _ys Wrong{} Wrong{} = EQ
comp _xs _ys Wrong{} _       = LT
comp _xs _ys _       Wrong{} = GT

comp _xs _ys Fail Fail = EQ
comp _xs _ys Fail _    = LT
comp _xs _ys _    Fail = GT

comp  xs  ys (a:=:b) (c:=:d) = comp xs ys a c <> comp xs ys b d
comp _xs _ys (_:=:_) _       = LT
comp _xs _ys _       (_:=:_) = GT

comp  xs  ys (a:~:b) (c:~:d) = comp xs ys (Var a) (Var c) <> comp xs ys (Var b) (Var d)
comp _xs _ys (_:~:_) _       = LT
comp _xs _ys _       (_:~:_) = GT

comp  xs  ys (a:>:b) (c:>:d) = comp xs ys a c <> comp xs ys b d
comp _xs _ys (_:>:_) _       = LT
comp _xs _ys _       (_:>:_) = GT

comp  xs  ys (a:|:b) (c:|:d) = comp xs ys a c <> comp xs ys b d
comp _xs _ys (_:|:_) _       = LT
comp _xs _ys _       (_:|:_) = GT

comp  xs  ys (a:@:b) (c:@:d) = comp xs ys a c <> comp xs ys b d
comp _xs _ys (_:@:_) _       = LT
comp _xs _ys _       (_:@:_) = GT

comp  xs  ys (One a) (One b) = comp xs ys a b
comp _xs _ys (One _) _       = LT
comp _xs _ys _       (One _) = GT

comp  xs  ys (All a) (All b) = comp xs ys a b
comp _xs _ys (All _) _       = LT
comp _xs _ys _       (All _) = GT

comp  xs  ys (Assume a) (Assume b) = comp xs ys a b
comp _xs _ys (Assume _) _          = LT
comp _xs _ys _          (Assume _) = GT

comp  xs  ys (Assert a) (Assert b) = comp xs ys a b
comp _xs _ys (Assert _) _          = LT
comp _xs _ys _          (Assert _) = GT

comp  xs  ys (Decide a) (Decide b) = comp xs ys a b
comp _xs _ys (Decide _) _          = LT
comp _xs _ys _          (Decide _) = GT

comp  xs  ys (Verify a) (Verify b) = comp xs ys a b
comp _xs _ys (Verify _) _          = LT
comp _xs _ys _          (Verify _) = GT

comp  xs  ys (Split e f g) (Split e' f' g') = comp xs ys e e' <> comp xs ys f f' <> comp xs ys g g'
comp _xs _ys Split {} _ = LT
comp _xs _ys _ Split {} = GT

comp  xs  ys (BlockC a) (BlockC b) = comp xs ys a b
comp _xs _ys BlockC{} _ = LT
comp _xs _ys _ BlockC{} = GT

comp  xs  ys (Store h e) (Store h' e') =
  compare (IM.keys h) (IM.keys h') <> comp xs ys (Arr (IM.elems h)) (Arr (IM.elems h')) <> comp xs ys e e'
comp _xs _ys Store {} _ = LT
comp _xs _ys _ Store {} = GT

comp _xs _ys (Ref p) (Ref q) = compare p q
comp _xs _ys Ref {} _        = LT
comp _xs _ys _ Ref {}        = GT

comp  xs  ys (If a1 a2 a3) (If b1 b2 b3) = comp xs ys a1 b1 <> comp xs ys a2 b2 <> comp xs ys a3 b3
comp  _   _  If{}           _            = LT
comp  _   _  _              If{}         = GT

comp  xs  ys (EXI x a) (EXI y b) = comp (x:xs) (y:ys) a b

comp _ _ _ _ = undefined -- GHC bug

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
  | Alloc
  | Read
  | Write
  | AddTo
  | DotDot
  | Print
  | Append
 deriving ( Show, Eq, Ord, Data )

instance Pretty Op where
  pPrintPrec _ _ = text . map toLower . show

opArity :: Op -> Int
opArity o | o `elem` [Neg, Plus, IsInt, MapAp, Alloc, Read, Print] = 1
          | o == Append = 3
          | otherwise = 2

getExis :: Expr -> ([Ident], Expr)
getExis = get []
  where get vs (EXI v b) = get (v:vs) b
        get vs b = (reverse vs, b)

exis :: [Ident] -> Expr -> Expr
exis is e = foldr EXI e is

--------------------------------------------------------------------------------
-- patterns

-- Expr

pattern EXI :: Ident -> Expr -> Expr
pattern EXI x e = Exi (Bind x e)

pattern LAM :: Ident -> Expr -> Expr
pattern LAM x e = Lam (Bind x e)

pattern Val :: Expr -> Expr
pattern Val e <- (getVal -> Just e)
  where Val e | Just _ <- getVal e = e
              | otherwise = error ("pattern Val " ++ prettyShow e)

getVal :: Expr -> Maybe Expr
getVal e@Var{} = Just e
getVal (Assume (getVal -> Just v)) = Just (Assume v)
getVal e = getHNF e

isVal :: Expr -> Bool
isVal = isJust . getVal

pattern HNF :: Expr -> Expr
pattern HNF e <- (getHNF -> Just e)
--  where HNF e = e

getHNF :: Expr -> Maybe Expr
getHNF e@Int{} = Just e
getHNF e@Op{}  = Just e
getHNF e@Arr{} = Just e
getHNF e@Ref{} = Just e
getHNF e@Lam{} = Just e
getHNF _ = Nothing

isHNF :: Expr -> Bool
isHNF = isJust . getHNF

pattern CON :: Expr -> Expr
pattern CON e <- (getCON -> Just e)

getCON :: Expr -> Maybe Expr
getCON e@Int{} = Just e
getCON e@Op{} = Just e
getCON e@Ref{} = Just e
getCON _ = Nothing

type Eqn = (Value, Expr)

pattern Block :: [Ident] -> [(Value, Expr)] -> Value -> Expr
pattern Block xs bs v <- (BlockC (getBlock -> Just (xs, bs, v)))
  where Block xs bs v = BlockC (exis xs (foldr eqn v bs))
          where eqn (a, b) r = (a :=: b) :>: r

getBlock :: Expr -> Maybe ([Ident], [Eqn], Value)
getBlock = blk
  where
    blk (EXI i e) = (\ (xs, qs, v) -> (i:xs, qs, v)) <$> blk e
    blk e = blk' e
    blk' ((Val a :=: e) :>: b) = (\ (xs, qs, v) -> (xs, (a, e) : qs, v)) <$> blk' b
--    blk' (Val v) = Just ([], [], v)
--    blk' _ = Nothing
    blk' (_ :>: _) = Nothing
    blk' (_ :=: _) = Nothing
    blk' e = Just ([], [], e)

--------------------------------------------------------------------------------

type TRSFlags = RuleEnv Expr

-- Where should derefA substitute?
data DerefPos
  = Consumed          -- Only in consuming positions (e.g. application)
  | ConsumedOrBarrEq  -- Consumed and in unification under barrier.
  deriving (Eq, Ord, Show)

defaultTRSFlags :: TRSFlags
defaultTRSFlags =
  TRSFlags { tfUnderLambda = True, tfDerefPos = Consumed, tfUseTilde = False
           , tfUseWFEqVar = False, tfNormSteps = 10000, tfTrace = False, tfRewriteSteps = 10000
           , bndVars = [] }

instance Rec Expr where
  data RuleEnv Expr = TRSFlags
    { tfUnderLambda :: !Bool     -- reduce under lambda
    , tfDerefPos    :: !DerefPos -- where derefH is substituting
    , tfUseTilde    :: !Bool     -- use x~y expressions
    , tfUseWFEqVar  :: !Bool     -- Use WF-Eq with flipped arguments, i.e., y=x
    , tfRewriteSteps:: !Int      -- Maximum rewrite steps
    , tfNormSteps   :: !Int      -- Maximum normalization steps
    , tfTrace       :: !Bool     -- trace evaluation
    , bndVars       :: ![BndVar] -- temporary during reduction
    }
  rec r s ae =
    r s ae ++
    case ae of
      a :=: b ->
           [ (n, a' :=: b)  | (n,a') <- rec r s a ]
        ++ [ (n, a  :=: b') | (n,b') <- rec r s b ]

      a :|: b ->
           [ (n, a' :|: b)  | (n,a') <- rec r s' a ]
        ++ [ (n, a  :|: b') | (n,b') <- rec r s' b ]
           where s' = addBound BBlk s

      a :>: b ->
           [ (n, a' :>: b)  | (n,a') <- rec r s a ]
        ++ [ (n, a  :>: b') | (n,b') <- rec r s b ]

      If a b c ->
            [ (n, If a' b c) | (n,a') <- rec r s a ]
          ++ [ (n, If a b' c) | (n,b') <- rec r s b ]
          ++ [ (n, If a b c') | (n,c') <- rec r s c ]

      Exi (Bind x a) ->
           [ (n, Exi (Bind x a')) | (n,a') <- rec r (addBound (BExi x) s) a ]

      f :@: a ->
           [ (n,f' :@: a)  | (n,f') <- rec r s f ]
        ++ [ (n,f  :@: a') | (n,a') <- rec r s a ]

      Arr as -> [ (n,Arr (take i as ++ [a'] ++ drop (i+1) as))
                | (i,a) <- [0..] `zip` as
                , (n,a') <- rec r s a
                ]
      Lam (Bind x e)
        | tfUnderLambda s -> [ (n,Lam (Bind x e')) | (n,e') <- rec r (addBound (BLam x) s) e ]

      One a -> [ (n, One a') | (n,a') <- rec r (addBound BBlk s) a ]
      All a -> [ (n, All a') | (n,a') <- rec r (addBound BBlk s) a ]
      Assume a -> [ (n, Assume a') | (n,a') <- rec r (addBound BBlk s) a ]
      Assert a -> [ (n, Assert a') | (n,a') <- rec r (addBound BBlk s) a ]
      Verify a -> [ (n, Verify a') | (n,a') <- rec r (addBound BBlk s) a ]
      Split a f g ->
           [ (n, Split a' f g) | (n,a') <- rec r (addBound BBlk s) a ]
        ++ [ (n, Split a f' g) | (n,f') <- rec r s f ]
        ++ [ (n, Split a f g') | (n,g') <- rec r s g ]
      BlockC a ->
           [ (n, BlockC a') | (n, a') <- rec r s a ]
      -- No reductions in the store, it's supposed to be a Value
      Store h e -> [ (n, Store h e') | (n,e') <- rec r s e ]
      _     -> []
     where addBound x tf = tf{ bndVars = x : bndVars tf }

data BndVar = BExi Ident | BLam Ident | BBlk
  deriving (Show)

boundVars :: TRSFlags -> [Ident]
boundVars = bndIds . bndVars

flexVars :: TRSFlags -> [Ident]
flexVars = bndIds . takeWhile isBExi . bndVars

rigidVars :: TRSFlags -> [Ident]
rigidVars = bndIds . dropWhile isBExi . bndVars

isBExi :: BndVar -> Bool
isBExi BExi{} = True
isBExi _ = False

bndIds :: [BndVar] -> [Ident]
bndIds [] = []
bndIds (BExi x : bs) = x : bndIds bs
bndIds (BLam x : bs) = x : bndIds bs
bndIds (BBlk   : bs) =     bndIds bs

--------------------------------------------------------------------------------

instance Free Expr where
  free (Var v)   = [v]
  free Int{}     = []
  free Op{}      = []
  free (Arr vs)  = free vs
  free (Lam bnd) = free bnd
  free (a :=: b) = free a `union` free b
  free (a :~: b) = free a `union` free b
  free (a :>: b) = free a `union` free b
  free (a :|: b) = free a `union` free b
  free (a :@: b) = free a `union` free b
  free (Exi bnd) = free bnd
  free (One a)   = free a
  free (All a)   = free a
  free (Assume a) = free a
  free (Assert a) = free a
  free (Verify a) = free a
  free (Decide a) = free a
  free (If a b c) = free a `union` free b `union` free c
  free (Split e f g) = free e `union` free f `union` free g
  free (BlockC e) = free e
  free Fail      = []
  free Wrong{}   = []
  free (Store h e) = free h `union` free e
  free Ref{}     = []

--------------------------------------------------------------------------------

class Term a where
  subst :: Subst Expr -> a -> a

-- rename the binder so that it is not the same as the first argument
alphaRename :: (Term a, Free a) => [Ident] -> Bind a -> Bind a
alphaRename xs bnd@(Bind x e)
  | x `notElem` xs = bnd
  | otherwise      = Bind y (subst [(x,Var y)] e)
 where
  y = identNotIn (x : (xs ++ free e))

instance Term Expr where
  subst [] e = e
  subst sub (Var x)   = fromMaybe (Var x) (lookup x sub)
  subst _sub e@Int{}  = e
  subst _sub e@Op{}   = e
  subst sub (Arr vs)  = Arr (map (subst sub) vs)
  subst sub (Lam bnd) = Lam (substBind Var subst sub bnd)
  subst sub (a :=: b) = subst sub a :=: subst sub b
  subst sub (a :~: b) = substVar sub a :~: substVar sub b
  subst sub (a :>: b) = subst sub a :>: subst sub b
  subst sub (a :|: b) = subst sub a :|: subst sub b
  subst sub (a :@: b) = subst sub a :@: subst sub b
  subst sub (If a b c) = If (subst sub a) (subst sub b) (subst sub c)
  subst _sub Fail     = Fail
  subst _sub e@Wrong{}= e
  subst sub (Exi bnd) = Exi (substBind Var subst sub bnd)
  subst sub (One a)   = One (subst sub a)
  subst sub (All a)   = All (subst sub a)
  subst sub (Assume a) = Assume (subst sub a)
  subst sub (Assert a) = Assert (subst sub a)
  subst sub (Verify a) = Verify (subst sub a)
  subst sub (Decide a) = Decide (subst sub a)
  subst sub (Split e f g) = Split (subst sub e) (subst sub f) (subst sub g)
  subst sub (BlockC e) = BlockC (subst sub e)
  subst sub (Store h e) = Store (IM.map (subst sub) h) (subst sub e)
  subst _sub e@Ref{}  = e

instance (Term a, Term b) => Term (a, b) where
  subst sub (a, b) = (subst sub a, subst sub b)

instance (Term a, Term b, Term c) => Term (a, b, c) where
  subst sub (a, b, c) = (subst sub a, subst sub b, subst sub c)

instance (Term a) => Term [a] where
  subst sub xs = map (subst sub) xs

-- TODO(augustss):
-- We don't normally substitute in ~, but we still need to be able to alpha convert,
-- so we need to handle that somehow.
-- It would probably be better to have an alpha conversion function.
substVar :: Subst Expr -> Ident -> Ident
substVar sub x =
  case lookup x sub of
    Nothing -> x
    Just (Var y) -> y
    Just _ -> error "substVar"

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
  arbitrary = sized (`arbExpr` map Name ["a","b","c"]) -- closed by default

  -- shrink _ = []
  shrink (Var _)   = [ Int 0, Arr [] ]
  shrink (Int n)   = [ Int n' | n' <- shrink n ] ++ [ Arr [] ]
  shrink (Op _)    = []
  shrink (Arr vs)  = [ Arr vs' | vs' <- shrink vs ]
  shrink (Lam (Bind x e)) = [ Arr [] ] ++ [ e | x `notElem` free e] ++ [ Lam (Bind x e') | e' <- shrink e ]
  shrink (a :=: b) = [a,b] ++ [a':=:b|a'<-shrink a] ++ [a:=:b'|b'<-shrink b]
  shrink (a :|: b) = [a,b] ++ [a':|:b|a'<-shrink a] ++ [a:|:b'|b'<-shrink b]
  shrink (a :>: b) = [a,b] ++ [a':>:b|a'<-shrink a] ++ [a:>:b'|b'<-shrink b]
  shrink (a :@: b) = [a,b] ++ [a':@:b|a'<-shrink a] ++ [a:@:b'|b'<-shrink b]
  shrink Fail      = []
  shrink (One a)   = [a] ++ [One a'| a'<-shrink a]
  shrink (All a)   = [a, One a, Arr []] ++ [All a'|a'<-shrink a]
  shrink (Assume a) = [a] ++ [Assume a'| a'<-shrink a]
  shrink (Assert a) = [a] ++ [Assert a'| a'<-shrink a]
  shrink (Decide a) = [a] ++ [Decide a'| a'<-shrink a]
  shrink (Verify a) = [a] ++ [Verify a'| a'<-shrink a]

  shrink (Exi (Bind x a)) = [a |x `notElem` ys]
                         ++ [subst [(x,Var y)] a |x `elem` ys, y <- ys, x /= y]
                         ++ [Exi (Bind x a') | a' <- shrink a] where ys = free a
  shrink (Split e f g) = [e, f, g] ++ [Split e' f g | e' <- shrink e]
                                   ++ [Split e f' g | f' <- shrink f]
                                   ++ [Split e f g' | g' <- shrink g]
  shrink (_ :~: _) = error "impossible"
  shrink (BlockC e)  = BlockC <$> shrink e
  shrink (Store _ _) = undefined
  shrink (Ref _)   = []
  shrink Wrong{}   = []
  shrink If{} = undefined

arbExpr :: Int -> [Ident] -> Gen Expr
arbExpr n xs =
  frequency $
  [ (length xs, Var <$> elements xs) ] ++
  [ (1, Int <$> arbitrary)
  , (1, Op  <$> arbitrary)
  , (1, return Fail)
  , (rv, Arr <$> do k <- choose (0,5)
                    sequence [ arbExpr (n `div` k) xs | _ <- [1..k] ])
  , (ri, Lam <$> arbBind n1 xs)
  , (ri, (:=:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (ri, (:>:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (ri, (:|:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (rv, (:@:) <$> arbExpr n2 xs <*> arbExpr n2 xs)
  , (ri, Exi <$> arbBind n1 xs)
  , (rv, One <$> arbExpr n1 xs)
  , (rv, All <$> arbExpr n1 xs)
  -- Don't generate Block, the anf-ing will do that.
  -- , (n, Split <$> arbExpr n3 xs <*> arbValue n3 xs <*> arbValue n3 xs)
  ]
 where
  n1 = n-1
  n2 = n `div` 2
  ri = 6 `min` n
  rv = 2 `min` n

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

invariant :: (Expr -> Bool) -> Expr -> Bool
invariant here = collect here (&&)

collect :: (Expr->a) -> (a->a->a) -> Expr -> a
collect here (\/) = col
 where
  col e = recr (here e) e

  recr a (Arr es)         = foldr (\/) a (map col es)
  recr a (Lam (Bind _ e)) = a \/ col e
  recr a (Exi (Bind _ e)) = a \/ col e
  recr a (e1 :=: e2)      = a \/ (col e1 \/ col e2)
  recr a (e1 :|: e2)      = a \/ (col e1 \/ col e2)
  recr a (e1 :>: e2)      = a \/ (col e1 \/ col e2)
  recr a (e1 :@: e2)      = a \/ (col e1 \/ col e2)
  recr a (One e)          = a \/ col e
  recr a (All e)          = a \/ col e
  recr a (Assume e)       = a \/ col e
  recr a (Assert e)       = a \/ col e
  recr a (Verify e)       = a \/ col e
  recr a (Split x y z)    = a \/ (col x \/ (col y \/ col z))
  recr a (Store h e)      = foldr (\/) a (map col (IM.elems h)) \/ col e
  recr a _                = a

--------------------------------------------------------------------------------

allVars :: Expr -> [Ident]
allVars = nub . expr
  where
    expr (Var i) = [i]
    expr (Arr es) = concatMap expr es
    expr (LAM i e) = i : expr e
    expr (EXI i e) = i : expr e
    expr (e1 :=: e2) = expr e1 ++ expr e2
    expr (e1 :@: e2) = expr e1 ++ expr e2
    expr (e1 :>: e2) = expr e1 ++ expr e2
    expr (One e) = expr e
    expr (All e) = expr e
    expr (Assume e) = expr e
    expr (Assert e) = expr e
    expr (Verify e) = expr e
    expr (Split e1 e2 e3) = expr e1 ++ expr e2 ++ expr e3
    expr (BlockC e) = expr e
    expr _ = []

--------------------------------------------------------------------------------

-- XXX Move somewhere better
check :: (HasCallStack) => (Expr -> Bool) -> Expr -> Expr
check p a | p a = a
          | otherwise = error $ "check failed: " ++ prettyShow a

--------------------------------------------------------------------------------

-- Substiture one expressions for another.
-- (Does now indeed avoid accidental capture in the 'to' expression.)
substExp :: Expr -> Expr -> Expr -> Expr
substExp from to = sub
  where
    fvs = free from
    tvs = free to
    sub e | e == from = to
    sub e@Var{}   = e
    sub e@Int{}   = e
    sub e@Op{}    = e
    sub (Arr vs)  = Arr (map sub vs)
{-
    sub (LAM x e) | x `elem` fvs = LAM x e
                  | x `elem` tvs = error "unimplemented"
                  | otherwise = LAM x (sub e)
-}
    sub (Lam bnd)
      | x `elem` fvs = Lam bnd
      | otherwise    = Lam (Bind x (sub e))
     where Bind x e = alphaRename tvs bnd
    sub (a :=: b) = sub a :=: sub b
    sub (a :>: b) = sub a :>: sub b
    sub (a :|: b) = sub a :|: sub b
    sub (a :@: b) = sub a :@: sub b
    sub Fail      = Fail
    sub e@Wrong{} = e
{-
    sub (EXI x e) | x `elem` fvs = EXI x e
                  | x `elem` tvs = error "unimplemented"
                  | otherwise = EXI x (sub e)
-}
    sub (Exi bnd)
      | x `elem` fvs = Exi bnd
      | otherwise    = Exi (Bind x (sub e))
     where Bind x e = alphaRename tvs bnd
    sub (One a)   = One (sub a)
    sub (All a)   = All (sub a)
    sub (Assume a) = Assume (sub a)
    sub (Assert a) = Assert (sub a)
    sub (Verify a) = Verify (sub a)
    sub (Split e f g) = Split (sub e) (sub f) (sub g)
    sub (BlockC e) = BlockC (sub e)
    sub (Store h e) = Store (IM.map sub h) (sub e)
    sub e@Ref{}   = e
    sub _         = undefined
