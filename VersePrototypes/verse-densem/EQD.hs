module EQD
  ( EQD
  , Atom(..)
  , mcubes
  , invariant
  , false
  , true
  , (.=.), (=:), (=~)
  , (/\), (\/), (EQD.==>), (<=>)
  , andl
  , orl
  , (|=)
  , nt
  , qall, qalls
  , qexi, qexis
  , subst
  , support
  )
 where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List( intercalate, sort, nub, union )
import Test.QuickCheck
import Control.Monad( mplus )

-----------------------------------------------------------------------

data Atom x a
  = Var x | Val a -- Var > Val
  -- = Val a | Var x -- Val > Var
 deriving ( Eq, Ord )

instance (Show x, Show a) => Show (Atom x a) where
  show (Val a) = show a
  show (Var x) = show x

data EQD x a = EQD [a] (VEQ x)
 deriving ( Eq, Ord )

mcubes :: (Ord x, Ord a) => EQD x a -> [[(Atom x a, Atom x a, Bool)]]
mcubes (EQD as p) = summarize cs0
 where
    cs0 = cubesVEQ Var [Val a|a<-as] p

    summarize []     = []
    summarize (c:cs) = c' : summarize [ c | c <- cs, not (cube c `satVEQ` cube c') ]
     where
      c' = minim (\c -> cube c `satVEQ` p) c

    cube c = foldr andVEQ TRUE
             [ (if b then id else notVEQ)
             $ case (x,y) of
                 _ | x==y       -> TRUE
                 (Val _, Val _) -> FALSE
                 (Val a, Var x) -> eqVarVal x a
                 (Var x, Val a) -> eqVarVal x a
                 (Var x, Var y) -> if x<y then eqVarVar x y else eqVarVar y x
             | (x,y,b) <- c
             ]

    eqVarVal x a = NODE x [ if a==b then TRUE else FALSE | b <- as ] FALSE
    eqVarVar x y = NODE x [ NODE y [ if a==b then TRUE else FALSE | a <- as ] FALSE | b <- as ]
                          (NODE y ([ FALSE | a <- as ]++[TRUE]) FALSE)

instance (Show x, Ord x, Show a, Ord a) => Show (EQD x a) where
  show p =
    case mcubes p of
      [] -> "fail"
      cs -> intercalate "∪" [ showCube c | c <- cs ]
   where
    showCube c = "⦅" ++ intercalate "," (pos c ++ neg c) ++ "⦆"
     where
      clss  = M.fromListWith union $
              [ (y,[x,y])
              | (x,y,True) <- c
              ]

      tab   = M.fromList
              [ (x, head $ [ x | x@(Val _) <- xs ] ++ sort xs)
              | (_,xs) <- M.toList clss
              , x <- xs
              ]

      rep x = case M.lookup x tab of
                Nothing -> x
                Just r  -> r
     
      pos c = sort
              [ intercalate "="
              $ [ show x | x@(Var _) <- sort xs ]
             ++ [ show x | x@(Val _) <- xs ]
              | (_,xs) <- M.toList clss
              ]

      neg c = nub
              [ show a ++ "≠" ++ show b
              | (x,y,False) <- c
              , let [a,b] = sort (map rep [x,y])
              , case (a,b) of
                  (Val u, Val v) -> u==v  -- only show u/=v if we don't already know it
                  _              -> True
              ]

invariant :: (Ord x, Ord a) => EQD x a -> Bool
invariant (EQD as p) = as == nub (sort as) && invariantVEQ (length as) p

false, true :: (Ord x, Ord a) => EQD x a
false = EQD [] FALSE
true  = EQD [] TRUE

infix 4 .=., =:, =~

(.=.) :: (Ord x, Ord a) => Atom x a -> Atom x a -> EQD x a
a     .=. b     | a == b = true
Val u .=. Val v | u /= v = false
Var x .=. Val v          = EQD [v] (NODE x [TRUE] FALSE)
Val v .=. Var x          = EQD [v] (NODE x [TRUE] FALSE)
Var x .=. Var y          = EQD [] (eqVEQ x y)

(=:) :: (Ord x, Ord a) => x -> a -> EQD x a
x =: a = Var x .=. Val a

(=~) :: (Ord x, Ord a) => x -> x -> EQD x a
x =~ y = Var x .=. Var y

infixr 3 /\, <=>
infixr 2 \/, ==>

(/\),(\/),(==>),(<=>) :: (Ord x, Ord a) => EQD x a -> EQD x a -> EQD x a
EQD vs p /\ EQD ws q = operAnd [] vs p ws q
 where
  operAnd us [] p [] q =
    mkEQD (reverse us) (p `andVEQ` q)

  operAnd us (v:vs) p (w:ws) q | v==w =
    operAnd (v:us) vs p ws q

  operAnd us (v:vs) p ws q | v <= head (ws ++ [v]) =
    operAnd (v:us) vs p ws (expand (length us) q)

  operAnd us vs p (w:ws) q =
    operAnd (w:us) vs (expand (length us) p) ws q

p \/ q  = nt (nt p /\ nt q)
p ==> q = nt (p /\ nt q)
p <=> q = (p /\ q) \/ (nt p /\ nt q)

andl, orl :: (Ord a, Ord x) => [EQD x a] -> EQD x a
andl = foldr (/\) true
orl  = foldr (\/) false

(|=) :: (Ord x, Ord a) => (x -> a) -> EQD x a -> Bool
mod |= EQD as p = go as p
 where
  go as TRUE  = True
  go as FALSE = False
  go as (NODE x olds new) =
    case [ old | (a,old) <- as `zip` olds, a == mod x ] of
      old:_ -> go as old
      []    -> go (as++[mod x]) new

mkEQD :: (Ord x, Ord a) => [a] -> VEQ x -> EQD x a
mkEQD as p = go 0 [] as p
 where
  go k bs []     p     = EQD (reverse bs) p
  go k bs (a:as) p
    | expand k p' == p = go 0 [] (reverse bs ++ as) p'
    | otherwise        = go (k+1) (a:bs) as p
   where
    p' = unexpand k p

support :: Ord x => EQD x a -> [x]
support (EQD _ p) = supp S.empty [p]
 where
  supp s (NODE x olds new : ps) = supp (S.insert x s) (new : olds ++ ps)
  supp s (_ : ps)               = supp s ps
  supp s []                     = S.toList s

-----------------------------------------------------------------------

qall, qexi :: (Ord x, Ord a) => x -> EQD x a -> EQD x a
qall x (EQD as p) = mkEQD as (allVEQ x p)
qexi x (EQD as p) = mkEQD as (exiVEQ x p)

qalls, qexis :: (Ord x, Ord a) => [x] -> EQD x a -> EQD x a
qalls []     p = p
qalls (x:xs) p = qall x (qalls xs p)

qexis []     p = p
qexis (x:xs) p = qexi x (qexis xs p)

-----------------------------------------------------------------------

nt :: (Ord x, Ord a) => EQD x a -> EQD x a
nt (EQD as p) = EQD as (notVEQ p)

-----------------------------------------------------------------------

subst :: (Ord x, Ord a) => x -> Atom x a -> EQD x a -> EQD x a
subst x a p = qexi x ((Var x .=. a) /\ p)

-----------------------------------------------------------------------
-- primitive EQD only on variables

data VEQ x
  = NODE x [VEQ x] (VEQ x)
  | TRUE
  | FALSE
 deriving ( Eq, Ord, Show )

invariantVEQ :: Ord x => Int -> VEQ x -> Bool
invariantVEQ k (NODE x olds new) =
     all (x <.) (new : olds)
  && k == length olds
  && all (invariantVEQ k) olds
  && invariantVEQ (k+1) new
 where
  x <. NODE y _ _ = x < y
  _ <. _          = True

invariantVEQ _ _ = True

mkNODE :: Ord x => x -> [VEQ x] -> VEQ x -> VEQ x
mkNODE x olds new
  | (olds,new) == dummy n p = p
  | otherwise               = NODE x olds new
 where
  n = length olds
  p = unexpand n new

expand :: Ord x => Int -> VEQ x -> VEQ x
expand i (NODE x olds new) = NODE x (insertAt i extra olds') new'
 where
  olds' = map (expand i) olds
  new'  = expand i new
  extra = swapVEQ i (length olds) new

  swapVEQ i j (NODE x olds new) = NODE x (insertAt i (olds'!!j) (remv j olds')) (swapVEQ i j new)
   where
    olds' = map (swapVEQ i j) olds
  swapVEQ i j t                 = t
  
expand i t                 = t

unexpand :: Ord x => Int -> VEQ x -> VEQ x
unexpand i (NODE x olds new) = mkNODE x (remv i (map (unexpand i) olds)) (unexpand i new)
unexpand i t                 = t

dummy :: Ord x => Int -> VEQ x -> ([VEQ x],VEQ x)
dummy d p = ([ p | i <- [1..d] ], expand d p)

opVEQ :: Ord x => (VEQ x -> VEQ x -> VEQ x) -> VEQ x -> VEQ x -> VEQ x
opVEQ op p@(NODE x _ _) q@(NODE y _ _) =
  mkNODE z (zipWith (opVEQ op) polds qolds) (opVEQ op pnew qnew)
 where
  (polds,pnew) = prep p
  (qolds,qnew) = prep q

  z = x `min` y

  prep t@(NODE x olds new)
    | x == z    = (olds,new)
    | otherwise = dummy (length olds) t

opVEQ op p q = p `op` q

andVEQ, equivVEQ :: Ord x => VEQ x -> VEQ x -> VEQ x
andVEQ = opVEQ and 
 where
  FALSE `and` _     = FALSE
  TRUE  `and` q     = q
  _     `and` FALSE = FALSE
  p     `and` TRUE  = p

equivVEQ = opVEQ equiv
 where
  FALSE `equiv` q     = notVEQ q
  TRUE  `equiv` q     = q
  p     `equiv` FALSE = notVEQ p
  p     `equiv` TRUE  = p

eqVEQ :: Ord x => x -> x -> VEQ x
eqVEQ x y
  | x == y    = TRUE
  | x < y     = NODE x [] (NODE y [TRUE] FALSE)
  | otherwise = eqVEQ y x

notVEQ :: VEQ x -> VEQ x
notVEQ (NODE x olds new) = NODE x (map notVEQ olds) (notVEQ new)
notVEQ TRUE              = FALSE
notVEQ FALSE             = TRUE

orVEQ p q = notVEQ (notVEQ p `andVEQ` notVEQ q)

-----------------------------------------------------------------------
-- quantification

allVEQ :: Ord x => x -> VEQ x -> VEQ x
allVEQ x t@(NODE y olds new)
  | x == y     = foldr andVEQ (allIndex (length olds) new) olds
  | x > y      = mkNODE y (map (allVEQ x) olds) (allVEQ x new)
  | otherwise  = t
 where
  allIndex i (NODE z olds new) = mkNODE z [ allIndex i old | (old,j) <- olds `zip` [0..], i/=j ]
                                          (andVEQ (pushVEQ i (length olds) (olds!!i)) (allIndex i new))
  allIndex i t                 = t

  pushVEQ i j (NODE x olds new) = NODE x (remv i $ insertAt j (olds'!!i) olds') (pushVEQ i j new)
   where
    olds' = map (pushVEQ i j) olds
  pushVEQ i j t                 = t
allVEQ x t     = t

substVEQ x y p = exiVEQ x (eqVEQ x y `andVEQ` p)
exiVEQ x p = notVEQ (allVEQ x (notVEQ p))

-----------------------------------------------------------------------
-- show

{-
all this is ONLY needed to print out eq-diagrams
-}

cubesVEQ :: (Ord x, Eq a) => (x->a) -> [a] -> VEQ x -> [[(a,a,Bool)]]
cubesVEQ inj dom FALSE = []
cubesVEQ inj dom TRUE  = [[]]
cubesVEQ inj dom (NODE x olds new) =
  [ [(inj x,y,False)|y<-dom]++ c | c <- cubesVEQ inj (dom++[inj x]) new ]
  ++
  [ [(inj x,z,z==y)|z<-dom]++c | (y,t) <- dom `zip` olds, c <- cubesVEQ inj dom t ]
  -- [ (x,y,True):c | (y,t) <- dom `zip` olds, c <- cubesVEQ dom t ]

satVEQ :: Ord x => VEQ x -> VEQ x -> Bool
satVEQ p@(NODE x _ _) q@(NODE y _ _) =
  and (zipWith satVEQ polds qolds) && satVEQ pnew qnew
 where
  (polds,pnew) = prep p
  (qolds,qnew) = prep q

  z = x `min` y

  prep t@(NODE x olds new)
    | x == z    = (olds,new)
    | otherwise = dummy (length olds) t

satVEQ FALSE _ = True
satVEQ _ FALSE = False
satVEQ _  TRUE = True
satVEQ TRUE  _ = False

-----------------------------------------------------------------------
-- aux

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = take i xs ++ [x] ++ drop i xs

remv :: Int -> [a] -> [a]
remv i xs = take i xs ++ drop (i+1) xs

-- pre: p is true for xs
minim :: ([a] -> Bool) -> [a] -> [a]
minim p []    = []
minim p [x]
  | p []      = []
  | otherwise = [x]

minim p xs
  | p xs2     = minim p xs2
  | otherwise = xs1' ++ xs2'
 where
  n2   = length xs `div` 2
  xs1  = take n2 xs
  xs2  = drop n2 xs
  xs1' = minim (p . (++ xs2))  xs1
  xs2' = minim (p . (xs1' ++)) xs2

-----------------------------------------------------------------------
-- for QuickCheck

instance (Arbitrary x, Arbitrary a) => Arbitrary (Atom x a) where
  arbitrary = frequency [(3, Val `fmap` arbitrary),(3, Var `fmap` arbitrary)]

instance (Ord x, Arbitrary x, Ord a, Arbitrary a) => Arbitrary (EQD x a) where
  arbitrary =
    do as <- (nub . sort) `fmap` arbitrary
       x  <- arbitrary
       p  <- sized $ arbVEQ x (length as)
       return (mkEQD as p)

  shrink (EQD as p) =
       [ mkEQD as p' | p' <- shrink p ]
    ++ [ mkEQD (remv i as) (unexpand i p)
       | i <- [0..length as-1]
       ]
  
instance (Ord x, Arbitrary x) => Arbitrary (VEQ x) where
  arbitrary =
    frequency
    [ (1, return FALSE)
    , (1, return TRUE)
    , (10, do x <- arbitrary
              p <- sized $ arbVEQ x 1
              return (mkNODE x [] p))
    ]

  shrink (NODE x olds new) =
         olds
      ++ [ unexpand (length olds) new ]
      ++ [ mkNODE x (take i olds ++ [old'] ++ drop (i+1) olds) new
         | i <- [0..length olds-1]
         , old' <- shrink (olds!!i)
         ]
      ++ [ mkNODE x olds new'
         | new' <- shrink new
         ]
  shrink _ = []

arbVEQ :: (Ord x, Arbitrary x) => x -> Int -> Int -> Gen (VEQ x)
arbVEQ x k n =
  frequency
  [ (1, return FALSE)
  , (1, return TRUE)
  , (n, do my <- arbitrary `suchThatMaybe` (>x)
           case my of
             Nothing -> do return FALSE
             Just y  -> do olds <- sequence [ arbVEQ y k (n `div` 2) | i <- [1..k] ]
                           new  <- arbVEQ y (k+1) (n `div` 2)
                           return (mkNODE y olds new))
  ]

-----------------------------------------------------------------------
