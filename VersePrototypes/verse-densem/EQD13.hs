module EQD
  ( EQD
  , Atom(..)
  , invariant
  , false
  , true
  , (.=.)
  , (/\), (\/), (EQD.==>), (<=>)
  , nt
  , qall
  , qexi
  , subst
  )
 where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List( intercalate, sort, union )
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

-----------------------------------------------------------------------
-- EQD

data EQD x a
  = NODE x [(a,EQD x a)] [VEQ x] (VEQ x)
  | TRUE
  | FALSE
 deriving ( Eq, Ord, Show )

-----------------------------------------------------------------------

invariant :: (Ord x, Ord a) => EQD x a -> Bool
invariant p = inv [] p
 where
  inv dom (NODE x vals olds new) =
       all (x <.) (new : olds)
    && all (x >) dom
    && length dom == length olds
    && all (invVEQ dom) olds
    && nub (sort (map fst vals)) == map fst vals
    && all (invVEQ dom) (map snd vals)
    && invVEQ (x:dom) new
   where
    x <. NODE y _ _ _ = x < y
    _ <. _            = True

  inv _ _ = True

-----------------------------------------------------------------------

false, true :: (Ord x, Ord a) => EQD x a
false = FALSE
true  = TRUE

(.=.) :: (Ord x, Ord a) => Atom x a -> Atom x a -> EQD x a
a     .=. b     | a == b = TRUE
Val u .=. Val v          = FALSE
Var x .=. Val v          = NODE x [(v,TRUE)] [] FALSE
Var v .=. Val x          = NODE x [(v,TRUE)] [] FALSE
Var x .=. Val y | x < y  = NODE x [] [] (NODE y [] [TRUE] FALSE)
Var y .=. Val x          = NODE x [] [] (NODE y [] [TRUE] FALSE)

-----------------------------------------------------------------------

nt :: (Ord x, Ord a) => EQD x a -> EQD x a
nt TRUE                   = FALSE
nt FALSE                  = TRUE
nt (NODE x vals olds new) = NODE x [(v, nt p)|(v,p)<-vals] (map nt olds) (nt new)

-----------------------------------------------------------------------

(/\),(\/),(==>),(<=>) :: (Ord x, Ord a) => EQD x a -> EQD x a -> EQD x a
p /\  q = oper opAnd   p q
p \/  q = oper opOr    p q
p ==> q = oper opImpl  p q
p <=> q = oper opEquiv p q

oper :: (Ord x, Ord a) => (EQD x a -> EQD x a -> EQD x a)
                       -> EQD x a -> EQD x a -> EQD x a
oper op p@(NODE x _ _ _) q@(NODE y _ _ _) =
  NODE z (mergeWith (oper op) xvals yvals)
         (zipWith (oper op) xolds yolds)
         (oper op xnew ynew)
 where
  z = x `min` y

oper op p q = op p q

opAnd FALSE _ = FALSE
opAnd _ FALSE = FALSE
opAnd TRUE q  = q
opAnd p TRUE  = p

opOr FALSE q = q
opOr p FALSE = p
opOr TRUE _  = TRUE
opOr _ TRUE  = TRUE

opImpl FALSE _ = TRUE
opImpl p FALSE = nt p
opImpl TRUE q  = q
opImpl _ TRUE  = TRUE

opEquiv FALSE q = nt q
opEquiv p FALSE = nt p
opEquiv TRUE q  = q
opEquiv p TRUE  = p

mergeWith :: (Ord x, Ord a) => (EQD x a -> EQD x a -> EQD x a)
                            -> [(a,EQD x a)] -> [(a,EQD x a)] -> [(a,EQD x a)]
mergeWith op [] ys = ys
mergeWith op xs [] = 

-----------------------------------------------------------------------

qall, qexi :: (Ord x, Ord a) => x -> EQD x a -> EQD x a
qall x (EQD p) = simpVal (allVEQ (Var x) p)
qexi x (EQD p) = simpVal (notVEQ (allVEQ (Var x) (notVEQ p)))

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


mkNODE :: Ord x => x -> [VEQ x] -> VEQ x -> VEQ x
mkNODE x olds new
  | (olds,new) == dummy n p = p
  | otherwise               = NODE x olds new
 where
  n = length olds
  p = unexpand n new

expand :: Ord x => Int -> VEQ x -> VEQ x
expand i (NODE x olds new) = NODE x (insertAt i new $ map (expand i) olds) (expand i new)
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
                                          (andVEQ (olds!!i) (allIndex i new))
  allIndex i t                 = t
allVEQ x t     = t

substVEQ x y p = exiVEQ x (eqVEQ x y `andVEQ` p)
exiVEQ x p = notVEQ (allVEQ x (notVEQ p))

-----------------------------------------------------------------------
-- show

{-
all this is ONLY needed to print out eq-diagrams
-}

cubesVEQ :: Ord x => [x] -> VEQ x -> [[(x,x,Bool)]]
cubesVEQ dom FALSE = []
cubesVEQ dom TRUE  = [[]]
cubesVEQ dom (NODE x olds new) =
  [ [(x,y,False)|y<-dom]++ c | c <- cubesVEQ (dom++[x]) new ]
  ++
  [ [(x,z,z==y)|z<-dom]++c | (y,t) <- dom `zip` olds, c <- cubesVEQ dom t ]
  -- [ (x,y,True):c | (y,t) <- dom `zip` olds, c <- cubesVEQ dom t ]

satVEQ :: Ord x => VEQ x -> VEQ x -> Bool
satVEQ FALSE _ = True
satVEQ _ FALSE = False
satVEQ _  TRUE = True
satVEQ TRUE  _ = False

satVEQ p@(NODE x xolds xnew) q@(NODE y yolds ynew)
  | x < y  = satVEQ (allVEQ x p) q
  | x > y  = satVEQ p (allVEQ y q)
  | x == y = and (zipWith satVEQ xolds yolds) && satVEQ xnew ynew

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

cube :: Ord x => [(x,x,Bool)] -> VEQ x
cube c = foldr andVEQ TRUE [ (if b then id else notVEQ) $ eqVEQ x y | (x,y,b) <- c ]

mcubesVEQ :: Ord x => VEQ x -> [[(x,x,Bool)]]
mcubesVEQ p = go (cubesVEQ [] p)
 where
  go [] = []
  go (c:cs) = c' : go [ c | let a = cube c', c <- cs, let b = cube c, not (a `satVEQ` b) ]
   where
    c' = minim (\c -> satVEQ (cube c) p) c

-----------------------------------------------------------------------
-- aux

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = take i xs ++ [x] ++ drop i xs

remv :: Int -> [a] -> [a]
remv i xs = take i xs ++ drop (i+1) xs

-----------------------------------------------------------------------
-- for QuickCheck

instance (Arbitrary x, Arbitrary a) => Arbitrary (Atom x a) where
  arbitrary = frequency [(3, Val `fmap` arbitrary),(3, Var `fmap` arbitrary)]

instance (Ord x, Arbitrary x, Ord a, Arbitrary a) => Arbitrary (EQD x a) where
  arbitrary = simpVal `fmap` arbitrary
  shrink (EQD p) = [ simpVal p' | p' <- shrink p ]
  
instance (Ord x, Arbitrary x) => Arbitrary (VEQ x) where
  arbitrary =
    frequency
    [ (1, return FALSE)
    , (1, return TRUE)
    , (10, do x <- arbitrary
              p <- sized $ arb x 1
              return (mkNODE x [] p))
    ]
   where
    arb x k n =
      frequency
      [ (1, return FALSE)
      , (1, return TRUE)
      , (n, do my <- arbitrary `suchThatMaybe` (>x)
               case my of
                 Nothing -> do return FALSE
                 Just y  -> do olds <- sequence [ arb y k (n `div` 2) | i <- [1..k] ]
                               new  <- arb y (k+1) (n `div` 2)
                               return (mkNODE y olds new))
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

-----------------------------------------------------------------------
