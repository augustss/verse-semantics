{-# LANGUAGE TemplateHaskell #-}
module Main where

import EQD
import Data.List( (\\), nub, sort )
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.All
import GHC.Generics

-----------------------------------------------------------------------

data Var = Idf String deriving ( Eq, Ord, Generic )
instance Show Var where show (Idf x) = x

instance Arbitrary Var where
  arbitrary = elements allVars

instance CoArbitrary Var
instance Function Var

allVars = [ Idf "x", Idf "y", Idf "z", Idf "u", Idf "v" ]

x,y,z,u :: Var
x = Idf "x"
y = Idf "y"
z = Idf "z"
u = Idf "u"

data Val = Int Int deriving ( Eq, Ord )
instance Show Val where show (Int n) = show n

instance Arbitrary Val where
  arbitrary = elements allVals

allVals = [ Int 1, Int 2, Int 3, Int 4 ]

instance Num Val where
  fromInteger n = Int (fromInteger n)
  (+) = undefined
  (-) = undefined
  (*) = undefined
  signum = undefined
  abs = undefined

-----------------------------------------------------------------------

prop_Invariant p =
  whenFail (print p) $
    invariant p
 where
  types = p :: EQD Var Val

prop_Invariant_eq a b =
  prop_Invariant (a .=. b)

prop_Invariant_and p q =
  prop_Invariant (p /\ q)

prop_Invariant_nt p =
  prop_Invariant (nt p)

prop_Invariant_qall x p =
  prop_Invariant (qall x p)

-----------------------------------------------------------------------

{-
--prop_And_Mod (Fun _ mod, p, q) =
    (mod |= (p /\ q))
  ==
    ((mod |= p) && (mod |= q))
 where
  types = p :: EQD Var Val

--prop_All_Mod x (Fun _ mod, p) =
    (mod |= qall x p)
  ==
    and [ (\y -> if y==x then a else mod y) |= p | a <- (allVals ++ [Int 17]) ]
 where
  types = p :: EQD Var Val
-}

-----------------------------------------------------------------------

prop_And_Assoc p q r =
  p /\ (q /\ r) ===
    (p /\ q) /\ r
 where
  types = p :: EQD Var Val

prop_And_Comm p q =
  p /\ q ===
    q /\ p
 where
  types = p :: EQD Var Val

prop_DeMorgan p q =
  nt (p /\ q) ===
    nt p \/ nt q
 where
  types = p :: EQD Var Val

prop_Impl p q =
  (p EQD.==> q) ===
    nt p \/ q
 where
  types = p :: EQD Var Val

prop_Distr (p,q,r) =
  p /\ (q \/ r) ===
    (p /\ q) \/ (p /\ r)
 where
  types = p :: EQD Var Val

prop_Eq_Comm a b =
  (a .=. b) === (b .=. a)
 where
  types = a :: Atom Var Val

prop_Eq_Refl a =
  (a .=. a) === true
 where
  types = a :: Atom Var Val

prop_QAll_And x p q =
  qall x (p /\ q) ===
    qall x p /\ qall x q
 where
  types = p :: EQD Var Val

prop_QAll_And_x x p q =
  qall x (p /\ q0) ===
    qall x p /\ q0
 where
  q0 = qexi x q
  types = p :: EQD Var Val

prop_QAll_Or_x x p q =
  qall x (p \/ q0) ===
    qall x p \/ q0
 where
  q0 = qexi x q
  types = p :: EQD Var Val

prop_QAll_Def x a p =
  Var x /= a Test.QuickCheck.==>
    qall x ((Var x .=. a) EQD.==> p) ===
      subst x a p
 where
  types = p :: EQD Var Val

prop_QAll_false x as =
  let p = foldr (\/) false [ Var x .=. a | a <- as, a /= Var x ] in
  whenFail (print p) $
  qall x p ===
    false
 where
  types = as :: [Atom Var Val]

prop_DeMorgan_Quant x p =
  nt (qall x p) ===
    qexi x (nt p)
 where
  types = p :: EQD Var Val

-----------------------------------------------------------------------

return []
main = $(quickCheckAll)
