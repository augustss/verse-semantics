{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-methods #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module ExpSugar(module ExpSugar) where
import Data.String
import Exp

instance Num Exp where
  fromInteger = Int
  x + y = App (Prim Oadd) (Tup [x, y])

instance IsString Exp where
  fromString = Var

int :: Exp -> Exp
int = App (Prim Oint)

infix 2 :=
pattern (:=) :: Ident -> Exp -> Exp
pattern x := e = Def x e

infix 2 :::
pattern (:::) :: Ident -> Exp -> Exp
pattern x ::: e = x := Colon e

infixl 3 :|
pattern (:|) :: Exp -> Exp -> Exp
pattern x :| y = Choice x y

infixl 3 :|||
pattern (:|||) :: Exp -> Exp -> Exp
pattern x :||| y = UChoice x y

infixl 4 ===
(===) :: Exp -> Exp -> Exp
(===) = Equ

infixl 1 :>
pattern (:>) :: Exp -> Exp -> Exp
pattern (:>) x y = Seq x y

infix 8 :@
pattern (:@) :: Exp -> Exp -> Exp
pattern (:@) x y = App x y

infix 0 `wher`
wher :: Exp -> Exp -> Exp
wher = Where

fun_c :: Exp -> Exp -> Exp
fun_c e1 e2 = Fun Closed e1 e2

fun_o :: Exp -> Exp -> Exp
fun_o e1 e2 = Fun Open e1 e2

exi :: Ident -> Exp
exi i = i ::: "any"
