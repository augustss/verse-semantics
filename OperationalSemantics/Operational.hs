module Operational where

import Data.Map( Map )
import Control.Monad ( when, guard )

--------------------------------------------------------------------------------
{-

== META-NOTES: ==

This file is mainly meant as a mathematical notation help. Eventually, it will
also be executable Haskell code, which is nice.

I redeclare the Core.hs types because I am slightly changing them and I do not
want to be bothered making adaptations for those changes everywhere else.

== TECHNICAL NOTES: ==

The operational semantics is:

- big-step for the (first) value a program computes

- small-step for the various choice values a program computes (one value at a time)

The judgement

  H, p ---> v, H', p'
  
means

  "In heap H, executing the program p, leads to a (first) value v,
  and a new heap H'. To get the other choice values, execute program p'
  (in heap H)."

(-->) is meant to be a partial function, i.e. it may fail, and v, H' and p' are uniquely determined by H and p.

In the below, (-->) is called "eval" and is implemented as a Maybe-function.

"Fix" is added because information needs to flow forwards as well as backwards.
The idea is that Fix can be added to any construct, I just haven't decided where
is the right place to add it. Here are 3 possibilities:

- sequential composition: When the programmer writes

  p ; q
  
  this would really mean
  
  fix (p ; q)
  
  so that the semantics takes care of propagating information backwards through ;.
  
- when using a def: When the programmer writes

  def x in p
  
  this would really mean
  
  def x (Fix p)
  
  so that all information about x is propagated everywhere.

- when using "one" or "all": When the programmer writes

  one p   or   all p
  
  this would really mean
  
  one (Fix p) or all (Fix p)
  
  because we expect one and all to be used on top-level.
  
-}
--------------------------------------------------------------------------------

data Expr
  = Val Value
  | Expr :>: Expr
  | Value :=: Expr
  | Expr :|: Expr
  | Def Ident Expr
  | Fail
  | One Expr
  | All Expr

  -- added explicitly for now
  -- (because I don't know exactly which construct should compute the fixpoint)
  | Fix Expr
 deriving ( Eq, Ord, Show )

data Value
  = Var Ident
  | Arr [Value]
  | Int Integer
 deriving ( Eq, Ord, Show )

type Ident = String

type Heap = Map Ident Value

--------------------------------------------------------------------------------

eval :: Heap -> Expr -> Maybe (Value, Heap, Expr)
eval h (Val v) =
  do return (v, h, Fail)

eval h1 (p :>: q) =
  do (_, h2, p') <- eval h1 p
     (v, h3, q') <- eval h2 q
     return (v, h3, q' :|: (p' :>: q))

eval h1 (v :=: p) =
  do (w, h2, p') <- eval h1 p
     h3 <- unify h2 v w
     return (look v h3, h3, v :=: p')

eval h1 (p :|: q) =
  case eval h1 p of
    Nothing ->
      do eval h1 q
    
    Just (v, h2, p') ->
      do return (v, h2, p' :|: q)

eval h1 (Def x p) =
  do (v, h2, p') <- eval h1' p
     let (_, h2') = hide x h2
     
 where
  (x',h1') = hide x h1

eval h Fail =
  Nothing

eval h (One p) =
  do (v, _, _) <- eval h p
     guard (null (free v))
     return (v, h, Fail)

eval h (All p) =
  case eval h p of
    Nothing ->
      return (Arr [], h, Fail)
    
    Just (v, _, p') ->
      do guard (null (free v))
         (Arr vs, _, _) <- eval h (All p')
         return (Arr (v:vs), h, Fail)

eval h1 (Fix p) =
  do (v, h2, p') <- eval h1 p
     if h1 == h2
       then return (v, h2, p')
       else eval h2 (Fix p)

--------------------------------------------------------------------------------

unify :: Heap -> Value -> Value -> Maybe Heap
unify = undefined

look :: Value -> Heap -> Value
look = undefined

rename :: Ident -> Heap -> (Ident, Heap)
rename = undefined

hide :: Ident -> Heap -> Heap
hide = undefined

--------------------------------------------------------------------------------

