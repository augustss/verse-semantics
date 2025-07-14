Require Import Imports.

From Stdlib Require Lists.List.
From Stdlib Require Import Classes.EquivDec.
Import ssreflect.

Require Import syntax.common.
Require syntax.mini.
Require Import PFun.
Require Import densem.Dom.
Require Import structures.Sets.
Import structures.List.
Import structures.Monad.

Import mini.MiniNotation.
Import FunctorNotation.
Import ApplicativeNotation.
Import MonadNotation.
Import SetNotations.
Import List.ListNotations.

Open Scope monad_scope.
Open Scope list_scope.
Open Scope mini_expr_scope.

(* --------------- (partial) environments ----------- *)

(* gives a value for *in-scope* identitfiers *)
(* The denotation of out of scope identifiers is NOT defined. *)
Definition env := Ident -> option value.

Module Env. 

Definition empty : env := fun x => None.

Definition extend : Ident -> value -> env -> env := 
  fun x v rho => 
    fun y => if Nat.eqb x y then Some v else rho y.

End Env.

Declare Scope env_scope.
Delimit Scope env_scope with env.
Bind Scope env_scope with env.

Module EnvNotation.
Notation " x |-> v " := (Env.extend x v Env.empty) (at level 80) : env_scope.
Notation " x |-> v , e " := (Env.extend x v e) (at level 80, right associativity): env_scope. 

Section NotationExamples.
Open Scope env_scope.
Variable x y : Ident.
Variable v w : value.
Check  x |-> v.
Check  x |-> v, y |-> w.
End NotationExamples.

End EnvNotation.
