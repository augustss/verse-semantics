Require Import Imports.

From Stdlib Require Lists.List.
From Stdlib Require Import Classes.EquivDec.
From Stdlib Require Export FunctionalExtensionality.

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

(* --------------- (total) environments ----------- *)

Definition env := Ident -> value.

Module Env.

Definition empty : env := fun x => Dom.Int 0.

Definition extend : Ident -> value -> env -> env := 
  fun x v rho => 
    fun y => if Nat.eqb x y then v else rho y.

End Env.

Declare Scope env_scope.
Delimit Scope env_scope with env.
Bind Scope env_scope with env.

Module EnvNotation.
Notation " x |-> v " := (Env.extend x v Env.empty) (at level 80) : env_scope.
Notation " x |-> v , e " := (Env.extend x v e) (at level 80, right associativity): env_scope. 
End EnvNotation.


Lemma empty_equal r:
  (forall x, r x = Dom.Int 0) -> 
  Env.empty = r.
Proof. 
  intro h.
  extensionality x.
  rewrite h.
  auto.
Qed.

Lemma extend_equal r s x v:
  s x = v -> (forall y, Nat.eqb x y = false -> r y = s y) ->
  Env.extend x v r = s.
Proof.
  intros.
  extensionality y.
  unfold Env.extend. destruct (Nat.eqb x y) eqn:E.
  rewrite PeanoNat.Nat.eqb_eq in E. subst. auto.
  eauto.
Qed.
