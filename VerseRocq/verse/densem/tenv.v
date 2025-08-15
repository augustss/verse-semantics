Require Import Imports.

From Stdlib Require Lists.List.
From Stdlib Require Import Classes.EquivDec.
From Stdlib Require Export FunctionalExtensionality.
From Stdlib Require Import PeanoNat.

Import ssreflect.

Require Import syntax.common.
Require syntax.mini.
Require Import PFun.
Require Import densem.Dom.
Require Import structures.Sets.
Import structures.List.

Import mini.MiniNotation.
Import SetNotations.
Import SetMonadNotation.
Import List.ListNotations.
Import ListMonadNotation.

Open Scope list_scope.
Open Scope mini_expr_scope.

(* --------------- (total) environments ----------- *)

Definition env := Ident -> value.

Module Env.

Definition empty : env := fun x => Dom.Int 0.

Definition extend : Ident -> value -> env -> env := 
  fun x v rho => 
    fun y => if Nat.eqb x y then v else rho y.


(* Overwrite the environment ρ1 with definitions for xs using corresponding 
   values in ρ2.
*)
Definition extend_env (xs : Scope.t) (ρ2 : env) (ρ1 : env) :=
  List.fold_right (fun x ρ' => Env.extend x (ρ2 x) ρ') ρ1 (Scope.elements xs).

End Env.

Declare Scope env_scope.
Delimit Scope env_scope with env.
Bind Scope env_scope with env.

Module EnvNotation.
Notation " x |-> v " := (Env.extend x v Env.empty) (at level 80) : env_scope.
Notation " x |-> v , e " := (Env.extend x v e) (at level 80, right associativity): env_scope. 
End EnvNotation.

Open Scope env_scope.
Import EnvNotation.

Section NotationTest.
Variable x : Ident.
Variable y : Ident.
Check (x |-> Int 3, y |-> Int 4).
End NotationTest.

Create HintDb env.

Lemma Extensionality_env (ρ1 ρ2 : env) : 
  (forall x, ρ1 x = ρ2 x) -> ρ1 = ρ2.
Proof. intros. extensionality y. auto. Qed.

Lemma empty_equal r:
  (forall x, r x = Int 0) -> 
  r = Env.empty.
Proof. 
  intro h.
  extensionality x.
  rewrite h.
  auto.
Qed.


Lemma extend_lookup_same {x ρ v} : (x |-> v, ρ) x = v. unfold Env.extend. rewrite Nat.eqb_refl. auto. Qed.

Lemma extend_lookup_diff {x y ρ v} : Nat.eqb x y = false -> (x |-> v, ρ) y = ρ y. 
  unfold Env.extend. destruct (x =? y). intro h; try done. intro h. done. Qed.

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


Lemma extend_env_spec1 xs ρ2 ρ1 : 
  forall x, ~(Scope.In x xs) -> (Env.extend_env xs ρ2 ρ1) x = ρ1 x.
Admitted.
Lemma extend_env_spec2 xs ρ2 ρ1 : 
  forall x, Scope.In x xs -> (Env.extend_env xs ρ2 ρ1) x = ρ2 x.
Proof.
  unfold Env.extend_env.
  intro x.
  remember (Scope.elements xs) as l.
  move: l Heql.
  induction l. cbn. 
Admitted.

