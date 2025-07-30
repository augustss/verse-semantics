Require Import Imports.

From Stdlib Require Lists.List.
From Stdlib Require Import Classes.EquivDec.
Import ssreflect.

From Stdlib Require Import Logic.PropExtensionality.
From Stdlib Require Import Logic.FunctionalExtensionality.
From Stdlib Require Import Sets.Classical_sets.

Require Import syntax.common.
Require syntax.mini.
Require Import PFun.
Require Import structures.Sets.
Import structures.List.

Require Import densem.Dom.
Require Import densem.tenv.  (* environments are total *)

Import mini.MiniNotation.
Import SetNotations.
Import SetMonadNotation.
Import List.ListNotations.
Import EnvNotation.

Open Scope list_scope.
Open Scope mini_expr_scope.
Open Scope env_scope.
Open Scope set_scope.

Definition ENV := P env.

(* Constrain a variable to be equal to a particular value.
   All other mappings in the environment are unconstrained. 
   x ≈ f
*)
Definition constrain_eq (x : Ident) (f : env -> value) : ENV := 
  fun ρ => ρ x = f ρ.
Definition constrain_ne (x : Ident) (f : env -> value) : ENV := 
  fun ρ => not (ρ x = f ρ).

(* Generalize all of the xs to be anything *)
(* "Envs Drop Variables" Δ \ xs *)
Definition hide (xs : Scope.t) (Δ : ENV) : ENV := 
  fun ρ => exists ρ', (ρ' ∈ Δ) /\ forall x, ~ (Scope.In x xs) -> (ρ x = ρ' x).

(* Generalize all of the xs to be anything *)
(* Envss Drop Variables Δs [\] xs *)
Definition hide_list (xs : Scope.t) (Δs : list ENV) : list ENV := 
  List.map (hide xs) Δs.

Definition envs_difference (Δ1 : ENV) (xs : Scope.t) (Δ2 : ENV) : ENV :=
  Δ1 - (hide xs Δ2).

(* The set of all environments that extend rho with arbitrary 
   definitions for the variables declared in e. 
*)
Definition X (e : mini.Expr) (ρ : env) : ENV :=
  hide (mini.I e) ⌈ ρ ⌉.

(* ------  Notation ----------------------------------------- *)

Module ENVNotation.
Infix "≈" := constrain_eq (at level 60).
Infix "≉" := constrain_ne (at level 60).
Notation "⟨ n ⟩" := (fun ρ => n) (at level 40).
Notation "⟪ x ⟫" := (fun ρ => ρ x) (at level 40).
Notation "Δ \ xs" := (hide xs Δ) (at level 70).
Notation "Δ [\] xs" := (hide_list xs Δ) (at level 70).
Notation "es \{ xs } fs" := (envs_difference es xs fs) (at level 40).
End ENVNotation.

Import ENVNotation.

(* ---- theory about constrain/hide/If3 ------------------  *)

Lemma in_constrain_eq (ρ : env) (x:Ident) k :
  ρ ∈ (x ≈ k) <-> ρ x = k ρ.
split. intro h. inversion h. done.
intro h. unfold constrain_eq. done.
Qed.

Hint Rewrite in_constrain_eq : set_simpl.

Lemma constrain_eq_same {r v} : 
  (r ≈ ⟨ v ⟩ ∩ r ≈ ⟨ v ⟩) = (r ≈ ⟨ v ⟩).
Proof.
  eapply set_extensionality; intros x.
  rewrite in_intersection.
  repeat rewrite in_constrain_eq. 
  tauto.
Qed.

Hint Rewrite @constrain_eq_same : set_simpl.

Lemma constrain_self {x} : 
  (x ≈ ⟪ x ⟫) = Total_set.
Proof.
  eapply set_extensionality; intros y.
  split; intro h; try done.
Qed.

Lemma constrain_self_imposs {x} : 
  (x ≉ ⟪ x ⟫) = ∅.
Proof.
  eapply set_extensionality; intros y.
  split; intro h; try done.
Qed.

Hint Rewrite @constrain_self  @constrain_self_imposs : set_simpl.

Lemma constrain_eq_intersection {r v1 v2} : 
  (r ≈ ⟨ v1 ⟩ ∩ r ≈ ⟨ v2 ⟩) = 
  if Value.eqb v1 v2 then 
    (r ≈ ⟨v1⟩)
  else 
    ∅.
Proof.
  destruct (Value.eqb v1 v2) eqn:EV;
   [rewrite Value.eqb_eq in EV; subst|
    rewrite Value.eqb_neq in EV].
    + eapply set_extensionality; intros x.
    rewrite in_intersection.
    rewrite in_constrain_eq. tauto.
    + eapply set_extensionality; intros x.  
      subst.
      rewrite in_intersection.
      repeat rewrite in_constrain_eq.
      split; try done.
      intros [h1 h2]; congruence.
Qed.

Lemma Empty_set_hide (s : Scope.t) : ∅ \ s = ∅.
unfold hide. 
eapply Extensionality_Ensembles.
split.
 + intros ρ ρIn. inversion ρIn. inversion H. done.
 + intros ρ ρIn. inversion ρIn.
Qed.

Hint Rewrite Empty_set_hide : set_simpl.

Lemma Total_set_hide (s : Scope.t) : Total_set \ s = Total_set.
unfold hide. 
eapply Extensionality_Ensembles.
split.
 + intros ρ ρIn. done.
 + intros ρ ρIn. exists ρ. split; auto.
Qed.
 
Hint Rewrite Total_set_hide : set_simpl.

Lemma hide_nothing (s : ENV) : s \ Scope.empty = s.
unfold hide. 
eapply Extensionality_Ensembles.
split.
 + intros ρ ρIn. 
   move: ρIn => [ρ' [h1 h2]]. 
   have EQ: (ρ = ρ').
   eapply functional_extensionality. intro x.
   eapply h2.
   intro h. inv h. subst. done.
 + intros ρ ρIn.
   exists ρ. split; auto.
Qed.

Hint Rewrite hide_nothing : set_simpl.


Lemma constrain_eq_hide_same r k : 
  ((r ≈ ⟨k⟩) \ Scope.singleton r) = Total_set.
eapply set_extensionality. intro ρ.
unfold hide.
split.
+ intro h. done.
+ intros _. 
  exists (r |-> k, ρ).
  split.
  rewrite in_constrain_eq.
  rewrite extend_lookup_same.
  done.
  intros y h.
  rewrite Scope.singleton_spec in h.
  rewrite extend_lookup_diff.
  rewrite PeanoNat.Nat.eqb_neq. easy.
  done.
Qed.

Hint Rewrite constrain_eq_hide_same : set_simpl.

Lemma constrain_eq_hide_diff x y k : 
  x <> y ->
  ((x ≈ ⟨k⟩) \ Scope.singleton y) = (x ≈ ⟨k⟩).
intro NE.
eapply set_extensionality. intro ρ.
Admitted.

Lemma constrain_eq_hide_two k1 k2 x r :
  (x <> r) -> 
  (x ≈ ⟨ k1 ⟩ ∩ r ≈ ⟨ k2 ⟩) \ Scope.singleton r = (x ≈ ⟨ k1 ⟩).
Proof.
intros diff.
eapply set_extensionality. intro ρ.
split.  
Admitted.

Lemma hide_intersect x s1 s2 : hide x (s1 ∩ s2) ⊆ ((hide x s1) ∩ (hide x s2)).
Proof. 
  intros ρ [_ [[ρ' h11] h2]].
  unfold hide.
    split. 
    exists ρ'. split; auto. exists ρ'. split; auto.
Qed. 
(* NOTE: converse is not true *)

Lemma hide_union x s1 s2 : hide x (s1 ∪ s2) = hide x s1 ∪ hide x s2.
Proof.
  unfold hide.
  set_ext ρ.
  unfold In.
  split.
  + intro h. set_crunch.
    inv H.
    left. unfold In. exists x0. split; eauto.
    right. exists x0. split; eauto.
  + intro h. inv h; unfold In in H; set_crunch.
    exists x0. split. left. auto. eauto.
    exists x0. split. right. auto. eauto.
Qed.

Hint Rewrite hide_union : set_simpl.
