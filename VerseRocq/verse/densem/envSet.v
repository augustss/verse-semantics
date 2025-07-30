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


Ltac unfoldIn :=  match goal with 
    | [ |- context[?ρ ∈ (fun ρ0 => @?f ρ0)] ] => 
  replace (ρ ∈ (fun ρ0 => f ρ0)) with (f ρ);[|auto] end.

Ltac foldIn x := 
  match goal with 
  |  [ |- context [?S x] ] => 
  replace (S x) with (x ∈ S); [|auto] end.

Tactic Notation "foldIn" constr(x) "in" hyp(H) := 
  match goal with 
  |  [ H : context [?S x] |- _ ] => 
  replace (S x) with (x ∈ S) in H; [|auto] end.


(* already in library *)
Lemma bind_map {A B} (f : A -> B) (s : P A) :
  Sets.map f s = Sets.bind s (fun x => ⌈ f x ⌉).
  rewrite bind_singleton_fmap.
  done.
Qed.

Lemma bind_map_l : forall {A B C : Type} (g : B -> P C) (f : A -> B) (xs : P A), 
    Sets.bind (Sets.map f xs) g =  Sets.bind xs (fun x : A => g (f x)).
Admitted.

Lemma bind_map_r : forall {A B C : Type} (g : A -> P B) (f : B -> C) (s : P A), 
    Sets.bind s (fun x => Sets.map f (g x)) = Sets.map f (Sets.bind s g).
Proof.
  intros.
  rewrite bind_map.
  set_simpl.
  f_equal.
  eapply functional_extensionality. intros x.
  rewrite bind_map. done.
Qed.

Lemma map_empty {A B} {f : A -> B} : 
  Sets.map f (∅ : P A) = ∅.
Admitted.

Lemma map_id: forall {A : Type} (s : P A), 
    (Sets.map id s) = s.
Admitted.

Lemma map_map: forall {A B C: Type} (f : B -> C) (g : A -> B) (s : P A), 
    (Sets.map f (Sets.map g s)) = Sets.map (fun x => f ( g x)) s.
Admitted.

#[export] Hint Rewrite @map_id @map_map @map_empty : set_simpl.

Lemma join_map {A B} (f : A -> P B) (S : P A) : ⨃ (map f S) = bind S f.
Admitted.

Lemma join_singleton {A} (S : (P A)) :
        ⨃ ⌈S⌉ = S.
Admitted.

Lemma join_empty {A} : ⨃ (∅ : P (P A)) = (∅ : P A).
Admitted.

Lemma join_union {A} (S1 S2 : P (P A)) :
        ⨃ (S1 ∪ S2) = (⨃ S1) ∪ (⨃ S2).
Admitted.

#[export] Hint Rewrite @join_union @join_singleton @join_empty : set_simpl.


Definition ENV := P env.

(* Constrain a variable to be equal to a particular value.
   All other mappings in the environment are unconstrained. 
   x ≈ f
*)
Definition constrain_eq (x : Ident) (f : env -> value) : ENV := 
  fun ρ => ρ x = f ρ.
Definition constrain_ne (x : Ident) (f : env -> value) : ENV := 
  fun ρ => not (ρ x = f ρ).

(* ρ \ xs *)
Definition hide_env (xs : Scope.t) (ρ : env) : ENV := 
  fun ρ' => forall x, ~ (Scope.In x xs) -> (ρ x = ρ' x).

(* Generalize all of the xs to be anything *)
(* "Envs Drop Variables" Δ \ xs *)
Definition hide (xs : Scope.t) (Δ : ENV) : ENV := 
  fun ρ => exists ρ', (ρ' ∈ Δ) /\ forall x, ~ (Scope.In x xs) -> (ρ x = ρ' x).
(* equivalent to: ⨃(map (hide_env xs) Δ)  *)


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
  hide_env (mini.I e) ρ.

(* A scope is unconstrained in a set *)
Definition hidden (xs: Scope.t) (Δ : ENV) : Prop := 
  hide xs Δ = Δ.


(* ------  Notation ----------------------------------------- *)

Module ENVNotation.
Infix "≈" := constrain_eq (at level 60).
Infix "≉" := constrain_ne (at level 60).
Notation "⟨ n ⟩" := (fun ρ => n) (at level 40).
Notation "⟪ x ⟫" := (fun ρ => ρ x) (at level 40).
Notation "ρ \\ xs" := (hide_env xs ρ) (at level 70).
Notation "Δ \ xs" := (hide xs Δ) (at level 70).
Notation "Δ [\] xs" := (hide_list xs Δ) (at level 70).
Notation "es \{ xs } fs" := (envs_difference es xs fs) (at level 40).
End ENVNotation.

Import ENVNotation.

(* ---- theory about hide/constrain/If3 ------------------  *)

Lemma hide_equiv xs Δ: 
  Δ \ xs = ⨃ (Sets.map (hide_env xs) Δ).
Proof.        
  set_ext ρ. split.
  - intros h.
    rewrite join_map.
    unfold hide,In in h. set_crunch.
    foldIn x in H.
    exists x. split. auto.
    unfold hide_env.
    intros y NI. rewrite H0; eauto.
  - unfold join. unfoldIn. intros h.
    move: h => [Δ0 [h1 h2]].
    unfold map in h1.
    unfold In in h1. move: h1 => [ρ0 [h3 h4]].
    foldIn ρ0 in h4.
    subst.
    unfold hide_env in h2. 
    unfold In in h2.
    unfold hide.
    unfold In. 
    exists ρ0. split. auto.
    intros x NI. rewrite h2; eauto.
Qed.    

Lemma hide_env_self xs ρ : ρ ∈ (ρ \\ xs).
Proof.
  unfold hide_env. intros x xNIn. done.
Qed.

(* This is the same as "hidden xs (ρ \\ xs)" *)
Lemma hide_env_hidden xs ρ : (ρ \\ xs) \ xs = (ρ \\ xs).
Proof.
  set_ext ρ0. 
  split.
  + intro h.
    intros x xIn.
    unfold hide,In in h.
    move: h => [ρ1 [h1 h2]].
    unfold hide_env in h1.
    rewrite h1; auto.
    rewrite h2; auto.
  + intro h.
    unfold hide, In. 
    unfold hide_env, In in h.
    exists ρ. split. eapply hide_env_self.
    intros x Ih. rewrite h; eauto.
Qed.
  
(* This is the same as "hidden xs (ρ \ xs)" *)
Lemma hide_hidden xs Δ : ((Δ \ xs) \ xs) = (Δ \ xs).
Proof.
  unfold hidden.
  repeat rewrite hide_equiv.
  repeat rewrite join_map.
  rewrite Sets.bind_bind.
  f_equal.
  eapply functional_extensionality. intros ρ. 
  set_ext ρ0.
  split.
  + intros h. inv h. destruct H as [h1 h2].
    unfold hide_env.
    intros y NI.
    rewrite <- h2; eauto.
  + intros h. exists ρ0. split; eauto. eapply hide_env_self.
Qed.

Lemma hide_union x s1 s2 : hide x (s1 ∪ s2) = hide x s1 ∪ hide x s2.
Proof.
  rewrite hide_equiv.
  set_simpl.
  repeat rewrite <- hide_equiv.
  done.
Qed.

#[export] Hint Rewrite hide_env_hidden hide_hidden hide_union : set_simpl.

(* This is the same as "hidden xs ∅" *)
Lemma hide_empty (xs : Scope.t) : ∅ \ xs = ∅.
  rewrite hide_equiv.
  set_simpl. done.
Qed.

#[export] Hint Rewrite hide_empty : set_simpl.

(* This is the same as "hidden xs Total_set" *)
Lemma Total_set_hide (xs : Scope.t) : Total_set \ xs = Total_set.
unfold hide. 
eapply Extensionality_Ensembles.
split.
 + intros ρ ρIn. done.
 + intros ρ ρIn. exists ρ. split; auto. 
Qed.
 
#[export] Hint Rewrite Total_set_hide : set_simpl.


Lemma hide_env_nothing ρ : ρ \\ Scope.empty = ⌈ ρ ⌉. 
Proof.
  set_ext ρ'. unfold hide. split.
  intros h. 
  have NI: ~ Scope.In mini.Test.r Scope.empty. admit.
  specialize (h mini.Test.r NI). 
Admitted.
 
Lemma hide_nothing (s : ENV) : s \ Scope.empty = s.
rewrite hide_equiv.
rewrite join_map.
transitivity (Sets.bind s (fun ρ => ⌈ ρ ⌉)).
f_equal.
eapply functional_extensionality. intro x. eapply hide_env_nothing.
set_simpl. 
done.
Qed.

#[export] Hint Rewrite hide_nothing : set_simpl.


Lemma hide_intersect x s1 s2 : hide x (s1 ∩ s2) ⊆ ((hide x s1) ∩ (hide x s2)).
Proof. 
  intros ρ [_ [[ρ' h11] h2]].
  unfold hide.
    split. 
    exists ρ'. split; auto. exists ρ'. split; auto.
Qed. 
(* NOTE: converse is not true *)

Lemma hide_intersection_l xs s1 s2 :
  hidden xs s1 ->
  hide xs (s1 ∩ s2) = s1 ∩ hide xs s2.
Proof.
  intro h.
  rewrite hide_equiv.
  set_simpl.
  set_ext ρ.
  split.
  + intro r.
    inv r. rename x into ρ0. move: H => [h1 h2].
    inv h1. rename x into ρ1. move: H => [h3 h4].
    unfold hidden in h. rewrite <- h in *.
    subst.
Admitted.

Lemma hide_intersection_r xs s1 s2 :
  hidden xs s2 ->
  hide xs (s1 ∩ s2) = hide xs s1 ∩ s2.
Proof.
Admitted.


Lemma hide_constrain x k xs :
  ~ (Scope.In x xs) ->
  (x ≈ ⟨ k ⟩) \ xs = (x ≈ ⟨ k ⟩).
intro h.
unfold hide.
set_ext ρ. unfold In.
split.
+ intros h1. set_crunch. unfold constrain_eq in *.
  rewrite H0; auto.
+ unfold constrain_eq. intros h1.
  exists ρ. split; eauto.
Qed.

(* ------------------------------------------------------ *)

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


