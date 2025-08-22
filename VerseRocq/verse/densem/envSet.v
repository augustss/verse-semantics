(* This module defines various operations on sets of environments and lemmas about their equality.

   Δ := x ≈ ⟨ k ⟩ | x ≉ ⟨ k ⟩    -- k :: value       constrain equal, non equal
     |  x ≈ ⟪ f ⟫ | x ≉ ⟪ f ⟫    -- f :: rho -> value
     |  ρ \\ xs                  -- hide (generalize single variable to set)
     |  Δ \ xs                   -- hide (generalize variables)
     |  ... other set ops ...

   also defines the property

     "hidden Δ xs"  when xs are already hidden (i.e. Δ \ xs = Δ)

   and adds several simplification rewrites to the 'set_simpl' database.

  By default, the notations are not inscope, but can be made accessible using 

      Import envSetNotation.

 *)


Require Import Imports.

From Stdlib Require Export Program.Basics.

From Stdlib Require Lists.List.

Require Import syntax.common.
Require Import PFun.
Require Import structures.Sets.
Import structures.List.

Require Import densem.Dom.
Require Import densem.tenv.  (* environments are total *)

Import SetNotations.
Import SetMonadNotation.
Import List.ListNotations.
Import EnvNotation.

Open Scope list_scope.
Open Scope env_scope.
Open Scope set_scope.

Notation ENV := (P env).

Notation envs := (Total_set : ENV).

(* Constrain a simple expr to be equal to another.
   All other mappings in the environment are unconstrained. 
*)
Definition constrain_eq (a1 : common.Simple) (a2 : common.Simple) : ENV := 
  fun ρ => evalA a1 ρ = evalA a2 ρ.
Definition constrain_lt (a1 : common.Simple) (a2 : common.Simple) : ENV := 
  fun ρ => Value.V_lt (evalA a1 ρ) (evalA a2 ρ).
Definition constrain_ne (a1 : common.Simple) (a2 : common.Simple) : ENV := 
  fun ρ => not (evalA a1 ρ = evalA a2 ρ).

Definition constrain_fun (a1 a2 a3 : common.Simple) (i : nat) : ENV := 
  fun ρ =>  exists hs h, evalA a1 ρ = Fun hs 
            /\ List.nth_error hs i = Some h 
            /\ List.In (evalA a2 ρ , evalA a3 ρ ) h.

(* ρ \ xs *)
Definition hide_env (xs : Scope.t) (ρ : env) : ENV := 
  fun ρ' => forall x, ~ (Scope.In x xs) -> (ρ x = ρ' x).

(* Generalize all of the xs to be anything *)
(* "Envs Drop Variables" Δ \ xs *)
Definition hide (xs : Scope.t) (Δ : ENV) : ENV := 
  fun ρ => exists ρ', (ρ' ∈ Δ) /\ forall x, ~ (Scope.In x xs) -> (ρ x = ρ' x).
(* NB: this is equivalent to: ⨃(map (hide_env xs) Δ)  *)

(* Generalize all of the xs to be anything *)
(* Envss Drop Variables Δs [\] xs *)
Definition hide_list (xs : Scope.t) (Δs : list ENV) : list ENV := 
  List.map (hide xs) Δs.

Definition envs_difference (Δ1 : ENV) (xs : Scope.t) (Δ2 : ENV) : ENV :=
  Δ1 - (hide xs Δ2).

(* A scope is unconstrained in a set *)
Definition hidden (xs: Scope.t) (Δ : ENV) : Prop := 
  hide xs Δ = Δ.


(* ------  Notation ----------------------------------------- *)

Module envSetNotation.
Notation "{{ a1 ≈ a2 }}" := (constrain_eq a1 a2) (at level 60, a1 at next level, a2 at next level) : set_scope.
Notation "{{ a1 ≉ a2 }}" := (constrain_ne a1 a2) (at level 60, a1 at next level, a2 at next level) : set_scope.
Notation "{{ a1 < a2 }}" := (constrain_lt a1 a2) (at level 60, a1 at next level, a2 at next level) : set_scope.
(* Notation "x ≈ v" := (constrain_eq (Var x) v) (at level 60) : set_scope. *)
(* Infix "≈" := constrain_eq (at level 60) : set_scope.
Infix "≉" := constrain_ne (at level 60) : set_scope. *)
Notation "{{ ( a2 ⤇ a3 ) ∈ a1 < i >  }}" := (constrain_fun a1 a2 a3 i)
    (at level 60, a1 at next level, a2 at next level, a3 at next level, 
    i at next level).
(* Notation "⟨ n ⟩" := (fun ρ => n) (at level 40).
   Notation "⟪ x ⟫" := (fun ρ => ρ x) (at level 40). *)
Notation "ρ \\ xs" := (hide_env xs ρ) (at level 70) : set_scope.
Notation "Δ \ xs" := (hide xs Δ) (at level 70) : set_scope.
Notation "Δ [\] xs" := (hide_list xs Δ) (at level 70) : list_scope.
Notation "es \{ xs } fs" := (envs_difference es xs fs) (at level 40) : set_scope.

Coercion common.Var : Ident >-> Simple.
Coercion common.Int : nat >-> LitType.
Coercion common.Lit : LitType >-> Simple.


(*
Parameter x : Ident.
Check {{ x ≈ 3 }}.
Check {{ (1 ⤇ 2) ∈ x < 0 > }}.  (* single brackets doesn't parse *)
*)

End envSetNotation.

Import envSetNotation.

(* ---- theory about hide ------------------  *)

Lemma hide_equiv xs Δ: 
  Δ \ xs = ⨃ (Sets.map (hide_env xs) Δ).
Proof.        
  set_ext ρ. split.
  - intros h.
    rewrite <- join_map.
    unfold hide,In in h. Sets.set_crunch.
    foldInH x in H.
    exists x. split. auto.
    unfold hide_env.
    intros y NI. rewrite H0; eauto.
  - unfold join. unfoldIn. intros h.
    move: h => [Δ0 [h1 h2]].
    unfold map in h1.
    unfold In in h1. move: h1 => [ρ0 [h3 h4]].
    foldInH ρ0 in h4.
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

#[export] Hint Rewrite hide_env_hidden : set_simpl.

  
(* This is the same as "hidden xs (ρ \ xs)" *)
Lemma hide_hidden xs Δ : ((Δ \ xs) \ xs) = (Δ \ xs).
Proof.
  unfold hidden.
  repeat rewrite hide_equiv.
  repeat rewrite <- join_map.
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

#[export] Hint Rewrite hide_hidden  : set_simpl.

Lemma hide_union x s1 s2 : hide x (s1 ∪ s2) = hide x s1 ∪ hide x s2.
Proof.
  rewrite hide_equiv.
  set_simpl.
  repeat rewrite <- hide_equiv.
  done.
Qed.

#[export] Hint Rewrite hide_union : set_simpl.

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
  - intros h. 
    unfold hide_env in h.
    unfold Ensembles.In in h.
    have EQ: ρ' = ρ.
    { extensionality x.
      rewrite h. intro h1. inv h1. done. }
    rewrite EQ. eapply in_singleton.
  - intro h. inversion h. 
    unfold Ensembles.In, hide_env.
    intros x NI. done.
Qed.

#[export] Hint Rewrite hide_env_nothing : set_simpl.

 
Lemma hide_nothing (s : ENV) : s \ Scope.empty = s.
rewrite hide_equiv.
rewrite <- join_map.
transitivity (Sets.bind s (fun ρ => ⌈ ρ ⌉)).
f_equal.
eapply functional_extensionality. intro x. eapply hide_env_nothing.
set_simpl. 
done.
Qed.

#[export] Hint Rewrite hide_nothing : set_simpl.


Lemma hide_empty_id : 
  (hide Scope.empty) = id.
Proof.
  extensionality x.
  set_simpl.
  done.
Qed.

Lemma hide_list_empty Ds : 
  Ds [\] Scope.empty = Ds.
Proof.
  unfold hide_list. 
  replace (hide Scope.empty) with (id : ENV -> ENV).
  list_simpl. done.
  rewrite hide_empty_id.
  done.
Qed.

#[export] Hint Rewrite hide_list_empty : set_simpl.


Lemma hide_list_empty_id : 
  (hide_list Scope.empty) = id.
Proof.
  extensionality x.
  rewrite hide_list_empty.
  done.
Qed.

#[export] Hint Rewrite hide_empty_id hide_list_empty_id : set_simpl.





Lemma hide_intersect xs s1 s2 : 
  hide xs (s1 ∩ s2) ⊆ ((hide xs s1) ∩ (hide xs s2)).
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
  unfold hidden in h.
  set_ext ρ.
  split.
  - intro r.
    apply hide_intersect in r.
    rewrite h in r.
    done.
  - intro r. inv r.
    destruct H0 as [ρ' [h1 h2]].
    exists ρ'. 
    split; auto. split; auto.
    rewrite <- h.
    unfold hide.
    exists ρ. split. auto.
    intros x NI. rewrite h2; auto.
Qed.

Lemma hide_intersection_r xs s1 s2 :
  hidden xs s2 ->
  hide xs (s1 ∩ s2) = hide xs s1 ∩ s2.
Proof.
  intro h.
  rewrite Intersection_commutes.
  rewrite hide_intersection_l. done.
  rewrite Intersection_commutes. done.
Qed.

Lemma hide_constrain (x : Ident) (k : nat) xs :
  ~ Scope.In x xs ->
  {{ x ≈ k }} \ xs = {{ x ≈ k }}.
Proof.
intro h.
unfold hide.
set_ext ρ. unfoldIn.
split.
+ intros h1. set_crunch. unfold constrain_eq in *.
  unfoldIn. unfold evalA. set_crunch.
  rewrite H0; auto.
+ unfold constrain_eq. intros h1.
  exists ρ. split; eauto.
Qed.


Lemma hide_hide xs ys s : 
  ((s \ xs ) \ ys) = (s \ Scope.union xs ys).
Proof.
  set_ext ρ.
  unfold hide.
  unfoldIn.
  split.
  - intros h. set_crunch.
    exists x0. split; auto.
    intros y yNI.
    rewrite <- H1. 
    rewrite <- H0. done.
    rewrite Scope.union_spec in yNI. tauto.
    rewrite Scope.union_spec in yNI. tauto.
  - intro h. set_crunch.
    exists (Env.extend_env xs ρ x). split.
    exists x. split. auto.
    intro y. specialize (H0 y).
    rewrite Scope.union_spec in H0. 
    intro h1.
    rewrite extend_env_spec1; auto. 
    intro y. specialize (H0 y).
    rewrite Scope.union_spec in H0. 
    intro h1.
    move: (Scope.mem_spec xs y) => MEM.
    destruct (Scope.mem y xs) eqn:DEC.
    rewrite extend_env_spec2; auto. tauto.
    have h2: ~(Scope.In y xs).  intro h3. apply MEM in h3. done.
    rewrite extend_env_spec1; auto. 
    rewrite H0.
    tauto.
    done.
Qed.

(* ------------------------------------------------------ *)

Lemma in_constrain_eq (ρ : env) (a1: common.Simple) (a2 : common.Simple) :
  ρ ∈ {{ a1 ≈ a2 }} <-> evalA a1 ρ = evalA a2 ρ.
split. intro h. inversion h. done.
intro h. unfold constrain_eq. done.
Qed.

Hint Rewrite in_constrain_eq : set_simpl.

Lemma constrain_eq_same {r v} : 
  {{ r ≈ v }} ∩ {{ r ≈ v }} = {{ r ≈ v }}.
Proof.
  eapply set_extensionality; intros x.
  rewrite in_intersection.
  repeat rewrite in_constrain_eq. 
  tauto.
Qed.

Hint Rewrite @constrain_eq_same : set_simpl.

Lemma constrain_self {x} : 
  {{ x ≈ x }} = envs.
Proof.
  eapply set_extensionality; intros y.
  split; intro h; try done.
Qed.

Lemma constrain_self_imposs {x} : 
  {{ x ≉ x }} = ∅.
Proof.
  eapply set_extensionality; intros y.
  split; intro h; try done.
Qed.

Hint Rewrite @constrain_self  @constrain_self_imposs : set_simpl.

Lemma diff_nat (k1 k2 : nat) ρ : k1 <> k2 -> evalA k1 ρ <> evalA k2 ρ.
Proof.
  intros NE.
  cbn. congruence.
Qed.

Lemma constrain_eq_intersection {r}{k1 k2 : nat} : 
  {{ r ≈ k1 }} ∩ {{ r ≈ k2 }} = 
  if Nat.eqb k1 k2 then 
    {{ r ≈ k1 }} 
  else 
    ∅.
Proof.
  destruct (Nat.eqb k1 k2) eqn:EV;
   [rewrite PeanoNat.Nat.eqb_eq in EV; subst|
    rewrite PeanoNat.Nat.eqb_neq in EV].
    + eapply set_extensionality; intros x.
    rewrite in_intersection.
    repeat rewrite in_constrain_eq.  tauto.
    + eapply set_extensionality; intros x.  
      subst.
      rewrite in_intersection.
      repeat rewrite in_constrain_eq.
      split; try done.
      cbn.
      intros [h1 h2]. congruence.
Qed.

Fixpoint eval_fresh ρ r v a : 
  ~ Scope.In r (fvs a) ->
  evalA a (r |-> v, ρ) = evalA a ρ.
Proof. 
  destruct a; cbn; intro Fr.
  - rewrite Scope.singleton_spec in Fr.
    rewrite extend_lookup_diff.
    rewrite PeanoNat.Nat.eqb_neq. done. done.
  - destruct l. done.
  - done.
  - induction l. cbn. done.
    cbn.
Admitted.    

Lemma constrain_eq_hide_same_simple (r : Ident) (a : common.Simple) : 
  not (Scope.In r (fvs a)) -> 
  ({{ r ≈ a }} \ Scope.singleton r) = envs.
Proof.
  intro Fr.
  eapply set_extensionality. intro ρ.
  unfold hide.
  split.
  + intro h. done.
  + intros _. 
    exists (r |-> evalA a ρ, ρ).
    split.
    ++ rewrite in_constrain_eq.
       cbn.
       rewrite extend_lookup_same.
       rewrite eval_fresh. auto.
       done.
    ++ intros y h.
       rewrite Scope.singleton_spec in h.
       rewrite extend_lookup_diff.
       rewrite PeanoNat.Nat.eqb_neq. easy.
       done.
Qed.

Lemma constrain_eq_hide_same_var (r y : Ident): 
   ({{r ≈ y}} \ Scope.singleton r) = envs.
Proof.
  destruct (Nat.eqb r y) eqn:h.
  - rewrite PeanoNat.Nat.eqb_eq in h. subst.
    set_simpl. done.
  - rewrite constrain_eq_hide_same_simple.
    cbn.
    rewrite PeanoNat.Nat.eqb_neq in h. 
    rewrite Scope.singleton_spec. done.
    done.
Qed.

Lemma constrain_eq_hide_same_nat (r : Ident) (k : nat): 
   ({{r ≈ k}} \ Scope.singleton r) = envs.
Proof.
  rewrite constrain_eq_hide_same_simple.
  cbn. intro h. inv h.
  done.
Qed.

Hint Rewrite constrain_eq_hide_same_var constrain_eq_hide_same_nat : set_simpl.

Lemma constrain_eq_hide_diff (x y:Ident) (k : nat) : 
  x <> y ->
  ({{ x ≈ k }} \ Scope.singleton y) = {{ x ≈ k }}.
intro NE.
eapply set_extensionality. intro ρ.
Admitted.

Lemma constrain_eq_hide_two (k1 k2 : nat) (x r : Ident) :
  (x <> r) -> 
  {{ x ≈ k1 }} ∩ {{ r ≈ k2 }} \ Scope.singleton r = {{ x ≈ k1 }}.
Proof.
intros diff.
eapply set_extensionality. intro ρ.
split.  
Admitted.


(* 
Lemma intersect_lookup (g : value -> value)(f : env -> value)  (y r : Ident) : 
  y <> r ->
  ((y ≈ compose g (fun ρ : env => ρ r)) ∩ r ≈ f) = 
  ((y ≈ compose g f) ∩ r ≈ f).
Proof.
  intro h.
  unfold compose.
  unfold constrain_eq.
  set_ext ρ.
  repeat rewrite in_intersection.
  unfold Ensembles.In.
  split. intros [h1 h2]. rewrite h1. rewrite h2. done.
  intros [h1 h2]. rewrite h1. rewrite h2. done.
Qed. *)

(* ---------------------------------------------------- *)


Lemma bind_Total_set {A B} (k : A  -> P B) (S : P B) : 
  inhabited A ->
  (forall ρ, k ρ = S) ->
  (ρ ⭅ Total_set ;; k ρ) = S.
Proof.
  intros iA h.
  set_ext ρ'.
  rewrite in_bind.
  split. 
  + intro h1. set_crunch. rewrite h in H0. done.
  + intros h1. inv iA. exists X. split; cbv; auto. rewrite h. done.
Qed.

Lemma bind_envs {B} (k : env -> P B) (S : P B) :
  (forall ρ, k ρ = S) -> 
  (ρ ⭅ envs ;; k ρ) = S.
eapply bind_Total_set. econstructor. exact Env.empty.
Qed.

Lemma bind_set {A B} (k : A -> P B) S1 (S2 : P B) :
  forall ρ1, ρ1 ∈ S1 ->
  (forall ρ, ρ ∈ S1 -> (k ρ = S2)) -> 
  (ρ ⭅ S1 ;; k ρ) = S2.
Proof.
  intros.
  set_ext ρ'.
  rewrite in_bind.
  split.
  + intro h. set_crunch. rewrite H0 in H2; done.
  + intro h. specialize (H0 ρ1). rewrite <- H0 in h; auto.
    exists ρ1. split; eauto.
Qed.


(* -------- *)

Lemma bind_singleton_r_id {A} (l1 : list (P A)) : 
  (List.flat_map (fun ρ0 => [ Total_set ∩ ρ0 ]) l1 = l1).
induction l1; cbn.
done. set_simpl. f_equal. auto. Qed.

#[export] Hint Rewrite @bind_singleton_r_id : set_simpl.
