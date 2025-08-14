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
Require Import densem.envSet. (* def of ENV , hide, constraints *)
Require Import densem.densem. (* semantic definitions *)

Import mini.MiniNotation.
Import SetNotations.
Import SetMonadNotation.
Import List.ListNotations.
Import ListMonadNotation.
Import EnvNotation.
Import envSetNotation.

Open Scope list_scope.
Open Scope mini_expr_scope.
Open Scope env_scope.
Open Scope set_scope.

Import DLS.

(* other distinguished variable (not equal to r) *)
Definition x := mini.Test.x.
Definition y := mini.Test.y.


(* This is a bit of a hack. For examples, we special case the variables
   x,y, and y when simplifying environment lookups *)

Ltac rewrite_env := 
  repeat match goal with 
    | [ |- context[ (?x |-> ?v , ?rho) ?x ] ] => 
        rewrite extend_lookup_same
    | [ |- context[ (x |-> ?v , ?rho) r ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
    | [ |- context[ (y |-> ?v , ?rho) r ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
    | [ |- context[ (r |-> ?v , ?rho) x ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
    | [ |- context[ (r |-> ?v , ?rho) y ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
    | [ |- context[ (y |-> ?v , ?rho) x ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
    | [ |- context[ (x |-> ?v , ?rho) y ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
  end.


(* -------------------------------------------------------- *)
(* ---- examples / theory about extract / squash_pick ----- *)

Lemma squash_pick_singleton {A} (v : A) : 
  squash_pick [ ⌈ v ⌉ ] = ⌈ [v] ⌉.  
  cbn.
  set_simpl.
  unfold Cons. set_simpl.
  done.
Qed.

Example squash_pickExample :
  squash_pick [  ⌈ (Int 0) ⌉ ; ⌈ (Int 1) ⌉ ] = 
    (⌈ [ Int 0 ; Int 1 ] ⌉).
cbn. set_simpl.
unfold Cons. set_simpl.
done.
Qed.

Lemma squash_pick_two_example :
  squash_pick [ ⌈ (Int 0) ⌉ ∪ ⌈ (Int 1) ⌉ ] = 
               (⌈ [ Int 0 ] ⌉ ∪ ⌈ [ Int 1 ] ⌉).
Proof.
  cbn. unfold Cons,If3. set_simpl.
Admitted.


Lemma extract_one {v} : 
  extract r (r ≈ ⟨ v ⟩) = ⌈ (v, Total_set) ⌉.
Proof. 
eapply set_extensionality. intro ρ.
unfold extract. 
set_simpl.
split.
- move=>h.
  set_crunch.
  set_simpl.
  f_equal.
- destruct ρ as [w Δ].
  intro h. inv h.
  exists (r |-> v).
  set_simpl.
  done.
Qed. 

Lemma extract_example {k} :
  extract r (x ≈ ⟨ k ⟩ ∩ r ≈ ⟨ k ⟩) = 
    ⌈ (k, x ≈ ⟨ k ⟩) ⌉.
Proof.
  unfold extract.
  eapply set_extensionality. intros [v Δ].
  set_simpl.
  split.
  + intros h.
    inv h.
    set_crunch.
    inv H.
    repeat rewrite H2.  clear H2.
    f_equal.
    set_simpl.
    rewrite constrain_eq_hide_two. done. done.
  + intro h. inv h.
    exists (x |-> k, r |-> k).
    set_simpl.
    rewrite_env.
    rewrite constrain_eq_hide_two. done. done.
Qed. 

Lemma extract_two :
  extract r (r ≈ ⟨ Int 0 ⟩ ∪ (r ≈ ⟨ Int 1 ⟩)) = 
    (⌈ (Int 0, Total_set) ⌉ ∪ ⌈ (Int 1, Total_set) ⌉).
Admitted.


Lemma bcp_example : 
  ALL [x ≈ ⟨ Int 0 ⟩ ∩ r ≈ ⟨ Int 0 ⟩] = 
      ((x ≈ ⟨ Int 0 ⟩ ∩ r ≈ ⟨mkTup [Int 0]⟩) ∪
       (x ≉ ⟨ Int 0 ⟩ ∩ r ≈ ⟨mkTup []⟩)).
Proof.
  unfold ALL.
  apply set_extensionality. intros ρ.
  unfoldIn.
  unfold consistent_results.
  list_simpl.
  rewrite extract_example.
  set_simpl.
  unfold squash_pick.
  remember (when (ρ ∈ x ≈ ⟨ Int 0 ⟩) ⌈ Int 0 ⌉) as ϕ.
  split.
  + intro h. 
    crunch.
    unfold If3 in H.
    inv H. 
    ++ set_crunch.
       unfold Cons in H1.
       set_crunch.
       inv H2.
       rewrite H3.
       left. split; eauto.
    ++ unfold when, guard in H1.
       set_crunch.       
       admit.
Admitted.
(*
rewrite H1.
       left.
       have hh: (Int 0 = Int 0) = True. { apply propositional_extensionality. tauto. }
       rewrite hh.
       set_simpl.
       reflexivity.
    ++ exists nil.
       cbn. 
       set_crunch.
       unfold constrain_ne,In in H0.
       have hh: ((x ≈ ⟨ Int 0 ⟩) ρ) = False.
       { unfold constrain_eq. 
         eapply propositional_extensionality. tauto. }
       rewrite hh. 
       set_simpl.
       eapply in_singleton.
       auto.
Qed. *)

(* one { 0 | 1 } = [ {{ r = 0 }} ]     *)
(* one { x = 0 } = [ {{ r = x = 0 }} ] *)
(* one { x = 0 | x = 1 } = [ {{ r=x=0 }} union {{r=x=1}} ] *)

Definition D0 : ENV := r ≈ ⟨ Int 0 ⟩.
Definition D1 : ENV := r ≈ ⟨ Int 1 ⟩.
Definition Dx v : ENV := x ≈ ⟨ v ⟩ ∩ r ≈ ⟨ v ⟩.


Lemma extract_D0 : 
  extract r D0 = ⌈ ( Int 0, Total_set ) ⌉. 
Admitted.
Lemma extract_D1 :
  extract r D1 = ⌈ (Int 1 , Total_set) ⌉.
Admitted.

Lemma ONE_example1 : ONE [ D0 ; D1 ] = D0.
Proof.
  unfold ONE.
  apply set_extensionality. intros ρ.
  unfold consistent_results.
  unfoldIn.
  unfold List.map.
  rewrite extract_D0. rewrite extract_D1.
  set_simpl.
  unfold squash_pick.
  unfold Head.
  split.
  + intros h.
    inv h.
    inv H.
Admitted.
(*    inv H0. 
    unfold D0, constrain_eq. 
    autorewrite with set_simpl in H1. inv H1.
  + unfold D0. 
    intro h. inv h.
    exists [ Int 0 ; Int 1].
    rewrite H0.
    split.
    cbn.
    set_simpl.
    eapply in_singleton. cbn.
    eapply in_singleton.
Qed.
*)

Lemma ONE_example2 : 
  ONE [ Dx (Int 0) ] = Dx (Int 0).
Admitted.

Lemma bind_when {A B} ϕ (s : P A) (k : A -> P B) : 
  (bind (when ϕ s) k) = (⟨ϕ⟩ ∩ (bind s k)).
Proof.
Admitted.

Lemma If3_when_ret {A B} (k : A) ϕ (s2 s3 : P B): 
  If3 (when ϕ ⌈k⌉) s2 s3 = ((⟨ϕ⟩ ∩ s2) ∪ (⟨not ϕ⟩ ∩ s3)).
Admitted.

Lemma ONE_example3 : 
  ONE [ Dx (Int 0) ; Dx (Int 1) ] = (Dx (Int 0) ∪ Dx (Int 1)).
Proof.
  unfold ONE, Dx, consistent_results.
  apply set_extensionality. intros ρ.
  unfoldIn.
  unfold List.map.
  repeat rewrite extract_example.
  set_simpl.
  split.
  + intros h.
    unfold Head, squash_pick in h.
Admitted.    


Lemma ALL_two_example :
  ALL [(r ≈ ⟨Int 0⟩ ∪ (r ≈ ⟨Int 1⟩)) ] = 
      (r ≈ ⟨mkTup [Int 0]⟩ ∪ (r ≈ ⟨mkTup [Int 1]⟩)).
  apply set_extensionality. intros ρ.
  set_simpl.
  unfold ALL.
  unfoldIn.
  list_simpl. unfold consistent_results.
  rewrite extract_two.
  set_simpl.
  repeat rewrite when_is_true; try easy.
  rewrite squash_pick_two_example.
  split.
  + intro h. crunch.
    inv H; inv H1. left; auto. right; auto.
  + intro h. inv h.
    exists [Int 0]. split; auto. left. eauto with sets.
    exists [Int 1]. split; auto. right. eapply in_singleton. 
Qed.

Lemma conflict_example : 
  ALL [ x ≈ ⟨ Int 0 ⟩ ∩ r ≈ ⟨ Int 0 ⟩ ; 
         x ≈ ⟨ Int 1 ⟩ ∩ r ≈ ⟨ Int 1 ⟩ ]
   = (((x ≈ ⟨ Int 0 ⟩) ∩ (r ≈ ⟨ mkTup [Int 0] ⟩))
   ∪ ((x ≈ ⟨ Int 1 ⟩) ∩ (r ≈ ⟨ mkTup [Int 1] ⟩))
   ∪ ((x ≉ ⟨ Int 0 ⟩) ∩ x ≉ ⟨ Int 1 ⟩ ∩ (r ≈ ⟨ mkTup []⟩))).
Admitted.  


Lemma squash_irr_ALL (s : list ENV) : 
  ALL s = ALL (squash s).
Proof.
  unfold ALL.
  eapply set_extensionality. intros ρ.
  repeat unfoldIn.
  split.
  + intro h. crunch.
    exists x0. split; auto.
Abort.

(* ---------------------------------------------------------- *)

(*** IF examples and variants ***)    


Definition negative_IF := 
  mini.If3 (x :=: 0 :|: x :=: 1) (x :=: 1) mini.Fail.

Notation "r ≈ x ≈ ⟨ k ⟩" := ((r ≈ ⟨ k ⟩) ∩ (x ≈ ⟨ k ⟩)) : set_scope.

Lemma E_scrut :
  E (x :=: 0 :|: x :=: 1) = [ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ].
Proof.
  cbn. 
Abort.  

Lemma xNr x r (EQ: x <> r) : ~ Scope.In x ⟅ r ⟆.
Admitted.

Lemma rIr r : Scope.In r ⟅ r ⟆.
Admitted.

Lemma hidden_constrain x r k (NEQ: x <> r) :
  (x ≈ ⟨ k ⟩) \ ⟅ r ⟆ = (x ≈ ⟨ k ⟩).
unfold hidden. rewrite hide_constrain. eapply xNr. done. done.
Qed.

Hint Resolve hidden_constrain : sets.

Lemma hide_result k:
  (r ≈ x ≈ ⟨ k ⟩ \ ⟅ r ⟆) = x ≈ ⟨ k ⟩.
Proof.
  rewrite hide_intersection_r. eapply hidden_constrain; auto.
  set_simpl. done.
Qed.

Lemma hide_arg k:
  (r ≈ x ≈ ⟨ k ⟩ \ ⟅ x ⟆) = r ≈ ⟨ k ⟩.
Proof.
  rewrite hide_intersection_l. eapply hidden_constrain; auto.
  set_simpl. done.
Qed.


Hint Rewrite hide_result hide_arg : sets.
(*
Lemma hide_example k: 
  ((r ≈ x ≈ ⟨ k ⟩) ∩ r ≈ ⟨ k ⟩ \ ⟅ r ⟆) = x ≈ ⟨ k ⟩.
Admitted.
*)
Lemma constrain_same1 r x k : 
  (r ≈ x ≈ ⟨ k ⟩ ∩ r ≈ ⟨ k ⟩) = (r ≈ x ≈ ⟨ k ⟩).
Admitted.

Lemma constrain_same2 r x k : 
  (r ≈ ⟨ k ⟩ ∩ (r ≈ x ≈ ⟨ k ⟩)) = (r ≈ x ≈ ⟨ k ⟩).
Admitted.

Lemma constrain_same3 r x k : 
  (x ≈ ⟨ k ⟩ ∩ (r ≈ x ≈ ⟨ k ⟩)) = (r ≈ x ≈ ⟨ k ⟩).
Admitted.

Lemma constrain_same4 r x k : 
  (r ≈ x ≈ ⟨ k ⟩ ∩ x ≈ ⟨ k ⟩) = (r ≈ x ≈ ⟨ k ⟩).
Admitted.


Hint Rewrite constrain_same1 constrain_same2 constrain_same3 constrain_same4 : sets.

Lemma constrain_conflict0 k1 k2 (NE : k1 <> k2) :
  (x ≈ ⟨ k1 ⟩) ∩ (x ≈ ⟨ k2 ⟩) = ∅.
Admitted.

Lemma constrain_conflict1 k1 k2 (NE : k1 <> k2) :
  (x ≈ ⟨ k1 ⟩) ∩ (r ≈ x ≈ ⟨ k2 ⟩) = ∅.
Admitted.

Lemma constrain_conflict2 k1 k2 (NE : k1 <> k2) :
 (r ≈ ⟨ k1 ⟩) ∩ (x ≈ ⟨ k1 ⟩) ∩ ((x ≈ ⟨ k2 ⟩ ∪ x ≈ ⟨k1⟩)) = (r ≈ ⟨ k1 ⟩) ∩ (x ≈ ⟨ k1 ⟩).
Admitted.

Lemma Setminus_disjoint1 k1 k2 (NEQ: k1 <> k2) :
  (x ≈ ⟨ k1 ⟩) - (x ≈ ⟨ k2 ⟩) = (x ≈ ⟨ k1 ⟩).
Admitted.

Lemma Setminus_disjoint k1 k2 (NEQ: k1 <> k2) :
  (r ≈ x ≈ ⟨ k1 ⟩) - (r ≈ ⟨ k2 ⟩) = (r ≈ x ≈ ⟨ k1 ⟩).
Proof.
  unfold Setminus.
  set_ext ρ. unfoldIn.
  split. 
  + intros [h1 h2].
    set_crunch.
    split. unfold constrain_eq. unfoldIn. auto.
    unfold constrain_eq. unfoldIn. auto.
  + intros [h1 h2].
    set_crunch; unfold constrain_eq; try split; try unfoldIn; auto.
    congruence.
Qed.


(* if (x: = (0 |1)) { x = 1 } else {fail} == [ 1 ] *)
(* Tim wants this to fail. *)
Lemma IF1a_Tim1_should_fail : 
  IF_TIM1 ⟅x⟆ ([ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ]) [(r ≈ x ≈ ⟨Int 1⟩) ] [] = 
  [r ≈ ⟨ Int 1 ⟩; ∅].
unfold IF_TIM1.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
repeat rewrite hide_arg; auto.
set_simpl.
auto.
Qed.


(* if (x: = (0 |1)) { x = 1 } else {fail} == [ 1 ] *)
(* Tim wants this to fail. *)
Lemma IF1a_Tim2_should_fail : 
  IF_TIM2 ⟅x⟆ ([ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ]) [(r ≈ x ≈ ⟨Int 1⟩) ] [] = 
  [∅; r ≈ ⟨ Int 1 ⟩].
unfold IF_TIM2.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
rewrite constrain_conflict1; try done.
rewrite Setminus_disjoint1; try done.
set_simpl.
rewrite constrain_same3.
rewrite hide_arg; auto.
Qed.


(* if (x: = (0 |1)) { x = 1 } else {fail} == [ 1 ] *)
(* Tim wants this to fail. *)
Lemma IF1a_Tim3_should_and_does_fail : 
  IF_TIM3 ⟅x⟆ ([ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ])
               [(r ≈ x ≈ ⟨Int 1⟩) ] [] = 
  [∅; ∅].
unfold IF_TIM3.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
rewrite constrain_conflict1; try done.
set_simpl.
auto.
Qed.




Lemma IF2a_Tim3 : 
  IF_TIM3 (Scope.empty) 
    ([ (r ≈ x ≈ ⟨Int 1⟩) ; (r ≈ x ≈ ⟨Int 3⟩) ]) 
         [(r ≈ x ≈ ⟨Int 2⟩) ] [r ≈ ⟨ Int 77⟩] = 
  [
   ∅; ∅;
   Total_set - ((x ≈ ⟨ Int 1 ⟩) ∪ x ≈ ⟨ Int 3 ⟩) ∩ r ≈ ⟨ Int 77 ⟩
   ].
   (* last one is the same as x <> 1/3 , r = 77 *)
unfold IF_TIM3.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
rewrite Setminus_disjoint1; try done.
rewrite constrain_conflict1; try done.
replace ((x ≈ ⟨ Int 3 ⟩) ∩ r ≈ x ≈ ⟨ Int 2 ⟩) with (∅ : ENV). 2: admit.
auto.
Admitted.

Lemma IF2b_Tim2 : 
  IF_TIM2 (Scope.empty) 
    ([ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ]) 
         [(r ≈ x ≈ ⟨Int 0⟩) ] [r ≈ ⟨ Int 77⟩] = 
  [r ≈ x ≈ ⟨ Int 0 ⟩;
   ∅; 
   Total_set - ((x ≈ ⟨ Int 0 ⟩) ∪ x ≈ ⟨ Int 1 ⟩) ∩ r ≈ ⟨ Int 77 ⟩].
   (* last one is the same as x <> 0/1 , r = 77 *)
unfold IF_TIM2.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
rewrite Setminus_disjoint1; try done.
rewrite constrain_same3.
rewrite constrain_conflict1; try done.
Qed.


Lemma IF2b_Tim3 : 
  IF_TIM3 (Scope.empty) 
    ([ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ]) 
         [(r ≈ x ≈ ⟨Int 0⟩) ] [r ≈ ⟨ Int 77⟩] = 
  [r ≈ x ≈ ⟨ Int 0 ⟩;
   ∅; 
   Total_set - ((x ≈ ⟨ Int 0 ⟩) ∪ x ≈ ⟨ Int 1 ⟩) ∩ r ≈ ⟨ Int 77 ⟩].
   (* last one is the same as x <> 0/1 , r = 77 *)
unfold IF_TIM3.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
rewrite Setminus_disjoint1; try done.
rewrite constrain_same3.
rewrite constrain_conflict1; try done.
Qed.



Lemma hide_equiv_arg k x r (NE : x <> r) :
        ((x ≈ ⟨ k ⟩) ∩ r ≈ ⟪ x ⟫ \ ⟅ x ⟆) = (r ≈ ⟨ k ⟩).
Admitted.

Lemma IF3_Tim3 : 
  IF_TIM3 ⟅x⟆ ([ (r ≈ x ≈ ⟨Int 1⟩) ; (r ≈ x ≈ ⟨Int 2⟩) ]) [(r ≈ ⟪x⟫)] [] = 
  [r ≈ ⟨Int 1⟩; ∅].
Proof.
  unfold IF_TIM3.
  cbn.
  set_simpl.
  repeat rewrite hide_result; auto.
  set_simpl.
  rewrite hide_equiv_arg. try done.
  auto.
Qed.


Lemma IF4_Tim3 : 
  IF_TIM3 ⟅x⟆ ([ (r ≈ x ≈ ⟨Int 1⟩) ; (r ≈ x ≈ ⟨Int 2⟩) ]) [(r ≈ ⟪x⟫);(r ≈ ⟪x⟫)] [] = 
  [r ≈ ⟨Int 1⟩; r ≈ ⟨ Int 1 ⟩; ∅; ∅].
Proof.
  unfold IF_TIM3.
  cbn.
  set_simpl.
  repeat rewrite hide_result; auto.
  set_simpl.
  rewrite hide_equiv_arg; try done.
Qed.


Lemma IF5b_Tim3 : 
  IF_TIM3 Scope.empty ([ (r ≈ x ≈ ⟨Int 7⟩) ; (r ≈ y ≈ ⟨Int 3⟩) ]) [(r ≈ ⟪x⟫)] [] = 
  [(x ≈ ⟨ Int 7 ⟩) ∩ r ≈ ⟪ x ⟫; 
   (y ≈ ⟨ Int 3 ⟩) - (x ≈ ⟨ Int 7 ⟩) ∩ r ≈ ⟪ x ⟫].
Proof.
  unfold IF_TIM3.
  cbn.
  set_simpl.
  repeat rewrite hide_result; auto.
  f_equal.
  replace (r ≈ y ≈ ⟨ Int 3 ⟩ \ ⟅ r ⟆) with (y ≈ ⟨ Int 3 ⟩). 2: admit.
Admitted.




(* if (x: = (0|1)) { x } else {fail} == [ 1 ] *)
(* Tim wants this to fail. *)
Lemma IF_Tim1_choice_communicate : 
  IF_TIM1 ⟅x⟆ ([ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ]) [(r ≈ ⟪x⟫) ] [] = 
  [ Total_set ].
unfold IF_TIM1.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
set_simpl.
admit.
Admitted.



Lemma IF_Tim_choice_no_bind : 
  IF_TIM2 (Scope.empty) 
    ([ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ]) 
         [(r ≈ x ≈ ⟨Int 1⟩) ] [] = 
  [∅; r ≈ x ≈ ⟨ Int 1 ⟩].
unfold IF_TIM2.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
rewrite constrain_conflict1; try done.
rewrite Setminus_disjoint1; try done.
rewrite constrain_same3.
auto.
Qed.


Lemma IF_Tim_choice_ifxx : 
  IF_TIM2 (Scope.empty) 
    ([ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ]) 
         [(r ≈ ⟪x⟫) ] [] = 
  [r ≈ x ≈ ⟨Int 0⟩; r ≈ x ≈ ⟨ Int 1 ⟩].
Proof.
unfold IF_TIM2.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
f_equal. admit.
rewrite Setminus_disjoint1; try done.
f_equal.
admit.
Admitted.



Lemma IF_Tim_choice_ifxxscoped : 
  IF_TIM2 ⟅x⟆ 
    ([ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ]) 
         [(r ≈ ⟪x⟫) ] [] = 
  [r ≈ ⟨Int 0⟩; r ≈ ⟨ Int 1 ⟩].
Proof.
unfold IF_TIM2.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
f_equal. admit.
rewrite Setminus_disjoint1; try done.
f_equal.
admit.
Admitted.



(* if (x: = 0 | x = 1) { x = 1 } else {fail} == [ 1 ] *)
Lemma IF_SPJ_choice : 
  IF_SPJ ⟅x⟆ ([ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ]) [(r ≈ x ≈ ⟨Int 1⟩) ] [] = 
  [r ≈ ⟨ Int 1 ⟩].
unfold IF_SPJ.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
rewrite constrain_conflict2; try done.
rewrite hide_arg.
auto.
Qed.

(* if (x = 0 | x = 1) { x = 1 } else {fail} == [ x = 1 ] *)
Lemma IF_SPJ_choice_no_bind : 
  IF_SPJ (Scope.empty) ([ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ])
     [(r ≈ x ≈ ⟨Int 1⟩) ] [] = 
  [r ≈ x ≈ ⟨ Int 1 ⟩].
Proof.
unfold IF_SPJ.
cbn.
set_simpl.
repeat rewrite hide_result; auto.
rewrite constrain_conflict2; try done.
Qed.


Lemma extract_xisk k : 
  extract r (r ≈ x ≈ ⟨k⟩) = ⌈ (k, x ≈ ⟨k⟩) ⌉.
unfold extract. set_simpl.
unfold Sets.map.
set_ext ρ. unfoldIn.
split.
- intro h. set_crunch. eapply in_singleton'.
  f_equal. set_simpl. 
  admit.
- intro h. set_crunch.
  exists (x |-> k, r|-> k). split. 
  f_equal.
  rewrite_env. 
  admit.
Admitted.

Hint Unfold constrain_eq : sets.

(*
Lemma ONEchoice :
  ONE [ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ] = ∅.
Proof.
  unfold ONE.
  unfold List.map.
  unfold consistent_results.
  repeat rewrite extract_xisk.
  set_ext ρ.
  split.
+ intro h. inv h.
  set_simpl in H.
*)

Lemma ONEresolved_example :
  (r ≈ x ≈ ⟨Int 1⟩) ∩ ONE [ (r ≈ x ≈ ⟨Int 0⟩) ; (r ≈ x ≈ ⟨Int 1⟩) ] =
    (r ≈ x ≈ ⟨Int 1⟩).
Proof.  
  unfold ONE.
  unfold List.map.
  unfold consistent_results.
  repeat rewrite extract_xisk.
  set_simpl.
  set_ext ρ.
  split. 
+ 
  intros h.
  set_crunch.
  set_simpl in H0.
  destruct x0. done.
  inv H2.
  unfold squash_pick in H0.
  rewrite when_is_false in H0. intro h. inv h. rewrite H4 in H0. inv H0.
  set_simpl in H0. rewrite H4.
  rewrite when_is_true in H0. auto with sets. 
  auto with sets. 
+ intro h. set_crunch. rewrite H2.
  split. auto with sets. split. auto with sets.
  unfoldIn.
  set_simpl.
  rewrite when_is_false. intro h. inv h. rewrite H2 in H0. done.
  rewrite when_is_true. auto with sets.
  unfold squash_pick. set_simpl.
  unfold Cons, Head. set_simpl.
  done.
Qed.

(**** SIMON's derivation of IF encoded using one. *)

(* Ed[ e1; r=<> ] = [ hide(r) D1 ∩ {{ r=<> }} | D1 <- E[e1] ] *)
Lemma part1 e1 : E (e1 :>: r :=: mini.Array []) = 
        List.map (fun D1 => hide ⟅r⟆ D1 ∩ (r ≈ ⟨mkTup []⟩)) (E e1).
Proof.
  cbn.
  unfold UNIFY.
  unfold hide_list.
  set_simpl.
Admitted.

(*
Ed[ (e1; r=<>) | r=0 ] = [ hide_r D1 \cap {{r=<>}} | D1 <- E[e1] ] ++ [ {{r=0}} ] 
*)
Lemma part2 e1 : E ( (e1 :>: r :=: mini.Array []) :|: r :=: 0) = 
                List.map (fun D1 => hide ⟅r⟆ D1 ∩ (r ≈ ⟨mkTup []⟩)) (E e1) ++ [ r ≈ ⟨Int 0⟩ ].
rewrite E_Choice. 
rewrite part1.
cbn. set_simpl. done.
Qed.

(* ---- y = one{ (e1; z=<>) | z=0}@z -------------
  Ed[ y = one{(e1; z=<>) | z=0}@z ]

= [ { rho | v =<>,  
      if rho ∈ UNIONLIST (map hide(z) E[e1])   # UNIONLIST :: [ENV] -> ENV, union them all
               = 0,   otherwise
     , rho y = v } ]
*)


Definition if2 (ϕ1 : Prop) (ϕ3 : Prop) := 
  (~ ϕ1 /\ ϕ3).

Definition if3 (ϕ1 : Prop) (ϕ2 : Prop) (ϕ3 : Prop) := 
  (ϕ1 /\ ϕ2) \/ (~ ϕ1 /\ ϕ3).

Lemma if3_rhoIn (S1 S2 S3 : ENV) :
  [ (fun ρ => if3 (ρ ∈ S1) (ρ ∈ S2) (ρ ∈ S3)) ] = 
  [(S1 ∩ S2) ∪ (Total_set - S1 ∩ S3)].
Proof.
  unfold if3.
  replace (fun ρ : env => ρ ∈ S1 /\ ρ ∈ S2 \/ ~ ρ ∈ S1 /\ ρ ∈ S3) with 
          ((fun ρ => ρ ∈ S1 /\ ρ ∈ S2) ∪ (fun ρ => ~ ρ ∈ S1 /\ ρ ∈ S3)).
  2: { set_ext ρ. rewrite in_union. intuition. }
  replace (fun ρ => ρ ∈ S1 /\ ρ ∈ S2) with (S1 ∩ S2).
  2: { set_ext ρ. rewrite in_intersection. intuition. inv H. auto. inv H. auto. } 
  replace (fun ρ => ~ ρ ∈ S1 /\ ρ ∈ S3) with (Total_set - S1 ∩ S3).
  2: { set_ext ρ. rewrite in_intersection. unfoldIn.
       intuition. inv H0. done. unfold Total_set, Setminus. unfoldIn. intuition. }
  done.
Qed.

Lemma squash_pick_app : 
  forall {A} (xs ys : list (P A)), squash_pick (xs ++ ys) = 
             xs' ⭅ (squash_pick xs) ;; 
             ys' ⭅ (squash_pick ys) ;; 
             ⌈ xs' ++ ys' ⌉ .
Proof. 
  induction xs.
  + intros ys. cbn. 
    set_simpl.
    reflexivity.
  + intros ys.
    cbn.
    have h: ((Inhabited a) \/ (a = ∅)). admit.
    inv h.
    ++ repeat rewrite If3_nonempty; auto.
       rewrite IHxs.
       unfold Cons.
       set_simpl.
       f_equal.
       apply functional_extensionality. intro v.
       set_simpl.
       f_equal.
       apply functional_extensionality. intro vs.
       rewrite Sets.bind_bind.
       rewrite Sets.bind_singleton_l.
       f_equal.
       apply functional_extensionality. intro ws.
       set_simpl.
       done.
    ++ repeat rewrite If3_empty; auto.
Admitted.


(* = 
   Ed[ y = one{(e1; z=<>) | z=0}@z ]
   = [ { rho | v  = tup[],  if rho ∈ UNIONLIST (map hide(z) E[e1]) 
                  = 0,      otherwise
             , rho y = v } ]
*)

Lemma if3_union: forall (S1 S1' : Prop) (S2 S3 : Prop),  
                if3 (S1 \/ S1') S2 S3 = if3 S1 S2 (if3 S1' S2 S3).
Proof.
 intros. unfold if3. 
 apply propositional_extensionality. tauto. Qed.

Lemma part3 e1 : 
    E (y :=: mini.One ((e1 :>: r :=: mini.Array []) :|: r :=: 0)) = 
      let GOOD := UNIONLIST (List.map hide_r (E e1)) in
      [ (GOOD ∩ (r ≈ y ≈ ⟨mkTup []⟩)) 
      ∪ ((Total_set - GOOD) ∩ (r ≈ y ≈ ⟨Int 0⟩)) ].
Proof.
  rewrite E_Unify.
  rewrite E_One.
  rewrite part2.
  rewrite hide_nothing.
  rewrite E_Var.
  unfold UNIFY.
  list_simpl.

  remember (fmap (fun D2 : ENV => hide_r D2 ∩ r ≈ ⟨ mkTup [] ⟩) (E e1) ++ [r ≈ ⟨ Int 0 ⟩]) as Δs.

  rewrite <- if3_rhoIn.
  remember (UNIONLIST (List.map hide_r (E e1))) as GOOD.

  unfold ONE.

  f_equal.
  set_ext ρ. rewrite in_intersection. 

  unfold In. 
  remember  (List.map (consistent_results ρ) Δs) as S.
  rewrite HeqΔs in HeqS.
  unfold fmap, Functor_list in HeqS.
  rewrite List.map_app in HeqS.
  rewrite List.map_map in HeqS.
  rewrite list_map_singleton in HeqS.
  unfold consistent_results in HeqS.
  rewrite extract_one in HeqS.
  set_simpl in HeqS.
  rewrite when_is_true in HeqS; [unfold Total_set;done|].

  replace (fun x : ENV => x0 ⭅ extract (hide_r x ∩ r ≈ ⟨ mkTup [] ⟩);; 
                       (let (v, Δ') := x0 in when (ρ ∈ Δ') (⌈v⌉)))
     with (fun x : ENV => when (ρ ∈ hide_r x) (⌈mkTup []⌉)) 
  in HeqS.
  2: { 
    apply functional_extensionality. intro Δ1.
    unfold extract.
    admit.
  } 
  subst.
  rewrite squash_pick_app.

  cbn.
  rewrite If3_nonempty. econstructor;econstructor; eauto.
  unfold Cons. set_simpl.
  
  split.
  + intros h. crunch. 
    rewrite H in H0. 
    unfold Head in H0. 
    set_simpl in H0.
    inv H0; crunch.
    inv H1; crunch.
    set_simpl in H1.
    destruct x0. 
    ++ (* squash_pick fails *) 
      inv H1.
      cbn in H2. inv H2.

       right. split; auto. 
       2: { unfold constrain_eq. split. unfoldIn. done.
            unfoldIn. done. }
       intro h.

       remember (E e1) as VS.
       move: H0 h.
       clear.
       (* need to prove that it fails *)
       move: ρ.
       induction VS; intro ρ.
       - cbn. done.
       - intros. 
         replace (a :: VS) with ([a] ++ VS) in H0. 2: { auto. } 
         rewrite List.map_app in H0.
         rewrite squash_pick_app in H0.
         inv H0; crunch.
         rewrite List.map_singleton in H.
         cbn in H.
         inv H0; crunch.
         destruct x0. 2: { inv H1. } 
         destruct x1. 2: { inv H1. } 
         cbn in h. inv h.
         -- rewrite when_is_true in H; auto.
            rewrite If3_nonempty in H. econstructor; econstructor; eauto.
            unfold Cons in H.
            autorewrite with set_simpl in H.
            inv H.
         -- apply IHVS in H0. done. auto.
    ++ (* squash pick succeeds *)
      inv H1. clear H3.
      remember (E e1) as VS.
      clear HeqVS.
      cbn in H2. inv H2.
      induction VS as [|W WS].
      - inv H0.
      - cbn.
        cbn in H0.
        have CL: (ρ ∈ hide_r W) \/ ~(ρ ∈ hide_r W). admit.
        destruct CL.
        -- (* first one works *) 
          clear IHWS.
          rewrite when_is_true in H0; auto.
          rewrite If3_nonempty in H0. econstructor; econstructor; eauto.
          unfold Cons in H0.
          set_simpl in H0.
          inv H0; crunch.
          inv H0; crunch.
          clear H2.

          replace ((hide_r W ∪ UNIONLIST (ListDef.map hide_r WS)) ρ) with
            ((ρ ∈ hide_r W) \/ (ρ ∈ UNIONLIST (ListDef.map hide_r WS))). 2: { unfold In. admit. } 
                                                                      
          rewrite if3_union.
          unfold if3. left. split. auto. split; eauto. 
          rewrite H4. unfold constrain_eq. unfold In. done.
      -- (* first one doesn't *)
        rewrite when_is_false in H0; auto.
        autorewrite with set_simpl in H0.
        move: (IHWS H0) => ih. clear IHWS H0.
        replace ((hide_r W ∪ UNIONLIST (ListDef.map hide_r WS)) ρ) with
            ((ρ ∈ hide_r W) \/ (ρ ∈ UNIONLIST (ListDef.map hide_r WS))). 2: { admit. } 
        rewrite if3_union.
        unfold if3.
        right.
        split. auto.
        unfold if3 in ih.
        inv ih; crunch. left. split. auto. auto.
        right. split. auto. auto.
 + intro h.
   unfold if3 in h.
   destruct h.
   ++ (* ρ in UNIONLIST (E e1 \ r) *)
     crunch.
     inv H0. inv H1. done.
     inv H0. inv H1. inv H2.
     unfold Head.
     set_simpl.
Admitted.

(* y = one{ (e1; z=<>) | z=0}@z; (y=⟨⟩; e2) | (y=0; e3)  
  [ GOOD ⋂ {{y=<>}} ⋂ D | D ∈ Ed[e2] ] ++
  [ BAD ⋂ {{y=0}}   ⋂ D  | D ∈ Ed[e3] ]
*)

Lemma UNIONLIST_hide VS s :
  UNIONLIST (VS [\] s) = ((UNIONLIST VS) \ s).
induction VS.
cbn. set_simpl. done.
cbn. set_simpl. f_equal.
auto.
Qed.

Lemma part4 e1 e2 e3 : 
  E (y :=: mini.One (e1 :>: r :=: mini.Array [] :|: r :=: 0) 
       :>: 
    ((y :=: mini.Array [] :>: e2) :|: (y :=: 0 :>: e3))) = 
  let GOOD := UNIONLIST (List.map hide_r (E e1)) in
  let BAD  := Total_set - GOOD in 
  (List.map (fun D => GOOD ∩ (y ≈ ⟨mkTup []⟩) ∩ D) (E e2)) ++
  (List.map (fun D => BAD  ∩ (y ≈ ⟨Int 0⟩) ∩ D) (E e3)).
Proof.
  rewrite E_Seq.
  rewrite part3.
  rewrite E_Choice.
  repeat rewrite E_Seq.
  repeat rewrite E_Unify.
  repeat rewrite E_Var.
  cbn.
  remember (UNIONLIST (ListDef.map hide_r (E e1))) as GOOD.
  remember (Total_set - GOOD) as BAD.
  repeat rewrite flat_map_map.
  repeat rewrite List.app_nil_r.
  repeat rewrite List.map_app.
  repeat rewrite List.map_map.
  f_equal.
  - f_equal.
    apply functional_extensionality. intro D.
    set_simpl.
    set_ext ρ.
    split.
    ++ set_simpl.
       intros [h1 h2].
       inv h2. inv H. inv H1. inv H. inv H3. inv H1.
       have NI: ~ Scope.In y ⟅ r ⟆. { admit. }
       move: (H2 y ltac:(eauto))=> NY.
       repeat split; try congruence.
       inv h1.
       -- unfold hide in H. inv H. 
          inv H1.
          inv H.
          unfold constrain_eq in H6. inv H6. inv H. inv H7.
          rewrite UNIONLIST_hide in H1. inv H1. crunch.
          rewrite UNIONLIST_hide. unfold hide.
          exists x2. split; auto. intros z zIn. rewrite H5; auto.
       -- unfold hide in H. inv H. crunch.
          inv H. unfold constrain_eq in H6.
          inv H6. inv H. inv H7.
          rewrite <- H1 in H6; auto.
          rewrite H2 in H6; auto.
          rewrite <- H3 in H6.
          rewrite H4 in H6.
          inv H6.
    ++ set_simpl. 
       intro h. inv h. inv H0.
       split.
       left.
       rewrite  UNIONLIST_hide.
       unfold hide. unfoldIn.
       rewrite UNIONLIST_hide in H.
       unfold hide in H. destruct H as [ρ' H].
       inv H.
       exists (r |-> mkTup [], ρ). repeat split.
Admitted.


(* E [if (x=1|x=2) { x } { 0 }] = [ {r=1,x=1} | {r=2,x=2} | {r=0,x<>1,2} *)
Definition if_iter := mini.If3 (x :=: 1 :|: x :=: 2) x 0.

(** other examples **)


(* if(a){b}else{c} <=> if(a){b}else{:false} | if(a){:false}else{c} *)
Lemma opinionated_if a b c: 
  E(mini.If3 a b c) =  E( mini.If3 a b mini.Fail :|: mini.If3 a mini.Fail c).
Proof. 
  unfold E. fold E.
  destruct (try (mini.I a) (E a)) as [success avoid].
  replace (SEQ [Total_set - avoid] []) with ([] : list ENV).
  2 : { cbn. auto. } 
  rewrite -> List.app_nil_r.
  replace (SEQ success ([] [\] mini.I a)) with ([] : list ENV).
  2: { cbn. unfold SEQ, UNIFY. cbn.
       admit. } 
  rewrite -> List.app_nil_l. auto.
Admitted.



(* 

if e1 e2 e3 =
  exists y; 
  y = one{ (e1; z=⟨⟩) | z=0 }@z;
  (y=⟨⟩; e2) | (y=0; e3)

koen_if =
  fun e1 e2 e3 : mini.Expr =>
  ∃ y :>: 
  y :=: mini.One (e1 :>: r :=: mini.Array [] :|: r :=: 0) :>: 
  y :=: mini.Array [] :>: e2 :|: y :=: 0 :>: e3
*)

(*
= [ D ⋂ GOOD | D ∈ Ed[e2] ] ++
  [ D \ GOOD | D ∈ Ed[e3] ]
where
  GOOD :: ENV = UNIONLIST (map hide(r) Ed[e1])

*)

End D_LS_Theory.
