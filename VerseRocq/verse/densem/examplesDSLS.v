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

Require Import examplesDLS.

Import common.ConcreteVars.


Notation x4 := (S (S (S (S x)))).
Notation x3 := (S (S (S x))).
Notation x2 := (S (S x)).
Notation x1 := (S x). 

(* ---------------------------------------- *)

Lemma UNIFY_Total_set {A} (Ds : list (P A)) : 
  [Total_set] * Ds = Ds.
cbn.
rewrite bind_singleton_r_id. rewrite app_nil_r. done. Qed.

#[export] Hint Rewrite  @UNIFY_Total_set : set_simpl.


(* ---------------------------------------- *)

(* Temporary place for properties about sets of environments. *)

Lemma intersect_single_same x v ρ :
⌈ x |-> v, ρ ⌉ ∩ ⌈ x |-> v, ρ ⌉ = ⌈ x |-> v, ρ ⌉.
Proof. 
  set_ext ρ1.
  split; intros h; set_crunch.
  eapply in_singleton.
  split; eapply in_singleton.
Qed.

Lemma intersect_single_diff x v1 v2 ρ1 ρ2 :
v1 <> v2 ->
⌈ x |-> v1, ρ1 ⌉ ∩ ⌈ x |-> v2, ρ2 ⌉ = ∅.
Proof. 
Admitted.


Lemma intersect_single_diff_tail x v1 v2 ρ1 ρ2 :
ρ1 <> ρ2 ->
⌈ x |-> v1, ρ1 ⌉ ∩ ⌈ x |-> v2, ρ2 ⌉ = ∅.
Proof. 
Admitted.


Lemma intersect_single_hide_r x v1 v2 ρ : 
(⌈ x |-> v1, ρ ⌉ ∩ (⌈ x |-> v2, ρ ⌉ \ ⟅ x ⟆)) = ⌈ x |-> v1, ρ ⌉.
Admitted.

(* ------------------------------------------- *)

Module ThinExamples.

Import Thin_DSLS.


Definition onlyOne k : 
  E (mini.ES (common.Int k)) = 
    ρ ⭅ Total_set ;; ⌈ [ ⌈ r |-> Int k, ρ ⌉ ] ⌉.
cbn. unfold SIMPLE. 
f_equal. 
Qed.

(* This is an issue: we would like there to be some connection between 
   ρ1 and ρ2. But the only way to do that would be to switch to 
   env -> P (list VAL) instead of P (list ENV).
*)
Definition oneOrTwo : 
   E (1 :|: 2) =
     ρ1 ⭅ Total_set ;;
     ρ2 ⭅ Total_set ;;
     ⌈ [ ⌈ r |-> Int 1, ρ1 ⌉ ; ⌈ r |-> Int 2, ρ2 ⌉ ] ⌉.
Proof.
  cbn. unfold SIMPLE.
  set_simpl.
  f_equal.
  extensionality ρ1.
  set_simpl.
  rewrite <- bind_singleton_map. 
  f_equal.
  extensionality ρ2.
  set_simpl.
  reflexivity.
Qed.


(* ------------------------------------------- *)
(* This is part of 

   for{x:nat; t:=(x,x+1); t[i:nat]=i*2; t}   # denotes ((0,1),(1,2))

*)

(* t = (x,y) ; y = x+1  *)
Definition example0 : mini.Expr := 
  t :=: mini.ES (SArray [Var x ; Var y]) :>: 
  (y :=: (EPrim common.Add) :@: (common.Var x)).

(* t i = i * 2 *)
Definition example1 : mini.Expr := 
  ((Var t :@: Var i) :=: EPrim common.TimesTwo :@: Var i).


Lemma eval_example0 h : 
  E example0 = 
    ρ1 ⭅ (Total_set : ENV) ;;
    ρ2 ⭅ (Total_set : ENV) ;;
    ρ3 ⭅ (Total_set : ENV) ;;
    h .
Proof. 
  unfold example0. cbn. unfold SIMPLE.
  set_simpl.
  f_equal.
  extensionality ρ1.
  set_simpl.
  f_equal.
  extensionality ρ2.
  set_simpl.
  f_equal.
  extensionality ρ3.
  set_simpl.
  replace (Scope.union Scope.empty Scope.empty) with Scope.empty. 
  cbn. set_simpl.
  eapply bind_set. 
  instantiate (1 := [ ⌈(r |-> Int 1)⌉ ] ).
  { unfold APP. cbn.
    rewrite in_bind. exists (r |-> Int 1). split. cbv. auto.
    cbv. eapply in_singleton'. f_equal. f_equal.
    extensionality y. destruct y. done. done. } 
  intros Ds hDs.
  unfold APP in hDs.
  rewrite in_bind in hDs. set_crunch.
  rename x0 into ρ4.
  list_simpl.
  set_simpl.
  Opaque apply.
  unfold id.
  list_simpl.
Admitted.

(* ---------------------------------------------------------- *)

(* 
  all{∃ x. if(x=0){1|2}else{3|4}}  
  # currently <1,2,3,4>    (strange!)
  # lennart wants { [<1,2>], [<3,4>] }
*)

Definition test2 :=
 mini.DefineV x :>:
   mini.If3 (x :=: 0) (1 :|: 2) (3:|:4).

Lemma test2_semantics : 
  E test2 = 
    (fun Ds => (Ds = [∅; ∅]) \/ 
            (exists ρ, ρ x = Int 0 /\ Ds = [ ⌈ r |-> Int 1, ρ ⌉ ; ⌈ r |-> Int 2, ρ ⌉ ])) ∪
    (fun Ds => exists ρ, ρ x <> Int 0 /\ Ds = [ ⌈ r |-> Int 3, ρ ⌉ ; ⌈ r |-> Int 4, ρ ⌉ ]).
cbn.
unfold IF, SIMPLE.
set_simpl.
eapply bind_envs. intros ρ1.
set_simpl.
eapply bind_envs. intros ρ2.
set_simpl.
eapply bind_envs. intros ρ3.
set_simpl.
eapply bind_envs. intros ρ4.
set_simpl.
eapply bind_envs. intros ρ5.
set_simpl.
eapply bind_envs. intros ρ6.
set_simpl.
cbn.
set_simpl.
replace (Scope.union Scope.empty Scope.empty) with Scope.empty.
replace (Scope.union ⟅ x ⟆ Scope.empty) with ⟅x⟆.
f_equal.
- (* x==0 case: h1 *)
 set_simpl.
 set_ext Δs.
 rewrite in_singleton_iff.
 unfold Ensembles.In.
 destruct Δs as [|D1 Ds].
 { split; intro h. inv h. inv h. inv H. set_crunch. inv H0. } 
 destruct Ds as [|D2 Ds].
 { split; intro h; inv h. inv H. set_crunch. inv H0. } 
 destruct Ds as [|D3 Ds].
 2: { split; intro h; inv h. inv H. set_crunch. inv H0. } 

 have REQ: (ρ1 = ρ2) \/ (ρ1 <> ρ2). admit.
 destruct REQ.
 2: { 
    rewrite intersect_single_diff_tail. done.
    set_simpl. 
    split. intro h. left. done.
    intro h. inv h. done. set_crunch.
    admit.
 } 
Admitted.


End ThinExamples.
    
