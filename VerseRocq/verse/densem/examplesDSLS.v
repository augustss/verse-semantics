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


(*** Examples of DSLS *)

Import DSLS.

(* Examples of the semantics. *)


(* (2,3)[x] =  [ {{x=0,r=2}} ; {{x=1}}; {{r=3}} ] *)
Example pair_app := mini.ApplyD (common.SArray [common.Lit 2; common.Lit 3]) 
                       (common.Var x).
Lemma example_pair_app :
  APP (fun _ => mkTup [Int 2;Int 3]) (fun rho => rho x) = 
    ⌈ [ {{ x ≈ 0 }} ∩ {{ r ≈ 2}}  ; {{ x ≈ 1 }} ∩ {{ r ≈ 3 }} ] ⌉.
Proof.
  unfold APP.
  eapply bind_Total_set. constructor. exact Env.empty.
  intros _.
  cbn.
  f_equal.
  f_equal.
  + 
  set_ext ρ2.
  rewrite in_bind.
  split.
  - intro h. set_crunch. rename x0 into ρ.
    destruct (Value.eqb (ρ x) (Int 0)) eqn:h0.
    inv H0. inv H2. inv H1.
    econstructor.
    unfold constrain_eq. unfold Ensembles.In.
    rewrite Value.eqb_eq in h0. cbn. rewrite H3. done.
    unfold Ensembles.In. unfold constrain_eq. done.
    inv H0.
  - intro h. set_crunch. 
    exists ρ2. split. cbv; auto.
    have R: Value.eqb (ρ2 x) (ρ2 x)= true. {
      rewrite Value.eqb_eq. done. } 
    rewrite R. 
    split. 
    unfold Ensembles.In, constrain_eq. done.
    unfold Ensembles.In. split; auto.
 + 
  f_equal.
  set_ext ρ2.
  rewrite in_bind.
  split.
  - intro h. set_crunch. rename x0 into ρ.
    destruct (Value.eqb (ρ x) (Int 1)) eqn:h0.
    rewrite Value.eqb_eq in h0.
    inv H0. inv H2. inv H1.
    econstructor.
    unfold constrain_eq, Ensembles.In. cbn.
    congruence.
    unfold constrain_eq, Ensembles.In. cbn.
    done.
    inv H0.
  - intro h. set_crunch. 
    exists ρ2. split. cbv; auto.
    have R: Value.eqb (ρ2 x) (ρ2 x)= true. {
      rewrite Value.eqb_eq. done. } 
    rewrite R. 
    split. 
    unfold constrain_eq, Ensembles.In.
    congruence.
    unfold constrain_eq, Ensembles.In. done.
Qed. 

(* 1..2 is the same as 1|2 *)
Lemma iter_choice : 
  E (mini.Iter 1 2) = E (1 :|: 2).
Proof.
  cbn. unfold ITER, SIMPLE.
  eapply bind_Total_set. constructor. exact Env.empty. intros _.
  cbn.
  set_simpl. cbn. 
  replace (fun _ => Int 1 = Int 1 /\ Int 2 = Int 2) with envs.
  set_simpl.
  done.
  { set_ext ρ. unfold Ensembles.In, envs, Total_set.  tauto. } 
Qed.

Lemma lookup_constraint (r x : Ident) (k: nat) :
  ({{ r ≈ x }} ∩ {{ r ≈ k }}) = {{ x ≈ k }} ∩ {{ r ≈ k }}.
Admitted.

(* if (x=0) (1|2) (3|4) is a set containing two lists:
   { [ {{x=0,r=1}} ; {{x=0;r=2}} ] , [ {{x<>0,r=3}} ; {{x<>0;r=4}} ] }

 *) 
Lemma if_example :
  E (mini.If3 (x:=: 0)(1:|:2)(3:|:4)) = 
  ⌈ [ {{x ≈ 0}} ∩ {{r ≈ 1}} ; {{x ≈ 0}} ∩ {{r ≈ 2}} ] ⌉ ∪
  ⌈ [ {{x ≉ 0}} ∩ {{r ≈ 3}} ; {{x ≉ 0}} ∩ {{r ≈ 4}} ] ⌉.
Proof.
  cbn.
  unfold IF, SIMPLE.
  set_simpl.
  replace (Scope.union Scope.empty Scope.empty) with Scope.empty.
  cbn.
  f_equal.
  - f_equal.
    set_simpl. unfold id.
    f_equal.
    set_ext ρ.
    split.
    unfold constrain_eq.
    + cbn. intro h. set_crunch. 
      split. unfold Ensembles.In.
      rewrite H0. rewrite Scope.singleton_spec. done.
      rewrite H3. done.
      unfold Ensembles.In. done.
    + intro h. set_crunch.
      econstructor.
      unfold Ensembles.In. 
      unfold constrain_eq, hide.
      exists (r |-> Int 0, ρ).
      split. split; unfold Ensembles.In.
      done. cbn. congruence.
      intros x xI. rewrite Scope.singleton_spec in xI. 
      rewrite extend_lookup_diff; auto.
      rewrite PeanoNat.Nat.eqb_neq. auto.
      unfold Ensembles.In. 
      unfold constrain_eq, hide. done.
    + rewrite lookup_constraint.
      rewrite constrain_eq_hide_two. done.
      done.
  - f_equal.
    set_simpl.
    rewrite lookup_constraint.
    rewrite constrain_eq_hide_two. done.
Admitted.


Lemma if_example_2 :
  E ((mini.If3 (x:=: 0)(1:|:2)(3:|:4)) :|: 5) = 
     ⌈ [ {{x ≈ 0}} ∩ {{r ≈ 1}} ; 
         {{x ≈ 0}} ∩ {{r ≈ 2}} ; 
         {{r ≈ 5}} ] ⌉ ∪
     ⌈ [ {{x ≉ 0}} ∩ {{r ≈ 3}} ; 
         {{x ≉ 0}} ∩ {{r ≈ 4}} ; 
         {{r ≈ 5}}] ⌉.
Proof.
  rewrite E_Choice.
  unfold B.
  rewrite if_example.
  set_simpl. unfold mini.I.
  set_simpl.
  unfold E, SIMPLE.
  set_simpl. unfold id.
  cbn.
  f_equal.
Qed.

(*
   (if x = 1 then 11| 22 else 33); 
    x = (1 | 2); 
    (if x = 1 then 44 else 55 | 66)
*)

Lemma if_example_3A :
  E (mini.If3 (x :=: 1) (11 :|: 22) 33) = 
   ⌈ [ ({{x ≈ 1}} ∩ {{r ≈ 11}}) ; 
       ({{x ≈ 1}} ∩ {{r ≈ 22}}) ] ⌉ ∪
   ⌈ [ ({{x ≉ 1}} ∩ {{r ≈ 33}}) ] ⌉.
Admitted.
Lemma if_example_3B :
  E (x :=: (1 :|: 2)) = 
   ⌈ [ ({{x ≈ 1}} ∩ {{r ≈ 1}}) ; 
        {{x ≈ 2}} ∩ {{r ≈ 2}} ] ⌉. 
Admitted.
Lemma if_example_3C :
  E (mini.If3 (x :=: 1) 44 (55 :|: 66)) = 
     ⌈ [ ({{x ≈ 1}} ∩ {{r ≈ 44}}) ] ⌉ ∪ 
     ⌈ [ ({{x ≉ 1}} ∩ {{r ≈ 55}}) ;
         ({{x ≉ 1}} ∩ {{r ≈ 66}}) ] ⌉.
Admitted.

Definition if_example_3 := 
  (mini.If3 (x :=: 1) (11 :|: 22) 33) :>:
  (x :=: (1 :|: 2)) :>:
  (mini.If3 (x :=: 1) 44 (55 :|: 66)).

Lemma if_example_3_meaning_AB : 
  E ((mini.If3 (x :=: 1) (11 :|: 22) 33) :>:
    (x :=: (1 :|: 2))) = 
  (⌈ [{{x ≈ 1 }} ∩ {{r ≈ 1 }};  ∅ ; 
      {{ x ≈ 1 }} ∩ {{ r ≈ 1 }};  ∅ ] ⌉
   ∪
   ⌈ [∅ ; {{ x ≈ 2 }} ∩ {{ r ≈ 2 }} ] ⌉ ).
Proof.
  rewrite E_Seq.
  rewrite if_example_3A.
  rewrite E_Unify.
  unfold E, SIMPLE, evalA.
  set_simpl.
  unfold hide_list.
  list_simpl.
  rewrite constrain_eq_hide_two. done.
  rewrite constrain_eq_hide_two. done.
  cbn.
Admitted.

Lemma if_example_3_meaning : 
  E (((mini.If3 (x :=: 1) (11 :|: 22) 33) :>:
    (x :=: (1 :|: 2))) :>:
    (mini.If3 (x :=: 1) 44 (55 :|: 66))) = 
  (⌈ [{{x ≈ 1}} ∩ {{r ≈ 44}};  ∅ ; 
      {{x ≈ 1}} ∩ {{r ≈ 44}};  ∅ ] ⌉
   ∪ ⌈ [∅; ∅; ∅; ∅; ∅; ∅; ∅; ∅] ⌉
   ∪ ⌈ [∅; ∅] ⌉ 
   ∪ ⌈ [∅ ; ∅ ; {{x ≈ 2}} ∩ {{r ≈ 55}} ; 
                {{x ≈ 2}} ∩ {{r ≈ 66}} ] ⌉ ).
Proof.
  rewrite E_Seq.
  rewrite if_example_3_meaning_AB.
  rewrite if_example_3C.
  set_simpl.
  unfold hide_list. list_simpl.
  set_simpl.
  rewrite constrain_eq_hide_two. done.
  rewrite constrain_eq_hide_two. done.
  cbn.
  set_simpl.
  f_equal.
  f_equal.
  f_equal.
  rewrite <- intersection_assoc.
  rewrite constrain_eq_same. done.
  f_equal.
  rewrite <- intersection_assoc.
  rewrite constrain_eq_same. done.
  rewrite <- intersection_assoc.
Admitted.



(* ------------------------------------------------------- *)

(* NB: This version of Iter DOESN't work. It includes partial runs that 
   don't correspond to any evaluation *)
(*
  k ⭅ (Total_set : P nat) ;;
  ⌈ let ks := enumFrom 0 k in 
    List.map (fun k => (r ≈ ⟨Int k⟩)
                      ∩ (fun ρ => Value.leb (v1 ρ) (Int k) = true) 
                      ∩ (fun ρ => Value.leb (Int k) (v2 ρ) = true)) ks ⌉.
*)

(* Slightly thicker application. This is NOT what we want.
   For (2,3)[i] gives us  
   { [r=2] | rho } ∪ { [(rho,i=1,r=3)] | rho } 

  { [{{r=2}}] } ∪ { [{{i=1,r=3}}] } 

 *)

(* apply domain function value f to x 
   f is a list of partial functions, corresponding to 
   iteration on the input. Any inputs that are in the 
   domain of the function will produce output.
 *)
Definition apply (f : value) (v : value) : list value := 
  match f with 
  | Dom.Fun hs => 
      h <- hs ;;
      match (PFun.apply_opt _ _ Value.eqb h v) with 
      | Some w => [w]
      | None => []
      end
  | _ => []
  end.


Definition APP' (v1 : env -> value) (v2 : env -> value) : P (list ENV) :=
   ρ ⭅ envs ;;
   let vs := apply (v1 ρ) (v2 ρ) in
   ⌈ List.map (fun v => (fun ρ => ρ r = v)) vs ⌉.

(*  (2,3)[x] == 2|3 *)
Lemma example_pair_app' :
  APP' (fun _ => mkTup [Int 2;Int 3]) (fun rho => rho x) = 
    ρ ⭅ envs ;;
    if (Value.eqb (ρ x) (Int 0)) then
    ⌈ [ {{r ≈ 2}} ] ⌉ 
    else if (Value.eqb (ρ x) (Int 1)) then
    ⌈ [ {{r ≈ 3}} ] ⌉ else ⌈[]⌉.
Proof.
  unfold APP'.
  f_equal.  extensionality ρ. 
  cbn.
  destruct (Value.eqb (ρ x) (Int 0)) eqn:h0.
  + cbn.
    have h1: (Value.eqb (ρ x) (Int 1) = false). admit.
    rewrite h1. cbn. done.
  + destruct (Value.eqb (ρ x) (Int 1)) eqn:h1.
    cbn. done.
  cbn. done.
Admitted. 


(*** Thin Semantics examples. *)

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
  (y :=: (EPrim common.AddOne) :@: (common.Var x)).

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



Example pair_app := mini.ApplyD (common.SArray [common.Lit 2; common.Lit 3]) 
                       (common.Var x).
Lemma example_pair_app :
  APP (fun _ => mkTup [Int 2;Int 3]) (fun rho => rho x) = 
    (ρ ⭅ envs ;; 
     if Value.eqb (ρ x) (Int 0) then 
     ⌈ [ ⌈ (r |-> Int 2, ρ) ⌉ ] ⌉
     else if Value.eqb (ρ x) (Int 1) then
     ⌈ [ ⌈ (r |-> Int 3, ρ) ⌉ ] ⌉
     else ⌈ [] ⌉).
Proof.    
  unfold APP.
  f_equal. extensionality ρ.
  cbn.
  destruct (Value.eqb (ρ x) (Int 0)) eqn:h0.
  + have h1: (Value.eqb (ρ x) (Int 1) = false). admit.
Admitted.



End ThinExamples.
    
