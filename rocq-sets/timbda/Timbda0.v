(* Timbda0.v: A set-theoretic semantics *)

From Stdlib Require Import Morphisms.
From Stdlib Require Import ssreflect.
From Stdlib Require Import FunctionalExtensionality.

Require Import Axioms.
Require Import Cartesian.
Require Import Omega.

Require Import ZFSet.
Require Import ZFNotation.
Require Import Diagonal.

Require Import Syntax.

Require Import utils.all.
From smpl Require Export Smpl.

(*** The type universe ***)

(* The partial *identity* functions on [A]: the subsets of the diagonal
   {⟨a,a⟩ : a ∈ A}. *)
Definition pIdFun (A : ZFSet) : ZFSet :=
  Power ( a ← A ;; {| ⟨ a , a ⟩ |} ).

(* [smallId] (the *small* partial identities — the predicative type
   universe) is defined in [lib.Diagonal]. *)

(* [is_type]: the identity relation on the small partial identities — the
   universe of types in the [Timbda0] model. *)
Definition is_type : ZFSet :=
  f ← smallId ;; {| ⟨ f , f ⟩ |}.

(*** Evaluator ***)


Fixpoint eval (e : Expr) (rho : Env) {struct e} : ZFSet :=
  match e with
  | Econ n        => {| n |}
  | Evar i        => {| rho i |}
  | Enat          => {| natId |}
  | Eany          => Big
  | Etype         => {| is_type |}
  | Elam e1 e2    => Π[ a ∈ eval e1 rho ] eval e2 (env_ext rho a)
  | Eapp e1 e2    =>
       f ← eval e1 rho ;;
       v ← eval e2 rho ;;
        image f {| v |}
  | Efail         => ∅
  | Eimg e        => f ← eval e rho ;; rng f 
  | Echoice e1 e2 => eval e1 rho ∪ eval e2 rho
  | Eequal e1 e2  => eval e1 rho ∩ eval e2 rho
  | Eadd e1 e2 =>
       v1 ← eval e1 rho ;;
       v2 ← eval e2 rho ;;
        {| v1 + v2 |} 
  | Efix e =>
       f ← eval e rho ;;
       ⦃ a ∈ dom f | ⟨ a , a ⟩ ∈ f ⦄
  | _ => ∅
  end.

(*** Equation lemmas ***)

Theorem eval_con :
  forall (n : nat) (rho : Env), eval (Econ n) rho = {| n |}.
Proof. intros; reflexivity. Qed.

Theorem eval_var :
  forall (i : nat) (rho : Env), eval (Evar i) rho = {| rho i |}.
Proof. intros; reflexivity. Qed.

Theorem eval_type : forall rho : Env, eval Etype rho = {| is_type |}.
Proof. intros; reflexivity. Qed.

(* The type universe: [Eimg Etype] is the set [smallId] of small partial
   identities (the types). *)
Theorem eval_img_type : forall rho : Env, eval (Eimg Etype) rho = smallId.
Proof.
  intros rho. cbn [eval]. rewrite iUnion_Sing_l.
  unfold is_type. apply set_ext.
  - apply Inc_def. intros y Hy. apply rng_elim in Hy. destruct Hy as [x Hxy].
    apply iUnion_IN in Hxy. destruct Hxy as [f [Hf Hxy]].
    apply IN_Sing_EQ in Hxy.
    pose proof (Couple_inj_right _ _ _ _ Hxy) as E. rewrite E. exact Hf.
  - apply Inc_def. intros f Hf. apply (rng_intro _ f f).
    apply IN_iUnion with (y := f); [ exact Hf | apply IN_Sing ].
Qed.

Theorem eval_nat : forall rho : Env, eval Enat rho = {| natId |}.
Proof. intros; reflexivity. Qed.

(* [Eany], the universe, denotes the inaccessible "Big" set: the
   Grothendieck-universe-like set of all small sets (see [ZFSet.Big] /
   [ZFC.Hierarchy]). It is transitive and closed under power sets. *)
Theorem eval_any : forall rho : Env, eval Eany rho = Big.
Proof. intros; reflexivity. Qed.

Theorem eval_lam :
  forall (e1 e2 : Expr) (rho : Env),
  eval (Elam e1 e2) rho =
    Pi (eval e1 rho) (fun a => eval e2 (env_ext rho a)) .
Proof. intros; reflexivity. Qed.

Theorem eval_app :
  forall (e1 e2 : Expr) (rho : Env),
  eval (Eapp e1 e2) rho
  = f ← eval e1 rho ;; v ← eval e2 rho ;; image f {| v |}.
Proof. intros; reflexivity. Qed.


Theorem eval_fail : forall rho : Env, eval Efail rho = ∅.
Proof. intros; reflexivity. Qed.

Theorem eval_img :
  forall (e : Expr) (rho : Env),
  eval (Eimg e) rho = f ← eval e rho ;; (rng f).
Proof. intros; reflexivity. Qed.

Theorem eval_choice :
  forall (e1 e2 : Expr) (rho : Env),
  eval (Echoice e1 e2) rho = eval e1 rho ∪ eval e2 rho.
Proof. intros; reflexivity. Qed.

Theorem eval_equal :
  forall (e1 e2 : Expr) (rho : Env),
  eval (Eequal e1 e2) rho = eval e1 rho ∩ eval e2 rho.
Proof. intros; reflexivity. Qed.

Theorem eval_add :
  forall (e1 e2 : Expr) (rho : Env),
  eval (Eadd e1 e2) rho
  = v1 ← eval e1 rho ;; v2 ← eval e2 rho ;; {| v1 + v2 |}.
Proof. intros; reflexivity. Qed.

Theorem eval_fix :
  forall (e : Expr) (rho : Env),
  eval (Efix e) rho
  = f ← eval e rho ;; ⦃ a ∈ dom f | ⟨ a , a ⟩ ∈ f ⦄.
Proof. intros; reflexivity. Qed.


(** ** Stepping lemmas.

    [step_eval] computes [eval] of concrete applications/projections.
    Under the relational semantics [Eapp e1 e2] reduces to [image f {|v|}]
    and [Eimg e] to [rng f], so evaluating a closed expression bottoms out
    in the image/range of a concrete graph.  The pure [ZFSet] reductions
    used for this — [image_Vide_l], [image_Sing_Couple], [image_BinUnion],
    [rng_Sing_Couple] — are general and live in [ZFSet.v]. *)


(** [step_eval] unfolds [eval] one layer and normalises the resulting
    denotation with the shared [zf_reduce] engine (defined in
    [ZFSetFacts.v]); the same engine drives [Timbda1.step1] and
    [Timbda2.step3]. *)
Ltac step_eval :=
  intros; cbn [eval]; zf_reduce; try reflexivity.

Lemma eval_tnat ρ : eval (Eimg Enat) ρ = Omega.
Proof. step_eval. Qed.


(** Tests **)

(* 1 + 2 = 3:  eval (Eadd (Econ 1) (Econ 2)) ∅ = {| 3 |}. *)
Example test1 :
  eval (Eadd (Econ 1) (Econ 2)) (fun _ : nat => ∅) = {| 3 |}.
Proof. step_eval. Qed.

(* var lookup against an extended environment. *)
Example test_var_ext :
  eval (Evar 0) (env_ext (fun _ : nat => ∅) 7) = {|  7 |}.
Proof. step_eval. Qed.

(* Echoice of two literals as set-union of singletons. *)
Example test_choice_lit :
  forall rho : Env,
  eval (Echoice (Econ 1) (Econ 2)) rho = {| 1 |} ∪ {| 2 |}.
Proof. step_eval. Qed.

(* Application of [Efail]: with nothing to apply the outer [iUnion]
   ranges over [∅] and the whole expression is [∅]. *)
Example test_app_fail :
  forall rho : Env,
  eval (Eapp Efail (Econ 5)) rho = ∅.
Proof. step_eval. Qed.

(* Applying a lambda with an *empty domain* to [Econ 3].  [Efail] is the
   domain, denoting [∅]; the product over the empty domain [Π_{a ∈ ∅} ∅]
   is the singleton of the (unique) empty function [{∅}].  Under the
   relational [Eapp], applying the empty function to [3] is the image
   [image ∅ {3} = ∅] (no edges), so the whole expression denotes [∅]. *)
Example eval_app_empty_dom (rho : Env) :
  eval (Eapp (Elam Efail Efail) (Econ 3)) rho = ∅.
Proof.
  (* the product over the empty domain is the singleton empty function *)
  assert (HPi : forall B : ZFSet -> ZFSet, Pi Empty B = {| Empty |}).
  { intro B. apply set_ext.
    - apply Inc_def. intros f Hf. unfold Pi in Hf.
      pose proof (Inc_IN _ _ _ (Comp_Inc _ _) Hf) as HfP.
      apply IN_Power_Inc in HfP.
      assert (Ef : f = Empty).
      { apply set_ext; apply Inc_def; intros p Hp.
        - apply (Inc_IN _ _ _ HfP) in Hp.
          apply IN_Prod_EX in Hp. destruct Hp as [a [b [Ha _]]].
          destruct (not_In_Empty _ Ha).
        - destruct (not_In_Empty _ Hp). }
      subst f. apply IN_Sing.
    - apply Sing_Inc_IN. unfold Pi. apply In_P_Comp.
      + apply Inc_IN_Power, Inc_def. intros x Hx. destruct (not_In_Empty _ Hx).
      + split.
        * intros a Ha. destruct (not_In_Empty _ Ha).
        * intros a b1 b2 H1 _. destruct (not_In_Empty _ H1). }
  rewrite eval_app. rewrite eval_con. rewrite eval_lam. rewrite (eval_fail rho).
  rewrite HPi. rewrite !iUnion_Sing_l.
  (* image of the empty function: no edges, so the image is empty *)
  apply set_ext; apply Inc_def; intros x Hx.
  - apply image_elim in Hx. destruct Hx as [a [_ Hab]]. destruct (not_In_Empty _ Hab).
  - destruct (not_In_Empty _ Hx).
Qed.

Example test_Pi :
  forall rho : Env,  eval (Elam (Econ 1) (Econ 1)) rho = {| {| ⟨ 1 , 1 ⟩ |} |}.
Proof.
  intros rho.
  step_eval.
  apply set_ext.
  + apply Pi_Sing_Sing_Inc.
  + apply Sing_Inc. apply Sing_Couple_mem_Pi. apply IN_Sing.
Qed.


Example test_nested_add :
  eval (Eadd (Econ 1) (Eadd (Econ 2) (Econ 3))) (fun _ : nat => ∅)
  = {| 6 |}.
Proof.
  step_eval.
Qed.


(** Application to [Enat]: [eval (Eapp Enat (Econ n)) rho = 
    {| natZ n |}].

  In this semantics [Enat] evaluates to [{| natId |}] (the singleton
  containing the identity relation on ω); applying that identity
  relation to [{| natZ n |}] returns [{| natZ n |}]. The Lean proof
  factors out a helper [pair_self_mem_natId] (the diagonal pair
  [⟨ natZ n , natZ n ⟩] lives in [natId]); we mirror it here. ***)


Example test_app_nat :
  forall (n : nat) (rho : Env),
  eval (Eapp Enat (Econ n)) rho = {| n |}.
Proof.
  step_eval.
  (* relational application reduces the goal to
     [image natId {| natZ n |} = {| n |}] *)
  apply set_ext.
  - apply image_Sing_Inc. intros b Hab.
    apply natId_pair_diagonal in Hab. subst b. apply IN_Sing.
  - apply Sing_Inc. apply image_intro with (a := natZ n).
    + apply IN_Sing.
    + apply pair_self_mem_natId.
Qed.


(** ** Polymorphism via [Eany].

    A function whose domain is the universe [Eany] is *polymorphic*: it
    accepts any value.  Since [eval Eany ρ = Big], its denotation is the
    dependent product over the inaccessible set [Big] — the
    Grothendieck-universe-like set of all "small" sets.  Applying such a
    function to any element [v ∈ Big] (a small set) computes as expected,
    via [applyFun_mem_of_pi]: the result must land in the codomain fibre
    at [v], which pins it down exactly. *)

(* The polymorphic identity [fun (x : Any) => x] denotes [Π_{a ∈ Big} {a}]. *)
Example eval_poly_id (rho : Env) :
  eval (Elam Eany (Evar 0)) rho = Pi Big (fun a => {| a |}).
Proof. reflexivity. Qed.

(* Applying the polymorphic identity to any element [v] of the universe
   returns [v].  The argument is a variable bound to [v] in the
   environment; [v ∈ Big] says [v] is a member of the universe. *)
Example poly_id_app (rho : Env) (v : ZFSet) :
  In v Big ->
  eval (Eapp (Elam Eany (Evar 0)) (Evar 0)) (env_ext rho v) = {| v |}.
Proof.
  intro Hv.
  (* the function value lives in [Π_{a ∈ Big} {a}]; its relational image
     at [v ∈ Big] is [{ f v } = {v}]. *)
  assert (Himg : forall f, In f (Pi Big (fun a => {| a |})) -> image f {| v |} = {| v |}).
  { intros f Hf.
    rewrite (image_Sing_of_pi _ _ _ _ Hf Hv).
    pose proof (applyFun_mem_of_pi Big (fun a => {| a |}) f v Hf Hv) as Hm.
    apply IN_Sing_EQ in Hm. rewrite Hm. reflexivity. }
  (* and that product is inhabited: the universe identity graph is in it. *)
  assert (Hid : In (iUnion Big (fun a => Sing (Couple a a)))
                   (Pi Big (fun a => {| a |}))).
  { apply (iUnion_graph_mem_pi Big (fun a => {| a |}) (fun a => a)).
    intros a Ha. apply IN_Sing. }
  rewrite eval_app.
  change (eval (Elam Eany (Evar 0)) (env_ext rho v))
    with (Pi Big (fun a => {| a |})).
  change (eval (Evar 0) (env_ext rho v)) with ({| v |}).
  apply set_ext.
  - apply iUnion_Inc. intros f Hf.
    apply iUnion_Inc. intros w Hw.
    apply IN_Sing_EQ in Hw. subst w.
    rewrite (Himg f Hf). apply Inc_refl.
  - apply Sing_Inc_IN.
    apply IN_iUnion with (y := iUnion Big (fun a => Sing (Couple a a))).
    + exact Hid.
    + apply IN_iUnion with (y := v).
      * apply IN_Sing.
      * rewrite (Himg _ Hid). apply IN_Sing.
Qed.

(* A polymorphic *constant* function [fun (x : Any) => 5] applied to any
   universe element returns [5], independent of the argument. *)
Example poly_const_app (rho : Env) (v : ZFSet) :
  In v Big ->
  eval (Eapp (Elam Eany (Econ 5)) (Evar 0)) (env_ext rho v) = {| 5 |}.
Proof.
  intro Hv.
  assert (Himg : forall f, In f (Pi Big (fun _ => {| natZ 5 |})) -> image f {| v |} = {| natZ 5 |}).
  { intros f Hf.
    rewrite (image_Sing_of_pi _ _ _ _ Hf Hv).
    pose proof (applyFun_mem_of_pi Big (fun _ => {| natZ 5 |}) f v Hf Hv) as Hm.
    apply IN_Sing_EQ in Hm. rewrite Hm. reflexivity. }
  assert (Hid : In (iUnion Big (fun a => Sing (Couple a (natZ 5))))
                   (Pi Big (fun _ => {| natZ 5 |}))).
  { apply (iUnion_graph_mem_pi Big (fun _ => {| natZ 5 |}) (fun _ => natZ 5)).
    intros a Ha. apply IN_Sing. }
  rewrite eval_app.
  change (eval (Elam Eany (Econ 5)) (env_ext rho v))
    with (Pi Big (fun _ => {| natZ 5 |})).
  change (eval (Evar 0) (env_ext rho v)) with ({| v |}).
  apply set_ext.
  - apply iUnion_Inc. intros f Hf.
    apply iUnion_Inc. intros w Hw.
    apply IN_Sing_EQ in Hw. subst w.
    rewrite (Himg f Hf). apply Inc_refl.
  - apply Sing_Inc_IN.
    apply IN_iUnion with (y := iUnion Big (fun a => Sing (Couple a (natZ 5)))).
    + exact Hid.
    + apply IN_iUnion with (y := v).
      * apply IN_Sing.
      * rewrite (Himg _ Hid). apply IN_Sing.
Qed.

(* Applying the polymorphic identity to a *constant natural* [Econ k].
   The argument denotes [{| natZ k |}], and the identity returns it, so
   the application denotes [{| k |}].  Unconditional: every natural is a
   small set, i.e. a member of the universe [Big] ([natZ_small]). *)
Example poly_id_app_nat (rho : Env) (k : nat) :
  eval (Eapp (Elam Eany (Evar 0)) (Econ k)) rho = {| k |}.
Proof.
  pose proof (natZ_small k) as Hk.
  assert (Himg : forall f, In f (Pi Big (fun a => {| a |})) ->
                           image f {| natZ k |} = {| natZ k |}).
  { intros f Hf.
    rewrite (image_Sing_of_pi _ _ _ _ Hf Hk).
    pose proof (applyFun_mem_of_pi Big (fun a => {| a |}) f (natZ k) Hf Hk) as Hm.
    apply IN_Sing_EQ in Hm. rewrite Hm. reflexivity. }
  assert (Hid : In (iUnion Big (fun a => Sing (Couple a a)))
                   (Pi Big (fun a => {| a |}))).
  { apply (iUnion_graph_mem_pi Big (fun a => {| a |}) (fun a => a)).
    intros a Ha. apply IN_Sing. }
  rewrite eval_app.
  change (eval (Elam Eany (Evar 0)) rho) with (Pi Big (fun a => {| a |})).
  change (eval (Econ k) rho) with ({| natZ k |}).
  apply set_ext.
  - apply iUnion_Inc. intros f Hf.
    apply iUnion_Inc. intros w Hw.
    apply IN_Sing_EQ in Hw. subst w.
    rewrite (Himg f Hf). apply Inc_refl.
  - apply Sing_Inc_IN.
    apply IN_iUnion with (y := iUnion Big (fun a => Sing (Couple a a))).
    + exact Hid.
    + apply IN_iUnion with (y := natZ k).
      * apply IN_Sing.
      * rewrite (Himg _ Hid). apply IN_Sing.
Qed.

(* Applying the polymorphic identity to the *type* [Eimg Enat], which
   denotes the whole set [ω] of naturals.  The identity maps every
   element of the universe to itself, and every natural lives in the
   universe ([natZ_small]), so the image of the entire type is the type:
   the result is [ω]. *)
Example poly_id_app_nat_type (rho : Env) :
  eval (Eapp (Elam Eany (Evar 0)) (Eimg Enat)) rho = Omega.
Proof.
  (* every element of [ω] is a member of the universe [Big] *)
  assert (HBig : forall v, In v Omega -> In v Big).
  { intros v Hv. destruct (In_omega v Hv) as [n Hn]. rewrite <- Hn. apply natZ_small. }
  (* the universe identity graph inhabits the function product *)
  assert (Hid : In (iUnion Big (fun a => Sing (Couple a a)))
                   (Pi Big (fun a => {| a |}))).
  { apply (iUnion_graph_mem_pi Big (fun a => {| a |}) (fun a => a)).
    intros a Ha. apply IN_Sing. }
  rewrite eval_app. rewrite eval_tnat.
  change (eval (Elam Eany (Evar 0)) rho) with (Pi Big (fun a => {| a |})).
  apply set_ext.
  - (* each edge lands at a natural [w = v ∈ ω] *)
    apply Inc_def. intros w Hw.
    apply iUnion_IN in Hw. destruct Hw as [f [Hf Hw]].
    apply iUnion_IN in Hw. destruct Hw as [v [Hv Hw]].
    apply image_elim in Hw. destruct Hw as [a [Ha Hedge]].
    apply IN_Sing_EQ in Ha. subst a.
    pose proof (Pi_edge_codomain _ _ _ _ _ Hf (HBig v Hv) Hedge) as Hcod.
    apply IN_Sing_EQ in Hcod. subst w. exact Hv.
  - (* conversely, [w ∈ ω] is the identity image of itself *)
    apply Inc_def. intros w Hw.
    apply IN_iUnion with (y := iUnion Big (fun a => Sing (Couple a a))); [ exact Hid | ].
    apply IN_iUnion with (y := w); [ exact Hw | ].
    apply image_intro with w; [ apply IN_Sing | ].
    apply IN_iUnion with (y := w); [ exact (HBig w Hw) | apply IN_Sing ].
Qed.


(** * Evaluation of the [Syntax] example expressions.

    For each [Syntax] example built only from constructors that [Timbda0]
    interprets, we state its denotation and prove it.  Examples that use
    the uninterpreted constructors ([Elet] / [Eassign] / [Eseq] / [Ebind]
    / [Etype]) collapse to [∅] here and are noted but not belaboured. *)

(** A reusable relational-application fact: applying (the iterated graphs
    of) [Pi A B] to a single in-domain point [v0] whose codomain fibre is
    the singleton [{g v0}] yields [{g v0}].  This is the shape produced by
    [eval_app] when the operator denotes a [Pi] and the operand a
    singleton. *)
Lemma app_pi_sing :
  forall (A : ZFSet) (B g : ZFSet -> ZFSet) (v0 w : ZFSet),
  In v0 A ->
  (forall a, In a A -> In (g a) (B a)) ->
  B v0 = Sing w ->
  ( f ← Pi A B ;; v ← Sing v0 ;; image f (Sing v) ) = Sing w.
Proof.
  intros A B g v0 w Hv0 Hg HB.
  assert (Himg : forall f, In f (Pi A B) ->
                 (v ← Sing v0 ;; image f (Sing v)) = Sing w).
  { intros f Hf. rewrite iUnion_Sing_l.
    rewrite (image_Sing_of_pi A B f v0 Hf Hv0).
    pose proof (applyFun_mem_of_pi A B f v0 Hf Hv0) as Hm.
    rewrite HB in Hm. apply IN_Sing_EQ in Hm. rewrite Hm. reflexivity. }
  pose proof (iUnion_graph_mem_pi A B g Hg) as Hwit.
  apply set_ext.
  - apply iUnion_Inc. intros f Hf. rewrite (Himg f Hf). apply Inc_refl.
  - apply Sing_Inc_IN. eapply IN_iUnion.
    + exact Hwit.
    + rewrite (Himg _ Hwit). apply IN_Sing.
Qed.

(* Fixpoint unfolding.

   [eval (Efix e) env] is the set of *fixed points* {a : ⟨a,a⟩ ∈ f} of the
   value(s) [f] of [e]; [eval (Eapp e (Efix e)) env] applies [e] to those
   fixed points.  These coincide exactly when [e] denotes a single
   *function* [f]: each fixed point maps to itself, so applying [f] leaves
   the fixed-point set unchanged.  (Unconditionally the equality fails:
   for a relational [f = {⟨0,0⟩,⟨0,1⟩}], the fixed points are [{0}] but
   applying [f] gives [{0,1}].) *)

Lemma fixpoint_unfolding e env f :
  eval e env = {| f |} -> isFunction f ->
  eval (Efix e) env = eval (Eapp e (Efix e)) env.
Proof.
  intros He Hfun.
  rewrite eval_app !eval_fix !He !iUnion_Sing_l.
  apply set_ext; apply Inc_def; intros x Hx.
  - (* a fixed point [x] is its own [f]-image *)
    pose proof (In_Comp_P _ _ _ Hx) as Hxx.
    apply IN_iUnion with (y := x); [ exact Hx | ].
    apply (image_intro f {| x |} x x); [ apply IN_Sing | exact Hxx ].
  - (* the [f]-image of a fixed point [v] is [v] itself (single-valued) *)
    apply iUnion_IN in Hx. destruct Hx as [v [Hv Hx]].
    apply image_elim in Hx. destruct Hx as [a [Ha Hax]].
    apply IN_Sing_EQ in Ha. subst a.
    pose proof (In_Comp_P _ _ _ Hv) as Hvv.
    rewrite (Hfun v x v Hax Hvv). exact Hv.
Qed.

(* For a lambda [Elam e1 e2] the two side conditions are automatic once
   the body is deterministic: if every fibre [eval e2 (env_ext env a)]
   (for [a] in the domain) is a singleton, then [eval (Elam e1 e2) env] is
   a singleton [Pi]-graph — which is in particular a function. *)
Corollary fixpoint_unfolding_lam e1 e2 env :
  (forall a, a ∈ eval e1 env -> exists b, eval e2 (env_ext env a) = {| b |}) ->
  eval (Efix (Elam e1 e2)) env
  = eval (Eapp (Elam e1 e2) (Efix (Elam e1 e2))) env.
Proof.
  intro Hsing.
  destruct (Pi_Sing_ex (eval e1 env) (fun a => eval e2 (env_ext env a)) Hsing) as [f Hf].
  assert (Hev : eval (Elam e1 e2) env = {| f |}) by (rewrite eval_lam; exact Hf).
  assert (Hfun : isFunction f).
  { assert (Hfin : f ∈ eval (Elam e1 e2) env) by (rewrite Hev; apply IN_Sing).
    rewrite eval_lam in Hfin. apply In_Pi_inv in Hfin.
    destruct Hfin as [_ [_ Hfun]]. exact Hfun. }
  exact (fixpoint_unfolding (Elam e1 e2) env f Hev Hfun).
Qed.


(** ** Ground examples (constants, addition, unification, choice). *)

(* ex_EV1 = Econ 2 *)
Example ev_ex_EV1 rho : eval ex_EV1 rho = {| 2 |}.
Proof. reflexivity. Qed.

(* ex_Arith1 = 3+4 *)
Example ev_ex_Arith1 rho : eval ex_Arith1 rho = {| 7 |}.
Proof. unfold ex_Arith1. step_eval. Qed.

(* ex_multiline = 1+2+3 *)
Example ev_ex_multiline rho : eval ex_multiline rho = {| 6 |}.
Proof. unfold ex_multiline. step_eval. Qed.

(* ex_Unif16 = (1=1) *)
Example ev_ex_Unif16 rho : eval ex_Unif16 rho = {| 1 |}.
Proof. unfold ex_Unif16. cbn [eval]. apply BinInter_Sing_same. Qed.

(* ex_Unif = (1=2) — clashing unification denotes [:false] = ∅ *)
Example ev_ex_Unif rho : eval ex_Unif rho = ∅.
Proof. unfold ex_Unif. cbn [eval]. apply BinInter_Sing_diff, natZ_neq. discriminate. Qed.

(* ex_Cmp17 = (3=3) *)
Example ev_ex_Cmp17 rho : eval ex_Cmp17 rho = {| 3 |}.
Proof. unfold ex_Cmp17. cbn [eval]. apply BinInter_Sing_same. Qed.

(* ex_Cmp16 = (3=4) -> ∅ *)
Example ev_ex_Cmp16 rho : eval ex_Cmp16 rho = ∅.
Proof. unfold ex_Cmp16. cbn [eval]. apply BinInter_Sing_diff, natZ_neq. discriminate. Qed.

(* ex_Cmp18 = (3=2) -> ∅ *)
Example ev_ex_Cmp18 rho : eval ex_Cmp18 rho = ∅.
Proof. unfold ex_Cmp18. cbn [eval]. apply BinInter_Sing_diff, natZ_neq. discriminate. Qed.

(* ex_plus1 = (1+2 = 3) *)
Example ev_ex_plus1 rho : eval ex_plus1 rho = {| 3 |}.
Proof.
  unfold ex_plus1. cbn [eval]. rewrite !iUnion_Sing_l natZAdd_natZ.
  apply BinInter_Sing_same.
Qed.

(* ex_plus2 = (1+2 = 4) -> ∅ *)
Example ev_ex_plus2 rho : eval ex_plus2 rho = ∅.
Proof.
  unfold ex_plus2. cbn [eval]. rewrite !iUnion_Sing_l natZAdd_natZ.
  apply BinInter_Sing_diff, natZ_neq. discriminate.
Qed.

(* ex_choice_1_2 = (1 | 2) *)
Example ev_ex_choice rho : eval ex_choice_1_2 rho = {| 1 |} ∪ {| 2 |}.
Proof. reflexivity. Qed.

(* ex_isnat = nat[5] -> {|5|}  (cf. step_is_nat) *)
Example ev_ex_isnat rho : eval ex_isnat rho = {| 5 |}.
Proof. unfold ex_isnat. apply test_app_nat. Qed.

(** ** Lambda denotations (as dependent products [Pi]). *)

(* ex_poly_id = (x:any => x) : the polymorphic identity over [Big]. *)
Example ev_ex_poly_id rho : eval ex_poly_id rho = Pi Big (fun a => {| a |}).
Proof. unfold ex_poly_id. apply eval_poly_id. Qed.

(* ex_int_id = (x:int => x) : the identity over the naturals [ω]. *)
Example ev_ex_int_id rho : eval ex_int_id rho = Pi Omega (fun a => {| a |}).
Proof. unfold ex_int_id. rewrite eval_lam eval_tnat. reflexivity. Qed.

(* ex_succ = (x:int => x+1) *)
Example ev_ex_succ rho : eval ex_succ rho = Pi Omega (fun a => {| a + natZ 1 |}).
Proof.
  unfold ex_succ. rewrite eval_lam eval_tnat. f_equal.
  apply functional_extensionality. intro a.
  cbn [eval env_ext]. rewrite !iUnion_Sing_l. reflexivity.
Qed.

(* ex_ReflInt1 = (z:int => z=z) : every fibre [{a} ∩ {a} = {a}]. *)
Example ev_ex_ReflInt1 rho : eval ex_ReflInt1 rho = Pi Omega (fun a => {| a |}).
Proof.
  unfold ex_ReflInt1. rewrite eval_lam eval_tnat. f_equal.
  apply functional_extensionality. intro a.
  cbn [eval env_ext]. apply BinInter_Sing_same.
Qed.

(* ex_Curry1 = (x:int => y:int => x+y) *)
Example ev_ex_Curry1 rho :
  eval ex_Curry1 rho = Pi Omega (fun a => Pi Omega (fun b => {| a + b |})).
Proof.
  unfold ex_Curry1. rewrite eval_lam eval_tnat. f_equal.
  apply functional_extensionality. intro a.
  rewrite eval_lam eval_tnat. f_equal.
  apply functional_extensionality. intro b.
  cbn [eval env_ext]. rewrite !iUnion_Sing_l. reflexivity.
Qed.

(* ex_curry3 = (x:int => y:int => z:int => x+y+z) *)
Example ev_ex_curry3 rho :
  eval ex_curry3 rho
  = Pi Omega (fun a => Pi Omega (fun b => Pi Omega (fun c => {| a + b + c |}))).
Proof.
  unfold ex_curry3. rewrite eval_lam eval_tnat. f_equal.
  apply functional_extensionality. intro a.
  rewrite eval_lam eval_tnat. f_equal.
  apply functional_extensionality. intro b.
  rewrite eval_lam eval_tnat. f_equal.
  apply functional_extensionality. intro c.
  cbn [eval env_ext]. rewrite !iUnion_Sing_l. reflexivity.
Qed.

(** ** Application: [ex_Fun1 = (x:int => x+1)[2]] reduces to [{3}]. *)
Example ev_ex_Fun1 rho : eval ex_Fun1 rho = {| 3 |}.
Proof.
  unfold ex_Fun1. rewrite eval_app (ev_ex_succ rho).
  change (eval (Econ 2) rho) with (Sing (natZ 2)).
  have HB : (fun a => Sing (a + natZ 1)) (natZ 2) = Sing (natZ 3).
  { change (Sing (natZ 2 + natZ 1) = Sing (natZ 3)). by rewrite natZAdd_natZ. }
  rewrite (app_pi_sing Omega (fun a => Sing (a + natZ 1)) (fun a => (a + natZ 1)%zf)
             (natZ 2) (natZ 3) (natZ_mem_omega 2) (fun a _ => IN_Sing ((a + natZ 1)%zf)) HB).
  reflexivity.
Qed.

(** ** The EV30–EV50 family.

    The members that avoid the (uninterpreted) sequencing [;] all denote
    the singleton [{1}]; those that use [;] collapse to [∅]. *)

Ltac ev_one := cbn [eval]; zf_reduce; try reflexivity.

(* (1=1) = (1=1) *)
Example ev_ex_EV30 rho : eval ex_EV30 rho = {| 1 |}.
Proof. unfold ex_EV30, e_eq11. ev_one. Qed.
(* (1=1) = (+1) *)
Example ev_ex_EV32 rho : eval ex_EV32 rho = {| 1 |}.
Proof. unfold ex_EV32, e_eq11, e_plus1. ev_one. Qed.
(* (+1) = (1=1) *)
Example ev_ex_EV36 rho : eval ex_EV36 rho = {| 1 |}.
Proof. unfold ex_EV36, e_eq11, e_plus1. ev_one. Qed.
(* (+1) = (+1) *)
Example ev_ex_EV38 rho : eval ex_EV38 rho = {| 1 |}.
Proof. unfold ex_EV38, e_plus1. ev_one. Qed.
(* + (1=1) *)
Example ev_ex_EV48 rho : eval ex_EV48 rho = {| 1 |}.
Proof. unfold ex_EV48, e_eq11. ev_one. Qed.
(* + (+1) *)
Example ev_ex_EV50 rho : eval ex_EV50 rho = {| 1 |}.
Proof. unfold ex_EV50, e_plus1. ev_one. Qed.

(* A representative member that uses [;]: it collapses to [∅] because
   [Timbda0] does not interpret [Eseq].  (EV31/33/34/35/37/39–47/49 alike.) *)
Example ev_ex_EV39 rho : eval ex_EV39 rho = ∅.
Proof. reflexivity. Qed.
