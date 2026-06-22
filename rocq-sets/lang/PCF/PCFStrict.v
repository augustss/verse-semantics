(* lang/PCF/PCFStrict.v

   Plotkin's PCF (Programming Computable Functions; "LCF considered as a
   programming language", 1977): the simply-typed lambda calculus over two
   ground types — naturals and booleans — extended with the arithmetic and
   boolean primitives and a fixpoint operator.

   It also carries a *relational* set-theoretic semantics in [ZFSet]: a
   type denotes the set of its values, a term denotes the *set* of values
   it may produce (the empty set = divergence), and a well-typed term is a
   subset of its type ([soundness]).  The fixpoint denotes the set of
   fixed points of the function value (as in [Timbda0.Efix]). *)

Require Import ZFSet.
Require Import ZFNotation.
Require Import PCFSyntax.

(** The syntax ([Ty]/[Tm]), typing ([has_type], [Ctx]/[ctx_cons]), and the
    substitution-based small-step reduction ([lift]/[subst]/[value]/[step]/
    [multistep]) are shared with [PCFLifted.v] and now live in
    [PCFSyntax.v]; this file adds the strict relational set-theoretic
    semantics on top. *)

(* ================================================================== *)
(** * Operational semantics (big-step, environment-based). *)
(* ================================================================== *)

From Stdlib Require Import List.

(** A call-by-name, environment-based big-step semantics — in the spirit
    of Plotkin's original (call-by-name) PCF.

    A *value* is a numeral, a boolean, or a *closure* [vclos ρ T e]: a λ
    paired with the environment that captured its free variables.  Since
    the calculus is call-by-name, a variable is bound not to a value but
    to a *thunk* [thunk ρ e] — an unevaluated term together with its
    environment — which is run only when the variable is looked up.  An
    environment is a list of thunks, indexed by de Bruijn position
    (index [0] = head).

    [fix e] unrolls to [e (fix e)] ([E_fix]); call-by-name makes this
    terminate exactly when the program does, because the recursive
    occurrence is pushed as a thunk and forced only on demand. *)

Inductive Thunk : Set :=
  | thunk : list Thunk -> Tm -> Thunk.

Definition VEnv := list Thunk.

Inductive Val : Set :=
  | vnat  : nat -> Val
  | vbool : bool -> Val
  | vclos : VEnv -> Ty -> Tm -> Val.

Reserved Notation "ρ '⊢' e '⇓' v" (at level 70, e at next level, no associativity).

Inductive BigStep : VEnv -> Tm -> Val -> Prop :=
  | E_var : forall ρ ρ' n e v,
      nth_error ρ n = Some (thunk ρ' e) ->
      ρ' ⊢ e ⇓ v ->
      ρ ⊢ tvar n ⇓ v
  | E_lam : forall ρ T e,
      ρ ⊢ tlam T e ⇓ vclos ρ T e
  | E_app : forall ρ ρ' T e1 e2 body v,
      ρ ⊢ e1 ⇓ vclos ρ' T body ->
      (thunk ρ e2 :: ρ') ⊢ body ⇓ v ->
      ρ ⊢ tapp e1 e2 ⇓ v
  | E_zero : forall ρ,
      ρ ⊢ tzero ⇓ vnat 0
  | E_succ : forall ρ e n,
      ρ ⊢ e ⇓ vnat n ->
      ρ ⊢ tsucc e ⇓ vnat (S n)
  | E_pred : forall ρ e n,
      ρ ⊢ e ⇓ vnat n ->
      ρ ⊢ tpred e ⇓ vnat (Nat.pred n)
  | E_iszero_zero : forall ρ e,
      ρ ⊢ e ⇓ vnat 0 ->
      ρ ⊢ tiszero e ⇓ vbool true
  | E_iszero_succ : forall ρ e n,
      ρ ⊢ e ⇓ vnat (S n) ->
      ρ ⊢ tiszero e ⇓ vbool false
  | E_true : forall ρ,
      ρ ⊢ ttrue ⇓ vbool true
  | E_false : forall ρ,
      ρ ⊢ tfalse ⇓ vbool false
  | E_if_true : forall ρ e0 e1 e2 v,
      ρ ⊢ e0 ⇓ vbool true ->
      ρ ⊢ e1 ⇓ v ->
      ρ ⊢ tif e0 e1 e2 ⇓ v
  | E_if_false : forall ρ e0 e1 e2 v,
      ρ ⊢ e0 ⇓ vbool false ->
      ρ ⊢ e2 ⇓ v ->
      ρ ⊢ tif e0 e1 e2 ⇓ v
  | E_fix : forall ρ e v,
      ρ ⊢ tapp e (tfix e) ⇓ v ->
      ρ ⊢ tfix e ⇓ v

where "ρ '⊢' e '⇓' v" := (BigStep ρ e v).

(** Driver tactic: apply the forced rule for each term form (variable
    lookups discharged by [reflexivity]).  The guard rules for
    [iszero]/[if]/[pred] depend on a computed value, so callers pick those
    branches (and supply [pred]'s argument) explicitly. *)
Ltac bstep1 :=
  match goal with
  | |- BigStep _ (tvar _) _    => eapply E_var; [ reflexivity | ]
  | |- BigStep _ (tlam _ _) _  => eapply E_lam
  | |- BigStep _ (tapp _ _) _  => eapply E_app
  | |- BigStep _ (tfix _) _    => eapply E_fix
  | |- BigStep _ tzero _       => eapply E_zero
  | |- BigStep _ ttrue _       => eapply E_true
  | |- BigStep _ tfalse _      => eapply E_false
  | |- BigStep _ (tsucc _) _   => eapply E_succ
  | |- BigStep _ (tiszero _) (vbool true)  => eapply E_iszero_zero
  | |- BigStep _ (tiszero _) (vbool false) => eapply E_iszero_succ
  end.
Ltac bstep := repeat bstep1.

(** Ground evaluations. *)
Example op_two : nil ⊢ tsucc (tsucc tzero) ⇓ vnat 2.
Proof. bstep. Qed.

Example op_pred : nil ⊢ tpred (tsucc tzero) ⇓ vnat 0.
Proof. apply (E_pred _ _ 1). bstep. Qed.

Example op_iszero_true : nil ⊢ tiszero tzero ⇓ vbool true.
Proof. bstep. Qed.

Example op_iszero_false : nil ⊢ tiszero (tsucc tzero) ⇓ vbool false.
Proof. bstep. Qed.

Example op_if : nil ⊢ tif ttrue tzero (tsucc tzero) ⇓ vnat 0.
Proof. eapply E_if_true; bstep. Qed.

(** Applying the identity [λx:ι. x] to [0]: the argument is pushed as a
    thunk and forced when [x] (index [0]) is looked up. *)
Example op_app_id : nil ⊢ tapp (tlam TNat (tvar 0)) tzero ⇓ vnat 0.
Proof. bstep. Qed.

(* ================================================================== *)
(** * Contextual equivalence. *)
(* ================================================================== *)

(** Two well-typed terms are *contextually equivalent* when no
    well-typed program context can tell them apart.  We use the big-step
    operational semantics above to fix the observation: a closed program
    of ground type [ι] is *observed* by whether it converges to a value.

    [Ctxt] is a term with a single hole [chole]; [fill C e] plugs [e]
    into the hole.  In de Bruijn form this is capture-free *and* needs no
    index shifting: the hole sits in a position whose variable scope is
    exactly the context the plugged term is typed in. *)
Inductive Ctxt : Set :=
  | chole   : Ctxt
  | clam    : Ty -> Ctxt -> Ctxt
  | capp1   : Ctxt -> Tm -> Ctxt
  | capp2   : Tm -> Ctxt -> Ctxt
  | csucc   : Ctxt -> Ctxt
  | cpred   : Ctxt -> Ctxt
  | ciszero : Ctxt -> Ctxt
  | cif0    : Ctxt -> Tm -> Tm -> Ctxt
  | cif1    : Tm -> Ctxt -> Tm -> Ctxt
  | cif2    : Tm -> Tm -> Ctxt -> Ctxt
  | cfix    : Ctxt -> Ctxt.

Fixpoint fill (C : Ctxt) (e : Tm) : Tm :=
  match C with
  | chole         => e
  | clam T C      => tlam T (fill C e)
  | capp1 C e2    => tapp (fill C e) e2
  | capp2 e1 C    => tapp e1 (fill C e)
  | csucc C       => tsucc (fill C e)
  | cpred C       => tpred (fill C e)
  | ciszero C     => tiszero (fill C e)
  | cif0 C e1 e2  => tif (fill C e) e1 e2
  | cif1 e0 C e2  => tif e0 (fill C e) e2
  | cif2 e0 e1 C  => tif e0 e1 (fill C e)
  | cfix C        => tfix (fill C e)
  end.

(** Context typing [CtxTy C Γ T Γ' T']: filling [C]'s hole with a
    [Γ ⊢ _ : T] term yields a [Γ' ⊢ _ : T'] term.  [Γ]/[T] (the hole's
    demands) are threaded unchanged; [Γ']/[T'] are the result. *)
Inductive CtxTy : Ctxt -> Ctx -> Ty -> Ctx -> Ty -> Prop :=
  | C_hole : forall Γ T,
      CtxTy chole Γ T Γ T
  | C_lam : forall C Γ T Γ' T1 T2,
      CtxTy C Γ T (ctx_cons T1 Γ') T2 ->
      CtxTy (clam T1 C) Γ T Γ' (TArr T1 T2)
  | C_app1 : forall C Γ T Γ' T1 T2 e2,
      CtxTy C Γ T Γ' (TArr T1 T2) ->
      has_type Γ' e2 T1 ->
      CtxTy (capp1 C e2) Γ T Γ' T2
  | C_app2 : forall C Γ T Γ' T1 T2 e1,
      has_type Γ' e1 (TArr T1 T2) ->
      CtxTy C Γ T Γ' T1 ->
      CtxTy (capp2 e1 C) Γ T Γ' T2
  | C_succ : forall C Γ T Γ',
      CtxTy C Γ T Γ' TNat ->
      CtxTy (csucc C) Γ T Γ' TNat
  | C_pred : forall C Γ T Γ',
      CtxTy C Γ T Γ' TNat ->
      CtxTy (cpred C) Γ T Γ' TNat
  | C_iszero : forall C Γ T Γ',
      CtxTy C Γ T Γ' TNat ->
      CtxTy (ciszero C) Γ T Γ' TBool
  | C_if0 : forall C Γ T Γ' T' e1 e2,
      CtxTy C Γ T Γ' TBool ->
      has_type Γ' e1 T' -> has_type Γ' e2 T' ->
      CtxTy (cif0 C e1 e2) Γ T Γ' T'
  | C_if1 : forall e0 C Γ T Γ' T' e2,
      has_type Γ' e0 TBool ->
      CtxTy C Γ T Γ' T' -> has_type Γ' e2 T' ->
      CtxTy (cif1 e0 C e2) Γ T Γ' T'
  | C_if2 : forall e0 e1 C Γ T Γ' T',
      has_type Γ' e0 TBool -> has_type Γ' e1 T' ->
      CtxTy C Γ T Γ' T' ->
      CtxTy (cif2 e0 e1 C) Γ T Γ' T'
  | C_fix : forall C Γ T Γ' T',
      CtxTy C Γ T Γ' (TArr T' T') ->
      CtxTy (cfix C) Γ T Γ' T'.

(** Filling a well-typed term into a well-typed context is well-typed. *)
Lemma fill_typing C Γ T Γ' T' e :
  CtxTy C Γ T Γ' T' -> has_type Γ e T -> has_type Γ' (fill C e) T'.
Proof.
  intros HC He. induction HC; cbn [fill]; eauto using has_type.
Qed.

(** Observation: a closed term *converges* if it evaluates to a value
    under the empty environment.  (A free variable cannot, since lookup
    in [nil] fails — so open programs vacuously diverge.) *)
Definition converges (e : Tm) : Prop := exists v : Val, nil ⊢ e ⇓ v.

(** Contextual equivalence at [Γ ⊢ _ : T]: both terms are well-typed,
    and every ground-type program context observes them alike. *)
Definition ctx_equiv (Γ : Ctx) (e1 e2 : Tm) (T : Ty) : Prop :=
  has_type Γ e1 T /\ has_type Γ e2 T /\
  forall (C : Ctxt) (Γ' : Ctx),
    CtxTy C Γ T Γ' TNat ->
    (converges (fill C e1) <-> converges (fill C e2)).

Notation "Γ '⊨' e1 '≈' e2 ':' T" := (ctx_equiv Γ e1 e2 T)
  (at level 70, e1 at next level, e2 at next level).

(** Contextual equivalence is an equivalence relation on well-typed
    terms.  (Congruence — closure under all contexts — and soundness of
    the denotational model w.r.t. it are the deeper results, requiring a
    logical relation; not attempted here.) *)
Lemma ctx_equiv_refl Γ e T : has_type Γ e T -> Γ ⊨ e ≈ e : T.
Proof. intro H. split; [exact H | split; [exact H |]]. intros; tauto. Qed.

Lemma ctx_equiv_sym Γ e1 e2 T : (Γ ⊨ e1 ≈ e2 : T) -> Γ ⊨ e2 ≈ e1 : T.
Proof.
  intros (H1 & H2 & H). split; [exact H2 | split; [exact H1 |]].
  intros C Γ' HC. specialize (H C Γ' HC). tauto.
Qed.

Lemma ctx_equiv_trans Γ e1 e2 e3 T :
  (Γ ⊨ e1 ≈ e2 : T) -> (Γ ⊨ e2 ≈ e3 : T) -> Γ ⊨ e1 ≈ e3 : T.
Proof.
  intros (H1 & _ & H12) (_ & H3 & H23). split; [exact H1 | split; [exact H3 |]].
  intros C Γ' HC. specialize (H12 C Γ' HC). specialize (H23 C Γ' HC). tauto.
Qed.

(* ================================================================== *)
(** * Set-theoretic semantics (relational). *)
(* ================================================================== *)

(** A type denotes the set of its values: [ι] the naturals [ω], [o] the
    two booleans [{0,1}] ([0 = false], [1 = true]), an arrow the
    set-theoretic function space [Pi] (total functional graphs). *)
Fixpoint evalTy (T : Ty) : ZFSet :=
  match T with
  | TNat       => Omega
  | TBool      => {| natZ 0 |} ∪ {| natZ 1 |}
  | TArr T1 T2 => Pi (evalTy T1) (fun _ => evalTy T2)
  end.

Definition Env := nat -> ZFSet.

Definition env_cons (v : ZFSet) (ρ : Env) : Env :=
  fun n => match n with O => v | S m => ρ m end.

(** A term denotes the *set* of values it may produce.  Deterministic
    constructs yield singletons; a lambda denotes the set of its total
    functional realizations ([Pi] over the fibres); [tfix] yields the set
    of fixed points of the function value (empty when there is none — i.e.
    divergence). *)
Fixpoint evalTm (e : Tm) (ρ : Env) : ZFSet :=
  match e with
  | tvar n     => {| ρ n |}
  | tlam T e   =>
      Pi (evalTy T) (fun a => evalTm e (env_cons a ρ))
  | tapp e1 e2 =>
      f ← evalTm e1 ρ ;; v ← evalTm e2 ρ ;; image f {| v |}
  | tzero      => {| natZ 0 |}
  | tsucc e    => v ← evalTm e ρ ;; {| natZSucc v |}
  | tpred e    => v ← evalTm e ρ ;; {| natZPred v |}
  | tiszero e  =>
      v ← evalTm e ρ ;;
        (⦃ _ ∈ {| natZ 1 |} | v = natZ 0 ⦄ ∪ ⦃ _ ∈ {| natZ 0 |} | v <> natZ 0 ⦄)
  | ttrue      => {| natZ 1 |}
  | tfalse     => {| natZ 0 |}
  | tif e0 e1 e2 =>
      b ← evalTm e0 ρ ;;
        (⦃ _ ∈ evalTm e1 ρ | b = natZ 1 ⦄ ∪ ⦃ _ ∈ evalTm e2 ρ | b = natZ 0 ⦄)
  | tfix e     =>
      f ← evalTm e ρ ;; ⦃ a ∈ dom f | ⟨ a , a ⟩ ∈ f ⦄
  end.

(* ================================================================== *)
(** * Type Soundness: a well-typed term is a subset of its type. *)
(* ================================================================== *)

Definition models (Γ : Ctx) (ρ : Env) : Prop :=
  forall n, ρ n ∈ evalTy (Γ n).

Lemma models_cons Γ ρ v T :
  models Γ ρ -> v ∈ evalTy T -> models (ctx_cons T Γ) (env_cons v ρ).
Proof. intros HM Hv [|n]; cbn; [ exact Hv | apply HM ]. Qed.

Theorem soundness Γ e T :
  has_type Γ e T -> forall ρ, models Γ ρ -> evalTm e ρ ⊆ evalTy T.
Proof.
  induction 1; intros ρ HM; cbn [evalTm evalTy].

  - (* T_var *)
    apply Sing_Inc_IN. apply HM.

  - (* T_lam: every fibre lands in the codomain, so [Pi] is monotone *)
    apply Pi_Inc_Codomain. intros a Ha.
    apply (IHhas_type (env_cons a ρ)). apply models_cons; assumption.

  - (* T_app: an edge of the (well-typed) function lands in the codomain *)
    apply Inc_def. intros w Hw.
    apply iUnion_IN in Hw. destruct Hw as [f [Hf Hw]].
    apply iUnion_IN in Hw. destruct Hw as [v [Hv Hw]].
    apply image_elim in Hw. destruct Hw as [a [Ha Hedge]].
    apply IN_Sing_EQ in Ha. subst a.
    apply (Inc_IN _ _ _ (IHhas_type1 ρ HM)) in Hf.
    apply (Inc_IN _ _ _ (IHhas_type2 ρ HM)) in Hv.
    exact (Pi_edge_codomain _ _ _ _ _ Hf Hv Hedge).

  - (* T_zero *)
    apply Sing_Inc_IN. apply natZ_mem_omega.

  - (* T_succ *)
    apply iUnion_Inc. intros v Hv.
    apply (Inc_IN _ _ _ (IHhas_type ρ HM)) in Hv.
    destruct (In_omega _ Hv) as [k Hk]. subst v.
    rewrite natZSucc_natZ. apply Sing_Inc_IN, natZ_mem_omega.

  - (* T_pred *)
    apply iUnion_Inc. intros v Hv.
    apply (Inc_IN _ _ _ (IHhas_type ρ HM)) in Hv.
    destruct (In_omega _ Hv) as [k Hk]. subst v.
    rewrite natZPred_natZ. apply Sing_Inc_IN, natZ_mem_omega.

  - (* T_iszero: the result is one of the two booleans *)
    apply iUnion_Inc. intros v Hv. apply Inc_def. intros x Hx.
    apply BinUnion_IN in Hx. destruct Hx as [Hx | Hx];
      apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hx; apply IN_Sing_EQ in Hx; subst x.
    + apply IN_BinUnion_r, IN_Sing.
    + apply IN_BinUnion_l, IN_Sing.

  - (* T_true *)
    apply Sing_Inc_IN. apply IN_BinUnion_r, IN_Sing.

  - (* T_false *)
    apply Sing_Inc_IN. apply IN_BinUnion_l, IN_Sing.

  - (* T_if: each branch is in [T] *)
    apply iUnion_Inc. intros b Hb. apply Inc_def. intros x Hx.
    apply BinUnion_IN in Hx. destruct Hx as [Hx | Hx];
      apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hx.
    + apply (Inc_IN _ _ _ (IHhas_type2 ρ HM)). exact Hx.
    + apply (Inc_IN _ _ _ (IHhas_type3 ρ HM)). exact Hx.

  - (* T_fix: a fixed point [a] of [f] lies in [T] (it is in [f]'s domain) *)
    apply iUnion_Inc. intros f Hf. apply Inc_def. intros a Ha.
    pose proof (In_Comp_P _ _ _ Ha) as Haa.
    apply (Inc_IN _ _ _ (IHhas_type ρ HM)) in Hf.
    apply In_Pi_inv in Hf. destruct Hf as [Hsub _].
    apply (Inc_IN _ _ _ Hsub) in Haa.
    apply Couple_Prod_IN in Haa. destruct Haa as [Ha1 _]. exact Ha1.
Qed.

(* ================================================================== *)
(** * Fixpoint unfolding. *)
(* ================================================================== *)

(** [fix e] denotes the *set of fixed points* of the value(s) of [e],
    while [e (fix e)] *applies* those value(s) to the fixed points.  The
    two coincide exactly when [e] denotes a single *function* [f]: each
    fixed point is its own [f]-image, and single-valuedness ([isFunction])
    rules out an edge sending a fixed point elsewhere.

    The side conditions are necessary: for a genuinely relational
    [f = {⟨0,0⟩, ⟨0,1⟩}] the fixed points are [{0}] but applying [f]
    gives [{0,1}].  (Operationally, by contrast, the unfolding [E_fix] is
    unconditional, because call-by-name never forms such an [f].)

    This is the [ZFSet] analogue of [Timbda0.fixpoint_unfolding]. *)
Lemma evalTm_fix_unfold e ρ f :
  evalTm e ρ = {| f |} -> isFunction f ->
  evalTm (tfix e) ρ = evalTm (tapp e (tfix e)) ρ.
Proof.
  intros He Hfun.
  cbn [evalTm]. rewrite !He, !iUnion_Sing_l.
  apply set_ext; apply Inc_def; intros x Hx.
  - (* a fixed point [x] is its own [f]-image *)
    pose proof (In_Comp_P _ _ _ Hx) as Hxx.
    apply IN_iUnion with (y := x); [ exact Hx | ].
    apply (image_intro f {| x |} x x); [ apply IN_Sing | exact Hxx ].
  - (* conversely, the [f]-image of a fixed point [v] is [v] itself *)
    apply iUnion_IN in Hx. destruct Hx as [v [Hv Hx]].
    apply image_elim in Hx. destruct Hx as [a [Ha Hax]].
    apply IN_Sing_EQ in Ha. subst a.
    pose proof (In_Comp_P _ _ _ Hv) as Hvv.
    rewrite (Hfun v x v Hax Hvv). exact Hv.
Qed.

(** For a λ the side conditions are automatic once the body is
    deterministic: if every fibre is a singleton, the function value is a
    singleton [Pi]-graph, hence a function. *)
Corollary evalTm_fix_unfold_lam T body ρ :
  (forall a, a ∈ evalTy T -> exists b, evalTm body (env_cons a ρ) = {| b |}) ->
  evalTm (tfix (tlam T body)) ρ
  = evalTm (tapp (tlam T body) (tfix (tlam T body))) ρ.
Proof.
  intro Hsing.
  destruct (Pi_Sing_ex (evalTy T) (fun a => evalTm body (env_cons a ρ)) Hsing)
    as [f Hf].
  assert (Hev : evalTm (tlam T body) ρ = {| f |}) by exact Hf.
  assert (Hfun : isFunction f).
  { assert (Hfin : f ∈ evalTm (tlam T body) ρ) by (rewrite Hev; apply IN_Sing).
    cbn [evalTm] in Hfin. apply In_Pi_inv in Hfin.
    destruct Hfin as [_ [_ Hfun]]. exact Hfun. }
  exact (evalTm_fix_unfold (tlam T body) ρ f Hev Hfun).
Qed.

(* ================================================================== *)
(** * Examples: evaluation of closed PCF programs. *)
(* ================================================================== *)

From Stdlib Require Import PeanoNat.

(** A comprehension over a singleton collapses to the singleton (predicate
    holds) or to [∅] (predicate fails). *)
Lemma Comp_Sing_in (x : ZFSet) (P : ZFSet -> Prop) :
  P x -> ⦃ y ∈ {| x |} | P y ⦄ = {| x |}.
Proof.
  intro HP. apply set_ext; apply Inc_def; intros y Hy.
  - apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hy. exact Hy.
  - apply IN_Sing_EQ in Hy. subst y. apply In_P_Comp; [ apply IN_Sing | exact HP ].
Qed.

Lemma Comp_Sing_out (x : ZFSet) (P : ZFSet -> Prop) :
  ~ P x -> ⦃ y ∈ {| x |} | P y ⦄ = ∅.
Proof.
  intro HP. apply set_ext; apply Inc_def; intros y Hy.
  - pose proof (In_Comp_P _ _ _ Hy) as HPy.
    apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hy. apply IN_Sing_EQ in Hy. subst y.
    destruct (HP HPy).
  - destruct (not_In_Empty _ Hy).
Qed.

(** Variants over an *arbitrary* base set, for a guard that does not
    depend on the comprehension variable: a constantly-true guard keeps
    the whole set, a constantly-false guard empties it.  (Needed for the
    [if] in a recursive body, whose else-branch is not a singleton.) *)
Lemma Comp_const_in (S : ZFSet) (P : ZFSet -> Prop) :
  (forall y, P y) -> ⦃ y ∈ S | P y ⦄ = S.
Proof.
  intro HP. apply set_ext; apply Inc_def; intros y Hy.
  - exact (Inc_IN _ _ _ (Comp_Inc _ _) Hy).
  - apply In_P_Comp; [ exact Hy | apply HP ].
Qed.

Lemma Comp_const_out (S : ZFSet) (P : ZFSet -> Prop) :
  (forall y, ~ P y) -> ⦃ y ∈ S | P y ⦄ = ∅.
Proof.
  intro HP. apply set_ext; apply Inc_def; intros y Hy.
  - destruct (HP y (In_Comp_P _ _ _ Hy)).
  - destruct (not_In_Empty _ Hy).
Qed.

(* ------------------------------------------------------------------ *)
(** ** An evaluation tactic for closed ground programs.

    A closed ground PCF program denotes a singleton numeral, reached by
    iterating singleton binds, computing [succ]/[pred] on numerals, and
    resolving the [iszero]/[if] guards.  The shared [zf_reduce] engine
    (in [ZFSetFacts]) handles the bind/union plumbing but not the
    numeral arithmetic or the guard decisions specific to PCF; the
    tactics below add exactly those, so the ground examples discharge in
    one step. *)

(** Decide the side conditions the [iszero]/[if] guards bottom out in:
    (dis)equalities of numerals.  Numerals are equal iff their indices
    are ([reflexivity] / [natZ_neq] + [discriminate]); the third branch
    closes the doubly-negated form ([~ (natZ i <> natZ j)] with [i = j]),
    which is the [Comp_Sing_in] obligation on a *failing* guard. *)
Ltac pcf_guard :=
  cbv beta;
  first
    [ reflexivity
    | apply natZ_neq; discriminate
    | let H := fresh in intro H; apply H; reflexivity ].

(** Resolve one comprehension over a singleton: keep it ([Comp_Sing_in])
    when the guard holds, collapse it to [∅] ([Comp_Sing_out]) when it
    fails — deciding which way with [pcf_guard]. *)
Ltac pcf_comp :=
  match goal with
  | [ |- context[ Comp (Sing ?x) ?P ] ] =>
      first [ rewrite (Comp_Sing_in x P ltac:(pcf_guard))
            | rewrite (Comp_Sing_out x P ltac:(pcf_guard)) ]
  | [ |- context[ Comp ?S ?P ] ] =>
      first [ rewrite (Comp_const_out S P ltac:(intro; pcf_guard))
            | rewrite (Comp_const_in S P ltac:(intro; pcf_guard)) ]
  end.

(** [pcf_reduce] normalises a denotation: it unfolds [evalTm] (and the
    type/environment helpers), iterates singleton binds, computes [succ]
    and [pred] on numerals, resolves the [iszero]/[if] guards, and
    discards the empty branch of the resulting union. *)
Ltac pcf_reduce :=
  cbn [evalTm evalTy env_cons ctx_cons];
  repeat first
    [ rewrite iUnion_Sing_l
    | rewrite natZSucc_natZ
    | rewrite natZPred_natZ
    | rewrite BinUnion_Vide_l
    | rewrite BinUnion_Vide_r
    | pcf_comp ].

(** Evaluate a closed ground program to its value. *)
Ltac pcf_eval := intros; pcf_reduce; try reflexivity.

(** [2 = succ (succ 0)]. *)
Example ev_two ρ : evalTm (tsucc (tsucc tzero)) ρ = {| natZ 2 |}.
Proof. pcf_eval. Qed.

(** [pred (succ 0) = 0]. *)
Example ev_pred ρ : evalTm (tpred (tsucc tzero)) ρ = {| natZ 0 |}.
Proof. pcf_eval. Qed.

(** [iszero 0 = true]. *)
Example ev_iszero_true ρ : evalTm (tiszero tzero) ρ = {| natZ 1 |}.
Proof. pcf_eval. Qed.

(** [iszero (succ 0) = false]. *)
Example ev_iszero_false ρ : evalTm (tiszero (tsucc tzero)) ρ = {| natZ 0 |}.
Proof. pcf_eval. Qed.

(** [if true then 0 else (succ 0)  =  0]. *)
Example ev_if_true ρ : evalTm (tif ttrue tzero (tsucc tzero)) ρ = {| natZ 0 |}.
Proof. pcf_eval. Qed.

(** Applying the identity [λx:ι. x] to [0] yields [0].

    The lambda denotes the singleton identity graph ([Pi_Sing]); once the
    two singleton binds reduce, relational application of a function value
    to an in-domain point is its functional value ([image_Sing_of_pi] /
    [applyFun_mem_of_pi]). *)
Example ev_app_id ρ : evalTm (tapp (tlam TNat (tvar 0)) tzero) ρ = {| natZ 0 |}.
Proof.
  cbn [evalTm evalTy env_cons].
  rewrite (Pi_Sing Omega (fun a => {| a |}) (fun a => a)) by (intros; reflexivity).
  rewrite !iUnion_Sing_l.
  set (idg := iUnion Omega (fun a => {| ⟨ a , a ⟩ |})).
  assert (Hpi : idg ∈ Pi Omega (fun a => {| a |}))
    by (apply (iUnion_graph_mem_pi Omega (fun a => {| a |}) (fun a => a));
        intros a Ha; apply IN_Sing).
  rewrite (image_Sing_of_pi Omega (fun a => {| a |}) idg (natZ 0) Hpi (natZ_mem_omega 0)).
  f_equal. apply IN_Sing_EQ.
  exact (applyFun_mem_of_pi Omega (fun a => {| a |}) idg (natZ 0) Hpi (natZ_mem_omega 0)).
Qed.

(** [fix (λx:ι. succ x)] diverges: the successor has no fixed point, so the
    set of fixed points — hence the denotation — is empty. *)
Example ev_fix_diverge ρ : evalTm (tfix (tlam TNat (tsucc (tvar 0)))) ρ = ∅.
Proof.
  cbn [evalTm evalTy env_cons].
  apply set_ext; apply Inc_def; intros x Hx.
  - exfalso.
    apply iUnion_IN in Hx. destruct Hx as [f [Hf Hx]].
    pose proof (In_Comp_P _ _ _ Hx) as Hxx.
    pose proof Hf as HfP. apply In_Pi_inv in HfP. destruct HfP as [Hsub _].
    pose proof (Inc_IN _ _ _ Hsub Hxx) as Hprod.
    apply Couple_Prod_IN in Hprod. destruct Hprod as [HxO _].
    pose proof (Pi_edge_codomain _ _ _ _ _ Hf HxO Hxx) as Hb.
    apply iUnion_IN in Hb. destruct Hb as [v [Hv Hb]].
    apply IN_Sing_EQ in Hv. subst v. apply IN_Sing_EQ in Hb.
    destruct (In_omega _ HxO) as [k Hk]. subst x.
    rewrite natZSucc_natZ in Hb.
    exact (natZ_neq k (S k) (fun e => Nat.neq_succ_diag_l k (eq_sym e)) Hb).
  - destruct (not_In_Empty _ Hx).
Qed.

(** A *recursive function definition*.  [tfix] at a function type [ι → ι]
    binds the function to itself, so the body may call it recursively.
    Here

      [double = fix (λf:ι→ι. λx:ι. if iszero x then 0 else succ (succ (f (pred x))))]

    is the doubling function defined by recursion on its argument:
    [double 0 = 0] and [double (n+1) = succ (succ (double n))], i.e.
    [double n = 2n].  The recursive call is the inner [f (pred x)] —
    [f] is the de Bruijn variable [1] (bound by the outer λ) and [x] is
    [0] (bound by the inner λ). *)
Definition dbl_body : Tm :=
  tif (tiszero (tvar 0))
      tzero
      (tsucc (tsucc (tapp (tvar 1) (tpred (tvar 0))))).

Definition double : Tm :=
  tfix (tlam (TArr TNat TNat) (tlam TNat dbl_body)).

(** The recursive definition is well-typed at [ι → ι]; the derivation
    bottoms out in [T_fix] for the recursion and [T_app] for the
    recursive call, and is fully mechanical (each term form has exactly
    one applicable typing rule). *)
(** Close a variable goal: [T_var] types [tvar n] at [Γ n], and the kernel
    computes the [ctx_cons] lookup by conversion (which plain [apply] will
    not do against a concrete expected type). *)
Ltac pcf_tvar :=
  match goal with
  | [ |- has_type ?G (tvar ?n) _ ] => exact (T_var G n)
  end.

Example double_typed Γ : has_type Γ double (TArr TNat TNat).
Proof.
  unfold double, dbl_body.
  apply T_fix, T_lam, T_lam. apply T_if.
  - apply T_iszero. pcf_tvar.
  - apply T_zero.
  - apply T_succ, T_succ. eapply T_app.
    + pcf_tvar.
    + apply T_pred. pcf_tvar.
Qed.

(** Reading the meaning of the recursion off [soundness]: every fixed
    point in [double]'s denotation is a genuine set-theoretic function
    [ι → ι] (an element of [Pi ω (fun _ => ω)]).  Being closed, [double]
    is well-typed in any context, so this holds for every model. *)
Corollary double_sound Γ ρ :
  models Γ ρ -> evalTm double ρ ⊆ evalTy (TArr TNat TNat).
Proof. intro HM. exact (soundness Γ double _ (double_typed Γ) ρ HM). Qed.

(* ================================================================== *)
(** ** Computing the recursive function: [double 2 = 4].

    This is substantial because [fix] denotes the *set of fixed points*
    of the functional value (not a least fixed point): so to evaluate
    [double 2] we must show that fixed points exist *and* all map [2] to
    [4].  Concretely we prove [evalTm double ρ = {| D |}], where [D] is
    the doubling graph [{⟨n, 2n⟩ : n ∈ ω}], and then apply [D] to [2]. *)

From Stdlib Require Import Lia.

(** The set-theoretic function space [ι → ι]. *)
Definition FSpace : ZFSet := Pi Omega (fun _ => Omega).

(** In a function [f ∈ Pi A B], the edge over an in-domain point [a] is
    [⟨a, applyFun f a⟩], and conversely every edge's target is the
    applied value.  (Bridges the relational graph and [applyFun].) *)
Lemma pi_value_edge A B f a :
  f ∈ Pi A B -> a ∈ A -> ⟨ a , applyFun f a ⟩ ∈ f.
Proof.
  intros Hf Ha.
  destruct (In_Pi_inv A B f Hf) as [_ [Htot _]].
  destruct (Htot a Ha) as [b [_ Hedge]].
  assert (Hval : b = applyFun f a).
  { assert (Hbin : b ∈ image f {| a |})
      by (apply image_intro with (a := a); [ apply IN_Sing | exact Hedge ]).
    rewrite (image_Sing_of_pi A B f a Hf Ha) in Hbin.
    apply IN_Sing_EQ in Hbin. exact Hbin. }
  rewrite <- Hval. exact Hedge.
Qed.

Lemma pi_edge_value A B f a b :
  f ∈ Pi A B -> ⟨ a , b ⟩ ∈ f -> b = applyFun f a.
Proof.
  intros Hf Hedge.
  destruct (In_Pi_inv A B f Hf) as [Hsub _].
  pose proof (Inc_IN _ _ _ Hsub Hedge) as Hprod.
  apply Couple_Prod_IN in Hprod. destruct Hprod as [Ha _].
  assert (Hbin : b ∈ image f {| a |})
    by (apply image_intro with (a := a); [ apply IN_Sing | exact Hedge ]).
  rewrite (image_Sing_of_pi A B f a Hf Ha) in Hbin.
  apply IN_Sing_EQ in Hbin. exact Hbin.
Qed.

(** Extensionality for [ι → ι]: two functions that agree on every
    numeral are equal (their graphs coincide). *)
Lemma fspace_ext f g :
  f ∈ FSpace -> g ∈ FSpace ->
  (forall n : nat, applyFun f (natZ n) = applyFun g (natZ n)) -> f = g.
Proof.
  unfold FSpace. intros Hf Hg Hpt. apply set_ext; apply Inc_def; intros p Hp.
  - destruct (In_Pi_inv _ _ f Hf) as [Hsub _].
    pose proof (Inc_IN _ _ _ Hsub Hp) as Hprod.
    apply IN_Prod_EX in Hprod. destruct Hprod as [a [b [Ha [_ Heq]]]]. subst p.
    pose proof (pi_edge_value _ _ f a b Hf Hp) as Hb.
    destruct (In_omega a Ha) as [n Hn]. subst a.
    rewrite Hb, (Hpt n). exact (pi_value_edge _ _ g (natZ n) Hg Ha).
  - destruct (In_Pi_inv _ _ g Hg) as [Hsub _].
    pose proof (Inc_IN _ _ _ Hsub Hp) as Hprod.
    apply IN_Prod_EX in Hprod. destruct Hprod as [a [b [Ha [_ Heq]]]]. subst p.
    pose proof (pi_edge_value _ _ g a b Hg Hp) as Hb.
    destruct (In_omega a Ha) as [n Hn]. subst a.
    rewrite Hb, <- (Hpt n). exact (pi_value_edge _ _ f (natZ n) Hf Ha).
Qed.

(** The doubling graph [D = {⟨n, 2n⟩ : n ∈ ω}].  Using [natZAdd a a] for
    the value makes the fibre uniform across all [a] (no base-case split
    in the graph itself). *)
Definition D : ZFSet := iUnion Omega (fun a => {| ⟨ a , natZAdd a a ⟩ |}).

Lemma D_pi_sing : D ∈ Pi Omega (fun a => {| natZAdd a a |}).
Proof.
  apply (iUnion_graph_mem_pi Omega (fun a => {| natZAdd a a |}) (fun a => natZAdd a a)).
  intros a Ha. apply IN_Sing.
Qed.

Lemma D_fspace : D ∈ FSpace.
Proof.
  unfold FSpace.
  apply (iUnion_graph_mem_pi Omega (fun _ => Omega) (fun a => natZAdd a a)).
  intros a Ha. destruct (In_omega a Ha) as [n Hn]. subst a.
  rewrite natZAdd_natZ. apply natZ_mem_omega.
Qed.

Lemma D_app n : applyFun D (natZ n) = natZ (n + n).
Proof.
  pose proof (applyFun_mem_of_pi Omega (fun a => {| natZAdd a a |})
                D (natZ n) D_pi_sing (natZ_mem_omega n)) as Hm.
  apply IN_Sing_EQ in Hm. rewrite Hm, natZAdd_natZ. reflexivity.
Qed.

Section DoubleEval.
Variable ρ : Env.

(** The fibre of the inner λ at a function value [F]: the body evaluated
    with [f := F] over each argument. *)
Definition fibre (F : ZFSet) : ZFSet :=
  Pi Omega (fun a => evalTm dbl_body (env_cons a (env_cons F ρ))).

(** The body computes one step of the recursion on a numeral argument:
    [0 ↦ 0], [S m ↦ succ (succ (F m))]. *)
Lemma dbl_body_eval F n :
  F ∈ FSpace ->
  evalTm dbl_body (env_cons (natZ n) (env_cons F ρ)) =
    match n with
    | O   => {| natZ 0 |}
    | S m => {| natZSucc (natZSucc (applyFun F (natZ m))) |}
    end.
Proof.
  unfold FSpace. intro HF. unfold dbl_body. destruct n as [|m].
  - pcf_reduce. reflexivity.
  - pcf_reduce.
    rewrite (image_Sing_of_pi Omega (fun _ => Omega) F
               (natZ (Nat.pred (S m))) HF (natZ_mem_omega _)).
    rewrite !iUnion_Sing_l. reflexivity.
Qed.

(** Every fibre is a singleton (the body is deterministic). *)
Lemma fibre_sing F : F ∈ FSpace -> exists b, fibre F = {| b |}.
Proof.
  intro HF. unfold fibre. apply Pi_Sing_ex. intros a Ha.
  destruct (In_omega a Ha) as [n Hn]. subst a.
  rewrite (dbl_body_eval F n HF). destruct n; eexists; reflexivity.
Qed.

(** The fibre at [D] is exactly [{| D |}]: [D] is a fixed point of the
    one-step map.  (Here the base/step split appears, but [D]'s graph is
    uniform [natZAdd a a].) *)
Lemma fibre_D : fibre D = {| D |}.
Proof.
  unfold fibre.
  rewrite (Pi_Sing Omega (fun a => evalTm dbl_body (env_cons a (env_cons D ρ)))
             (fun a => natZAdd a a)).
  - reflexivity.
  - intros a Ha. destruct (In_omega a Ha) as [n Hn]. subst a.
    rewrite (dbl_body_eval D n D_fspace). destruct n as [|m].
    + rewrite natZAdd_natZ. reflexivity.
    + rewrite D_app, !natZSucc_natZ, natZAdd_natZ. f_equal. f_equal. lia.
Qed.

Lemma Lam_sing : exists G, Pi FSpace fibre = {| G |}.
Proof. apply Pi_Sing_ex. intros F HF. apply fibre_sing. exact HF. Qed.

(** The fixed-point set of the functional is exactly [{| D |}]. *)
Lemma eval_double : evalTm double ρ = {| D |}.
Proof.
  destruct Lam_sing as [G0 HG0].
  assert (HG0pi : G0 ∈ Pi FSpace fibre) by (rewrite HG0; apply IN_Sing).
  assert (HappD : applyFun G0 D = D).
  { pose proof (applyFun_mem_of_pi FSpace fibre G0 D HG0pi D_fspace) as Hm.
    rewrite fibre_D in Hm. apply IN_Sing_EQ in Hm. exact Hm. }
  assert (Hedge : ⟨ D , D ⟩ ∈ G0).
  { pose proof (pi_value_edge FSpace fibre G0 D HG0pi D_fspace) as He.
    rewrite HappD in He. exact He. }
  assert (Hfix : evalTm double ρ = ⦃ a ∈ dom G0 | ⟨ a , a ⟩ ∈ G0 ⦄).
  { unfold double.
    change (evalTm (tfix (tlam (TArr TNat TNat) (tlam TNat dbl_body))) ρ)
      with (f ← Pi FSpace fibre ;; ⦃ a ∈ dom f | ⟨ a , a ⟩ ∈ f ⦄).
    rewrite HG0, iUnion_Sing_l. reflexivity. }
  rewrite Hfix. apply set_ext; apply Inc_def; intros x Hx.
  - (* every fixed point [x] equals [D] *)
    pose proof (In_Comp_P _ _ _ Hx) as Hxx.
    destruct (In_Pi_inv _ _ G0 HG0pi) as [Hsub _].
    pose proof (Inc_IN _ _ _ Hsub Hxx) as Hprod.
    apply Couple_Prod_IN in Hprod. destruct Hprod as [HxF _].
    pose proof (Pi_edge_codomain FSpace fibre G0 x x HG0pi HxF Hxx) as Hxfib.
    unfold fibre in Hxfib.
    assert (Hrec : forall k, applyFun x (natZ k) = natZ (k + k)).
    { induction k as [|m IH].
      - pose proof (applyFun_mem_of_pi Omega
          (fun a => evalTm dbl_body (env_cons a (env_cons x ρ)))
          x (natZ 0) Hxfib (natZ_mem_omega 0)) as Hm.
        cbv beta in Hm.
        rewrite (dbl_body_eval x 0 HxF) in Hm. apply IN_Sing_EQ in Hm.
        rewrite Hm. reflexivity.
      - pose proof (applyFun_mem_of_pi Omega
          (fun a => evalTm dbl_body (env_cons a (env_cons x ρ)))
          x (natZ (S m)) Hxfib (natZ_mem_omega (S m))) as Hm.
        cbv beta in Hm.
        rewrite (dbl_body_eval x (S m) HxF) in Hm. apply IN_Sing_EQ in Hm.
        rewrite Hm, IH, !natZSucc_natZ. f_equal. lia. }
    assert (HxD : x = D).
    { apply fspace_ext; [ exact HxF | exact D_fspace | ].
      intro n. rewrite Hrec, D_app. reflexivity. }
    rewrite HxD. apply IN_Sing.
  - (* [D] is a fixed point *)
    apply IN_Sing_EQ in Hx. subst x.
    apply In_P_Comp; [ exact (IN_dom D D G0 Hedge) | exact Hedge ].
Qed.

(** [double] applied to [2] is [4]. *)
Example ev_double_2 :
  evalTm (tapp double (tsucc (tsucc tzero))) ρ = {| natZ 4 |}.
Proof.
  cbn [evalTm]. rewrite eval_double. pcf_reduce.
  rewrite (image_Sing_of_pi Omega (fun _ => Omega) D (natZ 2)
             D_fspace (natZ_mem_omega 2)).
  rewrite D_app. reflexivity.
Qed.

End DoubleEval.

(* ================================================================== *)
(** * Reading values back into terms, and operational/denotational
      agreement. *)
(* ================================================================== *)

(** Numerals as terms: [numeral n = succ^n 0]. *)
Fixpoint numeral (n : nat) : Tm :=
  match n with O => tzero | S k => tsucc (numeral k) end.

(** Read a value back into a term.  Ground values map to the matching
    literal; a closure maps to its underlying λ.  This is a genuine
    injection on ground values; on a closure it drops the captured
    environment, so it is faithful only on closed (environment-free)
    closures. *)
Definition val_to_tm (v : Val) : Tm :=
  match v with
  | vnat n      => numeral n
  | vbool true  => ttrue
  | vbool false => tfalse
  | vclos _ T e => tlam T e
  end.

Lemma evalTm_numeral n ρ : evalTm (numeral n) ρ = {| natZ n |}.
Proof.
  induction n as [|k IH]; cbn [numeral].
  - reflexivity.
  - cbn [evalTm]. rewrite IH, iUnion_Sing_l, natZSucc_natZ. reflexivity.
Qed.

(** ** The naive correctness statement [⇓ ⟹ equal denotation] FAILS.

    One might hope that [nil ⊢ e ⇓ v -> evalTm e ρ = evalTm (val_to_tm v) ρ].
    It does not hold: [BigStep] is call-by-name (an unused argument is
    never forced) while the denotational [tapp] is *strict* in its
    argument ([v ← evalTm e2 ρ ;; …], which is [∅] as soon as [e2]
    diverges).  A constant function applied to a divergent argument is the
    witness. *)
Definition loop : Tm := tfix (tlam TNat (tsucc (tvar 0))).

(** Operationally, [(λx:ι. 0) loop] converges to [0]: [loop] is pushed as
    a thunk and never forced. *)
Example cbn_const_loop_op :
  nil ⊢ tapp (tlam TNat tzero) loop ⇓ vnat 0.
Proof. bstep. Qed.

(** Denotationally it is [∅] (divergence), since [evalTm loop ρ = ∅] and
    the application iterates over that empty argument-set. *)
Example cbn_const_loop_den ρ :
  evalTm (tapp (tlam TNat tzero) loop) ρ = ∅.
Proof.
  assert (Hl : evalTm loop ρ = ∅) by (unfold loop; apply ev_fix_diverge).
  assert (Happ : evalTm (tapp (tlam TNat tzero) loop) ρ
               = (f ← evalTm (tlam TNat tzero) ρ ;;
                  v ← evalTm loop ρ ;; image f {| v |})) by reflexivity.
  rewrite Happ, Hl.
  apply set_ext; apply Inc_def; intros x Hx; [ | destruct (not_In_Empty _ Hx) ].
  apply iUnion_IN in Hx. destruct Hx as [f [_ Hx]].
  rewrite iUnion_Vide in Hx. destruct (not_In_Empty _ Hx).
Qed.

(** Hence the equation is refuted ([∅] vs [{0}]) at [e = (λx:ι. 0) loop],
    [v = vnat 0]. *)
Remark cbn_correctness_fails :
  ~ (forall e v ρ, nil ⊢ e ⇓ v -> evalTm e ρ = evalTm (val_to_tm v) ρ).
Proof.
  intro H.
  pose proof (H (tapp (tlam TNat tzero) loop) (vnat 0) (fun _ => ∅)
                cbn_const_loop_op) as Heq.
  rewrite (cbn_const_loop_den (fun _ => ∅)) in Heq.
  cbn [val_to_tm] in Heq. rewrite evalTm_numeral in Heq.
  assert (Hin : natZ 0 ∈ (∅ : ZFSet)) by (rewrite Heq; apply IN_Sing).
  exact (not_In_Empty _ Hin).
Qed.

(** ** ...and call-by-value would NOT rescue the theorem.

    Switching [BigStep] to call-by-value removes the *strictness*
    counterexample above ([loop] as an argument would then diverge before
    the call).  But a deeper obstruction survives: [evalTy] interprets an
    arrow as the set of *total* functional graphs ([Pi]), so a function
    value whose body diverges on some input denotes [∅] — even though a λ
    is a perfectly good operational value under *either* strategy.  A
    constant function applied to such a value converges operationally (the
    value is accepted, the body ignores it) but denotes [∅]. *)

(** [λy:ι. loop] is an operational value (a λ), yet it denotes [∅]: every
    fibre of its [Pi] is empty, so there is no total realization. *)
Lemma evalTm_div_fun ρ : evalTm (tlam TNat loop) ρ = ∅.
Proof.
  cbn [evalTm evalTy].
  apply set_ext; apply Inc_def; intros f Hf; [ | destruct (not_In_Empty _ Hf) ].
  apply In_Pi_inv in Hf. destruct Hf as [_ [Htot _]].
  destruct (Htot (natZ 0) (natZ_mem_omega 0)) as [b [Hb _]].
  assert (E : evalTm loop (env_cons (natZ 0) ρ) = ∅)
    by (unfold loop; apply ev_fix_diverge).
  cbv beta in Hb. rewrite E in Hb. destruct (not_In_Empty _ Hb).
Qed.

(** Operationally [(λx:ι→ι. 0) (λy:ι. loop)] converges to [0].  This
    derivation forces nothing inside the argument, so it holds verbatim
    under call-by-value too (a λ is a value either way). *)
Example tot_app_op :
  nil ⊢ tapp (tlam (TArr TNat TNat) tzero) (tlam TNat loop) ⇓ vnat 0.
Proof. bstep. Qed.

(** Denotationally it is [∅], because the argument value denotes [∅] and
    application is strict. *)
Example tot_app_den ρ :
  evalTm (tapp (tlam (TArr TNat TNat) tzero) (tlam TNat loop)) ρ = ∅.
Proof.
  assert (Hl : evalTm (tlam TNat loop) ρ = ∅) by apply evalTm_div_fun.
  assert (Happ : evalTm (tapp (tlam (TArr TNat TNat) tzero) (tlam TNat loop)) ρ
               = (f ← evalTm (tlam (TArr TNat TNat) tzero) ρ ;;
                  v ← evalTm (tlam TNat loop) ρ ;; image f {| v |})) by reflexivity.
  rewrite Happ, Hl.
  apply set_ext; apply Inc_def; intros x Hx; [ | destruct (not_In_Empty _ Hx) ].
  apply iUnion_IN in Hx. destruct Hx as [f [_ Hx]].
  rewrite iUnion_Vide in Hx. destruct (not_In_Empty _ Hx).
Qed.

(** ** The deeper obstruction: [fix] denotes the *set of all* fixed
    points, which can be multi-valued — so even the refinement
    [evalTm e ⊆ {| den_val v |}] fails, independently of CBN/CBV.

    [fix (λx:ι. x)] is the fixpoint of the identity, whose graph is the
    identity relation on [ω]; *every* natural is a fixed point, so the
    denotation is all of [ω], not a singleton. *)
Definition idgraph : ZFSet := iUnion Omega (fun a => {| ⟨ a , a ⟩ |}).

Lemma fixid_eval ρ :
  evalTm (tfix (tlam TNat (tvar 0))) ρ = ⦃ a ∈ dom idgraph | ⟨ a , a ⟩ ∈ idgraph ⦄.
Proof.
  cbn [evalTm evalTy env_cons].
  rewrite (Pi_Sing Omega (fun a => {| a |}) (fun a => a)) by (intros; reflexivity).
  rewrite iUnion_Sing_l. reflexivity.
Qed.

(** Its denotation contains at least two distinct values, so it is not
    contained in any singleton [{| den_val v |}].  Under a call-by-value
    semantics [fix (λx:ι. x)] is an (unproductive) value, so this is a
    genuine counterexample to the refinement — and switching evaluation
    order does not change the denotation. *)
Lemma fixid_multivalued ρ :
  natZ 0 ∈ evalTm (tfix (tlam TNat (tvar 0))) ρ
  /\ natZ 1 ∈ evalTm (tfix (tlam TNat (tvar 0))) ρ.
Proof.
  assert (Hedge : forall k : nat, ⟨ natZ k , natZ k ⟩ ∈ idgraph)
    by (intro k; apply IN_iUnion with (y := natZ k);
        [ apply natZ_mem_omega | apply IN_Sing ]).
  rewrite !fixid_eval. split.
  - apply In_P_Comp; [ exact (IN_dom _ _ _ (Hedge 0)) | exact (Hedge 0) ].
  - apply In_P_Comp; [ exact (IN_dom _ _ _ (Hedge 1)) | exact (Hedge 1) ].
Qed.

(* ================================================================== *)
(** * Small-step call-by-name reduction. *)
(* ================================================================== *)

(** The reduction itself ([lift]/[subst]/[value]/[step]/[multistep] and the
    [-->] / [-->*] notations) is defined in [PCFSyntax.v]; below we only
    record examples and which rules the *strict* model validates. *)

(** Examples. *)
Example ss_beta : tapp (tlam TNat (tvar 0)) tzero --> tzero.
Proof. apply S_beta. Qed.

Example ss_iszero : tiszero tzero --> ttrue.
Proof. apply S_iszero_zero. Qed.

Example ss_if : tif ttrue tzero (tsucc tzero) --> tzero.
Proof. apply S_if_true. Qed.

Example ss_fix_unroll T body :
  tfix (tlam T body) --> subst 0 (tfix (tlam T body)) body.
Proof. apply S_fix. Qed.

(** Call-by-name: β fires on the *unevaluated* argument, which is only
    reduced afterwards (when the substituted copy is demanded). *)
Example ss_arg_reduces :
  tapp (tlam TNat (tvar 0)) (tpred (tsucc tzero)) -->* tzero.
Proof.
  eapply ms_step.
  { apply S_beta. }
  cbn [subst Nat.compare].
  eapply ms_step.
  { apply S_pred_succ. apply v_zero. }
  apply ms_refl.
Qed.

(** And β fires even on a *divergent* argument — it is substituted in
    without being run.  ([loop] would never reduce to a value.) *)
Example ss_cbn_beta : tapp (tlam TNat tzero) loop --> tzero.
Proof. apply S_beta. Qed.

(* ================================================================== *)
(** * Which small-step rules are sound for the model?

    A rule is *sound* when [e --> e'] implies [evalTm e ρ = evalTm e' ρ].

    - The congruence rules ([S_app1], [S_succ], [S_pred], [S_iszero],
      [S_if], [S_fix_cong]) preserve denotations: [evalTm] is
      compositional, so equal denotations for the redex give equal
      denotations for the whole term.  They are sound exactly when the
      inner step is.
    - The value/primitive rules ([S_pred_zero], [S_pred_succ],
      [S_iszero_zero], [S_iszero_succ], [S_if_true], [S_if_false]) are
      sound — direct computations on the model (samples below).
    - [S_beta] is UNSOUND under call-by-name or call-by-value.  Soundness has nothing
      to do with evaluation order: the *denotational* [tapp] is strict in
      its argument ([v ← evalTm e2 ρ ;; …], hence [∅] as soon as [e2]
      denotes [∅]) and [evalTy] interprets arrows as *total* function
      graphs ([Pi]).  So a λ whose body diverges on some input denotes
      [∅], whereas its β-reduct can denote a value.  If anything CBN makes
      the gap wider: β now fires on unevaluated, possibly-divergent
      arguments (e.g. [(λx:ι. 0) loop --> 0]), and every such step over an
      argument denoting [∅] is unsound.  Making β sound needs a *non-strict
      / partial* model (lift the argument, or interpret arrows as partial
      functions), not a change of strategy.
    - [S_fix] (fix unrolling) is the [evalTm_fix_unfold] equation; it is
      sound under the same condition as β — the abstracted body must be a
      single total function — and inherits β's failure otherwise. *)

(** Two representative sound rules. *)
Lemma S_if_true_sound e1 e2 ρ : evalTm (tif ttrue e1 e2) ρ = evalTm e1 ρ.
Proof.
  cbn [evalTm]. rewrite iUnion_Sing_l.
  rewrite (Comp_const_in (evalTm e1 ρ) (fun _ => natZ 1 = natZ 1))
    by (intros; reflexivity).
  rewrite (Comp_const_out (evalTm e2 ρ) (fun _ => natZ 1 = natZ 0))
    by (intros; apply natZ_neq; discriminate).
  apply BinUnion_Vide_r.
Qed.

Lemma S_pred_zero_sound ρ : evalTm (tpred tzero) ρ = evalTm tzero ρ.
Proof. cbn [evalTm]. rewrite iUnion_Sing_l, natZPred_natZ. reflexivity. Qed.

(** [S_beta] is unsound.  [(λx:ι→ι. 0) (λy:ι. loop)] β-reduces to [0], but
    the two have different denotations: [∅] (the argument denotes [∅],
    application is strict) versus [{0}]. *)
Example beta_step :
  tapp (tlam (TArr TNat TNat) tzero) (tlam TNat loop) --> tzero.
Proof. apply (S_beta (TArr TNat TNat) tzero (tlam TNat loop)). Qed.

Remark S_beta_unsound :
  ~ (forall e e' ρ, e --> e' -> evalTm e ρ = evalTm e' ρ).
Proof.
  intro H.
  pose proof (H _ _ (fun _ => ∅) beta_step) as Heq.
  rewrite (tot_app_den (fun _ => ∅)) in Heq.
  cbn [evalTm] in Heq.
  assert (Hin : natZ 0 ∈ (∅ : ZFSet)) by (rewrite Heq; apply IN_Sing).
  exact (not_In_Empty _ Hin).
Qed.
