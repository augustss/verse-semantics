(* TimSTLC.v:

   A set-theoretic semantics for the simply-typed lambda calculus.

 *)

Require Import ZFSet.
Require Import ZFNotation.
Require Import Diagonal.
Require Import Syntax.

From Stdlib Require Import ssreflect.
From Stdlib Require Import Logic.Epsilon.
Require Import utils.all.
Require Timbda0.
Require Timbda1.
Require Lang.STLC.

Import Syntax.STLC.

(* ================================================================== *)
(** * Translating the standalone STLC ([Lang.STLC]) into Timbda

    We compile [Lang.STLC] types and terms into Timbda [Expr]s and show
    the translation is type preserving (into the [Syntax.STLC] judgement)
    and semantics preserving (against the [Timbda0] model below). *)
(* ================================================================== *)

Module Src := Lang.STLC.

(** Types: the base type to the Timbda type [Eimg Enat], arrows to
    [Elam]. *)
Fixpoint trTy (T : Src.Ty) : Expr :=
  match T with
  | Src.TNat       => Eimg Enat
  | Src.TArr T1 T2 => Elam (trTy T1) (trTy T2)
  end.

(** Terms: de Bruijn indices already agree, so the translation is
    homomorphic. *)
Fixpoint trTm (e : Src.Tm) : Expr :=
  match e with
  | Src.tvar n     => Evar n
  | Src.tlam T e   => Elam (trTy T) (trTm e)
  | Src.tapp e1 e2 => Eapp (trTm e1) (trTm e2)
  | Src.tcon n     => Econ n
  | Src.tadd e1 e2 => Eadd (trTm e1) (trTm e2)
  end.

Definition trCtx (Γ : Src.Ctx) : Ctx := fun n => trTy (Γ n).

(** A translated type is a Timbda [IsType], so a translated context is an
    [IsCtx]. *)
Lemma IsType_trTy : forall T, IsType (trTy T).
Proof. induction T; simpl; [ apply IT_nat | apply IT_lam; assumption ]. Qed.

Lemma IsCtx_trCtx : forall Γ, IsCtx (trCtx Γ).
Proof. intros Γ i. apply IsType_trTy. Qed.

(** Translation commutes with context extension. *)
Lemma trCtx_ext : forall Γ T,
  trCtx (Src.ctx_cons T Γ) = ctx_ext (trCtx Γ) (trTy T).
Proof. intros Γ T. extensionality n. destruct n; reflexivity. Qed.

(** ** Type preservation: a well-typed STLC term translates to a
    well-typed Timbda program. *)
Theorem type_preservation :
  forall Γ e T,
  Src.has_type Γ e T ->
  HasType (trCtx Γ) (trTm e) (trTy T).
Proof.
  induction 1 as
    [ Γ n
    | Γ T1 T2 e He IHe
    | Γ T1 T2 e1 e2 He1 IHe1 He2 IHe2
    | Γ n
    | Γ e1 e2 He1 IHe1 He2 IHe2 ];
    cbn [trTm trTy].
  - (* T_var *) apply HT_var. apply IsCtx_trCtx.
  - (* T_lam *) apply HT_lam.
    + apply IsType_trTy.
    + rewrite <- trCtx_ext. exact IHe.
  - (* T_app *) eapply HT_app; [ exact IHe1 | exact IHe2 ].
  - (* T_con *) apply HT_con. apply IsCtx_trCtx.
  - (* T_add *) apply HT_add; [ exact IHe1 | exact IHe2 ].
Qed.


Module Timbda0.
Import Timbda0.

(* environments don't matter for the interpretation of types
   as there is no dependency *)
Lemma IsType_irrel T : 
  IsType T -> forall ρ1 ρ2, eval T ρ1 = eval T ρ2.
Proof.
  induction 1; intros ρ1 ρ2; cbn.
  reflexivity.
  erewrite IHIsType1. f_equal. ext.
  erewrite IHIsType2. reflexivity.
Qed.

(* well-typed environments *)
Definition WellTyped (Gamma : Ctx) (rho : Env) : Prop :=
  forall i,  rho i ∈ eval (Gamma i) rho.

Theorem WellTyped_ext :
  forall Γ ρ T v,
  WellTyped Γ ρ -> IsCtx Γ -> IsType T -> v ∈ eval T ρ ->
  WellTyped (ctx_ext Γ T) (env_ext ρ v).
Proof.
  intros Γ ρ T v Hwt HΓ HT Hv [|i]; cbn; auto.
  erewrite IsType_irrel; eauto.
  specialize (Hwt i).
  erewrite IsType_irrel; eauto.
Qed.

(** [iUnion2_Sing_Inc], [Pi_Sing], [Pi_Sing_ex] are general [ZFSet] facts
    and now live in [ZFSet.v]. *)

(** Soundness: every well-typed expression is a subset of
    its type. *)


Theorem soundness :
  forall Γ e T,
  Γ ⊢ e ⦂ T ->
  forall ρ, WellTyped Γ ρ ->
  eval e ρ ⊆ eval T ρ.
Proof.
  intros Γ e T h. induction h; intros ρ Hwt; cbn[eval].

  - (* HT_con *)
    step_eval.
    apply Sing_Inc_IN. apply natZ_mem_omega.

  - (* HT_var *)
    apply Sing_Inc, Hwt.

  - (* HT_abs *)
    eapply Pi_Inc_Codomain.
    intros x xIn.
    eapply IHh, WellTyped_ext; eauto.
    eapply HasType_IsCtx in h.
    eapply IsCtx_inv; eauto.

  - (* HT_app: relational application — every edge of the (well-typed)
       function value lands its target in the codomain type *)
    specialize (IHh1 _ Hwt). specialize (IHh2 _ Hwt).
    cbn[eval] in IHh1.
    apply Inc_def. intros w Hw.
    apply iUnion_IN in Hw. destruct Hw as [f [Hf Hw]].
    apply iUnion_IN in Hw. destruct Hw as [z [Hz Hw]].
    apply image_elim in Hw. destruct Hw as [a [Ha Hedge]].
    apply IN_Sing_EQ in Ha. subst a.
    apply (Inc_IN _ _ _ IHh1) in Hf.
    apply (Inc_IN _ _ _ IHh2) in Hz.
    move: (Pi_edge_codomain _ _ _ _ _ Hf Hz Hedge) => h.
    erewrite IsType_irrel. eauto.
    move: (HasType_IsType _ _ _ h1) => h3. inversion h3.
    done.

  - (* HT_add *)
    specialize (IHh1 _ Hwt). specialize (IHh2 _ Hwt).
    apply iUnion2_Sing_Inc. intros y z yIn zIn.
    apply (Inc_IN _ _ _ IHh1) in yIn.
    apply (Inc_IN _ _ _ IHh2) in zIn.
    rewrite eval_tnat in yIn.
    rewrite eval_tnat in zIn.
    eapply IN_iUnion. eapply IN_Sing.
    rewrite rng_natId.
    destruct (In_omega _ yIn) as [n1 E1].
    destruct (In_omega _ zIn) as [n2 E2].
    rewrite <- E1, <- E2, natZAdd_natZ.
    apply natZ_mem_omega.
Qed.


(* Every Well-typed STLC program produces a singleton set *)

Theorem nontrivial :
  forall Γ e T,
  Γ ⊢ e ⦂ T ->
  forall ρ, WellTyped Γ ρ ->
  exists X, {| X |} = (eval e ρ).
Proof.
  intros Γ e T h. induction h; intros ρ Hwt; cbn[eval].

  - (* HT_con: [eval (Econ n) ρ = {| n |}] *)
    eexists. reflexivity.

  - (* HT_var: [eval (Evar i) ρ = {| ρ i |}] *)
    eexists. reflexivity.

  - (* HT_lam: [eval (Elam T e) ρ = Π[ a ∈ eval T ρ ] eval e (env_ext ρ a)].
       Every fibre is a singleton (IH), so the whole [Pi] is a singleton
       by [Pi_Sing_ex]. *)
    assert (HCtx : IsCtx Gamma)
      by (eapply IsCtx_inv, HasType_IsCtx; eauto).
    destruct (Pi_Sing_ex (eval T ρ) (fun a => eval e (env_ext ρ a)))
      as [f Hf].
    { intros a Ha.
      assert (Hwt' : WellTyped (ctx_ext Gamma T) (env_ext ρ a))
        by (apply WellTyped_ext; auto).
      destruct (IHh _ Hwt') as [X HX]. exists X. symmetry. exact HX. }
    exists f. symmetry. exact Hf.

  - (* HT_app: a single (in-domain, by [soundness]) edge of the singleton
       function value applied to the singleton argument *)
    destruct (IHh1 _ Hwt) as [f0 Hf0].
    destruct (IHh2 _ Hwt) as [v0 Hv0].
    pose proof (soundness _ _ _ h1 _ Hwt) as Hs1.
    pose proof (soundness _ _ _ h2 _ Hwt) as Hs2.
    assert (Hf0mem : In f0 (eval (Elam T1 T2) ρ)).
    { apply (Inc_IN _ _ _ Hs1). rewrite <- Hf0. apply IN_Sing. }
    assert (Hv0mem : In v0 (eval T1 ρ)).
    { apply (Inc_IN _ _ _ Hs2). rewrite <- Hv0. apply IN_Sing. }
    cbn[eval] in Hf0mem.
    exists (f0 [ v0 ]).
    rewrite <- Hf0, <- Hv0, !iUnion_Sing_l.
    symmetry. apply (image_Sing_of_pi _ _ _ _ Hf0mem Hv0mem).

  - (* HT_add: the sum of the values of [e1] and [e2] *)
    destruct (IHh1 _ Hwt) as [w1 Hw1].
    destruct (IHh2 _ Hwt) as [w2 Hw2].
    exists (natZAdd w1 w2).
    rewrite <- Hw1, <- Hw2, !iUnion_Sing_l. reflexivity.
Qed.


(** ** Semantics preservation (against this [Timbda0] model).

    Two facts.  First, the Timbda denotation of a *translated type* is
    exactly its STLC denotation [Src.evalTy].  Second, the Timbda
    denotation of a *translated term* is the singleton of its STLC value
    [Src.evalTm] (the [Timbda0] model packs each value as a singleton). *)

(* [env_ext] (Timbda) and [Src.env_cons] (STLC) are the same cons. *)
Lemma env_ext_cons : forall (ρ : Env) (a : ZFSet),
  env_ext ρ a = Src.env_cons a ρ.
Proof. intros ρ a. extensionality n. destruct n; reflexivity. Qed.

Lemma eval_trTy : forall T ρ, eval (trTy T) ρ = Src.evalTy T.
Proof.
  induction T as [| T1 IHT1 T2 IHT2]; intro ρ.
  - (* TNat *) apply eval_tnat.
  - (* TArr *)
    cbn [trTy]. rewrite eval_lam. cbn [Src.evalTy].
    rewrite IHT1. f_equal. extensionality a. apply IHT2.
Qed.

Theorem sem_preservation :
  forall Γ e T,
  Src.has_type Γ e T ->
  forall ρ, Src.models Γ ρ ->
  eval (trTm e) ρ = {| Src.evalTm e ρ |}.
Proof.
  induction 1 as
    [ Γ n
    | Γ T1 T2 e He IHe
    | Γ T1 T2 e1 e2 He1 IHe1 He2 IHe2
    | Γ n
    | Γ e1 e2 He1 IHe1 He2 IHe2 ];
    intros ρ HM.

  - (* tvar *)
    reflexivity.

  - (* tlam: the [Pi] of singleton fibres is the singleton of the graph *)
    cbn [trTm]. rewrite eval_lam. rewrite eval_trTy.
    assert (HYP : forall a, In a (Src.evalTy T1) ->
              eval (trTm e) (env_ext ρ a) = {| Src.evalTm e (Src.env_cons a ρ) |}).
    { intros a Ha. rewrite env_ext_cons. apply IHe.
      apply Src.models_cons; [ exact HM | exact Ha ]. }
    rewrite (Pi_Sing (Src.evalTy T1)
                     (fun a => eval (trTm e) (env_ext ρ a))
                     (fun a => Src.evalTm e (Src.env_cons a ρ)) HYP).
    cbn [Src.evalTm]. reflexivity.

  - (* tapp: relational application meets [applyFun] in-domain *)
    cbn [trTm]. rewrite eval_app.
    rewrite (IHe1 ρ HM). rewrite (IHe2 ρ HM). rewrite !iUnion_Sing_l.
    assert (Hf : In (Src.evalTm e1 ρ)
                    (Pi (Src.evalTy T1) (fun _ => Src.evalTy T2))).
    { pose proof (Src.soundness _ _ _ He1 ρ HM) as Hs1.
      cbn [Src.evalTy] in Hs1. exact Hs1. }
    assert (Hv : In (Src.evalTm e2 ρ) (Src.evalTy T1)).
    { exact (Src.soundness _ _ _ He2 ρ HM). }
    rewrite (image_Sing_of_pi _ _ _ _ Hf Hv).
    cbn [Src.evalTm]. reflexivity.

  - (* tcon *)
    reflexivity.

  - (* tadd *)
    cbn [trTm]. rewrite eval_add.
    rewrite (IHe1 ρ HM). rewrite (IHe2 ρ HM). rewrite !iUnion_Sing_l.
    cbn [Src.evalTm]. reflexivity.
Qed.

End Timbda0.

Module Timbda1.

Import Timbda1.

(** The generic [iUnion] lemmas [iUnion_Inc_index] and [iUnion_ext_mem]
    are general [ZFSet] facts and now live in [ZFSet.v]. *)

(** The type of naturals evaluates to the identity relation [natId]. *)
Lemma eval_tnat (ρ : Env) : eval (Eimg Enat) ρ = natId.
Proof.
  cbn[eval]. unfold iUnion_pat. rewrite iUnion_Sing_l. apply psnd_Couple.
Qed.

(** ** Type denotations are environment-independent. *)
Lemma IsType_irrel T :
  IsType T -> forall ρ1 ρ2, eval T ρ1 = eval T ρ2.
Proof.
  induction 1; intros ρ1 ρ2; cbn[eval].
  - reflexivity.
  - assert (HPi : Pi (eval T ρ1) (fun ab => eval T' (env_ext ρ1 (psnd ab)))
                = Pi (eval T ρ2) (fun ab => eval T' (env_ext ρ2 (psnd ab)))).
    { f_equal.
      - apply IHIsType1.
      - ext. apply IHIsType2. }
    rewrite HPi. reflexivity.
Qed.

(** ** Diagonality.

    Every type denotes a set of *diagonal* pairs [⟨v,v⟩].  This is the
    key invariant that makes subset soundness go through: it lets a
    variable's diagonal value sit inside its type, and it forces the
    [sndRel] and [thdRel] projections of a function value to coincide.

    The general lemmas about diagonal relations ([IsDiag], [diag_eq],
    [sndRel_eq_thdRel], [diag_psnd_inj], [diag_pfst_inj]) live in
    [lib/Diagonal.v]; below we connect diagonality to the type semantics. *)

Lemma diag_type T : IsType T -> forall ρ, IsDiag (eval T ρ).
Proof.
  induction 1; intros ρ.
  - (* IT_nat *)
    rewrite eval_tnat. intros p Hp.
    apply IN_natId_EXType in Hp. destruct Hp as [n ->].
    rewrite psnd_Couple. reflexivity.
  - (* IT_lam T T' *)
    cbn[eval]. intros p Hp.
    apply iUnion_IN in Hp. destruct Hp as [h [Hh Hp]].
    (* the function value is separated by [isFunction]; drop the filter *)
    apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hp.
    apply IN_Sing_EQ in Hp. subst p.
    rewrite psnd_Couple. f_equal.
    apply (sndRel_eq_thdRel (eval T ρ)
             (fun ab => eval T' (env_ext ρ (psnd ab)))).
    + apply IHIsType1.
    + intros a Ha. apply IHIsType2.
    + exact Hh.
Qed.

(** A diagonal value can be folded into its own diagonal pair. *)
Lemma diag_self T ρ ab :
  IsType T -> ab ∈ eval T ρ -> ⟨ psnd ab , psnd ab ⟩ ∈ eval T ρ.
Proof.
  intros HT Hab. pose proof (diag_type T HT ρ ab Hab) as Hd.
  rewrite <- Hd. exact Hab.
Qed.

(** ** Well-typed environments.

    A variable evaluates to its diagonal pair, so well-typedness records
    that this diagonal pair sits in the (diagonal) type denotation. *)

Definition WellTyped (Γ : Ctx) (ρ : Env) : Prop :=
  forall i, ⟨ ρ i , ρ i ⟩ ∈ eval (Γ i) ρ.

Theorem WellTyped_ext :
  forall Γ ρ T v,
  WellTyped Γ ρ -> IsCtx Γ -> IsType T -> ⟨ v , v ⟩ ∈ eval T ρ ->
  WellTyped (ctx_ext Γ T) (env_ext ρ v).
Proof.
  intros Γ ρ T v Hwt HΓ HT Hv [|i]; cbn.
  - erewrite IsType_irrel; eauto.
  - erewrite IsType_irrel; eauto.
Qed.

(** ** Soundness: every well-typed expression is a subset of its type. *)

Theorem soundness :
  forall Γ e T,
  Γ ⊢ e ⦂ T ->
  forall ρ, WellTyped Γ ρ ->
  eval e ρ ⊆ eval T ρ.
Proof.
  intros Γ e T h. induction h; intros ρ Hwt.

  - (* HT_con *)
    rewrite eval_con eval_tnat.
    apply Sing_Inc_IN. apply pair_self_mem_natId.

  - (* HT_var *)
    rewrite eval_var. apply Sing_Inc_IN. apply Hwt.

  - (* HT_lam: [Elam T e ⦂ Elam T T'] *)
    rewrite !eval_lam.
    apply iUnion_Inc_index.
    apply Pi_Inc_Codomain. intros ab Hab.
    apply IHh. apply WellTyped_ext; auto.
    + eapply IsCtx_inv, HasType_IsCtx; eauto.
    + apply diag_self; auto.

  - (* HT_app: [Eapp e1 e2 ⦂ T2] *)
    specialize (IHh1 _ Hwt). specialize (IHh2 _ Hwt).
    assert (HT2 : IsType T2).
    { pose proof (HasType_IsType _ _ _ h1) as HT12. inversion HT12; assumption. }
    rewrite eval_app. unfold iUnion_pat.
    apply Inc_def. intros w Hw.
    apply iUnion_IN in Hw. destruct Hw as [p1 [Hp1 Hw]].
    apply iUnion_IN in Hw. destruct Hw as [p2 [Hp2 Hw]].
    apply iUnion_IN in Hw. destruct Hw as [p3 [Hp3 Hw]].
    (* w = ⟨ psnd p2 , psnd p2 ⟩ *)
    apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hw.
    apply IN_Sing_EQ in Hw.
    (* p1 lives in the function type, so psnd p1 = thdRel of a graph h0 *)
    apply (Inc_IN _ _ _ IHh1) in Hp1. cbn[eval] in Hp1.
    apply iUnion_IN in Hp1. destruct Hp1 as [h0 [Hh0 Hp1]].
    (* the function value is separated by [isFunction]; drop the filter *)
    apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hp1.
    apply IN_Sing_EQ in Hp1. subst p1. rewrite psnd_Couple in Hp2.
    (* p2 in thdRel h0: comes from a graph element q *)
    unfold iUnion_pat in Hp2.
    apply iUnion_IN in Hp2. destruct Hp2 as [q [Hq Hp2]].
    apply IN_Sing_EQ in Hp2. subst p2. rewrite !psnd_Couple in Hw.
    (* the graph element q = ⟨qa,qc⟩ with qc in the codomain type *)
    apply In_Pi_inv in Hh0. destruct Hh0 as [Hsub _].
    pose proof (Inc_IN _ _ _ Hsub Hq) as Hq'.
    apply IN_Prod_EX in Hq'. destruct Hq' as [qa [qc [Hqa [Hqc Heq]]]].
    subst q. rewrite !psnd_Couple in Hw.
    apply iUnion_IN in Hqc. destruct Hqc as [a' [Ha' Hqc]].
    (* qc ∈ eval T2 ρ by env-irrelevance, and is diagonal *)
    rewrite (IsType_irrel T2 HT2 (env_ext ρ (psnd a')) ρ) in Hqc.
    pose proof (diag_type _ HT2 ρ qc Hqc) as Dc.
    rewrite Hw -Dc. exact Hqc.

  - (* HT_add *)
    specialize (IHh1 _ Hwt). specialize (IHh2 _ Hwt).
    rewrite eval_tnat in IHh1 IHh2.
    rewrite eval_add eval_tnat.
    apply Inc_def. intros w Hw.
    apply iUnion_IN in Hw. destruct Hw as [v1 [Hv1 Hw]].
    apply iUnion_IN in Hw. destruct Hw as [v2 [Hv2 Hw]].
    apply IN_Sing_EQ in Hw. subst w.
    apply (Inc_IN _ _ _ IHh1) in Hv1.
    apply (Inc_IN _ _ _ IHh2) in Hv2.
    apply IN_natId_EXType in Hv1. destruct Hv1 as [n1 E1].
    apply IN_natId_EXType in Hv2. destruct Hv2 as [n2 E2].
    rewrite E1 E2 !psnd_Couple natZAdd_natZ.
    apply pair_self_mem_natId.
Qed.

(** ** Nontriviality: every well-typed expression denotes a singleton.

    Unlike [Timbda0], the pair semantics builds function values by
    separating two projected relations through an [isFunction] filter and
    then, on application, re-selecting the matching graph edge.  The proof
    therefore needs (a) that those projected relations really are
    functions, and (b) that application against a diagonal argument hits
    exactly one edge.  Both facts hinge on diagonality of the domain
    type. *)

(** A relation read off a graph [h ∈ Pi A B] by projecting an injective
    key [kf] out of the domain component and a value [vf] out of the
    codomain component is itself a function: distinct edges with the same
    key force the same domain pair (by injectivity of [kf]), hence the
    same codomain pair (by functionality of [h]), hence the same value. *)
Lemma proj_isFunction (A : ZFSet) (B : ZFSet -> ZFSet) (h : ZFSet)
      (kf vf : ZFSet -> ZFSet) :
  (forall ab ab', ab ∈ A -> ab' ∈ A -> kf ab = kf ab' -> ab = ab') ->
  h ∈ Pi A B ->
  isFunction (iUnion_pat h (fun ab cd => {| ⟨ kf ab , vf cd ⟩ |})).
Proof.
  intros Hkf Hh a b1 b2 H1 H2.
  apply In_Pi_inv in Hh. destruct Hh as [Hsub [_ Hfn]].
  unfold iUnion_pat in H1, H2.
  apply iUnion_IN in H1. destruct H1 as [q1 [Hq1 He1]].
  apply iUnion_IN in H2. destruct H2 as [q2 [Hq2 He2]].
  apply IN_Sing_EQ in He1, He2.
  pose proof (Inc_IN _ _ _ Hsub Hq1) as Hp1.
  pose proof (Inc_IN _ _ _ Hsub Hq2) as Hp2.
  apply IN_Prod_EX in Hp1. destruct Hp1 as [qa1 [qc1 [Hqa1 [_ Eq1]]]].
  apply IN_Prod_EX in Hp2. destruct Hp2 as [qa2 [qc2 [Hqa2 [_ Eq2]]]].
  subst q1 q2. rewrite !pfst_Couple !psnd_Couple in He1 He2.
  pose proof (Couple_inj_left _ _ _ _ He1) as Ea1.
  pose proof (Couple_inj_right _ _ _ _ He1) as Eb1.
  pose proof (Couple_inj_left _ _ _ _ He2) as Ea2.
  pose proof (Couple_inj_right _ _ _ _ He2) as Eb2.
  assert (Eqa : qa1 = qa2).
  { apply (Hkf qa1 qa2 Hqa1 Hqa2). rewrite <- Ea1, <- Ea2. reflexivity. }
  subst qa2.
  assert (Eqc : qc1 = qc2) by exact (Hfn qa1 qc1 qc2 Hq1 Hq2).
  subst qc2. rewrite Eb1 Eb2. reflexivity.
Qed.

(** Comprehending a singleton by a predicate that already holds of its
    element leaves it unchanged. *)
Lemma Comp_Sing_true (p : ZFSet) (P : ZFSet -> Prop) :
  P p -> Comp {| p |} P = {| p |}.
Proof.
  intro HP. apply set_ext.
  - apply Comp_Inc.
  - apply Sing_Inc_IN. apply In_P_Comp; [ apply IN_Sing | exact HP ].
Qed.

(** The relational application packed into the [Eapp] arm: iterating a
    function [G] and keeping the diagonal of the output whenever the input
    matches [a1].  When [G] is a single-valued relation containing the
    edge [⟨a1,b0⟩], exactly that edge survives, leaving [{|⟨b0,b0⟩|}]. *)
Lemma app_iUnion_Sing (G a1 b0 : ZFSet) :
  isFunction G ->
  (forall q, q ∈ G -> q = ⟨ pfst q , psnd q ⟩) ->
  ⟨ a1 , b0 ⟩ ∈ G ->
  ('⟨ a , b ⟩ ← G ;; ⦃ _ ∈ {| ⟨ b , b ⟩ |} | a = a1 ⦄) = {| ⟨ b0 , b0 ⟩ |}.
Proof.
  intros HF Hrel Hedge. unfold iUnion_pat. apply set_ext.
  - apply iUnion_Inc. intros q Hq.
    apply Inc_def. intros w Hw.
    pose proof (Inc_IN _ _ _ (Comp_Inc _ _) Hw) as HwS.
    apply In_Comp_P in Hw.
    apply IN_Sing_EQ in HwS. subst w.
    (* the kept edge has key [a1], so by functionality its value is [b0] *)
    pose proof (Hrel q Hq) as Hqc.
    assert (Hq' : ⟨ a1 , psnd q ⟩ ∈ G) by (rewrite <- Hw, <- Hqc; exact Hq).
    pose proof (HF a1 (psnd q) b0 Hq' Hedge) as Eb. rewrite Eb. apply IN_Sing.
  - apply Sing_Inc_IN.
    apply IN_iUnion with (y := ⟨ a1 , b0 ⟩); [ exact Hedge | ].
    rewrite pfst_Couple psnd_Couple.
    apply In_P_Comp; [ apply IN_Sing | reflexivity ].
Qed.

Theorem nontrivial :
  forall Γ e T,
  Γ ⊢ e ⦂ T ->
  forall ρ, WellTyped Γ ρ ->
  exists X, {| X |} = eval e ρ.
Proof.
  intros Γ e T h. induction h; intros ρ Hwt.

  - (* HT_con *)
    rewrite eval_con. eexists. reflexivity.

  - (* HT_var *)
    rewrite eval_var. eexists. reflexivity.

  - (* HT_lam: [eval (Elam T e) ρ] is the singleton of its packed
       function value [⟨f,g⟩], once the two projected relations are shown
       to be functions. *)
    rewrite eval_lam.
    assert (HCtx : IsCtx Gamma)
      by (eapply IsCtx_inv, HasType_IsCtx; eauto).
    set (B := fun ab => eval e (env_ext ρ (psnd ab))).
    (* each fibre is a singleton, so the [Pi] is a singleton {| h0 |} *)
    destruct (Pi_Sing_ex (eval T ρ) B) as [h0 Hh0].
    { intros ab Hab.
      assert (Hwt' : WellTyped (ctx_ext Gamma T) (env_ext ρ (psnd ab)))
        by (apply WellTyped_ext; auto using diag_self).
      destruct (IHh _ Hwt') as [X HX]. exists X. unfold B. symmetry. exact HX. }
    assert (Hh0mem : h0 ∈ Pi (eval T ρ) B)
      by (rewrite Hh0; apply IN_Sing).
    pose proof (diag_type T H ρ) as HdiagA.
    (* the two projected relations are functions *)
    set (f := '⟨ ab , cd ⟩ ← h0 ;; {| ⟨ psnd ab , pfst cd ⟩ |}).
    set (g := '⟨ ab , cd ⟩ ← h0 ;; {| ⟨ pfst ab , psnd cd ⟩ |}).
    assert (Hf : isFunction f).
    { apply (proj_isFunction (eval T ρ) B h0 psnd pfst); auto.
      apply diag_psnd_inj; exact HdiagA. }
    assert (Hg : isFunction g).
    { apply (proj_isFunction (eval T ρ) B h0 pfst psnd); auto.
      apply diag_pfst_inj; exact HdiagA. }
    exists (⟨ f , g ⟩).
    rewrite Hh0 iUnion_Sing_l. fold f g.
    symmetry. apply Comp_Sing_true. split; assumption.

  - (* HT_app: select the unique edge of the function value at the
       (diagonal) argument. *)
    assert (HT1 : IsType T1 /\ IsType T2).
    { pose proof (HasType_IsType _ _ _ h1) as HT12. inversion HT12; auto. }
    destruct HT1 as [HT1 HT2].
    destruct (IHh1 _ Hwt) as [p1 Hp1].
    destruct (IHh2 _ Hwt) as [p2 Hp2].
    (* [p1] is the packed function value; recover its graph [h0] *)
    pose proof (soundness _ _ _ h1 _ Hwt) as Hsnd1.
    assert (Hp1mem : p1 ∈ eval (Elam T1 T2) ρ)
      by (apply (Inc_IN _ _ _ Hsnd1); rewrite <- Hp1; apply IN_Sing).
    rewrite eval_lam in Hp1mem.
    apply iUnion_IN in Hp1mem. destruct Hp1mem as [h0 [Hh0 Hp1mem]].
    apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hp1mem.
    apply IN_Sing_EQ in Hp1mem.
    set (G := '⟨ ab , cd ⟩ ← h0 ;; {| ⟨ pfst ab , psnd cd ⟩ |}) in *.
    (* [G = psnd p1]; it is a function and a relation *)
    assert (HGfun : isFunction G).
    { apply (proj_isFunction (eval T1 ρ)
               (fun ab => eval T2 (env_ext ρ (psnd ab))) h0 pfst psnd); auto.
      apply diag_pfst_inj. apply (diag_type T1 HT1). }
    assert (HGrel : forall q, q ∈ G -> q = ⟨ pfst q , psnd q ⟩).
    { intros q Hq. unfold G, iUnion_pat in Hq.
      apply iUnion_IN in Hq. destruct Hq as [p [_ Hq]].
      apply IN_Sing_EQ in Hq. subst q. rewrite pfst_Couple psnd_Couple. reflexivity. }
    (* the argument [p2] is diagonal and in [eval T1 ρ] *)
    pose proof (soundness _ _ _ h2 _ Hwt) as Hsnd2.
    assert (Hp2mem : p2 ∈ eval T1 ρ)
      by (apply (Inc_IN _ _ _ Hsnd2); rewrite <- Hp2; apply IN_Sing).
    pose proof (diag_type T1 HT1 ρ p2 Hp2mem) as Hp2diag.
    set (a1 := psnd p2).
    (* [h0] is total over [eval T1 ρ], so [G] has an edge at [a1] *)
    apply In_Pi_inv in Hh0. destruct Hh0 as [_ [Htot _]].
    destruct (Htot p2 Hp2mem) as [cd [_ Hcd]].
    set (b0 := psnd cd).
    assert (Hedge : ⟨ a1 , b0 ⟩ ∈ G).
    { unfold G, iUnion_pat.
      apply IN_iUnion with (y := ⟨ p2 , cd ⟩); [ exact Hcd | ].
      rewrite pfst_Couple psnd_Couple.
      unfold a1, b0. rewrite <- (diag_eq p2 Hp2diag). apply IN_Sing. }
    (* assemble *)
    exists (⟨ b0 , b0 ⟩). rewrite eval_app -Hp1.
    unfold iUnion_pat at 1. rewrite iUnion_Sing_l.
    rewrite Hp1mem psnd_Couple -Hp2.
    (* reduce the inner argument-iteration over the singleton [eval e2 ρ] *)
    transitivity ('⟨ a , b ⟩ ← G ;;
                  ⦃ _ ∈ {| ⟨ b , b ⟩ |} | a = a1 ⦄).
    + symmetry. apply app_iUnion_Sing; auto.
    + unfold iUnion_pat. apply iUnion_ext_mem. intros y _.
      rewrite iUnion_Sing_l. reflexivity.

  - (* HT_add *)
    rewrite eval_add.
    destruct (IHh1 _ Hwt) as [p1 Hp1].
    destruct (IHh2 _ Hwt) as [p2 Hp2].
    exists (⟨ psnd p1 + psnd p2 , psnd p1 + psnd p2 ⟩).
    rewrite <- Hp1, <- Hp2, !iUnion_Sing_l. reflexivity.
Qed.

End Timbda1.
  
