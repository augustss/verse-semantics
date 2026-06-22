(* TimIntersection.v:

   Set-theoretic soundness for the Coppo–Dezani intersection type
   system [Syntax.IntersectionTypes], interpreted against the [Timbda0]
   semantics.

   This is the analogue, for intersection types, of the [Timbda0]
   soundness theorem in [TimSTLC.v].  Two things change relative to the
   simply-typed case:

   - The typing judgement [HasType] is *mutually* defined with
     [HasDomType] (an expression at an intersection of types), so the
     proofs go by mutual induction (via a [Combined Scheme]).

   - The intersection type [Eequal T TS] denotes the set-theoretic
     intersection, since [eval (Eequal e1 e2) ρ = eval e1 ρ ∩ eval e2 ρ].

   A further subtlety: unlike STLC, [HasType] does *not* preserve
   [IsType].  A variable may be typed at a full intersection (rule
   [HT_var] with [InDT_one] on an intersection context entry), and a
   lambda body may have an intersection codomain.  So the [IsType_irrel]
   lemma of [TimSTLC.v] is not available verbatim.  We instead use a
   weaker well-formedness predicate [Wf] (closed under intersection)
   that *is* preserved by [HasType]/[HasDomType] and still yields
   environment-irrelevance of the denotation — exactly what soundness
   needs in the application case.

   The main result is [soundness]: every well-typed expression is a
   subset of the denotation of its type. *)

Require Import ZFSet.
Require Import ZFNotation.
Require Import Syntax.

From Stdlib Require Import ssreflect.
From Stdlib Require Import Logic.Epsilon.
Require Import utils.all.
Require Import Timbda0.
Require Lang.Intersection.

Import IntersectionTypes.

(** ** Mutual induction principles for the (mutually defined) judgements. *)

Scheme HasType_min := Minimality for HasType Sort Prop
  with HasDomType_min := Minimality for HasDomType Sort Prop.
Combined Scheme HasType_HasDomType_min from HasType_min, HasDomType_min.

Scheme IsType_min := Minimality for IsType Sort Prop
  with IsDomType_min := Minimality for IsDomType Sort Prop.
Combined Scheme IsType_IsDomType_min from IsType_min, IsDomType_min.

(** The binary-intersection laws [BinInter_Inc_l], [BinInter_Inc_r],
    [Inc_BinInter] (used because [eval (Eequal _ _) = BinInter]) are
    general [ZFSet] facts and live in [ZFSet.v]. *)

(** ** Well-formed types.

    [Wf] is the closure of [Eimg Enat] under [Elam] and [Eequal]
    (intersection).  It is weaker than [IsType] / [IsDomType] (it does
    not constrain where intersections may appear), but, crucially, it is
    preserved by typing and is enough for environment-irrelevance.  *)

Inductive Wf : Expr -> Prop :=
  | Wf_nat : Wf (Eimg Enat)
  | Wf_lam T T' : Wf T -> Wf T' -> Wf (Elam T T')
  | Wf_inter T TS : Wf T -> Wf TS -> Wf (Eequal T TS).

(** A well-formed type is closed, so its denotation does not depend on
    the environment.  Mirrors [TimSTLC.Timbda0.IsType_irrel]. *)
Lemma Wf_irrel T : Wf T -> forall ρ1 ρ2, eval T ρ1 = eval T ρ2.
Proof.
  induction 1; intros ρ1 ρ2; cbn[eval].
  - reflexivity.
  - erewrite IHWf1. f_equal. ext. erewrite IHWf2. reflexivity.
  - erewrite IHWf1. erewrite IHWf2. reflexivity.
Qed.

(** Every [IsType] / [IsDomType] is [Wf]; in particular the entries of a
    well-formed context are [Wf]. *)
Lemma Wf_of_IsType_and :
  (forall T, IsType T -> Wf T) /\ (forall T, IsDomType T -> Wf T).
Proof.
  apply IsType_IsDomType_min.
  - apply Wf_nat.
  - intros T T' _ QT _ PT'. apply Wf_lam; assumption.
  - intros T _ PT. exact PT.
  - intros T TS _ PT _ QTS. apply Wf_inter; assumption.
Qed.

(* [proj1]/[proj2] are shadowed by ZFSet pair projections, so we
   destruct the conjunctions explicitly throughout. *)
Lemma IsType_Wf : forall T, IsType T -> Wf T.
Proof. destruct Wf_of_IsType_and as [H _]. exact H. Qed.
Lemma IsDomType_Wf : forall T, IsDomType T -> Wf T.
Proof. destruct Wf_of_IsType_and as [_ H]. exact H. Qed.

(** A component of a well-formed (domain) type is itself well-formed. *)
Lemma InDomType_Wf T S : InDomType T S -> Wf S -> Wf T.
Proof.
  induction 1; intros HS.
  - exact HS.
  - inversion HS; subst; assumption.
  - inversion HS; subst. apply IHInDomType. assumption.
Qed.

(** Semantically, an intersection is included in each of its components,
    so the denotation of the whole domain type sits inside that of any
    component.  This drives the [HT_var] case: a variable may be typed
    at any component of its context type. *)
Lemma InDomType_Inc T S ρ : InDomType T S -> eval S ρ ⊆ eval T ρ.
Proof.
  intro Hin. induction Hin.
  - apply Inc_refl.
  - rewrite eval_equal. apply BinInter_Inc_l.
  - rewrite eval_equal.
    eapply Inc_tran; [ apply BinInter_Inc_r | exact IHHin ].
Qed.

(** ** Context well-formedness preserved under typing. *)

Lemma IsCtx_inv Γ T : IsCtx (ctx_ext Γ T) -> IsCtx Γ.
Proof.
  intros h i. specialize (h (S i)).
  rewrite ctx_ext_succ in h. exact h.
Qed.

Lemma IsCtx_of_HasType_and :
  (forall Γ e T, HasType Γ e T -> IsCtx Γ) /\
  (forall Γ e T, HasDomType Γ e T -> IsCtx Γ).
Proof.
  apply HasType_HasDomType_min; intros; eauto using IsCtx_inv.
Qed.

Lemma HasType_IsCtx : forall Γ e T, HasType Γ e T -> IsCtx Γ.
Proof. destruct IsCtx_of_HasType_and as [H _]. exact H. Qed.

(** ** Well-formedness of the result type preserved under typing. *)

Lemma Wf_of_HasType_and :
  (forall Γ e T, HasType Γ e T -> Wf T) /\
  (forall Γ e T, HasDomType Γ e T -> Wf T).
Proof.
  apply HasType_HasDomType_min.
  - (* HT_con *) intros Gamma n HΓ. apply Wf_nat.
  - (* HT_var *) intros Gamma i T HΓ Hin.
    apply (InDomType_Wf T (Gamma i) Hin). apply IsDomType_Wf, (HΓ i).
  - (* HT_lam *) intros Gamma T T' e HdomT Hbody IH.
    apply Wf_lam; [ apply IsDomType_Wf; exact HdomT | exact IH ].
  - (* HT_app: invert [Wf (Elam T1 T2)] *)
    intros Gamma T1 T2 e1 e2 Hd1 IH1 Hd2 IH2. inversion IH1; subst; assumption.
  - (* HT_add *) intros Gamma e1 e2 Hd1 IH1 Hd2 IH2. apply Wf_nat.
  - (* HDT_one *) intros Γ e T Hd IH. exact IH.
  - (* HDT_inter *) intros Γ e T TS Hd1 IH1 Hd2 IH2.
    apply Wf_inter; [ exact IH1 | exact IH2 ].
Qed.

Lemma HasType_Wf : forall Γ e T, HasType Γ e T -> Wf T.
Proof. destruct Wf_of_HasType_and as [H _]. exact H. Qed.

(** ** Well-typed environments.

    As in the STLC development, a variable evaluates to (the singleton
    of) [ρ i], which must sit inside the denotation of its context
    type. *)

Definition WellTyped (Γ : Ctx) (ρ : Env) : Prop :=
  forall i, ρ i ∈ eval (Γ i) ρ.

Theorem WellTyped_ext Γ ρ T v :
  WellTyped Γ ρ -> IsCtx Γ -> Wf T -> v ∈ eval T ρ ->
  WellTyped (ctx_ext Γ T) (env_ext ρ v).
Proof.
  intros Hwt HΓ HT Hv [|i]; cbn.
  - erewrite Wf_irrel; eauto.
  - erewrite Wf_irrel; eauto using IsDomType_Wf.
Qed.

(** [iUnion2_Sing_Inc] (the subset rule for the [Eapp]/[Eadd] shape) is a
    general [ZFSet] fact and lives in [ZFSet.v]. *)

(** ** Soundness: every well-typed expression is a subset of its type. *)

Theorem soundness_and :
  (forall Γ e T, HasType Γ e T ->
     forall ρ, WellTyped Γ ρ -> eval e ρ ⊆ eval T ρ) /\
  (forall Γ e T, HasDomType Γ e T ->
     forall ρ, WellTyped Γ ρ -> eval e ρ ⊆ eval T ρ).
Proof.
  apply HasType_HasDomType_min.

  - (* HT_con: [{| n |} ⊆ Omega] *)
    intros Gamma n HΓ ρ Hwt.
    rewrite eval_con eval_tnat.
    apply Sing_Inc_IN. apply natZ_mem_omega.

  - (* HT_var: the value sits in every component of its context type *)
    intros Gamma i T HΓ Hin ρ Hwt.
    rewrite eval_var. apply Sing_Inc_IN.
    apply (Inc_IN _ _ _ (InDomType_Inc T (Gamma i) ρ Hin)). exact (Hwt i).

  - (* HT_lam *)
    intros Gamma T T' e HdomT Hbody IH ρ Hwt.
    rewrite !eval_lam. apply Pi_Inc_Codomain. intros x xIn.
    apply IH. apply WellTyped_ext.
    + exact Hwt.
    + eapply IsCtx_inv. eapply HasType_IsCtx. exact Hbody.
    + apply IsDomType_Wf. exact HdomT.
    + exact xIn.

  - (* HT_app (relational application): each edge of the function value
       lands its target in the codomain type; the argument is given an
       intersection type [HasDomType] *)
    intros Gamma T1 T2 e1 e2 Hd1 IH1 Hd2 IH2 ρ Hwt.
    specialize (IH1 _ Hwt). specialize (IH2 _ Hwt). cbn[eval] in IH1.
    rewrite eval_app.
    apply Inc_def. intros w Hw.
    apply iUnion_IN in Hw. destruct Hw as [f [Hf Hw]].
    apply iUnion_IN in Hw. destruct Hw as [z [Hz Hw]].
    apply image_elim in Hw. destruct Hw as [a [Ha Hedge]].
    apply IN_Sing_EQ in Ha. subst a.
    apply (Inc_IN _ _ _ IH1) in Hf.
    apply (Inc_IN _ _ _ IH2) in Hz.
    pose proof (Pi_edge_codomain _ _ _ _ _ Hf Hz Hedge) as hmem.
    assert (HwfT2 : Wf T2).
    { pose proof (HasType_Wf _ _ _ Hd1) as HwfL. inversion HwfL; subst; assumption. }
    rewrite (Wf_irrel T2 HwfT2 (env_ext ρ z) ρ) in hmem.
    exact hmem.

  - (* HT_add *)
    intros Gamma e1 e2 Hd1 IH1 Hd2 IH2 ρ Hwt.
    specialize (IH1 _ Hwt). specialize (IH2 _ Hwt).
    apply iUnion2_Sing_Inc. intros y z yIn zIn.
    apply (Inc_IN _ _ _ IH1) in yIn.
    apply (Inc_IN _ _ _ IH2) in zIn.
    rewrite eval_tnat in yIn. rewrite eval_tnat in zIn.
    eapply IN_iUnion. eapply IN_Sing.
    rewrite rng_natId.
    destruct (In_omega _ yIn) as [n1 E1].
    destruct (In_omega _ zIn) as [n2 E2].
    rewrite <- E1, <- E2, natZAdd_natZ.
    apply natZ_mem_omega.

  - (* HDT_one *)
    intros Γ e T Hd IH ρ Hwt. exact (IH ρ Hwt).

  - (* HDT_inter: [eval e ⊆ eval T ∩ eval TS] *)
    intros Γ e T TS Hd1 IH1 Hd2 IH2 ρ Hwt.
    rewrite eval_equal.
    apply Inc_BinInter; [ exact (IH1 ρ Hwt) | exact (IH2 ρ Hwt) ].
Qed.

(** The headline soundness theorem, for the main judgement. *)
Theorem soundness :
  forall Γ e T,
  HasType Γ e T ->
  forall ρ, WellTyped Γ ρ -> eval e ρ ⊆ eval T ρ.
Proof. destruct soundness_and as [H _]. exact H. Qed.

(** … and for an expression at an intersection of types. *)
Theorem soundness_dom :
  forall Γ e T,
  HasDomType Γ e T ->
  forall ρ, WellTyped Γ ρ -> eval e ρ ⊆ eval T ρ.
Proof. destruct soundness_and as [_ H]. exact H. Qed.

(** ** Nontriviality: every well-typed expression denotes a singleton.

    The conclusion depends only on the *expression* (and environment),
    not on its type, so it is unchanged by intersection: an expression
    that has several types still denotes the same single value.  These
    are the analogues of [TimSTLC.Timbda0.nontrivial].  The singleton-
    product lemmas [Pi_Sing] / [Pi_Sing_ex] are general [ZFSet] facts and
    live in [ZFSet.v]. *)

Theorem nontrivial_and :
  (forall Γ e T, HasType Γ e T ->
     forall ρ, WellTyped Γ ρ -> exists X, {| X |} = eval e ρ) /\
  (forall Γ e T, HasDomType Γ e T ->
     forall ρ, WellTyped Γ ρ -> exists X, {| X |} = eval e ρ).
Proof.
  apply HasType_HasDomType_min.

  - (* HT_con *)
    intros Gamma n HΓ ρ Hwt. rewrite eval_con. eexists. reflexivity.

  - (* HT_var *)
    intros Gamma i T HΓ Hin ρ Hwt. rewrite eval_var. eexists. reflexivity.

  - (* HT_lam: every fibre is a singleton (IH), so the [Pi] is *)
    intros Gamma T T' e HdomT Hbody IH ρ Hwt.
    rewrite eval_lam.
    destruct (Pi_Sing_ex (eval T ρ) (fun a => eval e (env_ext ρ a)))
      as [f Hf].
    { intros a Ha.
      assert (Hwt' : WellTyped (ctx_ext Gamma T) (env_ext ρ a)).
      { apply WellTyped_ext.
        - exact Hwt.
        - eapply IsCtx_inv. eapply HasType_IsCtx. exact Hbody.
        - apply IsDomType_Wf. exact HdomT.
        - exact Ha. }
      destruct (IH _ Hwt') as [X HX]. exists X. symmetry. exact HX. }
    exists f. symmetry. exact Hf.

  - (* HT_app: a single (in-domain, by soundness) edge of the singleton
       function value applied to the singleton argument *)
    intros Gamma T1 T2 e1 e2 Hd1 IH1 Hd2 IH2 ρ Hwt.
    destruct (IH1 _ Hwt) as [f0 Hf0].
    destruct (IH2 _ Hwt) as [v0 Hv0].
    pose proof (soundness _ _ _ Hd1 _ Hwt) as Hs1.
    pose proof (soundness_dom _ _ _ Hd2 _ Hwt) as Hs2.
    assert (Hf0mem : In f0 (eval (Elam T1 T2) ρ)).
    { apply (Inc_IN _ _ _ Hs1). rewrite <- Hf0. apply IN_Sing. }
    assert (Hv0mem : In v0 (eval T1 ρ)).
    { apply (Inc_IN _ _ _ Hs2). rewrite <- Hv0. apply IN_Sing. }
    cbn[eval] in Hf0mem.
    rewrite eval_app. exists (f0 [ v0 ]).
    rewrite <- Hf0, <- Hv0, !iUnion_Sing_l.
    symmetry. apply (image_Sing_of_pi _ _ _ _ Hf0mem Hv0mem).

  - (* HT_add *)
    intros Gamma e1 e2 Hd1 IH1 Hd2 IH2 ρ Hwt.
    destruct (IH1 _ Hwt) as [w1 Hw1].
    destruct (IH2 _ Hwt) as [w2 Hw2].
    rewrite eval_add. exists (natZAdd w1 w2).
    rewrite <- Hw1, <- Hw2, !iUnion_Sing_l. reflexivity.

  - (* HDT_one *)
    intros Γ e T Hd IH ρ Hwt. exact (IH ρ Hwt).

  - (* HDT_inter: same value, regardless of which type we pick *)
    intros Γ e T TS Hd1 IH1 Hd2 IH2 ρ Hwt. exact (IH1 ρ Hwt).
Qed.

Theorem nontrivial :
  forall Γ e T,
  HasType Γ e T ->
  forall ρ, WellTyped Γ ρ -> exists X, {| X |} = eval e ρ.
Proof. destruct nontrivial_and as [H _]. exact H. Qed.


(* ================================================================== *)
(** * Translating the standalone intersection calculus [Lang.Intersection]
      into Timbda programs.

    Because [Lang.Intersection] is built to match this file's
    [Syntax.IntersectionTypes] discipline (intersections only in domain
    positions, types/domain-types mutually inductive), the translation is
    a clean correspondence: it is type preserving (into
    [HasType]/[HasDomType]) and semantics preserving (against [Timbda0]). *)
(* ================================================================== *)

Module Src := Lang.Intersection.

Scheme Src_Ty_mind := Induction for Src.Ty Sort Prop
  with Src_DomTy_mind := Induction for Src.DomTy Sort Prop.
Combined Scheme Src_Ty_DomTy_mind from Src_Ty_mind, Src_DomTy_mind.

(** Types and domain (intersection) types translate to Timbda type-
    expressions: [TNat] to [Eimg Enat], an arrow to [Elam], a singleton
    intersection to its element, and a proper intersection to [Eequal]. *)
Fixpoint trTy (T : Src.Ty) : Expr :=
  match T with
  | Src.TNat      => Eimg Enat
  | Src.TArr D T2 => Elam (trDomTy D) (trTy T2)
  end
with trDomTy (D : Src.DomTy) : Expr :=
  match D with
  | Src.DOne T     => trTy T
  | Src.DInter T D => Eequal (trTy T) (trDomTy D)
  end.

Fixpoint trTm (e : Src.Tm) : Expr :=
  match e with
  | Src.tvar n     => Evar n
  | Src.tlam D e   => Elam (trDomTy D) (trTm e)
  | Src.tapp e1 e2 => Eapp (trTm e1) (trTm e2)
  | Src.tcon n     => Econ n
  | Src.tadd e1 e2 => Eadd (trTm e1) (trTm e2)
  end.

Definition trCtx (Γ : Src.Ctx) : Ctx := fun n => trDomTy (Γ n).

(** Translated types/domain-types are well formed ([IsType]/[IsDomType]). *)
Lemma Wf_tr_and :
  (forall T, IsType (trTy T)) /\ (forall D, IsDomType (trDomTy D)).
Proof.
  apply Src_Ty_DomTy_mind.
  - apply IT_nat.
  - intros D IHD T2 IHT2. apply IT_lam; [ exact IHD | exact IHT2 ].
  - intros T IHT. apply IDT_one. exact IHT.
  - intros T IHT D IHD. apply IDT_inter; [ exact IHT | exact IHD ].
Qed.

Lemma IsType_trTy : forall T, IsType (trTy T).
Proof. destruct Wf_tr_and as [H _]. exact H. Qed.

Lemma IsDomType_trDomTy : forall D, IsDomType (trDomTy D).
Proof. destruct Wf_tr_and as [_ H]. exact H. Qed.

Lemma IsCtx_trCtx : forall Γ, IsCtx (trCtx Γ).
Proof. intros Γ i. apply IsDomType_trDomTy. Qed.

(** Membership in a domain type translates to [InDomType]. *)
Lemma InDom_InDomType : forall T D,
  Src.InDom T D -> InDomType (trTy T) (trDomTy D).
Proof.
  intros T D H. induction H; cbn [trDomTy].
  - apply InDT_one.
  - apply InDT_inter_here.
  - apply InDT_inter_there. exact IHInDom.
Qed.

(** Translation commutes with context extension. *)
Lemma trCtx_ext : forall Γ D,
  trCtx (Src.ctx_cons D Γ) = ctx_ext (trCtx Γ) (trDomTy D).
Proof. intros Γ D. extensionality n. destruct n; reflexivity. Qed.

(** ** Type preservation. *)
Theorem type_preservation_and :
  (forall Γ e T, Src.has_type Γ e T ->
     HasType (trCtx Γ) (trTm e) (trTy T)) /\
  (forall Γ e D, Src.has_dom_type Γ e D ->
     HasDomType (trCtx Γ) (trTm e) (trDomTy D)).
Proof.
  apply Src.has_type_has_dom_type_mut.
  - (* T_con *) intros Γ n. cbn [trTm trTy]. apply HT_con. apply IsCtx_trCtx.
  - (* T_var *) intros Γ i T Hin. cbn [trTm].
    apply HT_var; [ apply IsCtx_trCtx | apply InDom_InDomType; exact Hin ].
  - (* T_lam *) intros Γ D T2 e Hbody IH. cbn [trTm trTy]. apply HT_lam.
    + apply IsDomType_trDomTy.
    + rewrite <- trCtx_ext. exact IH.
  - (* T_app *) intros Γ D T2 e1 e2 Hf IHf Ha IHa. cbn [trTm].
    eapply HT_app; [ exact IHf | exact IHa ].
  - (* T_add *) intros Γ e1 e2 H1 IH1 H2 IH2. cbn [trTm trTy].
    apply HT_add; [ exact IH1 | exact IH2 ].
  - (* TD_one *) intros Γ e T H IH. cbn [trDomTy]. apply HDT_one. exact IH.
  - (* TD_inter *) intros Γ e T D H1 IH1 H2 IH2. cbn [trDomTy].
    apply HDT_inter; [ exact IH1 | exact IH2 ].
Qed.

Theorem type_preservation Γ e T :
  Src.has_type Γ e T -> HasType (trCtx Γ) (trTm e) (trTy T).
Proof. destruct type_preservation_and as [H _]. exact (H Γ e T). Qed.

Theorem type_preservation_dom Γ e D :
  Src.has_dom_type Γ e D -> HasDomType (trCtx Γ) (trTm e) (trDomTy D).
Proof. destruct type_preservation_and as [_ H]. exact (H Γ e D). Qed.

(** ** Semantics preservation (against the [Timbda0] model). *)

(* [env_ext] (Timbda) and [Src.env_cons] are the same cons. *)
Lemma env_ext_cons : forall (ρ : Env) (a : ZFSet),
  env_ext ρ a = Src.env_cons a ρ.
Proof. intros ρ a. extensionality n. destruct n; reflexivity. Qed.

(** The Timbda denotation of a translated type / domain type is its
    [Lang.Intersection] denotation. *)
Lemma eval_trTy_and :
  (forall T ρ, eval (trTy T) ρ = Src.evalTy T) /\
  (forall D ρ, eval (trDomTy D) ρ = Src.evalDomTy D).
Proof.
  apply Src_Ty_DomTy_mind.
  - (* TNat *) intros ρ. apply eval_tnat.
  - (* TArr *) intros D IHD T2 IHT2 ρ. cbn [trTy]. rewrite eval_lam.
    cbn [Src.evalTy]. rewrite IHD. f_equal. extensionality a. apply IHT2.
  - (* DOne *) intros T IHT ρ. cbn [trDomTy Src.evalDomTy]. apply IHT.
  - (* DInter *) intros T IHT D IHD ρ. cbn [trDomTy]. rewrite eval_equal.
    cbn [Src.evalDomTy]. rewrite IHT IHD. reflexivity.
Qed.

Lemma eval_trTy : forall T ρ, eval (trTy T) ρ = Src.evalTy T.
Proof. destruct eval_trTy_and as [H _]. exact H. Qed.

Lemma eval_trDomTy : forall D ρ, eval (trDomTy D) ρ = Src.evalDomTy D.
Proof. destruct eval_trTy_and as [_ H]. exact H. Qed.

Theorem sem_preservation_and :
  (forall Γ e T, Src.has_type Γ e T ->
     forall ρ, Src.models Γ ρ -> eval (trTm e) ρ = {| Src.evalTm e ρ |}) /\
  (forall Γ e D, Src.has_dom_type Γ e D ->
     forall ρ, Src.models Γ ρ -> eval (trTm e) ρ = {| Src.evalTm e ρ |}).
Proof.
  apply Src.has_type_has_dom_type_mut.

  - (* T_con *) intros Γ n ρ HM. reflexivity.

  - (* T_var *) intros Γ i T Hin ρ HM. reflexivity.

  - (* T_lam *) intros Γ D T2 e Hbody IH ρ HM.
    cbn [trTm]. rewrite eval_lam. rewrite eval_trDomTy.
    assert (HYP : forall a, In a (Src.evalDomTy D) ->
              eval (trTm e) (env_ext ρ a) = {| Src.evalTm e (Src.env_cons a ρ) |}).
    { intros a Ha. rewrite env_ext_cons. apply IH.
      apply Src.models_cons; [ exact HM | exact Ha ]. }
    rewrite (Pi_Sing (Src.evalDomTy D)
                     (fun a => eval (trTm e) (env_ext ρ a))
                     (fun a => Src.evalTm e (Src.env_cons a ρ)) HYP).
    cbn [Src.evalTm]. reflexivity.

  - (* T_app *) intros Γ D T2 e1 e2 Hf IHf Ha IHa ρ HM.
    cbn [trTm]. rewrite eval_app.
    rewrite (IHf ρ HM). rewrite (IHa ρ HM). rewrite !iUnion_Sing_l.
    assert (Hfun : In (Src.evalTm e1 ρ)
                      (Pi (Src.evalDomTy D) (fun _ => Src.evalTy T2))).
    { pose proof (Src.soundness _ _ _ Hf ρ HM) as Hs1.
      cbn [Src.evalTy] in Hs1. exact Hs1. }
    assert (Harg : In (Src.evalTm e2 ρ) (Src.evalDomTy D)).
    { exact (Src.soundness_dom _ _ _ Ha ρ HM). }
    rewrite (image_Sing_of_pi _ _ _ _ Hfun Harg).
    cbn [Src.evalTm]. reflexivity.

  - (* T_add *) intros Γ e1 e2 H1 IH1 H2 IH2 ρ HM.
    cbn [trTm]. rewrite eval_add.
    rewrite (IH1 ρ HM). rewrite (IH2 ρ HM). rewrite !iUnion_Sing_l.
    cbn [Src.evalTm]. reflexivity.

  - (* TD_one *) intros Γ e T H IH ρ HM. exact (IH ρ HM).

  - (* TD_inter *) intros Γ e T D H1 IH1 H2 IH2 ρ HM. exact (IH1 ρ HM).
Qed.

Theorem sem_preservation Γ e T :
  Src.has_type Γ e T ->
  forall ρ, Src.models Γ ρ -> eval (trTm e) ρ = {| Src.evalTm e ρ |}.
Proof. destruct sem_preservation_and as [H _]. exact (H Γ e T). Qed.
