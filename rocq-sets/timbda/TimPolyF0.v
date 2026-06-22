(* TimPolyF0.v

   Soundness of (the image of) predicative System F ([Lang.PolyF]) under
   the *Timbda0* set-of-values semantics:

       has_type Γ e s  ->  eval (trTm Δ e) ρ ⊆ eval (trPoly Δ s) ρ.

   A type translates to its inhabitant *set*, a term to a set of values.
   The type universe is [Eimg Etype = smallId] (the *small* partial
   identities): [∀] is an [Elam] over it, a type variable is read back as
   the range of its bound partial identity ([Eimg (Evar _)]), and type
   application reifies the argument monotype's inhabitant set into a
   partial identity via the diagonalization construct [Ediag]
   ([trRel Δ τ = Ediag (trMono Δ τ)]).  This covers full predicative
   System F, including type application at *any* monotype. *)

From Stdlib Require Import ssreflect.
From Stdlib Require Import FunctionalExtensionality.
Require Import ZFSet.
Require Import ZFNotation.
Require Import Diagonal.
Require Import Syntax.
Require Import Timbda0.
Require Lang.PolyF.

Module Src := Lang.PolyF.
Module Tr  := Syntax.PolyF.

Local Open Scope list_scope.

(* ================================================================== *)
(** * Translation into Timbda0 [Expr]. *)
(* ================================================================== *)

Fixpoint trMono (Δ : list Tr.bnd) (t : Src.Mono) : Expr :=
  match t with
  | Src.MNat     => Eimg Enat
  | Src.MVar n   => Eimg (Evar (Tr.tyIx Δ n))
  | Src.MArr a b => Elam (trMono Δ a) (trMono (Tr.bTm :: Δ) b)
  end.

Fixpoint trPoly (Δ : list Tr.bnd) (s : Src.Poly) : Expr :=
  match s with
  | Src.PMono t  => trMono Δ t
  | Src.PArr a b => Elam (trPoly Δ a) (trPoly (Tr.bTm :: Δ) b)
  | Src.PAll a   => Elam (Eimg Etype) (trPoly (Tr.bTy :: Δ) a)
  end.


Definition Ediag (e : Expr) := Elam e (Evar 0).

Lemma eval_diag :
  forall (e : Expr) (rho : Env),
  eval (Ediag e) rho = {| a ← eval e rho ;; {| ⟨ a , a ⟩ |} |}.
Proof.
  intros e rho. unfold Ediag. rewrite eval_lam.
  apply (Pi_Sing (eval e rho) (fun a => eval (Evar 0) (env_ext rho a)) (fun a => a)).
  intros a Ha. rewrite eval_var env_ext_zero. reflexivity.
Qed.

(** The type-value of a monotype, for the argument of a type application:
    reify the monotype's inhabitant set [trMono Δ τ] into the partial
    identity [diag (·)] via [Ediag]. *)
Definition trRel (Δ : list Tr.bnd) (t : Src.Mono) : Expr := 
    Ediag (trMono Δ t).

Fixpoint trTm (Δ : list Tr.bnd) (e : Src.Tm) : Expr :=
  match e with
  | Src.tvar n     => Evar (Tr.tmIx Δ n)
  | Src.tlam s e   => Elam (trPoly Δ s) (trTm (Tr.bTm :: Δ) e)
  | Src.tapp e1 e2 => Eapp (trTm Δ e1) (trTm Δ e2)
  | Src.tTlam e    => Elam (Eimg Etype) (trTm (Tr.bTy :: Δ) e)
  | Src.tTapp e τ  => Eapp (trTm Δ e) (trRel Δ τ)
  | Src.tcon n     => Econ n
  | Src.tadd e1 e2 => Eadd (trTm Δ e1) (trTm Δ e2)
  end.

(* [diag], [rng_diag], [diag_small], [smallId], [diag_in_smallId] and the
   other small-partial-identity facts are now provided by [lib.Diagonal]. *)

(* ================================================================== *)
(** * Native Timbda0 type semantics. *)
(* ================================================================== *)

(** Monotypes denote exactly [Src.evalMono].  
    Polytypes have a different denotation in Timbda0 because types
    represent partical identity functions. Therefore they differ at
    [∀], which quantifies over [smallId], not Big. *)
Fixpoint tyPoly (s : Src.Poly) (δ : Src.tenv) : ZFSet :=
  match s with
  | Src.PMono t  => Src.evalMono t δ
  | Src.PArr a b => Π[ _ ∈ tyPoly a δ ] tyPoly b δ
  | Src.PAll a   => Π[ f ∈ smallId ] tyPoly a (Src.tcons (rng f) δ)
  end.

Lemma tyPoly_shift s : forall c X δ,
  tyPoly (Src.shiftPoly c s) (Src.insert c X δ) = tyPoly s δ.
Proof.
  induction s as [t | a IHa b IHb | a IHa]; intros c X δ; cbn [tyPoly Src.shiftPoly].
  - apply Src.shiftMono_eval.
  - rewrite IHa. f_equal. apply functional_extensionality. intro. apply IHb.
  - f_equal. apply functional_extensionality. intro f.
    rewrite <- Src.insert_cons. apply IHa.
Qed.

Lemma tyPoly_shift0 s Y δ : tyPoly (Src.shiftPoly 0 s) (Src.tcons Y δ) = tyPoly s δ.
Proof. rewrite <- (Src.insert0 Y δ). apply tyPoly_shift. Qed.

Lemma tyPoly_subst s : forall j m δ,
  tyPoly (Src.substPoly j m s) δ = tyPoly s (Src.insert j (Src.evalMono m δ) δ).
Proof.
  induction s as [t | a IHa b IHb | a IHa]; intros j m δ; cbn [tyPoly Src.substPoly].
  - apply Src.substMono_eval.
  - rewrite IHa. f_equal. apply functional_extensionality. intro. apply IHb.
  - f_equal. apply functional_extensionality. intro f.
    rewrite IHa. f_equal. rewrite Src.shiftMono_eval0. apply Src.insert_cons.
Qed.

(* ================================================================== *)
(** * The type environment read off the Timbda environment. *)
(* ================================================================== *)

Definition delta_of (Δ : list Tr.bnd) (ρ : Env) : Src.tenv :=
  fun k => rng (ρ (Tr.tyIx Δ k)).

Lemma delta_of_tm Δ ρ v :
  delta_of (Tr.bTm :: Δ) (env_ext ρ v) = delta_of Δ ρ.
Proof.
  apply functional_extensionality. intro k. unfold delta_of. cbn [Tr.tyIx].
  rewrite env_ext_succ. reflexivity.
Qed.

Lemma delta_of_ty Δ ρ f :
  delta_of (Tr.bTy :: Δ) (env_ext ρ f) = Src.tcons (rng f) ( delta_of Δ ρ).
Proof.
  apply functional_extensionality. intros [|k]; unfold delta_of, Src.tcons; cbn [Tr.tyIx].
  - rewrite env_ext_zero. reflexivity.
  - rewrite env_ext_succ. reflexivity.
Qed.

(** A type variable reads back as the range of its bound partial identity. *)
Lemma eval_img_var i ρ : eval (Eimg (Evar i)) ρ = rng (ρ i).
Proof. cbn [eval]. rewrite iUnion_Sing_l. reflexivity. Qed.

(* ================================================================== *)
(** * Correspondence: the translation denotes the native type semantics.
      for monotypes, and the modified semantics for polytypes. *)
(* ================================================================== *)

Lemma corr_mono t : forall Δ ρ,
  eval (trMono Δ t) ρ = Src.evalMono t (delta_of Δ ρ).
Proof.
  induction t as [ | k | a IHa b IHb ]; intros Δ ρ; cbn [trMono Src.evalMono].
  - apply eval_tnat.
  - rewrite eval_img_var. reflexivity.
  - rewrite eval_lam IHa. f_equal. apply functional_extensionality. intro x.
    rewrite IHb delta_of_tm. reflexivity.
Qed.

Lemma corr_poly s : forall Δ ρ,
  eval (trPoly Δ s) ρ = tyPoly s (delta_of Δ ρ).
Proof.
  induction s as [ t | a IHa b IHb | a IHa ]; intros Δ ρ; cbn [trPoly tyPoly].
  - apply corr_mono.
  - rewrite eval_lam IHa. f_equal. apply functional_extensionality. intro x.
    rewrite IHb delta_of_tm. reflexivity.
  - rewrite eval_lam eval_img_type. f_equal. apply functional_extensionality. intro f.
    rewrite IHa delta_of_ty. reflexivity.
Qed.

(** The reified type-value of any monotype is a small partial identity
    whose range is the monotype's denotation. *)
Lemma trRel_spec Δ ρ τ :
  (forall k, ρ (Tr.tyIx Δ k) ∈ smallId) ->
  exists v, eval (trRel Δ τ) ρ = {| v |}
            /\ v ∈ smallId
            /\ rng v = Src.evalMono τ (delta_of Δ ρ).
Proof.
  intro Hty.
  assert (Hok : Src.tenv_ok (delta_of Δ ρ)).
  { intro k. unfold delta_of. apply rng_small. apply smallId_small. apply Hty. }
  pose proof (Src.mono_small τ (delta_of Δ ρ) Hok) as Hsmall.
  exists (diag (Src.evalMono τ (delta_of Δ ρ))). split; [ | split ].
  - unfold trRel. rewrite eval_diag corr_mono. unfold diag. reflexivity.
  - apply diag_in_smallId. exact Hsmall.
  - apply rng_diag.
Qed.

(* ================================================================== *)
(** * Well-typed environments and soundness. *)
(* ================================================================== *)

Definition WellTyped (Γ : Src.Ctx) (Δ : list Tr.bnd) (ρ : Env) : Prop :=
  (forall n, ρ (Tr.tmIx Δ n) ∈ tyPoly (Γ n) (delta_of Δ ρ))
  /\ (forall k, ρ (Tr.tyIx Δ k) ∈ smallId).

Lemma WT_ext_tm Γ Δ ρ s a :
  WellTyped Γ Δ ρ -> a ∈ tyPoly s (delta_of Δ ρ) ->
  WellTyped (Src.pcons s Γ) (Tr.bTm :: Δ) (env_ext ρ a).
Proof.
  intros [Htm Hty] Ha. split.
  - intro n. rewrite (delta_of_tm Δ ρ a). destruct n as [|m]; cbn [Tr.tmIx Src.pcons].
    + rewrite env_ext_zero. exact Ha.
    + rewrite env_ext_succ. apply Htm.
  - intro k. cbn [Tr.tyIx]. rewrite env_ext_succ. apply Hty.
Qed.

Lemma WT_ext_ty Γ Δ ρ f :
  WellTyped Γ Δ ρ -> f ∈ smallId ->
  WellTyped (Src.shiftCtx Γ) (Tr.bTy :: Δ) (env_ext ρ f).
Proof.
  intros [Htm Hty] Hf. split.
  - intro n. cbn [Tr.tmIx]. rewrite env_ext_succ. rewrite (delta_of_ty Δ ρ f).
    unfold Src.shiftCtx. rewrite tyPoly_shift0. apply Htm.
  - intro k. destruct k as [|k]; cbn [Tr.tyIx].
    + rewrite env_ext_zero. exact Hf.
    + rewrite env_ext_succ. apply Hty.
Qed.

Theorem soundness Γ e s :
  Src.has_type Γ e s ->
  forall Δ ρ, WellTyped Γ Δ ρ -> eval (trTm Δ e) ρ ⊆ eval (trPoly Δ s) ρ.
Proof.
  induction 1; intros Δ ρ Hwt; cbn [trTm].

  - (* T_var *)
    destruct Hwt as [Htm Hty].
    rewrite eval_var. apply Sing_Inc_IN. rewrite corr_poly. apply Htm.

  - (* T_lam *)
    cbn [trPoly]. rewrite !eval_lam.
    apply Pi_Inc_Codomain. intros a Ha.
    apply (IHhas_type (Tr.bTm :: Δ)).
    apply WT_ext_tm; [ exact Hwt | rewrite <- corr_poly; exact Ha ].

  - (* T_app *)
    rewrite eval_app.
    pose proof (IHhas_type1 Δ ρ Hwt) as IH1.
    pose proof (IHhas_type2 Δ ρ Hwt) as IH2.
    cbn [trPoly] in IH1. rewrite eval_lam in IH1.
    apply Inc_def. intros w Hw.
    apply iUnion_IN in Hw. destruct Hw as [F [HF Hw]].
    apply iUnion_IN in Hw. destruct Hw as [z [Hz Hw]].
    apply image_elim in Hw. destruct Hw as [aa [Haa Hedge]].
    apply IN_Sing_EQ in Haa. subst aa.
    apply (Inc_IN _ _ _ IH1) in HF.
    apply (Inc_IN _ _ _ IH2) in Hz.
    pose proof (Pi_edge_codomain _ _ _ _ _ HF Hz Hedge) as Hcod.
    rewrite corr_poly delta_of_tm in Hcod.
    rewrite (corr_poly s2 Δ ρ). exact Hcod.

  - (* T_Tlam *)
    cbn [trPoly]. rewrite !eval_lam.
    apply Pi_Inc_Codomain. intros f Hf. rewrite eval_img_type in Hf.
    apply (IHhas_type (Tr.bTy :: Δ)).
    apply WT_ext_ty; [ exact Hwt | exact Hf ].

  - (* T_Tapp: instantiate at the reified type-value of [τ] *)
    destruct Hwt as [Htm Hty].
    rewrite eval_app.
    pose proof (IHhas_type Δ ρ (conj Htm Hty)) as IH.
    cbn [trPoly] in IH. rewrite eval_lam eval_img_type in IH.
    destruct (trRel_spec Δ ρ τ Hty) as [v [Hev [Hv Hrng]]].
    rewrite Hev.
    apply Inc_def. intros w Hw.
    apply iUnion_IN in Hw. destruct Hw as [F [HF Hw]].
    apply iUnion_IN in Hw. destruct Hw as [u [Hu Hw]].
    apply IN_Sing_EQ in Hu. subst u.
    apply image_elim in Hw. destruct Hw as [aa [Haa Hedge]].
    apply IN_Sing_EQ in Haa. subst aa.
    apply (Inc_IN _ _ _ IH) in HF.
    pose proof (Pi_edge_codomain _ _ _ _ _ HF Hv Hedge) as Hcod.
    rewrite corr_poly delta_of_ty in Hcod.
    rewrite (corr_poly (Src.substPoly 0 τ s) Δ ρ) tyPoly_subst Src.insert0.
    rewrite <- Hrng. exact Hcod.

  - (* T_con *)
    rewrite eval_con. cbn [trPoly trMono]. rewrite eval_tnat.
    apply Sing_Inc_IN. apply natZ_mem_omega.

  - (* T_add *)
    pose proof (IHhas_type1 Δ ρ Hwt) as IH1.
    pose proof (IHhas_type2 Δ ρ Hwt) as IH2.
    cbn [trPoly trMono] in IH1, IH2. rewrite eval_tnat in IH1, IH2.
    cbn [trPoly trMono]. rewrite eval_add eval_tnat.
    apply iUnion2_Sing_Inc. intros y z Hy Hz.
    apply (Inc_IN _ _ _ IH1) in Hy. apply (Inc_IN _ _ _ IH2) in Hz.
    destruct (In_omega _ Hy) as [n1 E1]. destruct (In_omega _ Hz) as [n2 E2].
    rewrite <- E1, <- E2, natZAdd_natZ. apply natZ_mem_omega.
Qed.

(* ================================================================== *)
(** * Determinacy: a well-typed term denotes a singleton. *)
(* ================================================================== *)

Theorem nontrivial Γ e s :
  Src.has_type Γ e s ->
  forall Δ ρ, WellTyped Γ Δ ρ -> exists X, {| X |} = eval (trTm Δ e) ρ.
Proof.
  induction 1 as
    [ Γ n
    | Γ s1 s2 e He IHe
    | Γ s1 s2 e1 e2 He1 IHe1 He2 IHe2
    | Γ s e He IHe
    | Γ s τ e He IHe
    | Γ n
    | Γ e1 e2 He1 IHe1 He2 IHe2 ];
    intros Δ ρ Hwt; cbn [trTm].

  - (* T_var *)
    eexists. rewrite eval_var. reflexivity.

  - (* T_lam: every fibre of the [Pi] is a singleton (IH) *)
    rewrite eval_lam.
    destruct (Pi_Sing_ex (eval (trPoly Δ s1) ρ)
                (fun a => eval (trTm (Tr.bTm :: Δ) e) (env_ext ρ a))) as [f Hf].
    { intros a Ha.
      destruct (IHe (Tr.bTm :: Δ) (env_ext ρ a)) as [X HX].
      - apply WT_ext_tm; [ exact Hwt | rewrite <- corr_poly; exact Ha ].
      - exists X. symmetry. exact HX. }
    exists f. symmetry. exact Hf.

  - (* T_app: the singleton function applied to the singleton argument *)
    rewrite eval_app.
    destruct (IHe1 Δ ρ Hwt) as [F0 HF0].
    destruct (IHe2 Δ ρ Hwt) as [v0 Hv0].
    pose proof (soundness _ _ _ He1 Δ ρ Hwt) as Hs1.
    pose proof (soundness _ _ _ He2 Δ ρ Hwt) as Hs2.
    assert (HFmem : F0 ∈ eval (trPoly Δ (Src.PArr s1 s2)) ρ).
    { apply (Inc_IN _ _ _ Hs1). rewrite <- HF0. apply IN_Sing. }
    assert (Hvmem : v0 ∈ eval (trPoly Δ s1) ρ).
    { apply (Inc_IN _ _ _ Hs2). rewrite <- Hv0. apply IN_Sing. }
    cbn [trPoly] in HFmem. rewrite eval_lam in HFmem.
    exists (F0 [ v0 ]).
    rewrite <- HF0, <- Hv0, !iUnion_Sing_l.
    symmetry. apply (image_Sing_of_pi _ _ _ _ HFmem Hvmem).

  - (* T_Tlam: every fibre over the type universe is a singleton (IH) *)
    rewrite eval_lam.
    destruct (Pi_Sing_ex (eval (Eimg Etype) ρ)
                (fun f => eval (trTm (Tr.bTy :: Δ) e) (env_ext ρ f))) as [g Hg].
    { intros a Ha. rewrite eval_img_type in Ha.
      destruct (IHe (Tr.bTy :: Δ) (env_ext ρ a)) as [X HX].
      - apply WT_ext_ty; [ exact Hwt | exact Ha ].
      - exists X. symmetry. exact HX. }
    exists g. symmetry. exact Hg.

  - (* T_Tapp: the singleton type-function applied at the reified type-value *)
    rewrite eval_app.
    destruct Hwt as [Htm Hty].
    destruct (IHe Δ ρ (conj Htm Hty)) as [F0 HF0].
    destruct (trRel_spec Δ ρ τ Hty) as [v0 [Hev [Hv0 _]]].
    pose proof (soundness _ _ _ He Δ ρ (conj Htm Hty)) as Hs.
    assert (HFmem : F0 ∈ eval (trPoly Δ (Src.PAll s)) ρ).
    { apply (Inc_IN _ _ _ Hs). rewrite <- HF0. apply IN_Sing. }
    cbn [trPoly] in HFmem. rewrite eval_lam eval_img_type in HFmem.
    exists (F0 [ v0 ]).
    rewrite <- HF0, Hev, !iUnion_Sing_l.
    symmetry. apply (image_Sing_of_pi _ _ _ _ HFmem Hv0).

  - (* T_con *)
    eexists. rewrite eval_con. reflexivity.

  - (* T_add: the sum of the two singleton values *)
    rewrite eval_add.
    destruct (IHe1 Δ ρ Hwt) as [w1 Hw1].
    destruct (IHe2 Δ ρ Hwt) as [w2 Hw2].
    exists (w1 + w2).
    rewrite <- Hw1, <- Hw2, !iUnion_Sing_l. reflexivity.
Qed.
