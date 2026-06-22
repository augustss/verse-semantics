(* lang/PCF/PCFLifted.v

   A *pointed* (⊥-lifted) variant of the relational set-theoretic semantics
   of [PCFStrict.v], designed to make β-reduction sound.  See [PCF.md] for the
   design discussion.

   The change in one line: *divergence becomes a value*.  Each ground type
   gains a distinguished bottom element [⊥] ([bot]); arrow types are left as
   the plain function space [Pi] over the now-lifted codomain (so a λ whose
   body diverges is realised by the all-⊥ graph — a genuine element of [Pi],
   not the empty set).  The ground primitives [succ]/[pred]/[iszero] become
   ⊥-strict (they map [⊥] to [⊥]); [if] is unchanged (a ⊥ guard already
   yields [∅], i.e. genuine divergence at the result type).

   With this, the substitution lemma holds for a single-valued argument, and
   β-reduction preserves denotations ([S_beta_sound]) — including the very
   witness that refuted β in [PCFStrict.v] ([S_beta_unsound]).  Type soundness is
   preserved ([soundness]).

   [tfix] here denotes the *unique* fixed point of its function value (or [∅]
   when there is none, or more than one), rather than the *set of all* fixed
   points as in [PCFStrict.v].  That keeps every well-typed term single-valued
   ([evalTm_subsingleton] below): each denotes one value or [∅].  This removes
   the multivalued-[fix] obstruction that [PCFStrict.v]/[PCF.md] §3 flag — so for a
   *terminating* argument (a singleton) the hypotheses of [S_beta_sound] are
   met and β holds.  It does not make β unconditional: a divergent argument
   denotes [∅] and the strict application discards it (see the note after
   [evalTm_subsingleton]). *)

Require Import ZFSet.
Require Import ZFNotation.
Require Import PCFSyntax.
From Stdlib Require Import PeanoNat Lia.
From Stdlib Require Import FunctionalExtensionality.

(** The syntax ([Ty]/[Tm]), typing ([has_type], [Ctx]/[ctx_cons]), and the
    substitution-based small-step reduction ([lift]/[subst]/[value]/[step])
    are shared with [PCFStrict.v] and live in [PCFSyntax.v]. *)

(* ================================================================== *)
(** * The pointed (⊥-lifted) set-theoretic semantics. *)
(* ================================================================== *)

(** The bottom element.  Any set outside every value set works; [Omega]
    (the naturals) is convenient — it is not a natural ([bot_not_Omega],
    by foundation) and not a functional graph, so it never collides with a
    ground value or a function value. *)
Definition bot : ZFSet := Omega.

(** The two booleans, as in [PCFStrict.v]. *)
Definition bools : ZFSet := {| natZ 0 |} ∪ {| natZ 1 |}.

(** A type denotes the set of its values, now *pointed* at the ground
    types.  Arrow types are the plain function space over the lifted
    codomain: a divergent-bodied λ is realised by the all-[⊥] graph, which
    already lives in [Pi] — so arrows need no separate [⊥]. *)
Fixpoint evalTy (T : Ty) : ZFSet :=
  match T with
  | TNat       => Omega ∪ {| bot |}
  | TBool      => bools ∪ {| bot |}
  | TArr T1 T2 => Pi (evalTy T1) (fun _ => evalTy T2)
  end.

Definition Env := nat -> ZFSet.

Definition env_cons (v : ZFSet) (ρ : Env) : Env :=
  fun n => match n with O => v | S m => ρ m end.

(** Insert value [a] at de Bruijn position [k] (the semantic counterpart of
    [subst k]/[lift k]): indices below [k] are unchanged, [k] becomes [a],
    indices above [k] are shifted down by one. *)
Definition env_ins (k : nat) (a : ZFSet) (ρ : Env) : Env :=
  fun n => match Nat.compare n k with
           | Lt => ρ n
           | Eq => a
           | Gt => ρ (Nat.pred n)
           end.

(** ⊥-strict ground map: [{| k |}] when the argument [v] is a proper
    natural, and [{| bot |}] when [v = ⊥] (i.e. [v ∉ Omega]).  Used for the
    strict ground primitives [succ]/[pred]. *)
Definition strictN (k v : ZFSet) : ZFSet :=
  ⦃ _ ∈ {| k |} | v ∈ Omega ⦄ ∪ ⦃ _ ∈ {| bot |} | ~ (v ∈ Omega) ⦄.

(** A term denotes the set of values it may produce.  [tvar]/[tlam]/[tapp]/
    the constructors/[tif]/[tfix] are exactly as in [PCFStrict.v]; only
    [succ]/[pred]/[iszero] change, becoming ⊥-strict. *)
Fixpoint evalTm (e : Tm) (ρ : Env) : ZFSet :=
  match e with
  | tvar n     => {| ρ n |}
  | tlam T e   =>
      Pi (evalTy T) (fun a => evalTm e (env_cons a ρ))
  | tapp e1 e2 =>
      f ← evalTm e1 ρ ;; v ← evalTm e2 ρ ;; image f {| v |}
  | tzero      => {| natZ 0 |}
  | tsucc e    => v ← evalTm e ρ ;; strictN (natZSucc v) v
  | tpred e    => v ← evalTm e ρ ;; strictN (natZPred v) v
  | tiszero e  =>
      v ← evalTm e ρ ;;
        (⦃ _ ∈ {| natZ 1 |} | v = natZ 0 ⦄
         ∪ ⦃ _ ∈ {| natZ 0 |} | v ∈ Omega /\ v <> natZ 0 ⦄
         ∪ ⦃ _ ∈ {| bot |} | ~ (v ∈ Omega) ⦄)
  | ttrue      => {| natZ 1 |}
  | tfalse     => {| natZ 0 |}
  | tif e0 e1 e2 =>
      b ← evalTm e0 ρ ;;
        (⦃ _ ∈ evalTm e1 ρ | b = natZ 1 ⦄ ∪ ⦃ _ ∈ evalTm e2 ρ | b = natZ 0 ⦄)
  | tfix e     =>
      (* keep [a] only when it is the *unique* fixed point of [f]; so [tfix]
         denotes a singleton (the lone fixed point) or [∅] (none, or several).
         This makes the model deterministic — closing the multivalued-[fix]
         gap of [PCFStrict.v]/[PCF.md] that otherwise breaks β under binders. *)
      f ← evalTm e ρ ;;
        ⦃ a ∈ dom f | ⟨ a , a ⟩ ∈ f /\ (forall b, ⟨ b , b ⟩ ∈ f -> b = a) ⦄
  end.

(** Foundation: [bot = Omega] is not a natural, so [bot ∉ Omega]. *)
Lemma bot_not_Omega : ~ (bot ∈ Omega).
Proof. unfold bot. apply not_In_self. Qed.

(* ================================================================== *)
(** * Type soundness: a well-typed term is a subset of its (lifted) type. *)
(* ================================================================== *)

Definition models (Γ : Ctx) (ρ : Env) : Prop :=
  forall n, ρ n ∈ evalTy (Γ n).

Lemma models_cons Γ ρ v T :
  models Γ ρ -> v ∈ evalTy T -> models (ctx_cons T Γ) (env_cons v ρ).
Proof. intros HM Hv [|n]; cbn; [ exact Hv | apply HM ]. Qed.

(** [natZ n] lands in the lifted ground type. *)
Lemma natZ_in_TNat n : natZ n ∈ evalTy TNat.
Proof. cbn. apply IN_BinUnion_l. apply natZ_mem_omega. Qed.

Lemma bot_in_TNat : bot ∈ evalTy TNat.
Proof. cbn. apply IN_BinUnion_r, IN_Sing. Qed.

Lemma natZ0_in_TBool : natZ 0 ∈ evalTy TBool.
Proof. cbn. apply IN_BinUnion_l, IN_BinUnion_l, IN_Sing. Qed.

Lemma natZ1_in_TBool : natZ 1 ∈ evalTy TBool.
Proof. cbn. apply IN_BinUnion_l, IN_BinUnion_r, IN_Sing. Qed.

Lemma bot_in_TBool : bot ∈ evalTy TBool.
Proof. cbn. apply IN_BinUnion_r, IN_Sing. Qed.

(** The ⊥-strict ground map lands in the lifted naturals. *)
Lemma strictN_in_TNat v : strictN (natZSucc v) v ⊆ evalTy TNat.
Proof.
  unfold strictN. apply Inc_def. intros x Hx.
  apply BinUnion_IN in Hx. destruct Hx as [Hx | Hx].
  - pose proof (In_Comp_P _ _ _ Hx) as Hin.
    apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hx. apply IN_Sing_EQ in Hx. subst x.
    destruct (In_omega _ Hin) as [k Hk]. subst v.
    rewrite natZSucc_natZ. apply natZ_in_TNat.
  - apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hx. apply IN_Sing_EQ in Hx. subst x.
    apply bot_in_TNat.
Qed.

Lemma strictNP_in_TNat v : strictN (natZPred v) v ⊆ evalTy TNat.
Proof.
  unfold strictN. apply Inc_def. intros x Hx.
  apply BinUnion_IN in Hx. destruct Hx as [Hx | Hx].
  - pose proof (In_Comp_P _ _ _ Hx) as Hin.
    apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hx. apply IN_Sing_EQ in Hx. subst x.
    destruct (In_omega _ Hin) as [k Hk]. subst v.
    rewrite natZPred_natZ. apply natZ_in_TNat.
  - apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hx. apply IN_Sing_EQ in Hx. subst x.
    apply bot_in_TNat.
Qed.

Theorem soundness Γ e T :
  has_type Γ e T -> forall ρ, models Γ ρ -> evalTm e ρ ⊆ evalTy T.
Proof.
  induction 1; intros ρ HM; cbn [evalTm evalTy].

  - (* T_var *)
    apply Sing_Inc_IN. apply HM.

  - (* T_lam *)
    apply Pi_Inc_Codomain. intros a Ha.
    apply (IHhas_type (env_cons a ρ)). apply models_cons; assumption.

  - (* T_app *)
    apply Inc_def. intros w Hw.
    apply iUnion_IN in Hw. destruct Hw as [f [Hf Hw]].
    apply iUnion_IN in Hw. destruct Hw as [v [Hv Hw]].
    apply image_elim in Hw. destruct Hw as [a [Ha Hedge]].
    apply IN_Sing_EQ in Ha. subst a.
    apply (Inc_IN _ _ _ (IHhas_type1 ρ HM)) in Hf.
    apply (Inc_IN _ _ _ (IHhas_type2 ρ HM)) in Hv.
    exact (Pi_edge_codomain _ _ _ _ _ Hf Hv Hedge).

  - (* T_zero *)
    apply Sing_Inc_IN. apply natZ_in_TNat.

  - (* T_succ *)
    apply iUnion_Inc. intros v Hv. apply strictN_in_TNat.

  - (* T_pred *)
    apply iUnion_Inc. intros v Hv. apply strictNP_in_TNat.

  - (* T_iszero *)
    apply iUnion_Inc. intros v Hv. apply Inc_def. intros x Hx.
    apply BinUnion_IN in Hx. destruct Hx as [Hx | Hx].
    + apply BinUnion_IN in Hx. destruct Hx as [Hx | Hx];
        apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hx; apply IN_Sing_EQ in Hx; subst x.
      * apply natZ1_in_TBool.
      * apply natZ0_in_TBool.
    + apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hx. apply IN_Sing_EQ in Hx. subst x.
      apply bot_in_TBool.

  - (* T_true *)
    apply Sing_Inc_IN. apply natZ1_in_TBool.

  - (* T_false *)
    apply Sing_Inc_IN. apply natZ0_in_TBool.

  - (* T_if *)
    apply iUnion_Inc. intros b Hb. apply Inc_def. intros x Hx.
    apply BinUnion_IN in Hx. destruct Hx as [Hx | Hx];
      apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hx.
    + apply (Inc_IN _ _ _ (IHhas_type2 ρ HM)). exact Hx.
    + apply (Inc_IN _ _ _ (IHhas_type3 ρ HM)). exact Hx.

  - (* T_fix *)
    apply iUnion_Inc. intros f Hf. apply Inc_def. intros a Ha.
    pose proof (In_Comp_P _ _ _ Ha) as [Haa _].
    apply (Inc_IN _ _ _ (IHhas_type ρ HM)) in Hf.
    apply In_Pi_inv in Hf. destruct Hf as [Hsub _].
    apply (Inc_IN _ _ _ Hsub) in Haa.
    apply Couple_Prod_IN in Haa. destruct Haa as [Ha1 _]. exact Ha1.
Qed.

(* ================================================================== *)
(** * Determinism: every well-typed term is single-valued. *)
(* ================================================================== *)

(** With the unique-fixed-point [tfix], the model is deterministic: a
    well-typed term denotes *at most one* value (a singleton, or [∅] for a
    divergent term).  We phrase this as [is_subsingleton] — [X] has at most
    one element.  This is what discharges the single-valuedness hypothesis
    of [S_beta_sound] for every well-typed argument (see [S_beta_sound']). *)

(** Membership in a constant-guard comprehension over a singleton: the
    element is the point, and the guard holds. *)
Lemma In_const_comp_sing (x s : ZFSet) (Q : Prop) :
  In x (Comp (Sing s) (fun _ => Q)) -> x = s /\ Q.
Proof.
  intro H. split.
  - apply IN_Sing_EQ. exact (Inc_IN _ _ _ (Comp_Inc _ _) H).
  - exact (In_Comp_P _ _ _ H).
Qed.

(** A bind [v ← S ;; F v] is single-valued when [S] is and every fibre is. *)
Lemma iUnion_subsingleton (S : ZFSet) (F : ZFSet -> ZFSet) :
  is_subsingleton S ->
  (forall v, v ∈ S -> is_subsingleton (F v)) ->
  is_subsingleton (iUnion S F).
Proof.
  intros HS HF x x' Hx Hx'.
  apply iUnion_IN in Hx. destruct Hx as [v [Hv Hx]].
  apply iUnion_IN in Hx'. destruct Hx' as [v' [Hv' Hx']].
  rewrite (HS v v' Hv Hv') in Hx. exact (HF v' Hv' x x' Hx Hx').
Qed.

(** A [Pi] is single-valued when every fibre is: two total graphs that pick
    the same (unique) value at each point are equal. *)
Lemma Pi_subsingleton A (B : ZFSet -> ZFSet) :
  (forall a, a ∈ A -> is_subsingleton (B a)) ->
  is_subsingleton (Pi A B).
Proof.
  intros HB f g Hf Hg. apply set_ext; apply Inc_def; intros p Hp.
  - destruct (In_Pi_inv A B f Hf) as [Hsubf _].
    pose proof (Inc_IN _ _ _ Hsubf Hp) as Hprod.
    apply IN_Prod_EX in Hprod. destruct Hprod as [a [b [Ha [_ Heq]]]]. subst p.
    pose proof (Pi_edge_codomain A B f a b Hf Ha Hp) as HbBa.
    destruct (In_Pi_inv A B g Hg) as [_ [Hgtot _]].
    destruct (Hgtot a Ha) as [b' [Hb'Ba Hedge']].
    rewrite (HB a Ha b b' HbBa Hb'Ba). exact Hedge'.
  - destruct (In_Pi_inv A B g Hg) as [Hsubg _].
    pose proof (Inc_IN _ _ _ Hsubg Hp) as Hprod.
    apply IN_Prod_EX in Hprod. destruct Hprod as [a [b [Ha [_ Heq]]]]. subst p.
    pose proof (Pi_edge_codomain A B g a b Hg Ha Hp) as HbBa.
    destruct (In_Pi_inv A B f Hf) as [_ [Hftot _]].
    destruct (Hftot a Ha) as [b' [Hb'Ba Hedge']].
    rewrite (HB a Ha b b' HbBa Hb'Ba). exact Hedge'.
Qed.

(** The ⊥-strict ground map is single-valued: its two comprehensions have
    mutually exclusive guards ([v ∈ Omega] vs [v ∉ Omega]). *)
Lemma strictN_subsingleton k v : is_subsingleton (strictN k v).
Proof.
  unfold strictN. intros x x' Hx Hx'.
  apply BinUnion_IN in Hx; apply BinUnion_IN in Hx'.
  destruct Hx as [Hx | Hx]; destruct Hx' as [Hx' | Hx'];
    apply In_const_comp_sing in Hx; apply In_const_comp_sing in Hx';
    destruct Hx as [Hx HQ]; destruct Hx' as [Hx' HQ'].
  - rewrite Hx, Hx'. reflexivity.
  - exfalso. exact (HQ' HQ).
  - exfalso. exact (HQ HQ').
  - rewrite Hx, Hx'. reflexivity.
Qed.

(** Applying a *functional* graph at a singleton domain is single-valued. *)
Lemma image_Sing_subsingleton f v :
  isFunction f -> is_subsingleton (image f {| v |}).
Proof.
  intros Hfun x x' Hx Hx'.
  apply image_elim in Hx. destruct Hx as [a [Ha Hax]].
  apply image_elim in Hx'. destruct Hx' as [a' [Ha' Hax']].
  apply IN_Sing_EQ in Ha; apply IN_Sing_EQ in Ha'. subst a a'.
  exact (Hfun v x x' Hax Hax').
Qed.

Theorem evalTm_subsingleton Γ e T :
  has_type Γ e T -> forall ρ, models Γ ρ -> is_subsingleton (evalTm e ρ).
Proof.
  induction 1 as
    [ Γ n
    | Γ T1 T2 e He IHe
    | Γ T1 T2 e1 e2 He1 IHe1 He2 IHe2
    | Γ
    | Γ e He IHe
    | Γ e He IHe
    | Γ e He IHe
    | Γ
    | Γ
    | Γ T e0 e1 e2 He0 IHe0 He1 IHe1 He2 IHe2
    | Γ T e He IHe ];
    intros ρ HM; cbn [evalTm].

  - (* T_var *) apply Sing_is_subsingleton.

  - (* T_lam *)
    apply Pi_subsingleton. intros a Ha.
    apply IHe. apply models_cons; assumption.

  - (* T_app *)
    apply iUnion_subsingleton; [ apply IHe1; exact HM | ].
    intros f Hf.
    pose proof (Inc_IN _ _ _ (soundness _ _ _ He1 ρ HM) Hf) as HfPi.
    destruct (In_Pi_inv _ _ f HfPi) as [_ [_ Hfun]].
    apply iUnion_subsingleton; [ apply IHe2; exact HM | ].
    intros v Hv. apply image_Sing_subsingleton. exact Hfun.

  - (* T_zero *) apply Sing_is_subsingleton.

  - (* T_succ *)
    apply iUnion_subsingleton; [ apply IHe; exact HM | ].
    intros v Hv. apply strictN_subsingleton.

  - (* T_pred *)
    apply iUnion_subsingleton; [ apply IHe; exact HM | ].
    intros v Hv. apply strictN_subsingleton.

  - (* T_iszero: the three branches have pairwise-exclusive guards, so at
       most one fires; classify each element, then close by value or by a
       contradiction between the guards. *)
    apply iUnion_subsingleton; [ apply IHe; exact HM | ].
    intros v Hv x x' Hx Hx'.
    assert (Hd : forall y, y ∈ (⦃ _ ∈ {| natZ 1 |} | v = natZ 0 ⦄
             ∪ ⦃ _ ∈ {| natZ 0 |} | v ∈ Omega /\ v <> natZ 0 ⦄
             ∪ ⦃ _ ∈ {| bot |} | ~ (v ∈ Omega) ⦄) ->
       (y = natZ 1 /\ v = natZ 0)
       \/ (y = natZ 0 /\ (v ∈ Omega /\ v <> natZ 0))
       \/ (y = bot /\ ~ (v ∈ Omega))).
    { intros y Hy. apply BinUnion_IN in Hy. destruct Hy as [Hy | Hy].
      - apply BinUnion_IN in Hy. destruct Hy as [Hy | Hy].
        + left. exact (In_const_comp_sing _ _ _ Hy).
        + right; left. exact (In_const_comp_sing _ _ _ Hy).
      - right; right. exact (In_const_comp_sing _ _ _ Hy). }
    pose proof (Hd x Hx) as Dx. pose proof (Hd x' Hx') as Dx'.
    destruct Dx as [[Ex Q]|[[Ex Q]|[Ex Q]]];
      destruct Dx' as [[Ex' Q']|[[Ex' Q']|[Ex' Q']]];
      subst x x'; try reflexivity.
    all: exfalso;
      repeat match goal with H : _ /\ _ |- _ => destruct H end;
      match goal with
      | [ H : ?w = natZ 0, H' : ?w <> natZ 0 |- _ ] => exact (H' H)
      | [ H : ?w <> natZ 0, H' : ?w = natZ 0 |- _ ] => exact (H H')
      | [ H : ?w = natZ 0, H' : ~ (?w ∈ Omega) |- _ ] =>
          apply H'; rewrite H; apply natZ_mem_omega
      | [ H : ~ (?w ∈ Omega), H' : ?w = natZ 0 |- _ ] =>
          apply H; rewrite H'; apply natZ_mem_omega
      | [ H : ?w ∈ Omega, H' : ~ (?w ∈ Omega) |- _ ] => exact (H' H)
      end.

  - (* T_true *) apply Sing_is_subsingleton.

  - (* T_false *) apply Sing_is_subsingleton.

  - (* T_if: the two branch-guards [b = 1] / [b = 0] are exclusive *)
    apply iUnion_subsingleton; [ apply IHe0; exact HM | ].
    intros b Hb x x' Hx Hx'.
    apply BinUnion_IN in Hx; apply BinUnion_IN in Hx'.
    destruct Hx as [Hx | Hx]; destruct Hx' as [Hx' | Hx'].
    + exact (IHe1 ρ HM x x' (Inc_IN _ _ _ (Comp_Inc _ _) Hx)
                            (Inc_IN _ _ _ (Comp_Inc _ _) Hx')).
    + exfalso.
      pose proof (In_Comp_P _ _ _ Hx) as Q1. pose proof (In_Comp_P _ _ _ Hx') as Q2.
      apply (natZ_neq 1 0 ltac:(discriminate)). rewrite <- Q1, <- Q2. reflexivity.
    + exfalso.
      pose proof (In_Comp_P _ _ _ Hx) as Q1. pose proof (In_Comp_P _ _ _ Hx') as Q2.
      apply (natZ_neq 1 0 ltac:(discriminate)). rewrite <- Q2, <- Q1. reflexivity.
    + exact (IHe2 ρ HM x x' (Inc_IN _ _ _ (Comp_Inc _ _) Hx)
                            (Inc_IN _ _ _ (Comp_Inc _ _) Hx')).

  - (* T_fix *)
    apply iUnion_subsingleton; [ apply IHe; exact HM | ].
    intros f Hf x x' Hx Hx'.
    pose proof (In_Comp_P _ _ _ Hx) as [_ Hux].
    pose proof (In_Comp_P _ _ _ Hx') as [Hx'fix _].
    symmetry. exact (Hux x' Hx'fix).
Qed.

(** Combined with [soundness], this pins down a well-typed [e2 : T] to one of
    two shapes in any model: a singleton [{| a |}] with [a ∈ evalTy T], or [∅]
    (a *divergent* term — e.g. a [fix] with no unique fixed point, like
    [fix (λx:ι. x)], whose function value has every point as a fixed point).

    For the *terminating* shape this discharges the single-valuedness
    hypothesis of [S_beta_sound], so β holds there.  It does **not** make β
    unconditional: a divergent argument denotes [∅], and the *strict*
    application then collapses to [∅] even when the body discards the
    argument — e.g. [(λy:ι. 0) (fix (λx:ι. x))] reduces to [0] but denotes
    [∅].  Making that case sound too would require divergence to denote [⊥]
    rather than [∅] at *every* type (a type-indexed bottom), which the
    ground-only lift here does not provide for a non-unique [fix]. *)

(* ================================================================== *)
(** * The substitution lemma (single-valued argument) and β-soundness. *)
(* ================================================================== *)

(** Environment-shifting identities (proved pointwise, by functional
    extensionality). *)
Lemma env_ins_0 a ρ : env_ins 0 a ρ = env_cons a ρ.
Proof.
  apply functional_extensionality. intros [|n]; reflexivity.
Qed.

Lemma env_ins_S_cons d b c ρ :
  env_ins (S d) b (env_cons c ρ) = env_cons c (env_ins d b ρ).
Proof.
  apply functional_extensionality. intro n. unfold env_ins, env_cons.
  destruct n as [|n]; cbn [Nat.compare].
  - reflexivity.
  - destruct (Nat.compare n d) eqn:E; cbn [Nat.pred]; try reflexivity.
    destruct n as [|m]; [ apply Nat.compare_gt_iff in E; lia | reflexivity ].
Qed.

(** Lifting and inserting a dummy slot cancel: [⟦lift d e⟧] under an
    environment with an extra slot at [d] is just [⟦e⟧]. *)
Lemma evalTm_lift e : forall d b ρ,
  evalTm (lift d e) (env_ins d b ρ) = evalTm e ρ.
Proof.
  induction e; intros d b ρ; cbn [lift evalTm].
  - (* tvar *)
    destruct (Nat.compare n d) eqn:E; cbn [evalTm].
    + (* Eq *) apply Nat.compare_eq_iff in E. subst n.
      unfold env_ins.
      assert (E2 : Nat.compare (S d) d = Gt) by (apply Nat.compare_gt_iff; lia).
      rewrite E2. cbn [Nat.pred]. reflexivity.
    + (* Lt *) unfold env_ins. rewrite E. reflexivity.
    + (* Gt *) apply Nat.compare_gt_iff in E. unfold env_ins.
      assert (E2 : Nat.compare (S n) d = Gt) by (apply Nat.compare_gt_iff; lia).
      rewrite E2. cbn [Nat.pred]. reflexivity.
  - (* tlam *)
    f_equal. apply functional_extensionality. intro a.
    rewrite <- (env_ins_S_cons d b a ρ). apply IHe.
  - (* tapp *) rewrite IHe1, IHe2. reflexivity.
  - reflexivity.
  - rewrite IHe. reflexivity.
  - rewrite IHe. reflexivity.
  - rewrite IHe. reflexivity.
  - reflexivity.
  - reflexivity.
  - rewrite IHe1, IHe2, IHe3. reflexivity.
  - rewrite IHe. reflexivity.
Qed.

(** The substitution lemma, for a *single-valued* substituend.  When
    [evalTm s ρ = {| a |}], substituting [s] for index [k] is inserting the
    value [a] at slot [k] of the environment. *)
Lemma evalTm_subst_sing e : forall k s ρ a,
  evalTm s ρ = {| a |} ->
  evalTm (subst k s e) ρ = evalTm e (env_ins k a ρ).
Proof.
  induction e; intros k s ρ a Hs; cbn [subst evalTm].
  - (* tvar *)
    destruct (Nat.compare n k) eqn:E; cbn [evalTm].
    + apply Nat.compare_eq_iff in E. subst k. rewrite Hs.
      unfold env_ins. rewrite Nat.compare_refl. reflexivity.
    + unfold env_ins. rewrite E. reflexivity.
    + unfold env_ins. rewrite E. reflexivity.
  - (* tlam *)
    f_equal. apply functional_extensionality. intro c.
    assert (Hs' : evalTm (lift 0 s) (env_cons c ρ) = {| a |}).
    { rewrite <- (env_ins_0 c ρ). rewrite (evalTm_lift s 0 c ρ).
      exact Hs. }
    rewrite (IHe (S k) (lift 0 s) (env_cons c ρ) a Hs').
    rewrite (env_ins_S_cons k a c ρ). reflexivity.
  - (* tapp *)
    rewrite (IHe1 k s ρ a Hs), (IHe2 k s ρ a Hs). reflexivity.
  - reflexivity.
  - rewrite (IHe k s ρ a Hs). reflexivity.
  - rewrite (IHe k s ρ a Hs). reflexivity.
  - rewrite (IHe k s ρ a Hs). reflexivity.
  - reflexivity.
  - reflexivity.
  - rewrite (IHe1 k s ρ a Hs), (IHe2 k s ρ a Hs), (IHe3 k s ρ a Hs). reflexivity.
  - rewrite (IHe k s ρ a Hs). reflexivity.
Qed.

(** A union over [Pi A φ] of the [a]-fibres recovers [φ a] when [φ] is
    everywhere a singleton — the [Pi] is then itself a singleton, and the
    application of its one member at the in-domain point [a] is the fibre. *)
Lemma Pi_apply_sing A (φ : ZFSet -> ZFSet) a c :
  a ∈ A ->
  (forall b, b ∈ A -> exists d, φ b = {| d |}) ->
  φ a = {| c |} ->
  (f ← Pi A φ ;; image f {| a |}) = {| c |}.
Proof.
  intros Ha Hsing Hfa.
  destruct (Pi_Sing_ex A φ Hsing) as [F HF].
  rewrite HF, iUnion_Sing_l.
  assert (HFpi : F ∈ Pi A φ) by (rewrite HF; apply IN_Sing).
  rewrite (image_Sing_of_pi A φ F a HFpi Ha).
  pose proof (applyFun_mem_of_pi A φ F a HFpi Ha) as Hm.
  rewrite Hfa in Hm. apply IN_Sing_EQ in Hm. rewrite Hm. reflexivity.
Qed.

(** β-soundness on the deterministic fragment.  When the argument denotes a
    single in-domain value [a] and the body is deterministic, the redex and
    its β-reduct have equal denotations.  Contrast [PCFStrict.v]'s
    [S_beta_unsound]. *)
Theorem S_beta_sound T body e2 ρ a :
  evalTm e2 ρ = {| a |} ->
  a ∈ evalTy T ->
  (forall b, b ∈ evalTy T -> exists c, evalTm body (env_cons b ρ) = {| c |}) ->
  evalTm (tapp (tlam T body) e2) ρ = evalTm (subst 0 e2 body) ρ.
Proof.
  intros He2 Ha Hsing.
  rewrite (evalTm_subst_sing body 0 e2 ρ a He2), env_ins_0.
  destruct (Hsing a Ha) as [c Hc].
  cbn [evalTm]. rewrite He2.
  (* ⟦e2⟧ = {|a|}: the argument bind collapses to the [a]-fibre union *)
  assert (Hstep :
    (f ← Pi (evalTy T) (fun b => evalTm body (env_cons b ρ)) ;;
        v ← {| a |} ;; image f {| v |})
    = (f ← Pi (evalTy T) (fun b => evalTm body (env_cons b ρ)) ;; image f {| a |})).
  { f_equal. apply functional_extensionality. intro f.
    rewrite iUnion_Sing_l. reflexivity. }
  rewrite Hstep.
  rewrite (Pi_apply_sing (evalTy T) (fun b => evalTm body (env_cons b ρ)) a c
             Ha Hsing Hc).
  symmetry. exact Hc.
Qed.

(* ================================================================== *)
(** * The witness that refuted β in [PCFStrict.v] is now sound. *)
(* ================================================================== *)

(** Comprehension over a singleton (as in [PCFStrict.v]). *)
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

(** Edge ↔ applied-value, in a function [f ∈ Pi A B] (as in [PCFStrict.v]). *)
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

(** [loop = fix (λx:ι. succ x)] : the divergent natural. *)
Definition loop : Tm := tfix (tlam TNat (tsucc (tvar 0))).

(** [evalTm loop ρ = {| bot |}]: the only fixed point of the ⊥-strict
    successor is [⊥]. *)
Lemma eval_loop ρ : evalTm loop ρ = {| bot |}.
Proof.
  unfold loop. cbn [evalTm evalTy env_cons].
  (* the abstracted function value is a singleton graph [g] *)
  set (φ := fun a => v ← {| a |} ;; strictN (natZSucc v) v).
  assert (Hphi : forall a, φ a = strictN (natZSucc a) a).
  { intro a. unfold φ. rewrite iUnion_Sing_l. reflexivity. }
  assert (Hsing : forall a, a ∈ (Omega ∪ {| bot |}) -> exists b, φ a = {| b |}).
  { intros a Ha. rewrite Hphi. unfold strictN.
    apply BinUnion_IN in Ha. destruct Ha as [Ha | Ha].
    - rewrite (Comp_Sing_in (natZSucc a) (fun _ => a ∈ Omega) Ha).
      rewrite (Comp_Sing_out bot (fun _ => ~ (a ∈ Omega)) (fun H => H Ha)).
      rewrite BinUnion_Vide_r. eexists; reflexivity.
    - apply IN_Sing_EQ in Ha. subst a.
      rewrite (Comp_Sing_out (natZSucc bot) (fun _ => bot ∈ Omega) bot_not_Omega).
      rewrite (Comp_Sing_in bot (fun _ => ~ (bot ∈ Omega)) bot_not_Omega).
      rewrite BinUnion_Vide_l. eexists; reflexivity. }
  destruct (Pi_Sing_ex (Omega ∪ {| bot |}) φ Hsing) as [g Hg].
  rewrite Hg, iUnion_Sing_l.
  assert (Hgpi : g ∈ Pi (Omega ∪ {| bot |}) φ) by (rewrite Hg; apply IN_Sing).
  assert (Hbotdom : bot ∈ (Omega ∪ {| bot |})) by (apply IN_BinUnion_r, IN_Sing).
  (* [bot] is a fixed point of [g]: g(bot) = bot *)
  assert (Hgbot : applyFun g bot = bot).
  { pose proof (applyFun_mem_of_pi _ φ g bot Hgpi Hbotdom) as Hm.
    rewrite Hphi in Hm. unfold strictN in Hm.
    rewrite (Comp_Sing_out (natZSucc bot) (fun _ => bot ∈ Omega) bot_not_Omega) in Hm.
    rewrite (Comp_Sing_in bot (fun _ => ~ (bot ∈ Omega)) bot_not_Omega) in Hm.
    rewrite BinUnion_Vide_l in Hm. apply IN_Sing_EQ in Hm. exact Hm. }
  assert (Hbotedge : ⟨ bot , bot ⟩ ∈ g).
  { pose proof (pi_value_edge _ φ g bot Hgpi Hbotdom) as He.
    rewrite Hgbot in He. exact He. }
  (* every fixed point of [g] equals [bot] — its uniqueness, used both ways *)
  assert (Huniq : forall x, ⟨ x , x ⟩ ∈ g -> x = bot).
  { intros x Hxx.
    destruct (In_Pi_inv _ _ g Hgpi) as [Hsub _].
    pose proof (Inc_IN _ _ _ Hsub Hxx) as Hprod.
    apply Couple_Prod_IN in Hprod. destruct Hprod as [HxD _].
    pose proof (pi_edge_value _ φ g x x Hgpi Hxx) as Hxval.
    apply BinUnion_IN in HxD. destruct HxD as [HxO | Hxb].
    - (* on a natural [g] is the (≠) successor, so no fixed point *)
      exfalso.
      pose proof (applyFun_mem_of_pi _ φ g x Hgpi (IN_BinUnion_l _ _ _ HxO)) as Hm.
      rewrite Hphi in Hm. unfold strictN in Hm.
      rewrite (Comp_Sing_in (natZSucc x) (fun _ => x ∈ Omega) HxO) in Hm.
      rewrite (Comp_Sing_out bot (fun _ => ~ (x ∈ Omega)) (fun H => H HxO)) in Hm.
      rewrite BinUnion_Vide_r in Hm. apply IN_Sing_EQ in Hm.
      rewrite <- Hxval in Hm.
      destruct (In_omega _ HxO) as [k Hk]. subst x.
      rewrite natZSucc_natZ in Hm.
      exact (natZ_neq k (S k) (fun e => Nat.neq_succ_diag_l k (eq_sym e)) Hm).
    - apply IN_Sing_EQ in Hxb. exact Hxb. }
  apply set_ext; apply Inc_def; intros x Hx.
  - (* the denotation is [⊆ {bot}]: its (unique) fixed point is [bot] *)
    pose proof (In_Comp_P _ _ _ Hx) as [Hxx _].
    rewrite (Huniq x Hxx). apply IN_Sing.
  - (* [bot] is *the* fixed point, so it is in the denotation *)
    apply IN_Sing_EQ in Hx. subst x.
    apply In_P_Comp;
      [ exact (IN_dom _ _ _ Hbotedge)
      | split; [ exact Hbotedge | intros b Hb; exact (Huniq b Hb) ] ].
Qed.

(** [λy:ι. loop] denotes a *single total function* (the all-⊥ graph), an
    element of [ι → ι] — not [∅] as in [PCFStrict.v] ([evalTm_div_fun]).  This is
    obstruction 2 of [PCF.md] dissolved: a divergent-bodied λ is a value. *)
Lemma eval_lam_loop ρ :
  exists G, evalTm (tlam TNat loop) ρ = {| G |} /\ G ∈ evalTy (TArr TNat TNat).
Proof.
  cbn [evalTm].
  assert (Hfib : forall a, a ∈ evalTy TNat -> exists b,
            evalTm loop (env_cons a ρ) = {| b |}).
  { intros a _. rewrite eval_loop. exists bot. reflexivity. }
  destruct (Pi_Sing_ex (evalTy TNat) (fun a => evalTm loop (env_cons a ρ)) Hfib)
    as [G HG].
  exists G. split; [ exact HG | ].
  assert (HGpi : G ∈ Pi (evalTy TNat) (fun a => evalTm loop (env_cons a ρ)))
    by (rewrite HG; apply IN_Sing).
  cbn [evalTy]. revert HGpi. apply Inc_IN.
  apply Pi_Inc_Codomain. intros a Ha. rewrite eval_loop.
  apply Sing_Inc_IN, bot_in_TNat.
Qed.

(** The payoff.  In [PCFStrict.v] the term [(λx:ι→ι. 0) (λy:ι. loop)] denotes [∅]
    ([tot_app_den]) — strictly *not* its β-reduct [0] — which is exactly what
    refutes β ([S_beta_unsound]).  In the lifted model it denotes [{| 0 |}]. *)
Theorem tot_app_lifted ρ :
  evalTm (tapp (tlam (TArr TNat TNat) tzero) (tlam TNat loop)) ρ = {| natZ 0 |}.
Proof.
  destruct (eval_lam_loop ρ) as [G [HG HGty]].
  assert (Hdet : forall b, b ∈ evalTy (TArr TNat TNat) ->
                   exists c, evalTm tzero (env_cons b ρ) = {| c |}).
  { intros b _. cbn [evalTm]. exists (natZ 0). reflexivity. }
  rewrite (S_beta_sound (TArr TNat TNat) tzero (tlam TNat loop) ρ G HG HGty Hdet).
  cbn [subst evalTm]. reflexivity.
Qed.

(** Stated as the β-step itself: the redex and its reduct [tzero] have equal
    denotations.  Compare [PCFStrict.v]'s [S_beta_unsound] / [beta_step], whose
    witness is precisely this redex. *)
Remark beta_step_sound ρ :
  evalTm (tapp (tlam (TArr TNat TNat) tzero) (tlam TNat loop)) ρ = evalTm tzero ρ.
Proof. rewrite tot_app_lifted. cbn [evalTm]. reflexivity. Qed.
