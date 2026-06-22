(* lang/PCF/PCFPointed.v

   A *total*, type-indexed refinement of the ⊥-lifted PCF model of
   [PCFLifted.v].  See [PCF.md] for the design discussion.

   [PCFLifted.v] makes divergence the value [⊥] at *ground* types, but a
   [tfix] with no unique fixed point (e.g. [fix (λx:ι. x)]) and a [tif] with
   a divergent guard still fall back to [∅].  That residual [∅] reintroduces
   the strictness that breaks β on a divergent argument.

   The fix: a *type-indexed bottom* [botT T] — the bottom element of *every*
   type ([⊥] at a ground type, the all-[⊥] function at an arrow) — and a
   *type-directed* evaluator [eval Γ e ρ] that emits [botT T] (rather than
   [∅]) for those divergent cases.  The type [T] is recovered from the
   context by a syntax-directed [infer].

   The payoff is [eval_total]: *every* well-typed term denotes exactly one
   value — never [∅].  (This subsumes both type soundness and the
   single-valuedness of [PCFLifted.evalTm_subsingleton], strengthening
   "subsingleton" to "singleton".)  With totality, the strictness obstruction
   to β is gone; the remaining work for an unconditional β-soundness theorem
   is the standard de Bruijn substitution metatheory, noted at the end. *)

Require Import ZFSet.
Require Import ZFNotation.
Require Import PCFSyntax.
Require Import PCFLifted.
From Stdlib Require Import PeanoNat Lia.
From Stdlib Require Import Classical.   (* excluded middle, for the [tfix] split;
                                           consistent with the development's
                                           [epsilon] + [prop_ext]. *)

(** * Syntax-directed type inference (total; correct on well-typed terms). *)
Fixpoint infer (Γ : Ctx) (e : Tm) : Ty :=
  match e with
  | tvar n     => Γ n
  | tlam T1 e  => TArr T1 (infer (ctx_cons T1 Γ) e)
  | tapp e1 e2 => match infer Γ e1 with TArr _ T2 => T2 | _ => TNat end
  | tzero      => TNat
  | tsucc _    => TNat
  | tpred _    => TNat
  | tiszero _  => TBool
  | ttrue      => TBool
  | tfalse     => TBool
  | tif _ e1 _ => infer Γ e1
  | tfix e     => match infer Γ e with TArr T1 _ => T1 | _ => TNat end
  end.

Lemma infer_sound Γ e T : has_type Γ e T -> infer Γ e = T.
Proof.
  induction 1; cbn [infer]; try reflexivity;
    repeat match goal with H : infer _ _ = _ |- _ => rewrite H; clear H end;
    reflexivity.
Qed.

(** The fixed-point type of a [T → T]-typed function, read off [infer]. *)
Definition fixTy (Γ : Ctx) (e : Tm) : Ty :=
  match infer Γ e with TArr T1 _ => T1 | _ => TNat end.

(** * The type-indexed bottom: the least element of each type. *)
Fixpoint botT (T : Ty) : ZFSet :=
  match T with
  | TNat       => bot
  | TBool      => bot
  | TArr T1 T2 => iUnion (evalTy T1) (fun a => {| ⟨ a , botT T2 ⟩ |})
  end.

Lemma botT_in_evalTy T : botT T ∈ evalTy T.
Proof.
  induction T; cbn [botT evalTy].
  - apply bot_in_TNat.
  - apply bot_in_TBool.
  - apply (iUnion_graph_mem_pi (evalTy T1) (fun _ => evalTy T2) (fun _ => botT T2)).
    intros a Ha. exact IHT2.
Qed.

(** * The type-directed, total evaluator.

    Identical to [PCFLifted.evalTm] except that the two divergent cases emit
    the type-indexed bottom instead of [∅]:
    - [tif] with a non-boolean (i.e. [⊥]) guard yields [botT] of the branch
      type;
    - [tfix] with no *unique* fixed point yields [botT] of the result type. *)
Fixpoint eval (Γ : Ctx) (e : Tm) (ρ : Env) : ZFSet :=
  match e with
  | tvar n     => {| ρ n |}
  | tlam T1 b  =>
      Pi (evalTy T1) (fun a => eval (ctx_cons T1 Γ) b (env_cons a ρ))
  | tapp e1 e2 =>
      f ← eval Γ e1 ρ ;; v ← eval Γ e2 ρ ;; image f {| v |}
  | tzero      => {| natZ 0 |}
  | tsucc e    => v ← eval Γ e ρ ;; strictN (natZSucc v) v
  | tpred e    => v ← eval Γ e ρ ;; strictN (natZPred v) v
  | tiszero e  =>
      v ← eval Γ e ρ ;;
        (⦃ _ ∈ {| natZ 1 |} | v = natZ 0 ⦄
         ∪ ⦃ _ ∈ {| natZ 0 |} | v ∈ Omega /\ v <> natZ 0 ⦄
         ∪ ⦃ _ ∈ {| bot |} | ~ (v ∈ Omega) ⦄)
  | ttrue      => {| natZ 1 |}
  | tfalse     => {| natZ 0 |}
  | tif e0 e1 e2 =>
      b ← eval Γ e0 ρ ;;
        (⦃ _ ∈ eval Γ e1 ρ | b = natZ 1 ⦄
         ∪ ⦃ _ ∈ eval Γ e2 ρ | b = natZ 0 ⦄
         ∪ ⦃ _ ∈ {| botT (infer Γ e1) |} | ~ (b ∈ bools) ⦄)
  | tfix e     =>
      f ← eval Γ e ρ ;;
        (⦃ a ∈ dom f | ⟨ a , a ⟩ ∈ f /\ (forall b, ⟨ b , b ⟩ ∈ f -> b = a) ⦄
         ∪ ⦃ _ ∈ {| botT (fixTy Γ e) |}
              | ~ (exists a, ⟨ a , a ⟩ ∈ f /\ (forall b, ⟨ b , b ⟩ ∈ f -> b = a)) ⦄)
  end.

(** [bot] is not a boolean (it is [Omega], not a numeral). *)
Lemma bot_not_bools : ~ (bot ∈ bools).
Proof.
  unfold bools. intro H. apply BinUnion_IN in H. destruct H as [H | H];
    apply IN_Sing_EQ in H; apply bot_not_Omega; rewrite H; apply natZ_mem_omega.
Qed.

(* ================================================================== *)
(** * Totality: every well-typed term denotes exactly one value. *)
(* ================================================================== *)

Theorem eval_total Γ e T :
  has_type Γ e T ->
  forall ρ, models Γ ρ -> exists a, a ∈ evalTy T /\ eval Γ e ρ = {| a |}.
Proof.
  induction 1 as
    [ Γ n
    | Γ T1 T2 b Hb IHb
    | Γ T1 T2 e1 e2 He1 IHe1 He2 IHe2
    | Γ
    | Γ e He IHe
    | Γ e He IHe
    | Γ e He IHe
    | Γ
    | Γ
    | Γ T e0 e1 e2 He0 IHe0 He1 IHe1 He2 IHe2
    | Γ T e He IHe ];
    intros ρ HM; cbn [eval].

  - (* T_var *) exists (ρ n). split; [ apply HM | reflexivity ].

  - (* T_lam: the body is a singleton at each point, so [Pi] is a singleton *)
    assert (Hf : forall a, a ∈ evalTy T1 ->
              exists c, eval (ctx_cons T1 Γ) b (env_cons a ρ) = {| c |}).
    { intros a Ha. destruct (IHb (env_cons a ρ) (models_cons _ _ _ _ HM Ha)) as [c [_ Hc]].
      exists c. exact Hc. }
    destruct (Pi_Sing_ex (evalTy T1) (fun a => eval (ctx_cons T1 Γ) b (env_cons a ρ)) Hf)
      as [G HG].
    exists G. split.
    + cbn [evalTy].
      assert (HGpi : G ∈ Pi (evalTy T1) (fun a => eval (ctx_cons T1 Γ) b (env_cons a ρ)))
        by (rewrite HG; apply IN_Sing).
      revert HGpi. apply Inc_IN. apply Pi_Inc_Codomain. intros a Ha.
      destruct (IHb (env_cons a ρ) (models_cons _ _ _ _ HM Ha)) as [c [Hcty Hc]].
      rewrite Hc. apply Sing_Inc_IN. exact Hcty.
    + exact HG.

  - (* T_app: apply the (singleton) function value to the (singleton) argument *)
    destruct (IHe1 ρ HM) as [f0 [Hf0ty Hf0]].
    destruct (IHe2 ρ HM) as [v0 [Hv0ty Hv0]].
    cbn [evalTy] in Hf0ty.
    exists (applyFun f0 v0). rewrite Hf0, Hv0, !iUnion_Sing_l.
    rewrite (image_Sing_of_pi (evalTy T1) (fun _ => evalTy T2) f0 v0 Hf0ty Hv0ty).
    split; [ | reflexivity ].
    exact (applyFun_mem_of_pi (evalTy T1) (fun _ => evalTy T2) f0 v0 Hf0ty Hv0ty).

  - (* T_zero *) exists (natZ 0). split; [ apply natZ_in_TNat | reflexivity ].

  - (* T_succ *)
    destruct (IHe ρ HM) as [v0 [Hv0 Hev]]. rewrite Hev, iUnion_Sing_l. unfold strictN.
    cbn [evalTy] in Hv0. apply BinUnion_IN in Hv0. destruct Hv0 as [Hv0 | Hv0].
    + exists (natZSucc v0).
      rewrite (Comp_Sing_in (natZSucc v0) (fun _ => v0 ∈ Omega) Hv0).
      rewrite (Comp_Sing_out bot (fun _ => ~ (v0 ∈ Omega)) (fun H => H Hv0)).
      rewrite BinUnion_Vide_r. split; [ | reflexivity ].
      destruct (In_omega _ Hv0) as [k Hk]. subst v0. rewrite natZSucc_natZ. apply natZ_in_TNat.
    + apply IN_Sing_EQ in Hv0. subst v0. exists bot.
      rewrite (Comp_Sing_out (natZSucc bot) (fun _ => bot ∈ Omega) bot_not_Omega).
      rewrite (Comp_Sing_in bot (fun _ => ~ (bot ∈ Omega)) bot_not_Omega).
      rewrite BinUnion_Vide_l. split; [ apply bot_in_TNat | reflexivity ].

  - (* T_pred *)
    destruct (IHe ρ HM) as [v0 [Hv0 Hev]]. rewrite Hev, iUnion_Sing_l. unfold strictN.
    cbn [evalTy] in Hv0. apply BinUnion_IN in Hv0. destruct Hv0 as [Hv0 | Hv0].
    + exists (natZPred v0).
      rewrite (Comp_Sing_in (natZPred v0) (fun _ => v0 ∈ Omega) Hv0).
      rewrite (Comp_Sing_out bot (fun _ => ~ (v0 ∈ Omega)) (fun H => H Hv0)).
      rewrite BinUnion_Vide_r. split; [ | reflexivity ].
      destruct (In_omega _ Hv0) as [k Hk]. subst v0. rewrite natZPred_natZ. apply natZ_in_TNat.
    + apply IN_Sing_EQ in Hv0. subst v0. exists bot.
      rewrite (Comp_Sing_out (natZPred bot) (fun _ => bot ∈ Omega) bot_not_Omega).
      rewrite (Comp_Sing_in bot (fun _ => ~ (bot ∈ Omega)) bot_not_Omega).
      rewrite BinUnion_Vide_l. split; [ apply bot_in_TNat | reflexivity ].

  - (* T_iszero *)
    assert (Hbn : forall m, bot <> natZ m)
      by (intros m HH; apply bot_not_Omega; rewrite HH; apply natZ_mem_omega).
    destruct (IHe ρ HM) as [v0 [Hv0 Hev]]. rewrite Hev, iUnion_Sing_l.
    cbn [evalTy] in Hv0. apply BinUnion_IN in Hv0. destruct Hv0 as [Hv0 | Hv0].
    + destruct (In_omega _ Hv0) as [k Hk]. subst v0. destruct k as [|k].
      * exists (natZ 1).
        rewrite (Comp_Sing_in (natZ 1) (fun _ => natZ 0 = natZ 0) eq_refl).
        rewrite (Comp_Sing_out (natZ 0) (fun _ => natZ 0 ∈ Omega /\ natZ 0 <> natZ 0)
                  (fun H => match H with conj _ q => q eq_refl end)).
        rewrite (Comp_Sing_out bot (fun _ => ~ (natZ 0 ∈ Omega))
                  (fun H => H (natZ_mem_omega 0))).
        rewrite BinUnion_Vide_r, BinUnion_Vide_r.
        split; [ apply natZ1_in_TBool | reflexivity ].
      * exists (natZ 0).
        rewrite (Comp_Sing_out (natZ 1) (fun _ => natZ (S k) = natZ 0)
                  (natZ_neq (S k) 0 ltac:(discriminate))).
        rewrite (Comp_Sing_in (natZ 0) (fun _ => natZ (S k) ∈ Omega /\ natZ (S k) <> natZ 0)
                  (conj (natZ_mem_omega (S k)) (natZ_neq (S k) 0 ltac:(discriminate)))).
        rewrite (Comp_Sing_out bot (fun _ => ~ (natZ (S k) ∈ Omega))
                  (fun H => H (natZ_mem_omega (S k)))).
        rewrite BinUnion_Vide_r, BinUnion_Vide_l.
        split; [ apply natZ0_in_TBool | reflexivity ].
    + apply IN_Sing_EQ in Hv0. subst v0. exists bot.
      rewrite (Comp_Sing_out (natZ 1) (fun _ => bot = natZ 0) (Hbn 0)).
      rewrite (Comp_Sing_out (natZ 0) (fun _ => bot ∈ Omega /\ bot <> natZ 0)
                (fun H => match H with conj p _ => bot_not_Omega p end)).
      rewrite (Comp_Sing_in bot (fun _ => ~ (bot ∈ Omega)) bot_not_Omega).
      rewrite BinUnion_Vide_l, BinUnion_Vide_l.
      split; [ apply bot_in_TBool | reflexivity ].

  - (* T_true *) exists (natZ 1). split; [ apply natZ1_in_TBool | reflexivity ].

  - (* T_false *) exists (natZ 0). split; [ apply natZ0_in_TBool | reflexivity ].

  - (* T_if *)
    assert (Hbn : forall m, bot <> natZ m)
      by (intros m HH; apply bot_not_Omega; rewrite HH; apply natZ_mem_omega).
    destruct (IHe0 ρ HM) as [b0 [Hb0 Heb0]].
    destruct (IHe1 ρ HM) as [v1 [Hv1 Hev1]].
    destruct (IHe2 ρ HM) as [v2 [Hv2 Hev2]].
    rewrite Heb0, iUnion_Sing_l, Hev1, Hev2.
    rewrite (infer_sound _ _ _ He1).
    cbn [evalTy] in Hb0. apply BinUnion_IN in Hb0. destruct Hb0 as [Hb0 | Hb0].
    + unfold bools in Hb0. apply BinUnion_IN in Hb0. destruct Hb0 as [Hb0 | Hb0];
        apply IN_Sing_EQ in Hb0; subst b0.
      * exists v2.
        rewrite (Comp_Sing_out v1 (fun _ => natZ 0 = natZ 1) (natZ_neq 0 1 ltac:(discriminate))).
        rewrite (Comp_Sing_in v2 (fun _ => natZ 0 = natZ 0) eq_refl).
        rewrite (Comp_Sing_out (botT T) (fun _ => ~ (natZ 0 ∈ bools))
                  (fun H => H (IN_BinUnion_l _ _ _ (IN_Sing _)))).
        rewrite BinUnion_Vide_l, BinUnion_Vide_r.
        split; [ exact Hv2 | reflexivity ].
      * exists v1.
        rewrite (Comp_Sing_in v1 (fun _ => natZ 1 = natZ 1) eq_refl).
        rewrite (Comp_Sing_out v2 (fun _ => natZ 1 = natZ 0) (natZ_neq 1 0 ltac:(discriminate))).
        rewrite (Comp_Sing_out (botT T) (fun _ => ~ (natZ 1 ∈ bools))
                  (fun H => H (IN_BinUnion_r _ _ _ (IN_Sing _)))).
        rewrite BinUnion_Vide_r, BinUnion_Vide_r.
        split; [ exact Hv1 | reflexivity ].
    + apply IN_Sing_EQ in Hb0; subst b0. exists (botT T).
      rewrite (Comp_Sing_out v1 (fun _ => bot = natZ 1) (Hbn 1)).
      rewrite (Comp_Sing_out v2 (fun _ => bot = natZ 0) (Hbn 0)).
      rewrite (Comp_Sing_in (botT T) (fun _ => ~ (bot ∈ bools)) bot_not_bools).
      rewrite BinUnion_Vide_l, BinUnion_Vide_l.
      split; [ apply botT_in_evalTy | reflexivity ].

  - (* T_fix: the unique fixed point, or [botT T] when there is none/several *)
    destruct (IHe ρ HM) as [f0 [Hf0 Hef]]. cbn [evalTy] in Hf0.
    rewrite Hef, iUnion_Sing_l.
    assert (HfixT : fixTy Γ e = T)
      by (unfold fixTy; rewrite (infer_sound _ _ _ He); reflexivity).
    rewrite HfixT.
    destruct (classic (exists a0, ⟨ a0, a0 ⟩ ∈ f0 /\ (forall b, ⟨ b, b ⟩ ∈ f0 -> b = a0)))
      as [Hp | Hnp].
    + destruct Hp as [astar [Hfix Huniq]].
      exists astar. split.
      * destruct (In_Pi_inv _ _ f0 Hf0) as [Hsub _].
        pose proof (Inc_IN _ _ _ Hsub Hfix) as Hprod.
        apply Couple_Prod_IN in Hprod. exact (match Hprod with conj p _ => p end).
      * assert (HU : ⦃ a0 ∈ dom f0 | ⟨ a0, a0 ⟩ ∈ f0 /\ (forall b, ⟨ b, b ⟩ ∈ f0 -> b = a0) ⦄
                     = {| astar |}).
        { apply set_ext; apply Inc_def; intros x Hx.
          - pose proof (In_Comp_P _ _ _ Hx) as [Hfx Hux].
            rewrite <- (Hux astar Hfix). apply IN_Sing.
          - apply IN_Sing_EQ in Hx. subst x.
            apply In_P_Comp; [ exact (IN_dom _ _ _ Hfix) | split; [ exact Hfix | exact Huniq ] ]. }
        rewrite HU.
        rewrite (Comp_Sing_out (botT T)
          (fun _ => ~ (exists a0, ⟨ a0, a0 ⟩ ∈ f0 /\ (forall b, ⟨ b, b ⟩ ∈ f0 -> b = a0)))
          (fun H => H (ex_intro _ astar (conj Hfix Huniq)))).
        rewrite BinUnion_Vide_r. reflexivity.
    + exists (botT T). split; [ apply botT_in_evalTy | ].
      assert (HU : ⦃ a0 ∈ dom f0 | ⟨ a0, a0 ⟩ ∈ f0 /\ (forall b, ⟨ b, b ⟩ ∈ f0 -> b = a0) ⦄
                   = ∅).
      { apply set_ext; apply Inc_def; intros x Hx; [ | destruct (not_In_Empty _ Hx) ].
        exfalso. apply Hnp. exists x. exact (In_Comp_P _ _ _ Hx). }
      rewrite HU.
      rewrite (Comp_Sing_in (botT T)
        (fun _ => ~ (exists a0, ⟨ a0, a0 ⟩ ∈ f0 /\ (forall b, ⟨ b, b ⟩ ∈ f0 -> b = a0))) Hnp).
      rewrite BinUnion_Vide_l. reflexivity.
Qed.

(** Totality subsumes both earlier results, now with no [∅] escape hatch:

    - *type soundness* — the denotation lies in the type; and
    - *determinism* — it is a singleton (stronger than the [subsingleton] of
      [PCFLifted.evalTm_subsingleton], which still allowed [∅]). *)
Corollary eval_sound Γ e T ρ :
  has_type Γ e T -> models Γ ρ -> eval Γ e ρ ⊆ evalTy T.
Proof.
  intros HT HM. destruct (eval_total Γ e T HT ρ HM) as [a [Ha Hev]].
  rewrite Hev. apply Sing_Inc_IN. exact Ha.
Qed.

Corollary eval_single_valued Γ e T ρ :
  has_type Γ e T -> models Γ ρ -> is_subsingleton (eval Γ e ρ).
Proof.
  intros HT HM. destruct (eval_total Γ e T HT ρ HM) as [a [_ Hev]].
  rewrite Hev. apply Sing_is_subsingleton.
Qed.

(** A well-typed term *converges* — its denotation is always inhabited. *)
Corollary eval_nonempty Γ e T ρ :
  has_type Γ e T -> models Γ ρ -> is_nonempty (eval Γ e ρ).
Proof.
  intros HT HM. destruct (eval_total Γ e T HT ρ HM) as [a [_ Hev]].
  exists a. rewrite Hev. apply IN_Sing.
Qed.

(* ================================================================== *)
(** * Toward unconditional β.

    With [eval_total] the strictness obstruction is gone: a well-typed
    argument is *always* a singleton (never [∅]), so the bind in [tapp]
    never collapses.  What remains for an unconditional β-soundness theorem
    [eval Γ (tapp (tlam T1 body) e2) ρ = eval Γ (subst 0 e2 body) ρ] is the
    standard de Bruijn substitution metatheory for the type-directed [eval]:
    a [lift] lemma and a substitution lemma threading the context insertion
    (so the [infer]-computed bottom types at [tif]/[tfix] line up).  Those
    are pure syntactic bookkeeping — [infer] commutes with [lift]/[subst]
    unconditionally — and no longer interact with any [∅]. *)
