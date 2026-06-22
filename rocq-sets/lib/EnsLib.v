(* EnsLib.v: Rocq port of ../../lean/set-semantics/SetSemantics/ZFLib.lean

   Notes on Lean → Rocq mapping:
   - Lean's ZFSet (quotient of PSet by extensional equality) is replaced
     by Ens, with extensional equality ≃ (= EQ) in place of Lean =.
   - The pair notation ⟪a,b⟫ is the Werner couple ⟨a,b⟩ from EnsNotation.v.
   - Triple ⟪a,b,c⟫ is ⟨a, ⟨b, c⟩⟩.
   - Lean's ZFSet.sep φ E becomes Comp E φ (notation ⦃ x ∈ E | P ⦄).
   - Lean's ⋃ i : I, f i becomes iUnion I f (notation ⦃ F | x ∈ E ⦄).
   - Lean's ZFSet.prod is Prod (from Cartesian.v).
   - Naturals: Lean's natZ n ≡ Nat n; natSet ≡ Omega (notation ω).
   - Where Lean uses Classical.choose, we use Coq-level computation
     (e.g. Nat_add on the underlying ℕ). *)

Require Import Sets.
Require Import Axioms.
Require Import Cartesian.
Require Import Omega.
Require Import EnsNotation.

From Stdlib Require Import Setoid Morphisms PropExtensionality.

(** EQ as a setoid equivalence + Proper instances for the standard
    operations, so that [setoid_rewrite] can drive rewrites under [≃].

    The instances are derived from the existing soundness lemmas
    (Sing_sound, Couple_sound_*, Paire_sound_*, Union_sound, natZAdd_sound).
    The [iUnion] instance gets a full [(EQ ==> (EQ ==> EQ) ==> EQ)] shape:
    rewriting the function argument requires the relation [(EQ ==> EQ)],
    so the rewritten function must respect [≃] — discharged automatically
    by typeclass resolution when the function is built from registered
    Proper operations. ***)

#[export] Instance EQ_Equivalence : Equivalence EQ.
Proof.
  constructor.
  - exact EQ_refl.
  - exact EQ_sym.
  - intros x y z; apply EQ_tran.
Defined.

#[export] Instance Sing_Proper : Proper (EQ ==> EQ) Sing.
Proof. intros x y H; exact (Sing_sound _ _ H). Qed.

#[export] Instance Paire_Proper : Proper (EQ ==> EQ ==> EQ) Paire.
Proof.
  intros a a' Ha b b' Hb.
  apply EQ_tran with (Paire a b').
  - apply Paire_sound_right; exact Hb.
  - apply Paire_sound_left; exact Ha.
Qed.

#[export] Instance Couple_Proper : Proper (EQ ==> EQ ==> EQ) Couple.
Proof.
  intros a a' Ha b b' Hb.
  apply EQ_tran with (Couple a b').
  - apply Couple_sound_right; exact Hb.
  - apply Couple_sound_left; exact Ha.
Qed.

Lemma Prod_sound :
  forall E E' F F' : Ens,
  EQ E F -> EQ E' F' -> EQ (Prod E E') (Prod F F').
Proof.
  intros E E' F F' HE HE'.
  apply INC_EQ; unfold INC; intros x Hx.
  - destruct (IN_Prod_EXType _ _ _ Hx) as [A [B HAB]].
    assert (HpEE : IN (Couple A B) (Prod E E'))
      by (apply IN_sound_left with x;
          [apply EQ_sym; exact HAB | exact Hx]).
    destruct (Couple_Prod_IN _ _ _ _ HpEE) as [HA HB].
    apply IN_sound_left with (Couple A B); [exact HAB |].
    apply Couple_IN_Prod.
    + apply IN_sound_right with E; assumption.
    + apply IN_sound_right with E'; assumption.
  - destruct (IN_Prod_EXType _ _ _ Hx) as [A [B HAB]].
    assert (HpFF : IN (Couple A B) (Prod F F'))
      by (apply IN_sound_left with x;
          [apply EQ_sym; exact HAB | exact Hx]).
    destruct (Couple_Prod_IN _ _ _ _ HpFF) as [HA HB].
    apply IN_sound_left with (Couple A B); [exact HAB |].
    apply Couple_IN_Prod.
    + apply IN_sound_right with F; [apply EQ_sym; exact HE | exact HA].
    + apply IN_sound_right with F'; [apply EQ_sym; exact HE' | exact HB].
Qed.

#[export] Instance Prod_Proper : Proper (EQ ==> EQ ==> EQ) Prod.
Proof. intros E E' HE F F' HF; apply Prod_sound; assumption. Qed.

#[export] Instance Power_Proper : Proper (EQ ==> EQ) Power.
Proof. intros E E' H; exact (Power_sound _ _ H). Qed.

#[export] Instance Union_Proper : Proper (EQ ==> EQ) Union.
Proof. intros E E' H; exact (Union_sound _ _ H). Qed.

#[export] Instance BinUnion_Proper : Proper (EQ ==> EQ ==> EQ) BinUnion.
Proof.
  intros a a' Ha b b' Hb. unfold BinUnion.
  apply Union_sound. apply Paire_Proper; assumption.
Qed.

(** Pointwise [Proper] instances for [IN] and [INC] (the latter
    derived directly from [IN_sound_*]). *)

#[export] Instance IN_Proper_left {e} :
  Proper (EQ ==> eq) (fun x : Ens => IN x e).
Proof.
  intros E E' H. apply propositional_extensionality.
  split; eapply IN_sound_left; [exact H | apply EQ_sym; exact H].
Qed.

#[export] Instance IN_Proper_right {x} : Proper (EQ ==> eq) (IN x).
Proof.
  intros E E' H. apply propositional_extensionality.
  split; eapply IN_sound_right; [exact H | apply EQ_sym; exact H].
Qed.

#[export] Instance INC_Proper_left {e} :
  Proper (EQ ==> eq) (fun x : Ens => INC x e).
Proof.
  intros E E' H. apply propositional_extensionality.
  split; intros HINC y Hy.
  - apply HINC, IN_sound_right with E';
      [apply EQ_sym; exact H | exact Hy].
  - apply HINC, IN_sound_right with E; assumption.
Qed.

#[export] Instance INC_Proper_right {x} : Proper (EQ ==> eq) (INC x).
Proof.
  intros E E' H. apply propositional_extensionality.
  split; intros HINC y Hy.
  - apply IN_sound_right with E;     [exact H | apply HINC; exact Hy].
  - apply IN_sound_right with E';
      [apply EQ_sym; exact H | apply HINC; exact Hy].
Qed.


(** Reusable lemmas about Union and Comp ***)

Lemma Union_Sing : forall E : Ens, ⋃ {| E |} ≃ E.
Proof.
  intro E; apply INC_EQ; unfold INC; intros x Hx.
  - destruct (Union_IN _ _ Hx) as [y [Hy Hxy]].
    apply IN_sound_right with y; [apply IN_Sing_EQ|]; assumption.
  - apply IN_Union with E; [apply IN_Sing|]; assumption.
Qed.

Lemma Comp_sound_left :
  forall (E E' : Ens) (P : Ens -> Prop),
  (forall x y, P x -> x ≃ y -> P y) ->
  E ≃ E' -> Comp E P ≃ Comp E' P.
Proof.
  intros E E' P HP HE.
  apply INC_EQ; unfold INC; intros x Hx.
  - assert (HxE : x ∈ E) by (exact (Comp_INC E P x Hx)).
    assert (HPx : P x) by (apply IN_Comp_P with E; assumption).
    apply IN_P_Comp; [assumption | apply IN_sound_right with E | ]; assumption.
  - assert (HxE' : x ∈ E') by (exact (Comp_INC E' P x Hx)).
    assert (HPx : P x) by (apply IN_Comp_P with E'; assumption).
    apply IN_P_Comp; [assumption | apply IN_sound_right with E';
      [apply EQ_sym | ] | ]; assumption.
Qed.


(** Simplification lemmas for iUnion

  Intro needs F extensionally sound (EQ-respecting) since elements of E are
  only EQ-witnessed by the Aczel index; elim does not. ***)

Theorem IN_iUnion :
  forall (E : Ens) (F : Ens -> Ens) (y x : Ens),
  (forall u v : Ens, u ≃ v -> F u ≃ F v) ->
  y ∈ E -> x ∈ F y -> x ∈ (z ← E ;; F z).
Proof.
  intros E F y x Fsnd Hy Hx.
  destruct E as [IE fE].
  destruct (IN_EXType (sup IE fE) y Hy) as [a Ha].
  change (iUnion (sup IE fE) F)
    with (Union (sup IE (fun a' => F (fE a')))).
  apply IN_Union with (F (fE a)).
  - exists a; apply EQ_refl.
  - apply IN_sound_right with (F y).
    + apply Fsnd; exact Ha.
    + exact Hx.
Qed.

Theorem iUnion_IN :
  forall (E : Ens) (F : Ens -> Ens) (x : Ens),
   x ∈ (z ← E ;; F z)  ->
  EXType _ (fun y : Ens => y ∈ E /\ x ∈ F y).
Proof.
  intros E F x H.
  destruct E as [IE fE].
  change (iUnion (sup IE fE) F)
    with (Union (sup IE (fun a' => F (fE a')))) in H.
  destruct (Union_IN _ _ H) as [E1 [HE1 Hx]].
  destruct HE1 as [a Ha].
  exists (fE a); split.
  - exists a; apply EQ_refl.
  - apply IN_sound_right with E1; assumption.
Qed.

Lemma iUnion_Sing_l E F :
  x ← {| E |} ;; F x ≃ F E.
Proof.
  apply INC_EQ; unfold INC; intros x Hx.
  - unfold iUnion, Sing, Paire in Hx; cbn beta iota in Hx.
    destruct (Union_IN _ _ Hx) as [y [[b Hb] Hxy]].
    destruct b; cbn in Hb;
      apply IN_sound_right with y; assumption.
  - unfold iUnion, Sing, Paire; cbn beta iota.
    apply IN_Union with (F E).
    + exists true; cbn; apply EQ_refl.
    + exact Hx.
Qed.

Lemma iUnion_Sing_r E :
  x ← E ;; {| x |} ≃ E.
Proof.
  apply INC_EQ; unfold INC; intros x Hx.
  - destruct (iUnion_IN _ _ _ Hx) as [y [HyE HxSy]].
    apply IN_Sing_EQ in HxSy.
    apply IN_sound_left with y; [apply EQ_sym; exact HxSy | exact HyE].
  - apply IN_iUnion with (y := x).
    + intros u v Huv; apply Sing_sound; exact Huv.
    + exact Hx.
    + apply IN_Sing.
Qed.

(** Soundness of iUnion in its index argument.

   Used by clients (e.g. Timbda0's [test_nested_add]) to evaluate a sub-
   expression sitting in the index position of an outer [iUnion]. The
   side condition is that the body function [F] respects [≃], so that
   replacing the index by an EQ-equal one is valid. ***)

Lemma iUnion_sound_l :
  forall (E E' : Ens) (F : Ens -> Ens),
  (forall u v : Ens, u ≃ v -> F u ≃ F v) ->
  E ≃ E' ->
  iUnion E F ≃ iUnion E' F.
Proof.
  intros E E' F HF HEE.
  apply INC_EQ; unfold INC; intros x Hx.
  - destruct (iUnion_IN _ _ _ Hx) as [y [HyE Hxy]].
    apply IN_iUnion with y; auto.
    apply IN_sound_right with E; assumption.
  - destruct (iUnion_IN _ _ _ Hx) as [y [HyE' Hxy]].
    apply IN_iUnion with y; auto.
    apply IN_sound_right with E'; [apply EQ_sym|]; assumption.
Qed.

Lemma iUnion_assoc E F G
  (HF : forall u v : Ens, u ≃ v -> F u ≃ F v)
  (HG : forall u v : Ens, u ≃ v -> G u ≃ G v) :
  y ← (x ← E ;; F x) ;; G y ≃  x ← E ;; y ← F x ;; G y.
Proof.
  apply INC_EQ; unfold INC; intros z Hz.
  - destruct (iUnion_IN _ _ _ Hz) as [y [HyU HzGy]].
    destruct (iUnion_IN _ _ _ HyU) as [x [HxE HyFx]].
    apply IN_iUnion with (y := x).
    + intros u v Huv. apply iUnion_sound_l; [exact HG | exact (HF _ _ Huv)].
    + exact HxE.
    + apply IN_iUnion with (y := y); [exact HG | exact HyFx | exact HzGy].
  - destruct (iUnion_IN _ _ _ Hz) as [x [HxE Hzx]].
    destruct (iUnion_IN _ _ _ Hzx) as [y [HyFx HzGy]].
    apply IN_iUnion with (y := y).
    + exact HG.
    + apply IN_iUnion with (y := x); [exact HF | exact HxE | exact HyFx].
    + exact HzGy.
Qed.

(** [iUnion] as a setoid morphism in both arguments.

   Body relation is [(EQ ==> EQ)] (respectful), so two functions are
   related iff EQ-equal inputs yield EQ-equal outputs. From this we
   derive that each side individually respects [EQ] (needed to use
   IN_iUnion). ***)

#[export] Instance iUnion_Proper :
  Proper (EQ ==> (EQ ==> EQ) ==> EQ) iUnion.
Proof.
  intros E E' HEE F G HFG.
  assert (HFsnd : forall u v, u ≃ v -> F u ≃ F v).
  { intros u v Huv.
    apply EQ_tran with (G v); [apply HFG; exact Huv|].
    apply EQ_sym; apply HFG; apply EQ_refl. }
  assert (HGsnd : forall u v, u ≃ v -> G u ≃ G v).
  { intros u v Huv.
    apply EQ_tran with (F u);
      [apply EQ_sym; apply HFG; apply EQ_refl | apply HFG; exact Huv]. }
  apply INC_EQ; unfold INC; intros x Hx.
  - destruct (iUnion_IN _ _ _ Hx) as [y [HyE Hxy]].
    apply IN_iUnion with y; auto.
    + apply IN_sound_right with E; assumption.
    + apply IN_sound_right with (F y); [apply HFG; apply EQ_refl | exact Hxy].
  - destruct (iUnion_IN _ _ _ Hx) as [y [HyE' Hxy]].
    apply IN_iUnion with y; auto.
    + apply IN_sound_right with E'; [apply EQ_sym|]; assumption.
    + apply IN_sound_right with (G y);
        [apply EQ_sym; apply HFG; apply EQ_refl | exact Hxy].
Qed.


Lemma Sing_is_subsingleton : forall a : Ens, is_subsingleton {| a |}.
Proof.
  intros a y z Hy Hz.
  apply EQ_tran with a.
  apply IN_Sing_EQ; assumption.
  apply EQ_sym; apply IN_Sing_EQ; assumption.
Qed.


(** pfst / psnd soundness and projection equations ***)

Lemma is_subsingleton_sound :
  forall x y : Ens, x ≃ y -> is_subsingleton x -> is_subsingleton y.
Proof.
  intros x y Hxy Hx u v Hu Hv.
  apply Hx;
    (apply IN_sound_right with y; [apply EQ_sym|]; assumption).
Qed.

Lemma not_subsingleton_PaireVS :
  forall b : Ens, ~ is_subsingleton (Paire ∅ {| b |}).
Proof.
  intros b Hsub.
  assert (HVS : ∅ ≃ {| b |}) by
    (apply Hsub; [apply IN_Paire_left | apply IN_Paire_right]).
  elim (not_EQ_Vide_Sing _ HVS).
Qed.

Lemma is_nonempty_Sing : forall E : Ens, is_nonempty {| E |}.
Proof. intro E; exists E; apply IN_Sing. Qed.

Lemma not_nonempty_Vide : ~ is_nonempty ∅.
Proof. intros [y Hy]; elim (Vide_est_vide _ Hy). Qed.

Lemma is_nonempty_sound :
  forall x y : Ens, x ≃ y -> is_nonempty x -> is_nonempty y.
Proof.
  intros x y Hxy [u Hu]; exists u.
  apply IN_sound_right with x; assumption.
Qed.

Lemma pfst_sound : forall x y : Ens, x ≃ y -> pfst x ≃ pfst y.
Proof.
  intros x y Hxy; unfold pfst.
  apply Union_sound; apply Union_sound.
  apply Comp_sound_left;
    [intros w1 w2 Pw Heq; apply is_subsingleton_sound with w1 | ];
    assumption.
Qed.

Lemma psnd_sound : forall x y : Ens, x ≃ y -> psnd x ≃ psnd y.
Proof.
  intros x y Hxy; unfold psnd.
  apply Union_sound; apply Union_sound.
  apply Comp_sound_left;
    [intros w1 w2 Pw Heq; apply is_nonempty_sound with w1; assumption | ].
  apply Union_sound; apply Comp_sound_left;
    [intros w1 w2 Pw Heq Hw2; apply Pw;
       apply is_subsingleton_sound with w2; [apply EQ_sym|]; assumption | ];
    assumption.
Qed.

#[export] Instance pfst_Proper : Proper (EQ ==> EQ) pfst.
Proof. intros x y H; exact (pfst_sound _ _ H). Qed.

#[export] Instance psnd_Proper : Proper (EQ ==> EQ) psnd.
Proof. intros x y H; exact (psnd_sound _ _ H). Qed.

Theorem pfst_Couple : forall a b : Ens, pfst ⟨ a , b ⟩ ≃ a.
Proof.
  intros a b; unfold pfst, Couple.
  assert (HComp :
    Comp (Paire {| a |} (Paire ∅ {| b |})) is_subsingleton ≃ {| {| a |} |}).
  { apply INC_EQ; unfold INC; intros x Hx.
    - assert (Hx_in : x ∈ Paire {| a |} (Paire ∅ {| b |}))
        by (exact (Comp_INC _ _ x Hx)).
      assert (Hx_sub : is_subsingleton x).
      { apply IN_Comp_P with (E := Paire {| a |} (Paire ∅ {| b |}));
          [intros w1 w2 Pw Heq; apply is_subsingleton_sound with w1|];
          assumption. }
      destruct (Paire_IN _ _ _ Hx_in) as [Heq | Heq].
      + apply IN_sound_left with {| a |};
          [apply EQ_sym; assumption | apply IN_Sing].
      + exfalso; apply (not_subsingleton_PaireVS b).
        apply is_subsingleton_sound with x; assumption.
    - assert (Heq : x ≃ {| a |}) by (apply IN_Sing_EQ; exact Hx).
      apply IN_P_Comp;
        [ intros w1 w2 Pw Heq'; apply is_subsingleton_sound with w1; assumption
        | apply IN_sound_left with {| a |};
            [apply EQ_sym; assumption | apply IN_Paire_left]
        | apply is_subsingleton_sound with {| a |};
            [apply EQ_sym; assumption | apply Sing_is_subsingleton] ]. }
  apply EQ_tran with (⋃ ⋃ {| {| a |} |}).
  - apply Union_sound; apply Union_sound; exact HComp.
  - apply EQ_tran with (⋃ {| a |}); [apply Union_sound|]; apply Union_Sing.
Qed.

Theorem psnd_Couple : forall a b : Ens, psnd ⟨ a , b ⟩ ≃ b.
Proof.
  intros a b; unfold psnd, Couple.
  pose (notSub := fun x : Ens => is_subsingleton x -> False).
  assert (Hne_sound :
    forall w1 w2 : Ens, is_nonempty w1 -> w1 ≃ w2 -> is_nonempty w2).
  { intros w1 w2 P Heq; apply is_nonempty_sound with w1; assumption. }
  assert (HComp1 :
    Comp (Paire {| a |} (Paire ∅ {| b |})) notSub ≃ {| Paire ∅ {| b |} |}).
  { apply INC_EQ; unfold INC; intros x Hx.
    - assert (Hx_in : x ∈ Paire {| a |} (Paire ∅ {| b |}))
        by (exact (Comp_INC _ _ x Hx)).
      assert (Hx_ns : notSub x).
      { apply IN_Comp_P with (E := Paire {| a |} (Paire ∅ {| b |}));
          [intros w1 w2 Pw Heq Hw2; apply Pw;
            apply is_subsingleton_sound with w2;
              [apply EQ_sym|]; assumption | assumption]. }
      destruct (Paire_IN _ _ _ Hx_in) as [Heq | Heq].
      + exfalso; apply Hx_ns.
        apply is_subsingleton_sound with {| a |};
          [apply EQ_sym; assumption | apply Sing_is_subsingleton].
      + apply IN_sound_left with (Paire ∅ {| b |});
          [apply EQ_sym; assumption | apply IN_Sing].
    - assert (Heq : x ≃ Paire ∅ {| b |}) by (apply IN_Sing_EQ; exact Hx).
      apply IN_P_Comp;
        [ intros w1 w2 Pw Heq' Hw2; apply Pw;
            apply is_subsingleton_sound with w2;
              [apply EQ_sym|]; assumption
        | apply IN_sound_left with (Paire ∅ {| b |});
            [apply EQ_sym; assumption | apply IN_Paire_right]
        | intros Hsub; apply (not_subsingleton_PaireVS b);
            apply is_subsingleton_sound with x; assumption ]. }
  assert (HComp2 :
    Comp (Paire ∅ {| b |}) is_nonempty ≃ {| {| b |} |}).
  { apply INC_EQ; unfold INC; intros x Hx.
    - assert (Hx_in : x ∈ Paire ∅ {| b |}) by (exact (Comp_INC _ _ x Hx)).
      assert (Hx_ne : is_nonempty x)
        by (apply IN_Comp_P with (E := Paire ∅ {| b |}); assumption).
      destruct (Paire_IN _ _ _ Hx_in) as [Heq | Heq].
      + exfalso; apply not_nonempty_Vide.
        apply is_nonempty_sound with x; assumption.
      + apply IN_sound_left with {| b |};
          [apply EQ_sym; assumption | apply IN_Sing].
    - assert (Heq : x ≃ {| b |}) by (apply IN_Sing_EQ; exact Hx).
      apply IN_P_Comp;
        [ assumption
        | apply IN_sound_left with {| b |};
            [apply EQ_sym; assumption | apply IN_Paire_right]
        | apply is_nonempty_sound with {| b |};
            [apply EQ_sym; assumption | apply is_nonempty_Sing] ]. }
  apply EQ_tran with (⋃ ⋃ {| {| b |} |}).
  - apply Union_sound; apply Union_sound.
    apply EQ_tran with (Comp (Paire ∅ {| b |}) is_nonempty); [|exact HComp2].
    apply Comp_sound_left; [exact Hne_sound|].
    apply EQ_tran with (⋃ {| Paire ∅ {| b |} |});
      [apply Union_sound; exact HComp1 | apply Union_Sing].
  - apply EQ_tran with (⋃ {| b |}); [apply Union_sound|]; apply Union_Sing.
Qed.

Theorem proj1_triple : forall a b c : Ens, proj1 ⟨ a , b , c ⟩ ≃ a.
Proof. intros; unfold proj1; apply pfst_Couple. Qed.

Theorem proj2_triple : forall a b c : Ens, proj2 ⟨ a , b , c ⟩ ≃ b.
Proof.
  intros; unfold proj2.
  apply EQ_tran with (pfst (Couple b c)).
  - apply pfst_sound; apply psnd_Couple.
  - apply pfst_Couple.
Qed.

Theorem proj3_triple : forall a b c : Ens, proj3 ⟨ a , b , c ⟩ ≃ c.
Proof.
  intros; unfold proj3.
  apply EQ_tran with (psnd (Couple b c)).
  - apply psnd_sound; apply psnd_Couple.
  - apply psnd_Couple.
Qed.



(* Every element of natId is EQ to a diagonal pair of encoded naturals. *)
Lemma IN_natId_EXType :
  forall p : Ens, p ∈ natId ->
  EXType _ (fun n : nat => p ≃ ⟨ Nat n , Nat n ⟩).
Proof.
  intros p Hp; unfold natId in Hp.
  destruct (iUnion_IN _ _ _ Hp) as [n [Hn Hpn]].
  destruct (IN_EXType _ _ Hn) as [k Hk]; simpl in Hk.
  exists k.
  apply EQ_tran with (⟨ n , n ⟩).
  - apply IN_Sing_EQ; exact Hpn.
  - apply EQ_tran with (⟨ Nat k , n ⟩).
    + apply Couple_sound_left; exact Hk.
    + apply Couple_sound_right; exact Hk.
Qed.

(* A Couple in natId is a diagonal pair. *)
Lemma natId_pair_diagonal :
  forall a b : Ens, ⟨ a , b ⟩ ∈ natId -> a ≃ b.
Proof.
  intros a b H.
  destruct (IN_natId_EXType _ H) as [k Hk].
  apply EQ_tran with (Nat k).
  - exact (Couple_inj_left _ _ _ _ Hk).
  - apply EQ_sym; exact (Couple_inj_right _ _ _ _ Hk).
Qed.

(* For elements of natId, the two projections coincide. *)
Lemma natId_diagonal :
  forall p : Ens, p ∈ natId -> pfst p ≃ psnd p.
Proof.
  intros p Hp.
  destruct (IN_natId_EXType p Hp) as [n Heq].
  apply EQ_tran with (Nat n).
  - apply EQ_tran with (pfst ⟨ Nat n , Nat n ⟩).
    + apply pfst_sound; exact Heq.
    + apply pfst_Couple.
  - apply EQ_sym; apply EQ_tran with (psnd ⟨ Nat n , Nat n ⟩).
    + apply psnd_sound; exact Heq.
    + apply psnd_Couple.
Qed.


(** rng / image: introduction and elimination lemmas.

   The membership predicates inside [rng] / [image] involve an
   existential carrying an ordered pair; their soundness conditions
   under [≃] are routine but verbose, so we package the rng / image
   constructors and destructors as helpers.

   Because Werner pairs ⟨a,b⟩ = {{a}, {∅,{b}}} put the [b] component at
   union-depth 3 (b ∈ {b} ∈ {∅,{b}} ∈ ⟨a,b⟩), the bound ambient set in
   [rng] is [⋃⋃⋃ r], and the introduction rules can derive that bound
   from [⟨a,b⟩ ∈ r] directly. ***)

Lemma b_IN_3unions_of_pair :
  forall (r a b : Ens), ⟨ a , b ⟩ ∈ r -> b ∈ ⋃ ⋃ ⋃ r.
Proof.
  intros r a b Hab.
  apply IN_Union with {| b |}; [|apply IN_Sing].
  apply IN_Union with (Paire ∅ {| b |}); [|apply IN_Paire_right].
  apply IN_Union with (⟨ a , b ⟩); [exact Hab|].
  unfold Couple; apply IN_Paire_right.
Qed.

(** Soundness / [Proper] instances for [dom], [rng], [image],
    [applyFun]. The predicate inside [Comp] depends on [r] (or [r]
    and [S]), so [Comp_sound_left] is not directly enough — we
    admit [dom_sound] / [rng_sound] / [image_sound] for now. *)

Lemma dom_sound :
  forall r r' : Ens, r ≃ r' -> dom r ≃ dom r'.
(* PROOF PLAN (medium, ≈15 lines):
   [dom r = Comp (⋃⋃ r) (fun a => ∃b, ⟨a,b⟩ ∈ r)]. The predicate
   depends on [r], so [Comp_sound_left] is not enough — prove both
   inclusions explicitly.
   [apply INC_EQ]; for each direction, given [a ∈ dom r]:
   - [Comp_INC] ⇒ [a ∈ ⋃⋃ r] ⇒ [a ∈ ⋃⋃ r'] via [Union_sound] × 2
     and [IN_sound_right].
   - [IN_Comp_P] ⇒ [∃b, ⟨a,b⟩ ∈ r] ⇒ [∃b, ⟨a,b⟩ ∈ r'] via
     [IN_sound_right] with [r ≃ r'].
   - [IN_P_Comp] closes the goal. *)
Proof.
Admitted.

Lemma rng_sound :
  forall r r' : Ens, r ≃ r' -> rng r ≃ rng r'.
(* PROOF PLAN (medium, ≈15 lines):
   Symmetric to [dom_sound]; bound set is [⋃⋃⋃ r] instead of [⋃⋃ r].
   Same INC_EQ structure with [Union_sound] × 3 and [IN_sound_right]. *)
Proof.
Admitted.

#[export] Instance dom_Proper : Proper (EQ ==> EQ) dom.
Proof. intros r r' H; apply dom_sound; exact H. Qed.

#[export] Instance rng_Proper : Proper (EQ ==> EQ) rng.
Proof. intros r r' H; apply rng_sound; exact H. Qed.

Lemma image_sound :
  forall r r' S S' : Ens,
  r ≃ r' -> S ≃ S' -> image r S ≃ image r' S'.
(* PROOF PLAN (medium, ≈20 lines):
   [image r S = Comp (rng r) (fun b => ∃a, a ∈ S ∧ ⟨a,b⟩ ∈ r)].
   - Bound-set side: use [rng_sound r r' Hr] to relate [rng r ≃ rng r'].
   - Predicate side: under [r ≃ r'] and [S ≃ S'], the existential
     respects EQ via [IN_sound_right] applied to both [a ∈ S] and
     [⟨a,b⟩ ∈ r].
   - Combine with INC_EQ + IN_Comp_P / IN_P_Comp as in [dom_sound]. *)
Proof.
Admitted.

#[export] Instance image_Proper : Proper (EQ ==> EQ ==> EQ) image.
Proof. intros r r' Hr S S' HS; apply image_sound; assumption. Qed.

Lemma applyFun_sound :
  forall f f' v v' : Ens,
  f ≃ f' -> v ≃ v' -> applyFun f v ≃ applyFun f' v'.
Proof.
  intros f f' v v' Hf Hv. unfold applyFun.
  apply Union_sound, image_sound; [exact Hf | apply Sing_Proper; exact Hv].
Qed.

#[export] Instance applyFun_Proper :
  Proper (EQ ==> EQ ==> EQ) applyFun.
Proof. intros f f' Hf v v' Hv; apply applyFun_sound; assumption. Qed.

Lemma rng_intro :
  forall (r a b : Ens), ⟨ a , b ⟩ ∈ r -> b ∈ rng r.
Proof.
  intros r a b Hab.
  unfold rng.
  apply IN_P_Comp;
    [|apply b_IN_3unions_of_pair with a; exact Hab|exists a; exact Hab].
  intros w1 w2 [a' Ha'b'] Hw12.
  exists a'.
  apply IN_sound_left with (⟨ a' , w1 ⟩); [|exact Ha'b'].
  apply Couple_sound_right; exact Hw12.
Qed.

Lemma rng_elim :
  forall (r b : Ens),
  b ∈ rng r -> exists a : Ens, ⟨ a , b ⟩ ∈ r.
Proof.
  intros r b H.
  apply IN_Comp_P
    with (E := ⋃ ⋃ ⋃ r)
         (P := fun b' => exists a : Ens, ⟨ a , b' ⟩ ∈ r);
    [|exact H].
  intros w1 w2 [a Hab] Hw12.
  exists a; apply IN_sound_left with (⟨ a , w1 ⟩); [|exact Hab].
  apply Couple_sound_right; exact Hw12.
Qed.

Lemma image_intro :
  forall (r S b a : Ens),
  a ∈ S ->
  ⟨ a , b ⟩ ∈ r ->
  b ∈ image r S.
Proof.
  intros r S b a Ha Hab.
  unfold image.
  apply IN_P_Comp;
    [|apply rng_intro with a; exact Hab|exists a; split; assumption].
  intros w1 w2 [a' [Ha'S Ha'b']] Hw12.
  exists a'; split; [exact Ha'S|].
  apply IN_sound_left with (⟨ a' , w1 ⟩); [|exact Ha'b'].
  apply Couple_sound_right; exact Hw12.
Qed.

Lemma image_elim :
  forall (r S b : Ens),
  b ∈ image r S ->
  exists a : Ens, a ∈ S /\ ⟨ a , b ⟩ ∈ r.
Proof.
  intros r S b H.
  apply IN_Comp_P
    with (E := rng r)
         (P := fun b' => exists a : Ens, a ∈ S /\ ⟨ a , b' ⟩ ∈ r);
    [|exact H].
  intros w1 w2 [a [HaS Hab]] Hw12.
  exists a; split; [exact HaS|].
  apply IN_sound_left with (⟨ a , w1 ⟩); [|exact Hab].
  apply Couple_sound_right; exact Hw12.
Qed.

(** iUnion / union reductions over literal singletons and ∅ ***)

(** Simplification lemmas for BinUnion ***)

Theorem IN_BinUnion_l : forall x E E' : Ens, x ∈ E -> x ∈ E ∪ E'.
Proof.
  intros x E E' Hx; unfold BinUnion.
  apply IN_INC_Union with E. apply IN_Paire_left. assumption.
Qed.

Theorem IN_BinUnion_r : forall x E E' : Ens, x ∈ E' -> x ∈ E ∪ E'.
Proof.
  intros x E E' Hx; unfold BinUnion.
  apply IN_INC_Union with E'. apply IN_Paire_right. assumption.
Qed.

Theorem BinUnion_IN :
  forall x E E' : Ens, x ∈ E ∪ E' -> x ∈ E \/ x ∈ E'.
Proof.
  intros x E E' H; unfold BinUnion in H.
  destruct (Union_IN _ _ H) as [y [Hy Hxy]].
  destruct (Paire_IN _ _ _ Hy) as [Heq | Heq].
  - left;  apply IN_sound_right with y; assumption.
  - right; apply IN_sound_right with y; assumption.
Qed.


Theorem BinUnion_Vide_l : forall E : Ens, ∅ ∪ E ≃ E.
Proof.
  intro E; apply INC_EQ; unfold INC; intros x Hx.
  - destruct (BinUnion_IN _ _ _ Hx) as [Hv | He].
    + elim (Vide_est_vide _ Hv).
    + exact He.
  - apply IN_BinUnion_r; exact Hx.
Qed.

Theorem BinUnion_Vide_r : forall E : Ens, E ∪ ∅ ≃ E.
Proof.
  intro E; apply INC_EQ; unfold INC; intros x Hx.
  - destruct (BinUnion_IN _ _ _ Hx) as [He | Hv].
    + exact He.
    + elim (Vide_est_vide _ Hv).
  - apply IN_BinUnion_l; exact Hx.
Qed.


Theorem iUnion_Vide : forall F : Ens -> Ens, x ← ∅ ;; F x  ≃ ∅.
Proof.
  intro F; apply INC_EQ; unfold INC; intros x Hx.
  - apply iUnion_IN in Hx.
    destruct Hx as [y [Hy _]]; elim (Vide_est_vide _ Hy).
  - elim (Vide_est_vide _ Hx).
Qed.

Theorem empty_union_empty : ∅ ∪ ∅ ≃ ∅.
Proof. apply BinUnion_Vide_l. Qed.

Lemma iUnion_graph_mem_pi {A : Ens} {B : Ens -> Ens}
    (g : Ens -> Ens) (hg_sound : forall a a', a ≃ a' -> g a ≃ g a')
    (hg : forall a, a ∈ A -> g a ∈ B a) :
    (iUnion A (fun v => {| ⟨v, g v⟩ |})) ∈ Pi A B.
(* PROOF PLAN (ZF-style [Pi] = total functional graphs separated from
   [Power (Prod A (⋃B))]):
   - Subset bound: every pair [⟨a, g a⟩] with [a ∈ A] satisfies
     [g a ∈ ⋃B] (via [hg] and [IN_iUnion]), so the graph is a subset
     of [Prod A (⋃B)] and hence an element of its power-set.
   - Totality: for [a ∈ A], witness [b := g a]; [hg] gives [g a ∈ B a]
     and [IN_iUnion] gives [⟨a, g a⟩ ∈ iUnion ...].
   - Functionality: from [⟨a, b1⟩, ⟨a, b2⟩ ∈ iUnion ...], by
     [iUnion_IN] both come from singletons [{|⟨y_k, g y_k⟩|}] with
     [y_k ∈ A] and [⟨a, b_k⟩ ≃ ⟨y_k, g y_k⟩]; [Couple_inj] gives
     [a ≃ y_k] and [b_k ≃ g y_k], so [y_1 ≃ y_2] and (by [hg_sound])
     [g y_1 ≃ g y_2], hence [b1 ≃ b2]. *)
Admitted.

Lemma applyFun_mem_of_pi {A : Ens} {B : Ens -> Ens} {f v : Ens}
    (hf : f ∈ Pi A B) (hv : v ∈ A) :
    applyFun f v ∈ B v.
Proof.
  unfold Pi in hf.
  (* The Pi predicate respects ≃ on f via IN_sound_right. *)
  assert (Hsound :
    forall w1 w2 : Ens,
      ((forall a : Ens, a ∈ A -> exists b : Ens, b ∈ B a /\ ⟨ a , b ⟩ ∈ w1)
       /\ (forall a b1 b2 : Ens,
              ⟨ a , b1 ⟩ ∈ w1 -> ⟨ a , b2 ⟩ ∈ w1 -> b1 ≃ b2)) ->
      w1 ≃ w2 ->
      ((forall a : Ens, a ∈ A -> exists b : Ens, b ∈ B a /\ ⟨ a , b ⟩ ∈ w2)
       /\ (forall a b1 b2 : Ens,
              ⟨ a , b1 ⟩ ∈ w2 -> ⟨ a , b2 ⟩ ∈ w2 -> b1 ≃ b2))).
  { intros w1 w2 [Htot Hfunc] Hw; split.
    - intros a Ha. destruct (Htot a Ha) as [b [Hb Hab]].
      exists b; split; [exact Hb | apply IN_sound_right with w1; assumption].
    - intros a b1 b2 H1 H2.
      apply (Hfunc a b1 b2);
        apply IN_sound_right with w2; auto with zfc. }
  destruct (IN_Comp_P _ _ _ Hsound hf) as [Htot Hfunc].
  destruct (Htot v hv) as [b [HbInBv HCouple]].

  (* image f (Sing v) collapses to {| b |}. *)
  assert (HimgSing : image f (Sing v) ≃ {| b |}).
  { apply INC_EQ; unfold INC; intros x Hx.
    - destruct (image_elim _ _ _ Hx) as [a [Ha Hab]].
      assert (Hav : a ≃ v) by (apply IN_Sing_EQ; exact Ha).
      assert (Hvx : ⟨ v , x ⟩ ∈ f).
      { apply IN_sound_left with ⟨ a , x ⟩;
          [apply Couple_sound_left; exact Hav | exact Hab]. }
      assert (Hxb : x ≃ b) by exact (Hfunc v x b Hvx HCouple).
      apply IN_sound_left with b;
        [apply EQ_sym; exact Hxb | apply IN_Sing].
    - apply IN_Sing_EQ in Hx.
      apply image_intro with (a := v); [apply IN_Sing|].
      apply IN_sound_left with ⟨ v , b ⟩;
        [apply Couple_sound_right; apply EQ_sym; exact Hx | exact HCouple]. }

  (* applyFun f v ≃ b, then conclude. *)
  assert (HappEQ : applyFun f v ≃ b).
  { unfold applyFun.
    apply EQ_tran with (⋃ {| b |});
      [apply Union_sound; exact HimgSing | apply Union_Sing]. }
  apply IN_sound_left with b; [apply EQ_sym; exact HappEQ | exact HbInBv].
Qed.
