(* lib/Diagonal.v

   General-purpose lemmas about *diagonal* relations over [ZFSet]: sets of
   pairs all of the form ⟨v,v⟩.  These are pure [ZFSet] facts (no object
   language), factored out of [Timbda.TimSTLC]; the semantics-specific
   results connecting diagonality to a type interpretation live there. *)

From Stdlib Require Import ssreflect.
Require Import ZFSet.
Require Import ZFNotation.

(** A relation [R] is *diagonal* when every element is its own diagonal
    pair ⟨psnd p, psnd p⟩. *)
Definition IsDiag (R : ZFSet) : Prop :=
  forall p, p ∈ R -> p = ⟨ psnd p , psnd p ⟩.

(** From a diagonal pair [p = ⟨psnd p, psnd p⟩], the two projections
    coincide. *)
Lemma diag_eq p : p = ⟨ psnd p , psnd p ⟩ -> pfst p = psnd p.
Proof.
  intro H. transitivity (pfst ⟨ psnd p , psnd p ⟩).
  - f_equal. exact H.
  - apply pfst_Couple.
Qed.

(** On a graph [h ∈ Pi A B] whose domain [A] and every fibre [B a] are
    diagonal, the two relations obtained by reading off ⟨psnd,pfst⟩ vs.
    ⟨pfst,psnd⟩ of each edge coincide. *)
Lemma sndRel_eq_thdRel (A : ZFSet) (B : ZFSet -> ZFSet) (h : ZFSet) :
  IsDiag A ->
  (forall a, a ∈ A -> IsDiag (B a)) ->
  h ∈ Pi A B ->
  iUnion_pat h (fun ab cd => {| ⟨ psnd ab , pfst cd ⟩ |})
  = iUnion_pat h (fun ab cd => {| ⟨ pfst ab , psnd cd ⟩ |}).
Proof.
  intros HA HB Hh.
  apply In_Pi_inv in Hh. destruct Hh as [Hsub _].
  unfold iUnion_pat. apply iUnion_ext_mem. intros q qIn.
  pose proof (Inc_IN _ _ _ Hsub qIn) as Hq.
  apply IN_Prod_EX in Hq. destruct Hq as [qa [qc [Hqa [Hqc Heq]]]].
  subst q. rewrite pfst_Couple psnd_Couple.
  (* qa is diagonal: pfst qa = psnd qa *)
  pose proof (diag_eq qa (HA qa Hqa)) as Efa.
  (* qc lives in some fibre B a', which is diagonal *)
  apply iUnion_IN in Hqc. destruct Hqc as [a' [Ha' Hqc]].
  pose proof (diag_eq qc (HB a' Ha' qc Hqc)) as Efc.
  rewrite Efa Efc. reflexivity.
Qed.

(** On a diagonal relation, the [psnd] projection is injective (the pair
    is determined by it). *)
Lemma diag_psnd_inj (A : ZFSet) :
  IsDiag A ->
  forall ab ab', ab ∈ A -> ab' ∈ A -> psnd ab = psnd ab' -> ab = ab'.
Proof.
  intros HA ab ab' Hab Hab' He.
  rewrite (HA ab Hab) (HA ab' Hab') He. reflexivity.
Qed.

(** ... and so is [pfst], since on a diagonal pair both projections
    coincide. *)
Lemma diag_pfst_inj (A : ZFSet) :
  IsDiag A ->
  forall ab ab', ab ∈ A -> ab' ∈ A -> pfst ab = pfst ab' -> ab = ab'.
Proof.
  intros HA ab ab' Hab Hab' He.
  apply (diag_psnd_inj A HA); auto.
  rewrite -(diag_eq ab (HA ab Hab)) -(diag_eq ab' (HA ab' Hab')).
  exact He.
Qed.

(* ================================================================== *)
(** * The diagonal constructor and the small-partial-identity universe.

    [diag S] is the identity relation on [S], i.e. the diagonal
    [{⟨a,a⟩ : a ∈ S}].  [smallId] is the *small* partial identities — the
    subsets of the diagonal on [Big] that are themselves small.  This is
    the predicative type universe shared by the [PolyF] interpreters: a
    type variable ranges over [smallId], and a type's inhabitants are the
    [rng] of the chosen partial identity. *)
(* ================================================================== *)

Definition diag (S : ZFSet) : ZFSet := a ← S ;; {| ⟨ a , a ⟩ |}.

(** The full diagonal on the universe [Big] — the identity relation on
    every small set. *)
Definition anyId : ZFSet := diag Big.

(** The range of a diagonal is its support set. *)
Lemma rng_diag S : rng (diag S) = S.
Proof.
  apply set_ext; apply Inc_def; intros b Hb.
  - apply rng_elim in Hb. destruct Hb as [a Hab].
    unfold diag in Hab. apply iUnion_IN in Hab. destruct Hab as [c [Hc Hab]].
    apply IN_Sing_EQ in Hab.
    pose proof (Couple_inj_right _ _ _ _ Hab) as Eb. rewrite Eb. exact Hc.
  - unfold diag. apply (rng_intro _ b b).
    apply IN_iUnion with (y := b); [ exact Hb | apply IN_Sing ].
Qed.

(** The diagonal of a subset of [Big] is a subset of the diagonal on [Big]. *)
Lemma diag_Inc S : S ⊆ Big -> diag S ⊆ diag Big.
Proof.
  intros HS. apply Inc_def. intros x Hx.
  unfold diag in Hx. apply iUnion_IN in Hx. destruct Hx as [a [Ha Hx]].
  apply IN_Sing_EQ in Hx. rewrite Hx.
  unfold diag. apply IN_iUnion with (y := a); [ exact (Inc_IN _ _ _ HS Ha) | apply IN_Sing ].
Qed.

(** The diagonal of a small set is small. *)
Lemma diag_small S : S ∈ Big -> diag S ∈ Big.
Proof.
  intros HS. apply small_of_Inc with (V := Prod S S).
  - apply Inc_def. intros x Hx. unfold diag in Hx.
    apply iUnion_IN in Hx. destruct Hx as [a [Ha Hx]]. apply IN_Sing_EQ in Hx.
    rewrite Hx. apply Couple_IN_Prod; exact Ha.
  - apply Prod_small; exact HS.
Qed.

(** A member of [Big] is a subset of [Big] (transitivity). *)
Lemma Big_subset S : S ∈ Big -> S ⊆ Big.
Proof. intros HS. apply Inc_def. intros x Hx. exact (Big_transitive _ _ Hx HS). Qed.

(** The range of a small set is small. *)
Lemma rng_small X : X ∈ Big -> rng X ∈ Big.
Proof.
  intros HX. apply small_of_Inc with (V := Union (Union (Union X))).
  - apply Inc_def. intros b Hb.
    apply rng_elim in Hb. destruct Hb as [a Hab].
    rewrite Couple_unfold in Hab.
    apply (IN_Union (Union (Union X)) (Sing b) b).
    + apply (IN_Union (Union X) (Paire Empty (Sing b)) (Sing b)).
      * apply (IN_Union X (Paire (Sing a) (Paire Empty (Sing b))) (Paire Empty (Sing b))).
        -- exact Hab.
        -- apply IN_Paire_right.
      * apply IN_Paire_right.
    + apply IN_Sing.
  - apply Union_small, Union_small, Union_small; exact HX.
Qed.

(** [smallId]: the small partial identities — small subsets of the
    diagonal on [Big].  The predicative type universe. *)
Definition smallId : ZFSet :=
  ⦃ r ∈ Big | r ⊆ diag Big ⦄.

(** A small partial identity is small. *)
Lemma smallId_small r : r ∈ smallId -> r ∈ Big.
Proof. intro H. unfold smallId in H. exact (Inc_IN _ _ _ (Comp_Inc _ _) H). Qed.

(** The diagonal of any small set is a small partial identity. *)
Lemma diag_in_smallId S : S ∈ Big -> diag S ∈ smallId.
Proof.
  intro HS. unfold smallId. apply In_P_Comp.
  - apply diag_small; exact HS.
  - apply diag_Inc. apply Big_subset; exact HS.
Qed.

(* ================================================================== *)
(** * Partial functions and the partial-identity type universe.

    These are the set-theoretic ingredients of the [Timbda] type machinery.
    [pFun A B] is the partial functions from [A] to [B]; [pIdFun A] the
    partial *identity* functions on [A]; and [is_type] the identity
    relation on [pIdFun Big] — the type universe of the triple semantics
    (see [Timbda.Timbda2]). *)
(* ================================================================== *)

(** The partial functions from [A] to [B]: the single-valued
    ([isFunction]) subsets of the product [A × B]. *)
Definition pFun (A B : ZFSet) : ZFSet :=
  ⦃ G ∈ Power (Prod A B) | isFunction G ⦄.

(** The partial *identity* functions on [A]: the subsets of the diagonal
    [diag A].  (A subset of the diagonal is automatically single-valued,
    so no separate [isFunction] side-condition is needed.) *)
Definition pIdFun (A : ZFSet) : ZFSet := Power (diag A).

(** The identity relation on the partial identities [pIdFun Big] — the
    diagonal {⟨f,f⟩ : f ∈ pIdFun Big}. *)
Definition is_type : ZFSet := diag (pIdFun Big).
