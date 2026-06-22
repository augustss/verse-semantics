(** * Projections.v — the [FST] / [SND] operations on ZFSets.

    Following the Werner-pair / relation reading:

      FST(x,y) = x                 SND(x,y) = y
      FST(f)   = { ⟨FST x, FST y⟩ | ⟨x,y⟩ ∈ f }
      SND(f)   = { ⟨SND x, SND y⟩ | ⟨x,y⟩ ∈ f }

    [FST]/[SND] are defined by recursion on set *rank* (∈-recursion), not by
    structural recursion: the recursive calls land on [pfst e] / [psnd e] of
    the elements [e ∈ f], which are smaller in rank but are not structural
    subterms of [f].  We therefore use well-founded recursion over the
    relation [projBelow] ("is a projection of an element of"), and — since
    this development is already classical (it uses [prop_ext] and Hilbert's
    [epsilon] via [repr]) — we decide the [Couple] test with
    [excluded_middle_informative].  Working directly on [ZFSet] (whose
    equality is Leibniz [=]) keeps these honest functions: there is no
    [Proper]/[EQ]-soundness obligation, unlike a lift through [quot_map].

    [FST] and [SND] differ only in the base case, so both are instances of a
    single recursor [projRec] parameterised by the base operation. *)

From Stdlib Require Import FunctionalExtensionality.
From Stdlib Require Import ClassicalEpsilon.

Require Import Sets.
Require Import Axioms.
Require Import Cartesian.

Require Import ZFSet.
Require Import ZFNotation.
Require Import Diagonal.

(** ** Membership helpers for the [mk]/[repr]/[sup] interface. *)

(* Every [pi2]-component of an [Ens] is a member of it. *)
Lemma IN_pi2 : forall (E : Ens) (a : pi1 E), IN (pi2 E a) E.
Proof. intros [A g] a. apply (EXTypei _ _ a). apply EQ_refl. Qed.

(* The [a]-th representative element of [X] really is a member of [X]. *)
Lemma In_mk_pi2 : forall (X : ZFSet) (a : pi1 (repr X)),
  In (mk (pi2 (repr X) a)) X.
Proof.
  intros X a.
  assert (H : In (mk (pi2 (repr X) a)) (mk (repr X))).
  { rewrite In_mk. apply IN_pi2. }
  rewrite mk_repr in H. exact H.
Qed.

(* Membership in a [mk (sup A F)] is exactly "is one of the [mk (F a)]". *)
Lemma In_mk_sup_iff : forall (A : Type) (F : A -> Ens) (w : ZFSet),
  In w (mk (sup A F)) <-> exists a, w = mk (F a).
Proof.
  intros A F w. split.
  - intro H. rewrite <- (mk_repr w) in H. rewrite In_mk in H.
    destruct (IN_EXType (sup A F) (repr w) H) as [a Ha].
    exists a. rewrite <- (mk_repr w). apply mk_eq. exact Ha.
  - intros [a Ha]. subst w. rewrite In_mk.
    apply (EXTypei _ _ a). apply EQ_refl.
Qed.

(* Conversely, a member of [f] is [mk] of some representative component. *)
Lemma In_inv_pi2 : forall (e f : ZFSet),
  In e f -> exists a, e = mk (pi2 (repr f) a).
Proof.
  intros e f H.
  rewrite <- (mk_repr f) in H. rewrite <- (mk_repr e) in H. rewrite In_mk in H.
  destruct (IN_EXType (repr f) (repr e) H) as [a Ha].
  exists a. rewrite <- (mk_repr e). apply mk_eq. exact Ha.
Qed.

(** ** The recursion: rank order and the generic step. *)

(* [f] is a Couple iff it is the Werner pair of its two projections. *)
Definition isCouple (f : ZFSet) : Prop := exists x y, f = ⟨ x , y ⟩.

(* The well-founded order for the recursion: [x] is [pfst] or [psnd] of some
   element of [y].  Well-founded because [pfst e] and [psnd e] have strictly
   smaller rank than [e ∈ y]. *)
Definition projBelow (x y : ZFSet) : Prop :=
  exists e, In e y /\ (x = pfst e \/ x = psnd e).

(* TODO(proof): well-foundedness holds by ∈-foundation / rank
   ([rank (pfst e) , rank (psnd e) < rank e < rank y]); discharging it needs a
   rank/[Acc] development on [Ens] that is not yet in the library.  It is the
   sole nontrivial obligation behind [FST] / [SND]. *)
Lemma wf_projBelow : well_founded projBelow.
Admitted.

(* One step of the recursion, parameterised by the base operation [base]
   applied to Couples.  Off Couples, rebuild the set mapping every element
   ⟨x,y⟩ to ⟨rec x, rec y⟩.  The non-Couple branch is built on a representative
   [repr f] so that the [a : pi1 (repr f)] index supplies the membership proofs
   [projBelow] requires ([iUnion]/[Comp] would hide them). *)
Definition proj_step (base : ZFSet -> ZFSet) (f : ZFSet)
    (rec : forall y : ZFSet, projBelow y f -> ZFSet) : ZFSet.
Proof.
  destruct (excluded_middle_informative (isCouple f)) as [Hc | Hnc].
  - exact (base f).
  - refine (mk (sup (pi1 (repr f))
              (fun a => repr (⟨ rec (pfst (mk (pi2 (repr f) a))) _
                              , rec (psnd (mk (pi2 (repr f) a))) _ ⟩)))).
    + exists (mk (pi2 (repr f) a)). split; [ apply In_mk_pi2 | left; reflexivity ].
    + exists (mk (pi2 (repr f) a)). split; [ apply In_mk_pi2 | right; reflexivity ].
Defined.

Definition projRec (base : ZFSet -> ZFSet) : ZFSet -> ZFSet :=
  Fix wf_projBelow (fun _ => ZFSet) (proj_step base).

(** ** Unfolding lemmas for the generic recursor. *)

(* [proj_step] uses its recursive argument only by applying it, so it does not
   depend on the [projBelow] proofs — the hypothesis [Fix_eq] needs. *)
Lemma proj_step_ext :
  forall (base : ZFSet -> ZFSet) (x : ZFSet)
         (g h : forall y, projBelow y x -> ZFSet),
    (forall (y : ZFSet) (p : projBelow y x), g y p = h y p) ->
    proj_step base x g = proj_step base x h.
Proof.
  intros base x g h Hgh. unfold proj_step.
  destruct (excluded_middle_informative (isCouple x)) as [Hc | Hnc].
  - reflexivity.
  - do 2 f_equal. apply functional_extensionality. intro a.
    rewrite !Hgh. reflexivity.
Qed.

Lemma projRec_unfold :
  forall (base : ZFSet -> ZFSet) (f : ZFSet),
    projRec base f = proj_step base f (fun y _ => projRec base y).
Proof.
  intro base.
  apply (Fix_eq wf_projBelow (fun _ => ZFSet) (proj_step base)).
  intros x g h H. apply proj_step_ext. exact H.
Qed.

(* The image-of-singletons construction on a representative is just the
   [iUnion]/replacement of those singletons; key to the relational unfolding. *)
Lemma image_repr_eq_iUnion : forall (G : ZFSet -> ZFSet) (f : ZFSet),
  mk (sup (pi1 (repr f)) (fun a => repr (G (mk (pi2 (repr f) a)))))
  = (e ← f ;; {| G e |}).
Proof.
  intros G f. apply set_ext.
  - apply Inc_def. intros w Hw.
    apply In_mk_sup_iff in Hw. destruct Hw as [a Ha]. rewrite mk_repr in Ha.
    apply IN_iUnion with (y := mk (pi2 (repr f) a)).
    + apply In_mk_pi2.
    + rewrite Ha. apply IN_Sing.
  - apply Inc_def. intros w Hw.
    apply iUnion_IN in Hw. destruct Hw as [e [Hef Hw]].
    apply IN_Sing_EQ in Hw.
    destruct (In_inv_pi2 _ _ Hef) as [a Hae].
    apply In_mk_sup_iff. exists a. rewrite mk_repr, Hw, Hae. reflexivity.
Qed.

(* Base case: on a Couple, [projRec] is its base operation. *)
Lemma projRec_Couple :
  forall (base : ZFSet -> ZFSet) (x y : ZFSet),
    projRec base (⟨ x , y ⟩) = base (⟨ x , y ⟩).
Proof.
  intros base x y. rewrite projRec_unfold. unfold proj_step.
  destruct (excluded_middle_informative (isCouple (⟨ x , y ⟩))) as [Hc | Hnc].
  - reflexivity.
  - exfalso. apply Hnc. exists x, y. reflexivity.
Qed.

(* Recursive case: off Couples, [projRec] maps over the element-pairs. *)
Lemma projRec_rel :
  forall (base : ZFSet -> ZFSet) (f : ZFSet),
    ~ isCouple f ->
    projRec base f
      = (e ← f ;; {| ⟨ projRec base (pfst e) , projRec base (psnd e) ⟩ |}).
Proof.
  intros base f Hf. rewrite projRec_unfold. unfold proj_step.
  destruct (excluded_middle_informative (isCouple f)) as [Hc | Hnc].
  - contradiction.
  - apply (image_repr_eq_iUnion
             (fun e => ⟨ projRec base (pfst e) , projRec base (psnd e) ⟩)).
Qed.

(** ** [FST] and [SND]. *)

Definition FST : ZFSet -> ZFSet := projRec pfst.
Definition SND : ZFSet -> ZFSet := projRec psnd.

(* Base cases. *)
Lemma FST_Couple : forall x y : ZFSet, FST (⟨ x , y ⟩) = x.
Proof. intros x y. unfold FST. rewrite projRec_Couple. apply pfst_Couple. Qed.

Lemma SND_Couple : forall x y : ZFSet, SND (⟨ x , y ⟩) = y.
Proof. intros x y. unfold SND. rewrite projRec_Couple. apply psnd_Couple. Qed.

(* Relational unfolding (the recursive clause). *)
Lemma FST_rel : forall f : ZFSet,
  ~ isCouple f ->
  FST f = (e ← f ;; {| ⟨ FST (pfst e) , FST (psnd e) ⟩ |}).
Proof. intros f Hf. unfold FST. apply projRec_rel. exact Hf. Qed.

Lemma SND_rel : forall f : ZFSet,
  ~ isCouple f ->
  SND f = (e ← f ;; {| ⟨ SND (pfst e) , SND (psnd e) ⟩ |}).
Proof. intros f Hf. unfold SND. apply projRec_rel. exact Hf. Qed.

(* ================================================================== *)
(** ** [FST] / [SND] of a functional graph. *)
(* ================================================================== *)

(** A functional graph [{⟨a, g a⟩ : a ∈ S}] is never a couple: a Wiener pair
    [⟨a, g a⟩] has two members, so it cannot equal the singleton [{u}] that any
    couple [⟨u,v⟩] must contain. *)
Lemma graph_not_couple : forall S g,
  ~ isCouple (iUnion S (fun a => {| ⟨ a, g a ⟩ |})).
Proof.
  intros S g [u [v Huv]].
  assert (HSu : In (Sing u) (iUnion S (fun a => {| ⟨ a, g a ⟩ |})))
    by (rewrite Huv, Couple_unfold; apply IN_Paire_left).
  apply iUnion_IN in HSu. destruct HSu as [a [Ha HSu]].
  apply IN_Sing_EQ in HSu. rewrite Couple_unfold in HSu.
  assert (E1 : In (Sing a) (Sing u)) by (rewrite HSu; apply IN_Paire_left).
  assert (E2 : In (Paire Empty (Sing (g a))) (Sing u)) by (rewrite HSu; apply IN_Paire_right).
  apply IN_Sing_EQ in E1. apply IN_Sing_EQ in E2.
  assert (Eeq : Sing a = Paire Empty (Sing (g a))) by (transitivity u; [ exact E1 | exact (eq_sym E2) ]).
  assert (Ha0 : In Empty (Sing a)) by (rewrite Eeq; apply IN_Paire_left).
  apply IN_Sing_EQ in Ha0.
  assert (Hg : In (Sing (g a)) (Sing a)) by (rewrite Eeq; apply IN_Paire_right).
  apply IN_Sing_EQ in Hg.
  assert (Hge : Sing (g a) = Empty) by (rewrite Hg; symmetry; exact Ha0).
  assert (Hcon : In (g a) (Sing (g a))) by apply IN_Sing.
  rewrite Hge in Hcon. exact (not_In_Empty _ Hcon).
Qed.

(** Its first projection [{⟨FST a, FST (g a)⟩ : a ∈ S}] is single-valued when
    [g] respects [FST] (so the value is determined by the key). *)
Lemma FST_graph_isFunction : forall S g,
  (forall a b, In a S -> In b S -> FST a = FST b -> FST (g a) = FST (g b)) ->
  isFunction (FST (iUnion S (fun a => {| ⟨ a, g a ⟩ |}))).
Proof.
  intros S g Hresp c d1 d2 H1 H2.
  rewrite (FST_rel _ (graph_not_couple S g)) in H1.
  rewrite (FST_rel _ (graph_not_couple S g)) in H2.
  apply iUnion_IN in H1. destruct H1 as [e1 [He1 H1]]. apply IN_Sing_EQ in H1.
  apply iUnion_IN in H2. destruct H2 as [e2 [He2 H2]]. apply IN_Sing_EQ in H2.
  apply iUnion_IN in He1. destruct He1 as [a1 [Ha1 He1]]. apply IN_Sing_EQ in He1. subst e1.
  apply iUnion_IN in He2. destruct He2 as [a2 [Ha2 He2]]. apply IN_Sing_EQ in He2. subst e2.
  rewrite pfst_Couple, psnd_Couple in H1. rewrite pfst_Couple, psnd_Couple in H2.
  pose proof (Couple_inj_left _ _ _ _ H1) as Hc1. pose proof (Couple_inj_right _ _ _ _ H1) as Hd1.
  pose proof (Couple_inj_left _ _ _ _ H2) as Hc2. pose proof (Couple_inj_right _ _ _ _ H2) as Hd2.
  assert (Heq : FST (g a1) = FST (g a2))
    by (apply Hresp; [ exact Ha1 | exact Ha2 | rewrite <- Hc1, <- Hc2; reflexivity ]).
  congruence.
Qed.

Lemma SND_graph_isFunction : forall S g,
  (forall a b, In a S -> In b S -> SND a = SND b -> SND (g a) = SND (g b)) ->
  isFunction (SND (iUnion S (fun a => {| ⟨ a, g a ⟩ |}))).
Proof.
  intros S g Hresp c d1 d2 H1 H2.
  rewrite (SND_rel _ (graph_not_couple S g)) in H1.
  rewrite (SND_rel _ (graph_not_couple S g)) in H2.
  apply iUnion_IN in H1. destruct H1 as [e1 [He1 H1]]. apply IN_Sing_EQ in H1.
  apply iUnion_IN in H2. destruct H2 as [e2 [He2 H2]]. apply IN_Sing_EQ in H2.
  apply iUnion_IN in He1. destruct He1 as [a1 [Ha1 He1]]. apply IN_Sing_EQ in He1. subst e1.
  apply iUnion_IN in He2. destruct He2 as [a2 [Ha2 He2]]. apply IN_Sing_EQ in He2. subst e2.
  rewrite pfst_Couple, psnd_Couple in H1. rewrite pfst_Couple, psnd_Couple in H2.
  pose proof (Couple_inj_left _ _ _ _ H1) as Hc1. pose proof (Couple_inj_right _ _ _ _ H1) as Hd1.
  pose proof (Couple_inj_left _ _ _ _ H2) as Hc2. pose proof (Couple_inj_right _ _ _ _ H2) as Hd2.
  assert (Heq : SND (g a1) = SND (g a2))
    by (apply Hresp; [ exact Ha1 | exact Ha2 | rewrite <- Hc1, <- Hc2; reflexivity ]).
  congruence.
Qed.

(** Diagonals [diag X = {⟨a,a⟩ : a ∈ X}] are the special case [g = id]. *)
Lemma diag_not_couple : forall X, ~ isCouple (diag X).
Proof. intro X. unfold diag. exact (graph_not_couple X (fun a => a)). Qed.

Lemma FST_diag_isFunction : forall X, isFunction (FST (diag X)).
Proof.
  intro X. unfold diag.
  apply (FST_graph_isFunction X (fun a => a)). intros a b _ _ H. exact H.
Qed.

Lemma SND_diag_isFunction : forall X, isFunction (SND (diag X)).
Proof.
  intro X. unfold diag.
  apply (SND_graph_isFunction X (fun a => a)). intros a b _ _ H. exact H.
Qed.
