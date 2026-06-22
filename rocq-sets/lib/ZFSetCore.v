(** * ZFSetCore.v — the quotient construction and the Ens-lifted interface.

    This file is the *implementation* layer of the quotient set theory: it
    is the only place that knows [ZFSet = quot Ens].  Everything here is
    either a definition of a ZFSet operation (lifted from [EnsDefs.v]) or a
    lemma proved by *descending to the underlying [Ens] representation* —
    the proofs use [quot_ind], [mk], [In_mk], [mk_eq], the [*_mk]
    computation rules, and the [EnsLib] / [EnsNat] / [Hierarchy] lemmas.

    The *abstract* theory built on top of this interface — reasoning purely
    at the [ZFSet] level, never unfolding [mk] — lives in [ZFSetFacts.v].
    Downstream code should [Require Import ZFSet], the umbrella that
    re-exports both. *)

Require ZFC.Sets.
Require ZFC.Axioms.
Require ZFC.Cartesian.
Require ZFC.Omega.
Require ZFC.Hierarchy.

Import ZFC.Sets.
Import ZFC.Axioms.
Import ZFC.Cartesian.
Import ZFC.Omega.
Import ZFC.Hierarchy.

Require Import utils.all.
Require Import EnsNotation.
Require EnsLib.
Require EnsNat.

From Stdlib Require Import Logic.Epsilon.

(* Re-enable just the typeclass instances from [EnsLib] (Proper /
   Equivalence) without bringing its bare lemma names into scope —
   they are referenced as [EnsLib.X] throughout this file. *)
#[export] Existing Instances
  EnsLib.EQ_Equivalence
  EnsLib.Sing_Proper
  EnsLib.Paire_Proper
  EnsLib.Couple_Proper
  EnsLib.Union_Proper
  EnsLib.BinUnion_Proper
  EnsLib.IN_Proper_left
  EnsLib.IN_Proper_right
  EnsLib.INC_Proper_left
  EnsLib.INC_Proper_right
  EnsLib.iUnion_Proper
  EnsLib.pfst_Proper
  EnsLib.psnd_Proper
  EnsLib.dom_Proper
  EnsLib.rng_Proper
  EnsLib.image_Proper
  EnsLib.applyFun_Proper
  EnsLib.Power_Proper
  EnsLib.Prod_Proper
  EnsNat.natZAdd_Proper
  EnsNat.natZSucc_Proper
  EnsNat.natZPred_Proper.

(* [ens_scope] is transitively opened by EnsLib via EnsNotation and
   is kept open inside this file so that Ens-level terms appearing in
   ZFSet definitions and proofs render with the [⟨·,·⟩], [{|·|}], [≃],
   [∈], etc. notations. It is closed at the bottom of the file because
   downstream [ZFNotation.v] rebinds the same symbols in [zf_scope]. *)

From Stdlib Require Import Setoid Morphisms.
From Stdlib Require Import Logic.Epsilon.

(** * Quotient-based set theory.

    [ZFSet] is the quotient of [Ens] by extensional equality [EQ];
    equality on [ZFSet] is Coq's Leibniz [=].  Each [Ens]-level
    operation whose soundness lemma is in [EnsDefs.v] / [EnsLib.v] /
    [EnsNat.v] lifts to a [ZFSet]-level operation using [quot_map] /
    [quot_map2], and its lemmas are restated here on [ZFSet].

    Operations that take a function argument ([iUnion], [Pi])
    are lifted via [repr], which chooses a representative of a
    [ZFSet] using Hilbert's epsilon (the development already relies
    on this through [natOfEns]). *)


(** ** The quotient type and helpers *)

Definition ZFSet : Type := quot EQ.
Definition mk (e : Ens) : ZFSet := to_quot e.

Lemma mk_eq : forall x y : Ens, x ≃ y -> mk x = mk y.
Proof. intros x y H; apply quot_ext; exact H. Qed.

Lemma mk_inj : forall x y : Ens, mk x = mk y -> x ≃ y.
Proof. intros x y H; apply quot_eq; exact H. Qed.


Lemma ZFSet_ind1 :
  forall P : ZFSet -> Prop,
  (forall x : Ens, P (mk x)) -> forall X, P X.
Proof. intros P H X; pattern X; apply quot_ind. exact H. Qed.

Lemma ZFSet_ind2 :
  forall P : ZFSet -> ZFSet -> Prop,
  (forall x y : Ens, P (mk x) (mk y)) -> forall X Y, P X Y.
Proof.
  intros P H X Y.
  pattern X; apply quot_ind. intros x.
  pattern Y; apply quot_ind. intros y.
  apply H.
Qed.

Lemma ZFSet_ind3 :
  forall P : ZFSet -> ZFSet -> ZFSet -> Prop,
  (forall x y z : Ens, P (mk x) (mk y) (mk z)) ->
  forall X Y Z, P X Y Z.
Proof.
  intros P H X Y Z.
  pattern X; apply quot_ind. intros x.
  pattern Y; apply quot_ind. intros y.
  pattern Z; apply quot_ind. intros z.
  apply H.
Qed.

(** ** Choosing a representative of a [ZFSet].

    [repr X] picks some [Ens] with [mk (repr X) = X]; used to lift
    [ZFSet -> ZFSet] arguments back down to [Ens -> Ens]. *)

Definition repr (X : ZFSet) : Ens :=
  epsilon (inhabits ∅) (fun e => to_quot e = X).

Lemma repr_eq : forall X : ZFSet, to_quot (repr X) = X.
Proof.
  intros X. unfold repr.
  apply (epsilon_spec (inhabits ∅) (fun e => to_quot e = X)).
  pattern X; apply quot_ind. intros x. exists x. reflexivity.
Qed.

Lemma mk_repr : forall X : ZFSet, mk (repr X) = X.
Proof. exact repr_eq. Qed.

Lemma repr_sound : forall X Y : ZFSet, X = Y -> repr X ≃ repr Y.
Proof.
  intros X Y H. apply quot_eq. rewrite !repr_eq. exact H.
Qed.

Definition liftF (F : ZFSet -> ZFSet) (e : Ens) : Ens := repr (F (mk e)).

#[export] Instance liftF_Proper (F : ZFSet -> ZFSet) :
  Proper (EQ ==> EQ) (liftF F).
Proof.
  intros x y H. unfold liftF.
  apply repr_sound. f_equal. apply quot_ext; exact H.
Qed.


(** ** Constants and operations lifted from [EnsDefs.v] *)

Definition Empty    : ZFSet                  := mk ∅.
Definition Omega    : ZFSet                  := mk ω.

Definition Sing     : ZFSet -> ZFSet         := quot_map Sing.
Definition Paire    : ZFSet -> ZFSet -> ZFSet := quot_map2 Paire.
Definition Couple   : ZFSet -> ZFSet -> ZFSet := quot_map2 Couple.
Definition Union    : ZFSet -> ZFSet         := quot_map Union.
Definition BinUnion : ZFSet -> ZFSet -> ZFSet := quot_map2 EnsDefs.BinUnion.


(** *** Membership and inclusion.

    [IN] and [INC] are lifted by a double [quot_rec]; the inner
    instance ([{IN,INC}_inner_Proper]) discharges the soundness
    obligation of the outer [quot_rec]. *)

#[export] Instance In_inner_Proper (B : ZFSet) :
  Proper (EQ ==> eq) (fun x : Ens => quot_rec (IN x) B).
Proof.
  intros x x' Hx.
  pattern B; apply quot_ind. intros y.
  rewrite !quot_rec_eq. apply prop_ext.
  split; intro H.
  - apply IN_sound_left with x; assumption.
  - apply IN_sound_left with x';
      [apply EQ_sym; assumption | assumption].
Qed.

(* Set membership *)
Definition In (a B : ZFSet) : Prop :=
  quot_rec (fun x : Ens => quot_rec (IN x) B) a.

#[export] Instance Inc_inner_Proper (B : ZFSet) :
  Proper (EQ ==> eq) (fun x : Ens => quot_rec (INC x) B).
Proof.
  intros x x' Hx.
  pattern B; apply quot_ind. intros y.
  rewrite !quot_rec_eq.
  apply EnsLib.INC_Proper_left; exact Hx.
Qed.

Lemma In_mk : forall x y : Ens, In (mk x) (mk y) = (x ∈ y).
Proof.
  intros x y. unfold In, mk.
  rewrite quot_rec_eq, quot_rec_eq. reflexivity.
Qed.


(* Set inclusion *)
Definition Inc (a B : ZFSet) : Prop :=
  quot_rec (fun x : Ens => quot_rec (INC x) B) a.

Lemma Inc_mk : forall x y : Ens, Inc (mk x) (mk y) = (x ⊆ y).
Proof.
  intros x y. unfold Inc, mk.
  rewrite quot_rec_eq, quot_rec_eq. reflexivity.
Qed.


(** *** Equality is extensional.

    [Inc_def] and [set_ext] are placed here (before comprehension /
    Pi / applyFun) because the later proofs rely on them. *)

Lemma Inc_def (A B : ZFSet)  :
  Inc A B <-> (forall x, In x A -> In x B).
Proof.
  revert A B. apply ZFSet_ind2. intros eA eB.
  rewrite Inc_mk. split.
  - intros HINC x. pattern x; apply quot_ind. intros ex.
    rewrite !In_mk. apply HINC.
  - intros H ex Hex.
    specialize (H (mk ex)). rewrite !In_mk in H.
    apply H; exact Hex.
Qed.

(** Forward application of an inclusion: the [->] direction of
    [Inc_def] as a standalone lemma, so an [Inc] hypothesis can be
    applied to a membership fact without rewriting. *)
Lemma Inc_IN : forall A B x, Inc A B -> In x A -> In x B.
Proof. intros A B x H. now apply Inc_def. Qed.

Lemma set_ext : forall X Y, Inc X Y -> Inc Y X -> X = Y.
Proof.
  intros X Y H1 H2.
  rewrite Inc_def in H1, H2.
  pattern X, Y. revert X Y H1 H2.
  refine (ZFSet_ind2 _ _).
  intros eX eY H1 H2.
  apply mk_eq, INC_EQ; unfold INC; intros e He.
  - specialize (H1 (mk e)). unfold mk in H1; rewrite !In_mk in H1.
    apply H1; exact He.
  - specialize (H2 (mk e)). unfold mk in H2; rewrite !In_mk in H2.
    apply H2; exact He.
Qed.


(** *** Comprehension. *)

Lemma Comp_mk_Proper (P : ZFSet -> Prop) :
  Proper (EQ ==> EQ) (fun e => ⦃ x ∈ e | P (mk x) ⦄).
Proof.
  intros e e' H.
  apply EnsLib.Comp_sound_left; [|exact H].
  intros w1 w2 Pw Heq.
  assert (mk w1 = mk w2) as Hmk by (apply quot_ext; exact Heq).
  rewrite <- Hmk. exact Pw.
Qed.

Definition Comp (E : ZFSet) (P : ZFSet -> Prop) : ZFSet :=
  quot_rec (fun e => mk (⦃ x ∈ e | P (mk x) ⦄))
    (p := fun e e' H => quot_ext _ _ (Comp_mk_Proper P e e' H)) E.

(** Comprehension helpers: introduction, elimination, and inclusion.
    Predicates [P : ZFSet -> Prop] are automatically sound under [=]
    on [ZFSet], so the Ens-level soundness side condition collapses. *)

Lemma Comp_mk : forall (e : Ens) (P : ZFSet -> Prop),
  Comp (mk e) P = mk ⦃ x ∈ e | P (mk x) ⦄.
Proof. intros e P. unfold Comp, mk. rewrite quot_rec_eq. reflexivity. Qed.

(* Predicates [P : ZFSet -> Prop] composed with [mk] are sound under
   [EQ] on [Ens] because [EQ w1 w2] implies [mk w1 = mk w2]. *)
Local Lemma Comp_pred_sound (P : ZFSet -> Prop) :
  forall w1 w2 : Ens, P (mk w1) -> EQ w1 w2 -> P (mk w2).
Proof.
  intros w1 w2 Pw1 Hw12.
  rewrite <- (mk_eq _ _ Hw12). exact Pw1.
Qed.

Lemma In_Comp_P : forall (E : ZFSet) (P : ZFSet -> Prop) (x : ZFSet),
  In x (Comp E P) -> P x.
Proof.
  intros E P x.
  pattern E; apply quot_ind; intros eE.
  pattern x; apply quot_ind; intros ex.
  rewrite Comp_mk. unfold mk; rewrite In_mk; intro H.
  exact (IN_Comp_P eE ex (fun y => P (mk y)) (Comp_pred_sound P) H).
Qed.

Lemma In_P_Comp : forall (E : ZFSet) (P : ZFSet -> Prop) (x : ZFSet),
  In x E -> P x -> In x (Comp E P).
Proof.
  intros E P x.
  pattern E; apply quot_ind; intros eE.
  pattern x; apply quot_ind; intros ex.
  rewrite Comp_mk. unfold mk; rewrite !In_mk; intros HxE HPx.
  exact (IN_P_Comp eE ex (fun y => P (mk y))
           (Comp_pred_sound P) HxE HPx).
Qed.

Definition BinInter (A B : ZFSet) : ZFSet :=
  Comp A (fun x => In x B).

(** [Inter E] is the intersection of all members of [E]. Defined
    ZF-style via comprehension over [⋃ E], so that the (membership
    on ZFSet is Leibniz) predicate carries no soundness obligation.
    Matches zfc's [Inter'] (extensionally equal to its primitive
    [Inter]). When [E] is empty, [Inter E] is [Empty] under this
    definition (rather than a proper class). *)
Definition Inter (E : ZFSet) : ZFSet :=
  Comp (Union E) (fun x => forall a : ZFSet, In a E -> In x a).


(** *** Indexed union, dependent product. *)

#[export] Instance iUnion_liftF_Proper (F : ZFSet -> ZFSet) :
  Proper (EQ ==> EQ) (fun e => x ← e ;; liftF F x).
Proof.
  intros e e' H. apply (EnsLib.iUnion_Proper _ _ H).
  intros u v Huv. apply liftF_Proper; exact Huv.
Qed.

Definition iUnion (E : ZFSet) (F : ZFSet -> ZFSet) : ZFSet :=
  quot_map (fun e => x ← e ;; liftF F x) E.

Definition Power : ZFSet -> ZFSet := quot_map Power.
Definition Prod  : ZFSet -> ZFSet -> ZFSet := quot_map2 Prod.

(** [Pi A B] is the set of total functional graphs [f] from [A] into
    [⋃B] with [f(a) ∈ B(a)]. Built ZF-style: separate the power-set
    of [A × ⋃B] down to the subsets that are total and functional.
    Equality on ZFSet is Leibniz [=], so the functionality clause is
    just [b1 = b2]. *)

(** A relation [f] is *functional* (single-valued) when each input is
    paired with at most one output.  This is exactly the second clause
    of the [Pi] comprehension below, named as a standalone predicate so
    that the [Elam] arm of [eval] can separate the function values it
    builds. *)
Definition isFunction (f : ZFSet) : Prop :=
  forall a b1 b2, In (Couple a b1) f -> In (Couple a b2) f -> b1 = b2.

Definition Pi (A : ZFSet) (B : ZFSet -> ZFSet) : ZFSet :=
  Comp (Power (Prod A (iUnion A B)))
       (fun f =>
          (forall a, In a A -> exists b, In b (B a) /\ In (Couple a b) f)
          /\ (forall a b1 b2,
                In (Couple a b1) f -> In (Couple a b2) f -> b1 = b2)).


(** *** Werner-pair projections and the predicate helpers. *)

Definition is_subsingleton (X : ZFSet) : Prop :=
  forall y z : ZFSet, In y X -> In z X -> y = z.

Definition is_nonempty (X : ZFSet) : Prop :=
  exists y : ZFSet, In y X.

Definition pfst : ZFSet -> ZFSet := quot_map EnsDefs.pfst.
Definition psnd : ZFSet -> ZFSet := quot_map EnsDefs.psnd.

(* Pair-pattern variant of [iUnion]: bind [pfst] / [psnd] of
   each iterated element. *)
Definition iUnion_pat
  (E : ZFSet) (F : ZFSet -> ZFSet -> ZFSet) : ZFSet :=
  iUnion E (fun p => F (pfst p) (psnd p)).


(** *** Triple projections and the relational helpers. *)

Definition proj1 (t : ZFSet) : ZFSet := pfst t.
Definition proj2 (t : ZFSet) : ZFSet := pfst (psnd t).
Definition proj3 (t : ZFSet) : ZFSet := psnd (psnd t).

(* Triple-pattern variant of [iUnion]: bind [proj1] / [proj2] / [proj3]
   of each iterated element. *)
Definition iUnion_pat3
  (E : ZFSet) (F : ZFSet -> ZFSet -> ZFSet -> ZFSet) : ZFSet :=
  iUnion E (fun t => F (proj1 t) (proj2 t) (proj3 t)).

Definition dom   : ZFSet -> ZFSet         := quot_map EnsDefs.dom.
Definition rng   : ZFSet -> ZFSet         := quot_map EnsDefs.rng.
Definition image : ZFSet -> ZFSet -> ZFSet := quot_map2 EnsDefs.image.

(** [applyFun f v]: relational application of [f] at [v], defined
    in terms of [Union], [image], and [Sing]. When [f] is functional
    and [v ∈ dom f], [image f {v}] is a singleton and [Union] collapses
    it to the unique output. *)
Definition applyFun (f v : ZFSet) : ZFSet := Union (image f (Sing v)).


(** *** Naturals (from [EnsNat.v]). *)

Definition natZ (n : nat) : ZFSet := mk (EnsDefs.natZ n).
Definition natId   : ZFSet := mk EnsDefs.natId.
Definition natZAdd : ZFSet -> ZFSet -> ZFSet := quot_map2 EnsNat.natZAdd.
Definition natZSucc : ZFSet -> ZFSet         := quot_map EnsNat.natZSucc.
Definition natZPred : ZFSet -> ZFSet         := quot_map EnsNat.natZPred.


(** ** Computation rules for the lifted operations. *)

Lemma Union_mk    e   : Union (mk e)        = mk (⋃ e).
Proof. apply quot_map_eq. Qed.

Lemma Sing_mk     e   : Sing (mk e)         = mk {| e |}.
Proof. apply quot_map_eq. Qed.

Lemma natZSucc_mk e   : natZSucc (mk e)     = mk (EnsNat.natZSucc e).
Proof. apply quot_map_eq. Qed.

Lemma natZPred_mk e   : natZPred (mk e)     = mk (EnsNat.natZPred e).
Proof. apply quot_map_eq. Qed.

Lemma Paire_mk    e f : Paire (mk e) (mk f) = mk {| e ; f |}.
Proof. apply quot_map2_eq. Qed.

Lemma Couple_mk   e f : Couple (mk e) (mk f) = mk ⟨ e , f ⟩.
Proof. apply quot_map2_eq. Qed.

Lemma BinUnion_mk e f : BinUnion (mk e) (mk f) = mk (e ∪ f).
Proof. apply quot_map2_eq. Qed.

Lemma pfst_mk     e   : pfst (mk e) = mk (EnsDefs.pfst e).
Proof. apply quot_map_eq. Qed.

Lemma psnd_mk     e   : psnd (mk e) = mk (EnsDefs.psnd e).
Proof. apply quot_map_eq. Qed.

Lemma dom_mk      e   : dom (mk e) = mk (EnsDefs.dom e).
Proof. apply quot_map_eq. Qed.

Lemma rng_mk      e   : rng (mk e) = mk (EnsDefs.rng e).
Proof. apply quot_map_eq. Qed.

Lemma image_mk    e f : image (mk e) (mk f) = mk (EnsDefs.image e f).
Proof. apply quot_map2_eq. Qed.

Lemma applyFun_mk e f : applyFun (mk e) (mk f) = mk (EnsDefs.applyFun e f).
Proof.
  unfold applyFun, EnsDefs.applyFun.
  rewrite Sing_mk, image_mk, Union_mk. reflexivity.
Qed.

Lemma Power_mk e : Power (mk e) = mk (Axioms.Power e).
Proof. apply quot_map_eq. Qed.

Lemma Prod_mk  e f : Prod (mk e) (mk f) = mk (Cartesian.Prod e f).
Proof. apply quot_map2_eq. Qed.

Lemma natZAdd_mk  e f : natZAdd (mk e) (mk f) = mk (EnsNat.natZAdd e f).
Proof. apply quot_map2_eq. Qed.


(** ** Lifted lemmas. *)

(** *** Membership-introduction lemmas (from [Sets.v] / [Axioms.v]). *)

Lemma IN_Sing : forall x : ZFSet, In x (Sing x).
Proof.
  intro x. pattern x; apply quot_ind. intros e.
  unfold Sing. rewrite quot_map_eq.
  unfold mk. rewrite In_mk. apply IN_Sing.
Qed.

Lemma IN_Sing_EQ :
  forall x y : ZFSet, In x (Sing y) -> x = y.
Proof.
  intros x y. pattern x; apply quot_ind. intros ex.
  pattern y; apply quot_ind. intros ey.
  unfold Sing. rewrite quot_map_eq.
  unfold mk. rewrite In_mk.
  intro H. apply mk_eq, IN_Sing_EQ; exact H.
Qed.

Lemma IN_Paire_left  : forall a b : ZFSet, In a (Paire a b).
Proof.
  intros a b. pattern a; apply quot_ind. intros ea.
  pattern b; apply quot_ind. intros eb.
  rewrite Paire_mk. unfold mk; rewrite In_mk.
  apply IN_Paire_left.
Qed.

Lemma IN_Paire_right : forall a b : ZFSet, In b (Paire a b).
Proof.
  intros a b. pattern a; apply quot_ind. intros ea.
  pattern b; apply quot_ind. intros eb.
  rewrite Paire_mk. unfold mk; rewrite In_mk.
  apply IN_Paire_right.
Qed.

Lemma Paire_IN :
  forall a b x : ZFSet, In x (Paire a b) -> x = a \/ x = b.
Proof.
  intros a b x.
  pattern x; apply quot_ind. intros ex.
  pattern a; apply quot_ind. intros ea.
  pattern b; apply quot_ind. intros eb.
  rewrite Paire_mk. unfold mk; rewrite In_mk.
  intro H. destruct (Paire_IN _ _ _ H) as [Heq | Heq];
    [left | right]; apply mk_eq; assumption.
Qed.


(** *** Union laws (from [Axioms.v] and [EnsLib.v]). *)

Lemma IN_Union :
  forall E Y X : ZFSet, In Y E -> In X Y -> In X (Union E).
Proof.
  intros E Y X.
  pattern E; apply quot_ind. intros eE.
  pattern Y; apply quot_ind. intros eY.
  pattern X; apply quot_ind. intros eX.
  rewrite Union_mk. unfold mk. rewrite !In_mk.
  apply IN_Union.
Qed.

Lemma IN_Inc_Union :
  forall E Y : ZFSet, In Y E -> Inc Y (Union E).
Proof.
  intros E Y.
  pattern E; apply quot_ind. intros eE.
  pattern Y; apply quot_ind. intros eY.
  rewrite Union_mk. unfold mk. rewrite In_mk, Inc_mk.
  apply IN_INC_Union.
Qed.

Lemma Union_IN :
  forall E X : ZFSet,
  In X (Union E) -> exists Y : ZFSet, In Y E /\ In X Y.
Proof.
  intros E X.
  pattern E; apply quot_ind. intros eE.
  pattern X; apply quot_ind. intros eX.
  rewrite Union_mk. unfold mk. rewrite In_mk.
  intro H. destruct (Union_IN _ _ H) as [eY [HYE HXY]].
  exists (mk eY). unfold mk; rewrite !In_mk. split; assumption.
Qed.

Lemma Union_mon :
  forall E E' : ZFSet, Inc E E' -> Inc (Union E) (Union E').
Proof.
  intros E E'.
  pattern E; apply quot_ind. intros eE.
  pattern E'; apply quot_ind. intros eE'.
  rewrite !Union_mk, !Inc_mk. apply Union_mon.
Qed.

Lemma Union_Sing : forall E : ZFSet, Union (Sing E) = E.
Proof.
  intro E. pattern E; apply quot_ind. intros e.
  rewrite Sing_mk, Union_mk.
  apply mk_eq, EnsLib.Union_Sing.
Qed.

Lemma BinUnion_Vide_l : forall E : ZFSet, BinUnion Empty E = E.
Proof.
  intro E. pattern E; apply quot_ind. intros e.
  unfold Empty. rewrite BinUnion_mk.
  apply mk_eq, EnsLib.BinUnion_Vide_l.
Qed.

Lemma BinUnion_Vide_r : forall E : ZFSet, BinUnion E Empty = E.
Proof.
  intro E. pattern E; apply quot_ind. intros e.
  unfold Empty. rewrite BinUnion_mk.
  apply mk_eq, EnsLib.BinUnion_Vide_r.
Qed.

Lemma empty_union_empty : BinUnion Empty Empty = Empty.
Proof. apply BinUnion_Vide_l. Qed.

Lemma IN_BinUnion_l :
  forall x E E' : ZFSet, In x E -> In x (BinUnion E E').
Proof.
  intros x E E'.
  pattern x; apply quot_ind. intros ex.
  pattern E; apply quot_ind. intros eE.
  pattern E'; apply quot_ind. intros eE'.
  rewrite BinUnion_mk.
  unfold mk. rewrite !In_mk. apply EnsLib.IN_BinUnion_l.
Qed.

Lemma IN_BinUnion_r :
  forall x E E' : ZFSet, In x E' -> In x (BinUnion E E').
Proof.
  intros x E E'.
  pattern x; apply quot_ind. intros ex.
  pattern E; apply quot_ind. intros eE.
  pattern E'; apply quot_ind. intros eE'.
  rewrite BinUnion_mk.
  unfold mk. rewrite !In_mk. apply EnsLib.IN_BinUnion_r.
Qed.

Lemma BinUnion_IN :
  forall x E E' : ZFSet,
  In x (BinUnion E E') -> In x E \/ In x E'.
Proof.
  intros x E E'.
  pattern x; apply quot_ind. intros ex.
  pattern E; apply quot_ind. intros eE.
  pattern E'; apply quot_ind. intros eE'.
  rewrite BinUnion_mk.
  unfold mk. rewrite !In_mk. apply EnsLib.BinUnion_IN.
Qed.


(** *** Indexed union laws (from [EnsLib.v]).

    The [iUnion] introduction / elimination and equational laws are
    stated; the proofs of the equational laws depend on
    [EnsLib.iUnion_Sing_l] / [EnsLib.iUnion_Sing_r] / [EnsLib.iUnion_assoc] in [EnsLib.v]
    which are themselves still admitted there. *)

Lemma iUnion_mk (e0 : Ens) (F : ZFSet -> ZFSet) :
  iUnion (mk e0) F = mk (EnsDefs.iUnion e0 (liftF F)).
Proof.
  unfold iUnion, mk.
  exact (quot_map_eq (fun e1 => EnsDefs.iUnion e1 (liftF F)) e0).
Qed.

Lemma iUnion_Vide :
  forall F : ZFSet -> ZFSet, iUnion Empty F = Empty.
Proof.
  intro F. unfold Empty. rewrite iUnion_mk.
  apply mk_eq, EnsLib.iUnion_Vide.
Qed.

Lemma iUnion_Sing_r :
  forall E : ZFSet, iUnion E (fun x => Sing x) = E.
Proof.
  intro E.
  pattern E; apply quot_ind. intros eE.
  cbv beta. fold (mk eE).
  rewrite iUnion_mk.
  apply mk_eq.
  apply EQ_tran with (EnsDefs.iUnion eE Axioms.Sing).
  - apply EnsLib.iUnion_Proper. apply EQ_refl.
    intros u v Huv.
    apply EQ_tran with (Axioms.Sing u);
      [| apply Sing_sound; exact Huv].
    apply quot_eq. unfold liftF.
    rewrite mk_repr. apply Sing_mk.
  - apply EnsLib.iUnion_Sing_r.
Qed.

Lemma iUnion_Sing_l :
  forall (E : ZFSet) (F : ZFSet -> ZFSet),
  iUnion (Sing E) F = F E.
Proof.
  intros E F.
  pattern E; apply quot_ind. intros eE.
  cbv beta. fold (mk eE).
  rewrite Sing_mk. rewrite iUnion_mk.
  transitivity (mk (liftF F eE)).
  - apply mk_eq, EnsLib.iUnion_Sing_l.
  - unfold liftF. apply mk_repr.
Qed.

Lemma iUnion_assoc :
  forall (E : ZFSet) (F G : ZFSet -> ZFSet),
  iUnion (iUnion E F) G = iUnion E (fun x => iUnion (F x) G).
Proof.
  intros E F G.
  pattern E; apply quot_ind. intros eE.
  cbv beta. fold (mk eE).
  rewrite !iUnion_mk.
  apply mk_eq.
  apply EQ_tran
    with (EnsDefs.iUnion eE (fun x => EnsDefs.iUnion (liftF F x) (liftF G))).
  - apply EnsLib.iUnion_assoc.
    + intros u v Huv. apply (liftF_Proper F); exact Huv.
    + intros u v Huv. apply (liftF_Proper G); exact Huv.
  - apply EnsLib.iUnion_Proper. apply EQ_refl.
    intros u v Huv.
    apply EQ_tran with (EnsDefs.iUnion (liftF F v) (liftF G)).
    + apply EnsLib.iUnion_sound_l.
      * intros a b Hab. apply (liftF_Proper G); exact Hab.
      * apply (liftF_Proper F); exact Huv.
    + apply quot_eq.
      transitivity (iUnion (F (mk v)) G).
      * transitivity (iUnion (mk (liftF F v)) G).
        -- symmetry. apply iUnion_mk.
        -- f_equal. unfold liftF. apply mk_repr.
      * symmetry. unfold liftF. apply mk_repr.
Qed.

Lemma IN_iUnion :
  forall (E : ZFSet) (F : ZFSet -> ZFSet) (y x : ZFSet),
  In y E -> In x (F y) -> In x (iUnion E F).
Proof.
  intros E F y x.
  pattern E; apply quot_ind. intros eE.
  pattern y; apply quot_ind. intros ey.
  pattern x; apply quot_ind. intros ex.
  cbv beta. intros HyE HxFy.
  rewrite iUnion_mk, In_mk.
  apply EnsLib.IN_iUnion with (y := ey).
  - intros u v Huv. apply liftF_Proper; exact Huv.
  - rewrite In_mk in HyE. exact HyE.
  - assert (HF : mk (liftF F ey) = F (mk ey))
      by (unfold liftF; apply mk_repr).
    rewrite <- In_mk, HF. exact HxFy.
Qed.

Lemma iUnion_IN :
  forall (E : ZFSet) (F : ZFSet -> ZFSet) (x : ZFSet),
  In x (iUnion E F) ->
  exists y : ZFSet, In y E /\ In x (F y).
Proof.
  intros E F x.
  pattern E; apply quot_ind. intros eE.
  pattern x; apply quot_ind. intros ex.
  cbv beta. rewrite iUnion_mk. rewrite In_mk.
  intro H.
  destruct (EnsLib.iUnion_IN _ _ _ H) as [ey [HyE Hex]].
  exists (mk ey). split.
  - rewrite In_mk. exact HyE.
  - assert (HF : F (mk ey) = mk (liftF F ey))
      by (unfold liftF; symmetry; apply mk_repr).
    rewrite HF. rewrite In_mk. exact Hex.
Qed.

(** Subset rule for [iUnion]: a union is included in [T] exactly when
    every fiber is.  This is the [Inc]-level counterpart of [iUnion_IN],
    letting one prove [⊆] goals over a union without unfolding to
    pointwise membership. *)
Lemma iUnion_Inc :
  forall (E : ZFSet) (F : ZFSet -> ZFSet) (T : ZFSet),
  (forall y, In y E -> Inc (F y) T) -> Inc (iUnion E F) T.
Proof.
  intros E F T h. apply Inc_def. intros x xIn.
  apply iUnion_IN in xIn. destruct xIn as [y [yIn xFy]].
  exact (Inc_IN _ _ _ (h y yIn) xFy).
Qed.


(** *** Power-set laws (from [Axioms.v]). *)

Lemma IN_Power_Inc :
  forall E X : ZFSet, In X (Power E) -> Inc X E.
Proof.
  intros E X.
  pattern E; apply quot_ind. intros eE.
  pattern X; apply quot_ind. intros eX.
  rewrite Power_mk. unfold mk. rewrite In_mk, Inc_mk.
  apply IN_Power_INC.
Qed.

Lemma Inc_IN_Power :
  forall E X : ZFSet, Inc X E -> In X (Power E).
Proof.
  intros E X.
  pattern E; apply quot_ind. intros eE.
  pattern X; apply quot_ind. intros eX.
  rewrite Power_mk. unfold mk. rewrite In_mk, Inc_mk.
  apply INC_IN_Power.
Qed.

Lemma Power_mon :
  forall E E' : ZFSet, Inc E E' -> Inc (Power E) (Power E').
Proof.
  intros E E'.
  pattern E; apply quot_ind. intros eE.
  pattern E'; apply quot_ind. intros eE'.
  rewrite !Power_mk, !Inc_mk. apply Power_mon.
Qed.


(** *** Comprehension inclusion (from [Axioms.v]). *)

Lemma Comp_Inc :
  forall (E : ZFSet) (P : ZFSet -> Prop), Inc (Comp E P) E.
Proof.
  intros E P. pattern E; apply quot_ind. intros eE.
  rewrite Comp_mk, Inc_mk. apply Comp_INC.
Qed.


(** *** Intersection laws (from [Axioms.v]). *)

Lemma IN_Inter_all :
  forall E X : ZFSet,
  In X (Inter E) -> forall A : ZFSet, In A E -> In X A.
Proof.
  intros E X HX.
  apply (In_Comp_P (Union E)
           (fun x => forall a : ZFSet, In a E -> In x a) X HX).
Qed.

Lemma all_IN_Inter :
  forall E X Y : ZFSet,
  In Y E ->
  (forall A : ZFSet, In A E -> In X A) ->
  In X (Inter E).
Proof.
  intros E X Y HY HX. apply In_P_Comp.
  - apply IN_Union with Y; [exact HY | apply HX; exact HY].
  - exact HX.
Qed.

Lemma Inter_Inc_Union : forall E : ZFSet, Inc (Inter E) (Union E).
Proof. intros E. apply Comp_Inc. Qed.


(** *** Inclusion is reflexive and transitive (from [Sets.v]). *)

Lemma Inc_refl : forall X : ZFSet, Inc X X.
Proof.
  intros X. pattern X; apply quot_ind. intros e.
  rewrite Inc_mk. apply INC_refl.
Qed.

Lemma Inc_tran :
  forall X Y Z : ZFSet, Inc X Y -> Inc Y Z -> Inc X Z.
Proof.
  intros X Y Z.
  pattern X; apply quot_ind. intros eX.
  pattern Y; apply quot_ind. intros eY.
  pattern Z; apply quot_ind. intros eZ.
  rewrite !Inc_mk. apply INC_tran.
Qed.


(** *** Empty-set laws (from [Axioms.v]). *)

Lemma not_In_Empty : forall X : ZFSet, ~ In X Empty.
Proof.
  intros X. pattern X; apply quot_ind. intros e Hin.
  change Empty with (mk ∅) in Hin.
  rewrite In_mk in Hin.
  elim (Vide_est_vide _ Hin).
Qed.

Lemma all_empty_eq_Empty :
  forall X : ZFSet, (forall Y : ZFSet, ~ In Y X) -> X = Empty.
Proof.
  intros X. pattern X; apply quot_ind. intros eX HX.
  change Empty with (mk ∅). apply mk_eq.
  apply tout_vide_est_Vide. intros eY HY.
  exfalso. apply (HX (mk eY)). rewrite In_mk. exact HY.
Qed.


(** *** Further singleton laws (from [Axioms.v]). *)

Lemma Sing_inj : forall a b : ZFSet, Sing a = Sing b -> a = b.
Proof.
  intros a b. pattern a; apply quot_ind. intros ea.
  pattern b; apply quot_ind. intros eb.
  rewrite !Sing_mk. intros H. apply mk_inj in H.
  apply mk_eq, EQ_Sing_EQ. exact H.
Qed.

Lemma Sing_neq_Empty : forall a : ZFSet, Sing a <> Empty.
Proof.
  intros a. pattern a; apply quot_ind. intros ea.
  rewrite Sing_mk. unfold Empty.
  intro H. apply mk_inj in H. elim (not_EQ_Sing_Vide _ H).
Qed.

Lemma Empty_neq_Sing : forall a : ZFSet, Empty <> Sing a.
Proof.
  intros a H. apply (Sing_neq_Empty a). symmetry. exact H.
Qed.


(** *** Subsingleton / nonempty laws (from [EnsLib.v]). *)

Lemma Sing_is_subsingleton :
  forall a : ZFSet, is_subsingleton (Sing a).
Proof.
  intros a y z Hy Hz.
  rewrite (IN_Sing_EQ _ _ Hy), (IN_Sing_EQ _ _ Hz).
  reflexivity.
Qed.

Lemma not_subsingleton_PaireVS :
  forall b : ZFSet,
  ~ is_subsingleton (Paire Empty (Sing b)).
Proof.
  intros b. revert b. apply ZFSet_ind1. intros eb Hsub.
  pose proof (Hsub Empty (Sing (mk eb))) as HVS.
  specialize (HVS (IN_Paire_left _ _) (IN_Paire_right _ _)).
  unfold Empty in HVS. rewrite Sing_mk in HVS.
  elim (not_EQ_Vide_Sing eb). apply mk_inj. exact HVS.
Qed.

Lemma is_nonempty_Sing : forall E : ZFSet, is_nonempty (Sing E).
Proof. intro E. exists E. apply IN_Sing. Qed.

Lemma not_nonempty_Vide : ~ is_nonempty Empty.
Proof.
  intros [y Hy].
  pattern y in Hy; revert Hy; pattern y; apply quot_ind.
  intros e HIN.
  change Empty with (mk Vide) in HIN.
  rewrite In_mk in HIN.
  elim (Vide_est_vide _ HIN).
Qed.


(** *** Werner-pair projection laws (from [EnsLib.v]). *)

Lemma Couple_inj_left :
  forall A A' B B' : ZFSet,
  Couple A A' = Couple B B' -> A = B.
Proof.
  intros A A' B B'.
  pattern A; apply quot_ind. intros eA.
  pattern A'; apply quot_ind. intros eA'.
  pattern B; apply quot_ind. intros eB.
  pattern B'; apply quot_ind. intros eB'.
  rewrite !Couple_mk. intro H.
  apply mk_inj in H. apply mk_eq.
  exact (Couple_inj_left _ _ _ _ H).
Qed.

Lemma Couple_inj_right :
  forall A A' B B' : ZFSet,
  Couple A A' = Couple B B' -> A' = B'.
Proof.
  intros A A' B B'.
  pattern A; apply quot_ind. intros eA.
  pattern A'; apply quot_ind. intros eA'.
  pattern B; apply quot_ind. intros eB.
  pattern B'; apply quot_ind. intros eB'.
  rewrite !Couple_mk. intro H.
  apply mk_inj in H. apply mk_eq.
  exact (Couple_inj_right _ _ _ _ H).
Qed.

Lemma Couple_IN_Prod :
  forall E1 E2 X1 X2 : ZFSet,
  In X1 E1 -> In X2 E2 -> In (Couple X1 X2) (Prod E1 E2).
Proof.
  intros E1 E2 X1 X2.
  pattern E1; apply quot_ind. intros e1.
  pattern E2; apply quot_ind. intros e2.
  pattern X1; apply quot_ind. intros x1.
  pattern X2; apply quot_ind. intros x2.
  rewrite Couple_mk, Prod_mk. unfold mk. rewrite !In_mk.
  apply Couple_IN_Prod.
Qed.

Lemma Couple_Prod_IN :
  forall E1 E2 X1 X2 : ZFSet,
  In (Couple X1 X2) (Prod E1 E2) -> In X1 E1 /\ In X2 E2.
Proof.
  intros E1 E2 X1 X2.
  pattern E1; apply quot_ind. intros e1.
  pattern E2; apply quot_ind. intros e2.
  pattern X1; apply quot_ind. intros x1.
  pattern X2; apply quot_ind. intros x2.
  rewrite Couple_mk, Prod_mk. unfold mk. rewrite !In_mk.
  apply Couple_Prod_IN.
Qed.

(** Domain introduction: the first component of any edge lies in the
    domain.  (The rest of the development only ever *eliminates* [dom]
    via [Comp_Inc]; this dual is needed to place a fixed point inside the
    [⦃ a ∈ dom f | … ⦄] comprehension that a [fix] denotes.) *)
Lemma IN_dom : forall a b f : ZFSet, In (Couple a b) f -> In a (dom f).
Proof.
  intros a b f.
  pattern f; apply quot_ind. intros ef.
  pattern a; apply quot_ind. intros ea.
  pattern b; apply quot_ind. intros eb.
  rewrite Couple_mk, dom_mk. unfold mk. rewrite !In_mk.
  intro Hab. unfold EnsDefs.dom.
  apply Axioms.IN_P_Comp.
  - intros w1 w2 [bb Hbb] Hw. exists bb.
    apply Sets.IN_sound_left with (Cartesian.Couple w1 bb);
      [ apply Cartesian.Couple_sound_left; exact Hw | exact Hbb ].
  - apply Axioms.IN_Union with (Axioms.Sing ea); [ | apply Axioms.IN_Sing ].
    apply Axioms.IN_Union with (Cartesian.Couple ea eb); [ exact Hab | ].
    unfold Cartesian.Couple. apply Axioms.IN_Paire_left.
  - exists eb. exact Hab.
Qed.

Lemma IN_Prod_EXType :
  forall E E' X : ZFSet,
  In X (Prod E E') ->
  exists A B : ZFSet, Couple A B = X.
Proof.
  intros E E' X.
  pattern E; apply quot_ind. intros eE.
  pattern E'; apply quot_ind. intros eE'.
  pattern X; apply quot_ind. intros eX.
  rewrite Prod_mk. unfold mk. rewrite In_mk.
  intros H. destruct (IN_Prod_EXType _ _ _ H) as [eA [eB Hab]].
  exists (mk eA), (mk eB).
  rewrite Couple_mk. apply mk_eq. exact Hab.
Qed.

(** Bundled pair extractor: every element of [Prod E1 E2] is a [Couple]
    of components witnessed in [E1] and [E2]. *)
Lemma IN_Prod_EX :
  forall E1 E2 y : ZFSet, In y (Prod E1 E2) ->
  exists a b : ZFSet, In a E1 /\ In b E2 /\ y = Couple a b.
Proof.
  intros E1 E2 y Hy.
  destruct (IN_Prod_EXType _ _ _ Hy) as [a [b Hab]].
  subst y. destruct (Couple_Prod_IN _ _ _ _ Hy) as [Ha Hb].
  exists a, b. repeat split; assumption.
Qed.

Lemma Prod_Sing_Sing :
  forall a b : ZFSet, Prod (Sing a) (Sing b) = Sing (Couple a b).
Proof.
  intros a b. apply set_ext; rewrite Inc_def; intros y Hy.
  - apply IN_Prod_EX in Hy. destruct Hy as [a' [b' [Ha [Hb Heq]]]].
    apply IN_Sing_EQ in Ha, Hb. subst a' b' y. apply IN_Sing.
  - apply IN_Sing_EQ in Hy. subst y.
    apply Couple_IN_Prod; apply IN_Sing.
Qed.


Lemma pfst_Couple : forall a b : ZFSet, pfst (Couple a b) = a.
Proof.
  intros a b.
  pattern a; apply quot_ind. intros ea.
  pattern b; apply quot_ind. intros eb.
  rewrite Couple_mk, pfst_mk.
  apply mk_eq, EnsLib.pfst_Couple.
Qed.

Lemma psnd_Couple : forall a b : ZFSet, psnd (Couple a b) = b.
Proof.
  intros a b.
  pattern a; apply quot_ind. intros ea.
  pattern b; apply quot_ind. intros eb.
  rewrite Couple_mk, psnd_mk.
  apply mk_eq, EnsLib.psnd_Couple.
Qed.

Lemma proj1_triple :
  forall a b c : ZFSet, proj1 (Couple a (Couple b c)) = a.
Proof. intros; apply pfst_Couple. Qed.

Lemma proj2_triple :
  forall a b c : ZFSet, proj2 (Couple a (Couple b c)) = b.
Proof.
  intros a b c. unfold proj2.
  rewrite psnd_Couple. apply pfst_Couple.
Qed.

Lemma proj3_triple :
  forall a b c : ZFSet, proj3 (Couple a (Couple b c)) = c.
Proof.
  intros a b c. unfold proj3.
  rewrite psnd_Couple. apply psnd_Couple.
Qed.


(** *** Range / image laws (from [EnsLib.v]). *)

Lemma rng_intro :
  forall r a b : ZFSet,
  In (Couple a b) r -> In b (rng r).
Proof.
  intros r a b.
  pattern r; apply quot_ind. intros er.
  pattern a; apply quot_ind. intros ea.
  pattern b; apply quot_ind. intros eb.
  rewrite Couple_mk, rng_mk.
  unfold mk. rewrite !In_mk. apply EnsLib.rng_intro.
Qed.

Lemma rng_elim :
  forall r b : ZFSet,
  In b (rng r) -> exists a : ZFSet, In (Couple a b) r.
Proof.
  intros r b.
  pattern r; apply quot_ind. intros er.
  pattern b; apply quot_ind. intros eb.
  rewrite rng_mk. unfold mk. rewrite In_mk.
  intro H. destruct (EnsLib.rng_elim _ _ H) as [a Ha].
  exists (mk a). rewrite Couple_mk. unfold mk; rewrite In_mk. exact Ha.
Qed.

Lemma image_intro :
  forall r S b a : ZFSet,
  In a S ->
  In (Couple a b) r ->
  In b (image r S).
Proof.
  intros r S b a.
  pattern r; apply quot_ind. intros er.
  pattern S; apply quot_ind. intros eS.
  pattern b; apply quot_ind. intros eb.
  pattern a; apply quot_ind. intros ea.
  rewrite Couple_mk, image_mk.
  unfold mk. rewrite !In_mk. apply EnsLib.image_intro.
Qed.

Lemma image_elim :
  forall r S b : ZFSet,
  In b (image r S) ->
  exists a : ZFSet, In a S /\ In (Couple a b) r.
Proof.
  intros r S b.
  pattern r; apply quot_ind. intros er.
  pattern S; apply quot_ind. intros eS.
  pattern b; apply quot_ind. intros eb.
  rewrite image_mk. unfold mk. rewrite In_mk.
  intro H. destruct (EnsLib.image_elim _ _ _ H) as [a [HaS Hab]].
  exists (mk a). rewrite Couple_mk. unfold mk; rewrite !In_mk.
  split; assumption.
Qed.

(** [Inc]-elimination for the image of a singleton domain: the image
    of [{|a|}] is included in [B] when every [b] paired with [a] in [r]
    lies in [B].  Encapsulates the [image_elim] / [IN_Sing_EQ] reasoning
    so callers stay at the [Inc] level. *)
Lemma image_Sing_Inc r a B :
  (forall b, In (Couple a b) r -> In b B) -> Inc (image r (Sing a)) B.
Proof.
  intro h. apply Inc_def. intros b Hb.
  apply image_elim in Hb. destruct Hb as [a' [Ha' Hab]].
  apply IN_Sing_EQ in Ha'. subst a'. exact (h b Hab).
Qed.



(* ----- naturals, foundation and Omega (Ens-lifted) ----- *)

(** *** Naturals (from [EnsNat.v]). *)

(** *** Foundation / Omega laws (from [Omega.v]). *)

Lemma not_In_self : forall X : ZFSet, ~ In X X.
Proof.
  intros X. pattern X; apply quot_ind. intros e Hin.
  unfold mk in Hin. rewrite In_mk in Hin.
  elim (E_not_IN_E _ Hin).
Qed.

Lemma Omega_eq_Union : Omega = Union Omega.
Proof.
  unfold Omega. rewrite Union_mk.
  apply mk_eq. apply Omega_EQ_Union.
Qed.


Lemma natZAdd_natZ :
  forall k1 k2 : nat,
  natZAdd (natZ k1) (natZ k2) = natZ (k1 + k2).
Proof.
  intros k1 k2. unfold natZ.
  rewrite natZAdd_mk. apply mk_eq, EnsNat.natZAdd_natZ.
Qed.

Lemma natZSucc_natZ : forall k : nat, natZSucc (natZ k) = natZ (S k).
Proof.
  intro k. unfold natZ.
  rewrite natZSucc_mk. apply mk_eq, EnsNat.natZSucc_natZ.
Qed.

Lemma natZPred_natZ : forall k : nat, natZPred (natZ k) = natZ (pred k).
Proof.
  intro k. unfold natZ.
  rewrite natZPred_mk. apply mk_eq, EnsNat.natZPred_natZ.
Qed.


Lemma natZ_mem_omega : forall n : nat, In (natZ n) Omega.
Proof.
  intro n. unfold natZ, Omega, mk.
  rewrite In_mk. apply EnsNat.natZ_mem_natSet.
Qed.

(* TODO: add a counterpart to IN_Omega_EXType, which extracts an abstract nat. *)

Lemma In_omega : forall X, In X Omega -> { n | (natZ n) = X }.
Proof.
  intros X HX.
  set (eX := repr X).
  assert (HmkeX : mk eX = X) by apply mk_repr.
  assert (HIN : eX ∈ ω).
  { rewrite <- HmkeX in HX.
    change Omega with (mk ω) in HX.
    rewrite In_mk in HX. exact HX. }
  exists (epsilon (inhabits 0%nat) (fun k => Nat k ≃ eX)).
  unfold natZ. rewrite <- HmkeX. apply mk_eq.
  apply (epsilon_spec (inhabits 0%nat) (fun k => Nat k ≃ eX)).
  destruct (IN_Omega_EXType eX HIN) as [k Hk].
  exists k. exact Hk.
Qed.


Lemma natZ_zero_eq_empty : natZ 0 = Empty.
Proof.
  unfold natZ, Empty. apply mk_eq, EnsNat.natZ_zero_eq_empty.
Qed.

Lemma pair_self_mem_natId :
  forall n : nat, In (Couple (natZ n) (natZ n)) natId.
Proof.
  intro n. unfold natZ, natId.
  rewrite Couple_mk. unfold mk. rewrite In_mk.
  apply EnsNat.pair_self_mem_natId.
Qed.

Lemma IN_Nat_lt :
  forall k1 k2 : nat, (k1 < k2)%nat -> In (natZ k1) (natZ k2).
Proof.
  intros k1 k2 H. unfold natZ, mk.
  rewrite In_mk. apply EnsNat.IN_Nat_lt; exact H.
Qed.

Lemma Nat_inj :
  forall k1 k2 : nat, natZ k1 = natZ k2 -> k1 = k2.
Proof.
  intros k1 k2 H. apply EnsNat.Nat_inj.
  apply mk_inj. exact H.
Qed.

(** Distinct naturals denote distinct sets (contrapositive of [Nat_inj]). *)
Lemma natZ_neq : forall n m : nat, n <> m -> natZ n <> natZ m.
Proof. intros n m Hnm H. apply Hnm, Nat_inj, H. Qed.

Lemma IN_natId_EXType :
  forall p : ZFSet,
  In p natId ->
  exists n : nat, p = Couple (natZ n) (natZ n).
Proof.
  intro p. pattern p; apply quot_ind. intros e.
  unfold natId, mk. rewrite In_mk.
  intro H. destruct (EnsLib.IN_natId_EXType _ H) as [n Hn].
  exists n. unfold natZ. rewrite Couple_mk.
  apply mk_eq. exact Hn.
Qed.

Lemma natId_pair_diagonal :
  forall a b : ZFSet, In (Couple a b) natId -> a = b.
Proof.
  intros a b. pattern a; apply quot_ind. intros ea.
  pattern b; apply quot_ind. intros eb.
  rewrite Couple_mk. unfold natId, mk.
  rewrite In_mk.
  intro H. apply mk_eq, EnsLib.natId_pair_diagonal. exact H.
Qed.

Lemma natId_diagonal :
  forall p : ZFSet, In p natId -> pfst p = psnd p.
Proof.
  intro p. pattern p; apply quot_ind. intros e.
  unfold natId, mk. rewrite In_mk.
  intro H. rewrite pfst_mk, psnd_mk.
  apply mk_eq, EnsLib.natId_diagonal. exact H.
Qed.


Lemma Sing_Inc_IN X Y:
  In X Y ->  Inc (Sing X) Y.
Proof.
  intro h. rewrite Inc_def. intros x XIn.
  apply IN_Sing_EQ in XIn. subst. auto.
Qed.

(** A singleton inclusion is just a membership: the [Inc]-level
    characterization of [Sing], so [⊆] goals with a singleton source
    reduce to a single membership without introducing the element. *)
Lemma Sing_Inc X Y : Inc (Sing X) Y <-> In X Y.
Proof.
  split.
  - intro h. exact (Inc_IN _ _ _ h (IN_Sing X)).
  - apply Sing_Inc_IN.
Qed.


Lemma rng_natId : rng natId = Omega.
eapply set_ext; rewrite Inc_def.
- intros x xIn. 
  apply rng_elim in xIn. destruct xIn as [y xIn].
  apply IN_natId_EXType in xIn.
  destruct xIn as [n EQ].
  apply Couple_inj_right in EQ. rewrite EQ.
  eapply natZ_mem_omega.
- intros x xIn.
  apply In_omega in xIn.
  destruct xIn as [n <-].
  eapply rng_intro.
  eapply pair_self_mem_natId.
Qed.

Lemma Sing_nat_Inc_Omega (n : nat): 
  Inc (Sing (natZ n)) (rng natId).
Proof.
  eapply Sing_Inc_IN.
  rewrite rng_natId.
  eapply natZ_mem_omega. 
Qed.


(* ----- the universe [Big] and the Ens-lifted smallness lemmas ----- *)



(** ** Inaccessible cardinals: the "Big" set, lifted from [ZFC.Hierarchy].

    [ZFC.Hierarchy] takes a step towards inaccessible cardinals by carving
    out the *small* sets [Ens'] — those built below one [Type] universe —
    and collecting them into a single [Ens]-level set [Big := sup Ens' inj].
    [Big] behaves like a Grothendieck universe: it is transitive and closed
    under power sets, so it models an (uncountable, strongly) inaccessible
    stage of the cumulative hierarchy.

    Here we lift that material across the quotient to [ZFSet].  The witness
    type [Ens'] and its injection [inj : Ens' -> Ens] live *below* the
    quotient, so a small set appears on [ZFSet] as [mk (inj E')]; the
    lemmas characterise membership in [Big] in those terms. *)

Definition Big : ZFSet := mk Hierarchy.Big.

(** Every small set is a member of [Big]. *)
Lemma Big_is_big_Z : forall E' : Ens', In (mk (inj E')) Big.
Proof.
  intro E'. unfold Big, mk. rewrite In_mk. apply Big_is_big.
Qed.

(** Conversely, every member of [Big] is (the image of) a small set. *)
Lemma IN_Big_small_Z :
  forall X : ZFSet, In X Big -> exists E' : Ens', X = mk (inj E').
Proof.
  intro X. pattern X; apply quot_ind. intros eX.
  unfold Big, mk. rewrite In_mk. intro H.
  destruct (IN_Big_small eX H) as [E' HE'].
  exists E'. apply mk_eq. exact HE'.
Qed.

(** Members of a small set are themselves small — the key downward-closure
    property behind transitivity. *)
Lemma IN_small_small_Z :
  forall (X : ZFSet) (E' : Ens'),
  In X (mk (inj E')) -> exists E1 : Ens', X = mk (inj E1).
Proof.
  intros X E'. pattern X; apply quot_ind. intros eX.
  unfold mk. rewrite In_mk. intro H.
  destruct (IN_small_small eX E' H) as [E1 HE1].
  exists E1. apply mk_eq. exact HE1.
Qed.

(** [Big] is transitive: a member of a member of [Big] is again in [Big]. *)
Lemma Big_transitive :
  forall X Y : ZFSet, In X Y -> In Y Big -> In X Big.
Proof.
  intros X Y HXY HYB.
  destruct (IN_Big_small_Z Y HYB) as [F' HF']. subst Y.
  destruct (IN_small_small_Z X F' HXY) as [E1 HE1]. subst X.
  apply Big_is_big_Z.
Qed.

(** [Big] is closed under power sets — the strong-inaccessibility clause. *)
Lemma In_Power_Big :
  forall X : ZFSet, In X Big -> In (Power X) Big.
Proof.
  intro X. pattern X; apply quot_ind. intros eX. fold (mk eX).
  unfold Big. rewrite Power_mk. unfold mk. rewrite !In_mk.
  apply Big_closed_for_power.
Qed.

(** *** Smallness toolkit.

    [Big] is closed under the basic set-formers (it behaves like a
    Grothendieck universe), so concrete sets — in particular the von
    Neumann naturals — are members of the universe.  Each lemma lifts the
    corresponding [inj]-soundness/closure theorem of [ZFC.Hierarchy]
    across the quotient. *)

Lemma Empty_small : In Empty Big.
Proof.
  unfold Empty, Big. rewrite In_mk. apply Big_contains_Vide.
Qed.

Lemma Sing_small : forall X : ZFSet, In X Big -> In (Sing X) Big.
Proof.
  intro X. pattern X; apply quot_ind. intros eX. fold (mk eX).
  unfold Big. rewrite Sing_mk. unfold mk. rewrite !In_mk.
  apply Big_closed_Sing.
Qed.

Lemma Paire_small :
  forall X Y : ZFSet, In X Big -> In Y Big -> In (Paire X Y) Big.
Proof.
  intros X Y. pattern X; apply quot_ind. intros eX.
  pattern Y; apply quot_ind. intros eY. fold (mk eX). fold (mk eY).
  unfold Big. rewrite Paire_mk. unfold mk. rewrite !In_mk.
  apply Big_closed_Paire.
Qed.

Lemma Union_small : forall X : ZFSet, In X Big -> In (Union X) Big.
Proof.
  intro X. pattern X; apply quot_ind. intros eX. fold (mk eX).
  unfold Big. rewrite Union_mk. unfold mk. rewrite !In_mk.
  apply Big_closed_Union.
Qed.

(** The von Neumann successor [n ∪ {n}], expressed with the [ZFSet]
    set-formers. *)
Lemma natZ_succ_eq :
  forall k : nat, natZ (S k) = Union (Paire (natZ k) (Sing (natZ k))).
Proof.
  intro k. unfold natZ, EnsDefs.natZ.
  change (Nat (S k)) with (Class_succ (Nat k)). unfold Class_succ.
  rewrite <- Union_mk. rewrite <- Paire_mk. rewrite <- Sing_mk. reflexivity.
Qed.

(** Hence every natural is small (a member of the universe [Big]). *)
Lemma natZ_small : forall k : nat, In (natZ k) Big.
Proof.
  induction k.
  - rewrite natZ_zero_eq_empty. apply Empty_small.
  - rewrite natZ_succ_eq. apply Union_small, Paire_small.
    + exact IHk.
    + apply Sing_small. exact IHk.
Qed.

(** [ω] itself is small: it is the image of the small type [nat], so it
    is a member of the universe [Big] (the closure-under-[ω] clause of
    inaccessibility). *)
Lemma Omega_small : In Omega Big.
Proof.
  unfold Omega, Big. rewrite In_mk. apply Big_contains_Omega.
Qed.

