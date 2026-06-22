(** * domains.decision: structure for decidability predicates *)
From Stdlib Require Import ssreflect Morphisms Relations RelationClasses.
From smpl Require Export Smpl.
From HB Require Import structures.
Require Import tactics basics.

Open Scope general_if_scope.

(** ** Decidable predicates *)
(** Inspired from stdpp's *)

Class Decision (P : Prop) := decide : {P} + {~P}.
#[global]Hint Mode Decision ! : typeclass_instances.
#[global]Arguments decide _ {_} : simpl never, assert.

Lemma dec_Some {A P} {Hdec : forall x, Decision (P x)} (a x : A) :
  ((if Hdec a then (Some a) else None) = Some x) <-> (x = a) /\ P a.
Proof.
  split.
  all: destruct (Hdec a) ; intuition (eauto ; congruence).
Qed.

(** ** Decidable equality *)

HB.mixin Record HasEqDec (T:Type) := {#[canonical=no]eqdec : forall x y:T, Decision (x = y)}.

#[short(type="EqTy"),primitive]
HB.structure Definition eqTy := {T of HasEqDec T}.

(** This is better than an instance [EqTyDec (A : EqTy) x y : Decision (x = y :> A)] because
  it will also fire if the carrier type is richer than [EqTy], in which case [apply:] will
  trigger canonical resolution *)
Hint Extern 100 (Decision (_ = _)) => (apply: eqdec) : typeclass_instances. 

(** ** Decidable properties *)

Lemma dec_stable P `{Decision P} : ~~P -> P.
Proof. firstorder. Qed.

Lemma decide_True {A P} `{Decision P} (x y : A) :
  P -> (if decide P then x else y) = x.
Proof. destruct (decide P); tauto. Qed.
Lemma decide_False {A P} `{Decision P} (x y : A) :
  ~P -> (if decide P then x else y) = y.
Proof. destruct (decide P); tauto. Qed.
Lemma decide_ext {A} P Q `{Decision P, Decision Q} (x y : A) :
  (P <-> Q) -> (if decide P then x else y) = (if decide Q then x else y).
Proof. intros [??]. destruct (decide P), (decide Q); tauto. Qed.

Lemma decide_True_pi {P} `{Decision P, !ProofIrrel P} (HP : P) : decide P = left HP.
Proof. destruct (decide P); [|contradiction]. f_equal. apply proof_irrel. Qed.
Lemma decide_False_pi {P} `{Decision P, !ProofIrrel (~P)} (HP : ~P) : decide P = right HP.
Proof. destruct (decide P); [contradiction|]. f_equal. apply proof_irrel. Qed.

(** ** Decidable logic *)

#[global]Instance True_dec: Decision True | 1000 := left I.
#[global]Instance False_dec: Decision False | 1000 := right (False_rect False).

Section prop_dec.
  Context {P Q} `(P_dec : Decision P) `(Q_dec : Decision Q).

  #[global]Instance not_dec: Decision (~P).
  Proof. refine (if P_dec then right _ else left _); intuition. Defined.
  #[global]Instance and_dec: Decision (P /\ Q).
  Proof. refine (if P_dec then (if Q_dec then left _ else right _) else right _); intuition. Defined.
  #[global]Instance or_dec: Decision (P \/ Q).
  Proof. refine (if P_dec then left _ else (if Q_dec then left _ else right _)); intuition. Defined.
  #[global]Instance impl_dec: Decision (P -> Q).
  Proof. refine (if P_dec then (if Q_dec then left _ else right _) else left _); intuition. Defined.
End prop_dec.
#[global]Instance iff_dec {P Q} `(P_dec : Decision P) `(Q_dec : Decision Q) :
  Decision (P <-> Q) := and_dec _ _.