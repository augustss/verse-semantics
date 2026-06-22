
Require Import Sets.
Require Import Axioms.
Require Import Cartesian.
Require Import Omega.
Require Import EnsNotation.


(* Hilbert's epsilon, used (just as in Mathlib's natZAdd, which uses
   Classical.choose) to recover a Coq nat from a ZF-encoded natural.
   PropExtensionality + FunctionalExtensionality are the companion
   axioms that make [epsilon] respect predicate equivalence, which we
   need to prove [natOfEns] sound under [≃]. *)
From Stdlib Require Import Logic.Epsilon Arith.PeanoNat.
From Stdlib Require Import PropExtensionality FunctionalExtensionality.


From Stdlib Require Import Setoid Morphisms.

Require Import EnsLib.


(* If k1 < k2 then Nat k1 ∈ Nat k2 (Nat k2 is the von Neumann ordinal). *)
Lemma IN_Nat_lt :
  forall k1 k2 : nat, (k1 < k2)%nat -> IN (Nat k1) (Nat k2).
Proof.
  intros k1 k2 H. induction H.
  - apply IN_Class_succ.
  - apply INC_Class_succ; assumption.
Qed.

(* Injectivity of the von Neumann encoding. *)
Lemma Nat_inj :
  forall k1 k2 : nat, EQ (Nat k1) (Nat k2) -> k1 = k2.
Proof.
  intros k1 k2 H.
  destruct (Nat.lt_trichotomy k1 k2) as [Hlt | [Heq | Hgt]].
  - assert (Hf : F).
    { apply (E_not_IN_E (Nat k2)).
      apply IN_sound_left with (Nat k1); auto.
      apply IN_Nat_lt; assumption. }
    case Hf.
  - assumption.
  - assert (Hf : F).
    { apply (E_not_IN_E (Nat k1)).
      apply IN_sound_left with (Nat k2).
      apply EQ_sym; assumption.
      apply IN_Nat_lt; assumption. }
    case Hf.
Qed.

(* Decoder. For E = Nat k it returns k; for any other E the result is
   unspecified (by Hilbert choice), but we never rely on its value
   there. *)
Definition natOfEns (E : Ens) : nat :=
  epsilon (inhabits 0%nat) (fun k => EQ E (Nat k)).

Lemma natOfEns_Nat : forall k : nat, natOfEns (Nat k) = k.
Proof.
  intro k.
  symmetry. apply Nat_inj.
  apply (epsilon_spec (inhabits 0%nat) (fun k' => EQ (Nat k) (Nat k'))).
  exists k. apply EQ_refl.
Qed.

Definition natZAdd (a b : Ens) : Ens := Nat (natOfEns a + natOfEns b).

Lemma natZAdd_natZ :
  forall k1 k2 : nat,
  EQ (natZAdd (Nat k1) (Nat k2)) (Nat (k1 + k2)).
Proof.
  intros k1 k2. unfold natZAdd.
  rewrite (natOfEns_Nat k1), (natOfEns_Nat k2).
  apply EQ_refl.
Qed.

(* The predicate [fun k => EQ a (Nat k)] depends on [a] only through [≃].
   Combining propositional extensionality (to turn [<->] into [=]) and
   functional extensionality (to turn pointwise [=] into function [=]),
   EQ-equivalent inputs yield syntactically equal predicates, so the
   epsilon witnesses coincide. *)

Lemma natOfEns_sound :
  forall a a' : Ens, a ≃ a' -> natOfEns a = natOfEns a'.
Proof.
  intros a a' Haa'. unfold natOfEns. f_equal.
  apply functional_extensionality; intro k.
  apply propositional_extensionality; split; intro H.
  - apply EQ_tran with a; [apply EQ_sym; exact Haa' | exact H].
  - apply EQ_tran with a'; [exact Haa' | exact H].
Qed.

Lemma natZAdd_sound :
  forall a a' b b' : Ens,
  a ≃ a' -> b ≃ b' -> natZAdd a b ≃ natZAdd a' b'.
Proof.
  intros a a' b b' Ha Hb. unfold natZAdd.
  rewrite (natOfEns_sound _ _ Ha), (natOfEns_sound _ _ Hb).
  apply EQ_refl.
Qed.

#[export] Instance natZAdd_Proper : Proper (EQ ==> EQ ==> EQ) natZAdd.
Proof. intros a a' Ha b b' Hb; apply natZAdd_sound; assumption. Qed.

(* Successor on naturals: [natZSucc (Nat k) ≃ Nat (S k)]. *)
Definition natZSucc (a : Ens) : Ens := Nat (S (natOfEns a)).

Lemma natZSucc_natZ : forall k : nat, EQ (natZSucc (Nat k)) (Nat (S k)).
Proof. intro k. unfold natZSucc. rewrite (natOfEns_Nat k). apply EQ_refl. Qed.

Lemma natZSucc_sound : forall a a' : Ens, a ≃ a' -> natZSucc a ≃ natZSucc a'.
Proof. intros a a' Ha. unfold natZSucc. rewrite (natOfEns_sound _ _ Ha). apply EQ_refl. Qed.

#[export] Instance natZSucc_Proper : Proper (EQ ==> EQ) natZSucc.
Proof. intros a a' Ha; apply natZSucc_sound; assumption. Qed.

(* Predecessor on naturals (truncated: [natZPred (Nat 0) ≃ Nat 0]). *)
Definition natZPred (a : Ens) : Ens := Nat (pred (natOfEns a)).

Lemma natZPred_natZ : forall k : nat, EQ (natZPred (Nat k)) (Nat (pred k)).
Proof. intro k. unfold natZPred. rewrite (natOfEns_Nat k). apply EQ_refl. Qed.

Lemma natZPred_sound : forall a a' : Ens, a ≃ a' -> natZPred a ≃ natZPred a'.
Proof. intros a a' Ha. unfold natZPred. rewrite (natOfEns_sound _ _ Ha). apply EQ_refl. Qed.

#[export] Instance natZPred_Proper : Proper (EQ ==> EQ) natZPred.
Proof. intros a a' Ha; apply natZPred_sound; assumption. Qed.

Theorem natZ_mem_natSet : forall n : nat, natZ n ∈ ω.
Proof. intro n; unfold natZ; simpl; exists n; apply EQ_refl. Qed.

Theorem natZ_zero_eq_empty : natZ 0 ≃ ∅.
Proof. apply EQ_refl. Qed.

Lemma pair_self_mem_natId :
  forall n : nat, ⟨ natZ n , natZ n ⟩ ∈ natId.
Proof.
  intro n; unfold natId.
  apply IN_iUnion with (y := natZ n).
  - intros u v Huv. apply Sing_sound.
    apply EQ_tran with (⟨ u , v ⟩).
    + apply Couple_sound_right; exact Huv.
    + apply Couple_sound_left; exact Huv.
  - apply natZ_mem_natSet.
  - apply IN_Sing.
Qed.
