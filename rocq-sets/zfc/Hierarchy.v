(* This program is free software; you can redistribute it and/or      *)
(* modify it under the terms of the GNU Lesser General Public License *)
(* as published by the Free Software Foundation; either version 2.1   *)
(* of the License, or (at your option) any later version.             *)
(*                                                                    *)
(* This program is distributed in the hope that it will be useful,    *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(* GNU General Public License for more details.                       *)
(*                                                                    *)
(* You should have received a copy of the GNU Lesser General Public   *)
(* License along with this program; if not, write to the Free         *)
(* Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA *)
(* 02110-1301 USA                                                     *)


(* Contribution to the Coq Library   V6.3 (July 1999)                    *)

(* This is a step towards inaccessible cardinals.                       *)
(* We define the "small" sets and collect them into a single set [Big]. *)
(*                                                                       *)
(* The original development duplicated the core definitions here         *)
(* ([Ens'], [EXType'], [prod_t'], [depprod''], [EQ'], [Power']) because  *)
(* Coq did not support universe polymorphism.  It now does: [Ens], [EQ], *)
(* [Power] (and [depprod]) in [Sets.v]/[Axioms.v] are universe           *)
(* polymorphic, so the small sets are simply those same definitions      *)
(* instantiated at a strictly lower universe — no duplication required.  *)

Require Import Sets.
Require Import Axioms.
Require Import Omega.


(* Two universe levels.  The [small] sets sit strictly below the [big]   *)
(* universe in which [Big], the set of all small sets, lives.  [small <   *)
(* big] is exactly what makes [Big] "inaccessible" from below: it is not  *)
(* itself a small set.                                                    *)
Universe small big.
Constraint small < big.


(* The small sets: [Ens] at the lower universe (formerly the duplicated  *)
(* inductive [Ens']).                                                     *)
Definition Ens' : Type := Ens@{small}.


(* Small sets inject into big sets, rebuilding each [sup] node at the big *)
(* universe.  (With [small < big] the index type [A : Type@{small}] is    *)
(* accepted by [sup] at the big universe through ordinary cumulativity.)  *)
Fixpoint inj (e : Ens') : Ens@{big} :=
  match e with
  | sup A f => sup A (fun a => inj (f a))
  end.

Theorem inj_sound : forall E1 E2 : Ens', EQ E1 E2 -> EQ (inj E1) (inj E2).
simple induction E1; intros A1 f1 fr1; simple induction E2; intros A2 f2 r2;
 simpl in |- *.
simple induction 1; intros HR1 HR2; split.
intros a1; elim (HR1 a1); intros a2 Ha2; exists a2; auto with zfc.
intros a2; elim (HR2 a2); intros a1 Ha1; exists a1; auto with zfc.
Qed.


(* The injection commutes with power sets.  Here the left [Power] is at   *)
(* the small universe and the right one at the big universe — the same    *)
(* polymorphic [Power], replacing the former [Power'].                     *)
Theorem Power_sound_inj :
 forall E : Ens', EQ (inj (Power E)) (Power (inj E)).
simple induction E; intros A f HR.
simpl in |- *; split.
intros P; exists P; split.
intros c; elim c; intros a p.
exists (dep_i A (fun a0 : A => P a0) a p); simpl in |- *; auto with zfc.
intros c; elim c; intros a p.
exists (dep_i A (fun a0 : A => P a0) a p); simpl in |- *; auto with zfc.
intros P; exists P; split.
intros c; elim c; intros a p.
exists (dep_i A (fun a0 : A => P a0) a p); simpl in |- *; auto with zfc.
intros c; elim c; intros a p.
exists (dep_i A (fun a0 : A => P a0) a p); simpl in |- *; auto with zfc.
Qed.


(* The set of small sets                        *)

Definition Big : Ens@{big} := sup Ens' inj.

Theorem Big_is_big : forall E : Ens', IN (inj E) Big.
intros E; unfold Big in |- *; simpl in |- *; exists E; auto with zfc.
Qed.

Theorem IN_Big_small :
 forall E : Ens@{big}, IN E Big -> exists E' : Ens', EQ E (inj E').
unfold Big in |- *; simpl in |- *; simple induction 1; intros E' HE';
 exists E'; auto with zfc.
Qed.


Theorem IN_small_small :
 forall (E : Ens@{big}) (E' : Ens'),
 IN E (inj E') -> exists E1 : Ens', EQ E (inj E1).
simple induction E'; intros A' f' HR'; simpl in |- *; simple induction 1;
 intros a' e'; exists (f' a'); auto with zfc.
Qed.



Theorem Big_closed_for_power : forall E : Ens@{big}, IN E Big -> IN (Power E) Big.
unfold Big in |- *; simpl in |- *; intros E; simple induction 1;
 intros E' HE'; exists (Power E').
apply EQ_tran with (Power (inj E')).
apply Power_sound; assumption.
apply EQ_sym; apply Power_sound_inj.
Qed.


(* ------------------------------------------------------------------- *)
(* Further closure properties of [Big]: it behaves like a Grothendieck *)
(* universe.  For each set-former we exhibit a "small" witness (built   *)
(* at the [small] universe from the polymorphic primitives [sup],       *)
(* [depprod], [pi1], [pi2]) and show that [inj] commutes with the       *)
(* operation, mirroring [Power_sound_inj].                              *)
(* ------------------------------------------------------------------- *)

(* The empty set is small. *)
Definition Vide_s : Ens' := sup F (fun f : F => match f return Ens@{small} with
                                                end).

Theorem inj_Vide_s : EQ (inj Vide_s) Vide.
unfold Vide_s, Vide in |- *; simpl in |- *; split; intros x; elim x.
Qed.

Theorem Big_contains_Vide : IN Vide Big.
apply IN_sound_left with (inj Vide_s).
apply inj_Vide_s.
apply Big_is_big.
Qed.

(* Small (unordered) pairing: [bool : Set] is below every universe. *)
Definition Paire_s (A B : Ens') : Ens' :=
  sup bool (fun b : bool => if b then A else B).

Theorem inj_Paire_s :
 forall A B : Ens', EQ (inj (Paire_s A B)) (Paire (inj A) (inj B)).
intros A B; unfold Paire_s, Paire in |- *; simpl in |- *; split;
 intros b; exists b; elim b; simpl in |- *; auto with zfc.
Qed.

Theorem Big_closed_Paire :
 forall E E' : Ens@{big}, IN E Big -> IN E' Big -> IN (Paire E E') Big.
intros E E' HE HE'.
elim (IN_Big_small E HE); intros A' HA.
elim (IN_Big_small E' HE'); intros B' HB.
apply IN_sound_left with (inj (Paire_s A' B')).
apply EQ_tran with (Paire (inj A') (inj B')).
apply inj_Paire_s.
apply EQ_tran with (Paire E (inj B')).
apply Paire_sound_left; apply EQ_sym; assumption.
apply Paire_sound_right; apply EQ_sym; assumption.
apply Big_is_big.
Qed.

Theorem Big_closed_Sing : forall E : Ens@{big}, IN E Big -> IN (Sing E) Big.
intros E HE; unfold Sing in |- *; apply Big_closed_Paire; assumption.
Qed.

(* [inj] commutes with the base type and value function of each node:
   [pi1 (inj e) = pi1 e] (definitionally, once [e] is a [sup]) and
   [pi2 (inj e) b' = inj (pi2 e b)] at the matching index. *)
Lemma pi_inj_l :
 forall (e : Ens') (b : pi1 e),
 EXType (pi1 (inj e)) (fun b' => EQ (inj (pi2 e b)) (pi2 (inj e) b')).
intros e; case e; intros B g b; simpl in |- *; exists b; apply EQ_refl.
Qed.

Lemma pi_inj_r :
 forall (e : Ens') (b' : pi1 (inj e)),
 EXType (pi1 e) (fun b => EQ (inj (pi2 e b)) (pi2 (inj e) b')).
intros e; case e; intros B g b'; simpl in |- *; exists b'; apply EQ_refl.
Qed.

(* Small union: the same construction as [Union], performed at the
   [small] universe with the polymorphic [pi1]/[pi2]/[depprod]. *)
Definition Union_s (E : Ens') : Ens' :=
  match E with
  | sup A f =>
      sup (depprod A (fun x : A => pi1 (f x)))
          (fun c : depprod A (fun x : A => pi1 (f x)) =>
           match c with
           | dep_i a b => pi2 (f a) b
           end)
  end.

Theorem inj_Union_s :
 forall E : Ens', EQ (inj (Union_s E)) (Union (inj E)).
simple induction E; intros A f HR; simpl in |- *; split.
intros c1; elim c1; intros a b.
elim (pi_inj_l (f a) b); intros b' Hb'.
exists (dep_i A (fun x : A => pi1 (inj (f x))) a b'); exact Hb'.
intros c2; elim c2; intros a b'.
elim (pi_inj_r (f a) b'); intros b Hb.
exists (dep_i A (fun x : A => pi1 (f x)) a b); exact Hb.
Qed.

Theorem Big_closed_Union : forall E : Ens@{big}, IN E Big -> IN (Union E) Big.
intros E HE.
elim (IN_Big_small E HE); intros E' HE'.
apply IN_sound_left with (inj (Union_s E')).
apply EQ_tran with (Union (inj E')).
apply inj_Union_s.
apply Union_sound; apply EQ_sym; assumption.
apply Big_is_big.
Qed.


(* ------------------------------------------------------------------- *)
(* [Omega] is small.  [ω] is the image of the small type [nat] under    *)
(* the von Neumann naturals, so it is built at the [small] universe and *)
(* [inj] commutes with it.  This is the closure-under-[ω] clause of      *)
(* inaccessibility.                                                      *)
(* ------------------------------------------------------------------- *)

(* [Class_succ] respects extensional equality (its components do). *)
Theorem Class_succ_sound :
 forall A B : Ens, EQ A B -> EQ (Class_succ A) (Class_succ B).
intros A B H; unfold Class_succ in |- *; apply Union_sound.
apply EQ_tran with (Paire B (Sing A)).
apply Paire_sound_left; assumption.
apply Paire_sound_right; apply Sing_sound; assumption.
Qed.

(* Small singleton and successor, built from the small witnesses. *)
Definition Sing_s (E : Ens') : Ens' := Paire_s E E.

Theorem inj_Sing_s : forall E : Ens', EQ (inj (Sing_s E)) (Sing (inj E)).
intros E; unfold Sing_s, Sing in |- *; apply inj_Paire_s.
Qed.

Definition Class_succ_s (E : Ens') : Ens' := Union_s (Paire_s E (Sing_s E)).

Theorem inj_Class_succ_s :
 forall E : Ens', EQ (inj (Class_succ_s E)) (Class_succ (inj E)).
intros E; unfold Class_succ_s, Class_succ in |- *.
apply EQ_tran with (Union (inj (Paire_s E (Sing_s E)))).
apply inj_Union_s.
apply Union_sound.
apply EQ_tran with (Paire (inj E) (inj (Sing_s E))).
apply inj_Paire_s.
apply Paire_sound_right; apply inj_Sing_s.
Qed.

(* The small von Neumann naturals, indexed by [nat]. *)
Fixpoint Nat_s (n : nat) : Ens' :=
  match n with
  | O => Vide_s
  | S m => Class_succ_s (Nat_s m)
  end.

Theorem inj_Nat_s : forall n : nat, EQ (inj (Nat_s n)) (Nat n).
induction n.
apply inj_Vide_s.
apply EQ_tran with (Class_succ (inj (Nat_s n))).
apply inj_Class_succ_s.
apply Class_succ_sound; exact IHn.
Qed.

(* The small [ω] and the proof it injects to [Omega]. *)
Definition Omega_s : Ens' := sup nat Nat_s.

Theorem inj_Omega_s : EQ (inj Omega_s) Omega.
unfold Omega_s, Omega in |- *; simpl in |- *; split.
intros n; exists n; apply inj_Nat_s.
intros n; exists n; apply inj_Nat_s.
Qed.

Theorem Big_contains_Omega : IN Omega Big.
apply IN_sound_left with (inj Omega_s).
apply inj_Omega_s.
apply Big_is_big.
Qed.
