(** * domains.basics: basic definitions *)
From Stdlib Require Import ssreflect Morphisms Relations RelationClasses.
From smpl Require Export Smpl.
From HB Require Import structures.
Require Import tactics.

Open Scope general_if_scope.

(** ** Equalities *)

Definition transport {A : Type} (P : A -> Type) {x y : A} (p : x = y) (u : P x) : P y
  := match p with eq_refl => u end.

Arguments transport {A}%_type_scope P%_function_scope {x y} p u : simpl nomatch.

Definition ap {A B : Type} (f : A -> B) {x y : A} (p : x = y) : f x = f y
  := match p with eq_refl => eq_refl end.

Global Arguments ap {A B}%_type_scope f%_function_scope {x y} p : simpl nomatch.

(** Transport is very common so it is worth introducing a parsing notation for it.  However, we do not use the notation for output because it hides the fibration, and so makes it very hard to read involved transport expression. *)
Notation "p # u" := (transport _ p u) (at level 55, u at next level, only parsing).

Lemma transport_const {A B} {x y : A} {e : x = y} (b : B) :
  transport (fun _ => B) e b = b.
Proof.
  destruct e ; reflexivity.
Qed.

(*
(** ** Lemmas for working with [iffT] *)
Lemma arrowTE: CMorphisms.Proper (iffT ==> iffT ==> iffT) arrow.
Proof. move=>A A' e B B' f; split; move=>p a; apply f, p, e, a. Defined.

Definition allT {A} (f: A -> Type) := forall a, f a.
Definition pointwise_crelation {A B} (R: crelation B): crelation (A -> B) := fun f g => forall a, R (f a) (g a).
Lemma allTE {A}: CMorphisms.Proper (@pointwise_crelation A _ iffT ==> iffT) allT.
Proof. move=>f g fg; split; move=>Hf x; apply fg, Hf. Defined.
Lemma allTE' {A B} (f: A -> Type) (g: B -> Type):
  iffT A B -> (forall a b, iffT (f a) (g b)) -> iffT (allT f) (allT g).
Proof. move=>ab fg; split; move=>H x; unshelve eapply fg, H; apply ab, x. Qed.

Lemma sigTE {A}: CMorphisms.Proper (pointwise_crelation iffT ==> iffT) (@sigT A).
Proof. move=>f g fg; split; move=>[x H]; exists x; apply fg, H. Defined.
Lemma sigTE' {A B} (f: A -> Type) (g: B -> Type):
  iffT A B -> (forall a b, iffT (f a) (g b)) -> iffT (sigT f) (sigT g).
Proof. move=>ab fg; split; move=>[x H]; (unshelve eexists; [apply ab, x|eapply fg, H]). Qed.

Lemma iffT_hyp {A A' B}: (iffT A A') -> (A' -> B) -> A -> B.
Proof. move=>[+ _]; auto. Qed.
*)

Lemma rrefl {A} {R : relation A} `{Reflexive _ R} (x y : A) : x = y -> R x y.
Proof.
  intros ->.
  reflexivity.
Qed.

(** ** Unique existence *)

(** unique existence, in Prop *)
Definition unique [A : Type] (P : A -> Prop) : A -> Prop :=
  fun (x : A) => P x /\ (forall x' : A, P x' -> x' = x).

Notation "∃! x .. y , P" :=
  (ex (unique (fun x => .. (ex (unique (fun y => P))) ..)))
  (at level 200, x binder, right associativity).

Lemma unique_exists {A : Type} {P : A -> Prop} :
  (∃! x, P x) -> exists x, P x.
Proof.
  intros [] ; unfold unique in *.
  now eexists.
Qed.

Lemma unique_exists_unique {A : Type} {P : A -> Prop} x y :
  (∃! x, P x) ->
  P x ->
  P y ->
  x = y.
Proof.
  intros [z [? e]] ?? ; unfold unique in *.
  transitivity z.
  all: now erewrite <- e.
Qed.

Notation "Σ! x .. y , P" := (sig (unique (fun x => .. (sig (unique (fun y => P))) ..)))
  (at level 200, x binder, y binder, right associativity).

Definition unique_elt {A} {P : A -> Prop} (s : Σ! x : A, P x) : A := proj1_sig s.
Definition unique_prop {A} {P : A -> Prop} (s : Σ! x : A, P x) : P (unique_elt s) :=
  proj1 (proj2_sig s).
Definition unique_unique {A} {P : A -> Prop} (s : Σ! x : A, P x) :
  forall x : A, P x -> x = (unique_elt s) :=
  proj2 (proj2_sig s).

Lemma unique_unique_impl {T : Type} (P Q: T -> Prop) :
  (forall x, P x -> Q x) ->
  forall (p: Σ! x, (P x)), forall (q: Σ! x, (Q x)), unique_elt p = unique_elt q.
Proof.
  move=>PQ p q. apply unique_unique, PQ, unique_prop.
Qed.

(** ** H-propositions *)

Class ProofIrrel (T : Type) := proof_irrel : forall x y : T, x = y.
Global Hint Mode ProofIrrel ! : typeclass_instances.

Smpl Add 200 (apply proof_irrel) : extensionality.

(** Option *)

Definition onSome {A} (P : A -> Prop) (x : option A) : Prop :=
  match x with
  | None => False
  | Some x => P x
  end.

Arguments onSome {_}_ !_/.

Definition option_rel {A} (R : relation A) : relation (option A) :=
  fun x y => match x, y with
  | None, None => True
  | Some a, Some a' => R a a'
  | _, _ => False
  end.

Instance Equiv_option {A} (R : relation A) `{Equivalence A R} : Equivalence (option_rel R).
Proof.
  split ; red.
  all: repeat (intros []) ; cbn ; intros ; eauto.
  - reflexivity.
  - now symmetry.
  - now etransitivity.
Qed.

Instance Proper_some {A} (R : relation A) : Proper (R ==> option_rel R) Some.
Proof.
  now cbv.
Qed.

Lemma option_map_some A B (f : A -> B) (a : option A) (b : B) :
  (option_map f a = Some b) ->
  exists a', a = Some a' /\ b = f a'.
Proof.
 destruct a ; cbn.
 2: congruence.
 intros [= <-].
 now eexists.
Qed.

Definition option_bind {A B} (f : A -> option B) : option A -> option B :=
  fun x =>
  match x with
  | None => None
  | Some a => f a
  end.

(** Unit *)

Lemma unit_ext (x y : unit) : x = y.
Proof (match x, y with | tt, tt => eq_refl end).

Smpl Add (apply unit_ext) : extensionality.