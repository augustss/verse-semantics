(* EnsNotation.v: [ens_scope] syntax for the Ens-level operations
   in [Defs.v]. Used by ZF-library files that still work directly
   with [Ens] (notably [ZFLib.v] and [ZFNat.v]).

   For ZFSet-level notations, see [Notation.v] instead. *)

Require Export Sets.
Require Export Axioms.
Require Export Cartesian.
Require Export Omega.
Require Export EnsDefs.


(** Standard set notations.

  Equality on [Ens] is extensional, not Leibniz; we use [≃] for it
  and reserve [=] for the Coq-level identity. The brackets
  [{| ... |}] form set literals (singleton and unordered pair) and
  [⟨ _ , _ ⟩] is the Werner ordered pair. *)

Declare Scope ens_scope.
Delimit Scope ens_scope with ens.
(* [Ens] is pinned to a notation ([Ens@{ulib}], see [EnsDefs.v]); refer to
   the underlying inductive by its qualified name for the scope binding. *)
Bind Scope ens_scope with Sets.Ens.
Open Scope ens_scope.

Notation "∅" := Vide : ens_scope.
Notation "'ω'" := Omega : ens_scope.
Notation "x ≃ y" := (EQ x y) (at level 70, no associativity) : ens_scope.
Notation "x ∈ y" := (IN x y) (at level 70, no associativity) : ens_scope.
Notation "x ⊆ y" := (INC x y) (at level 70, no associativity) : ens_scope.
Notation "⋃ E" := (Union E) (at level 35, right associativity) : ens_scope.
Notation "⋂ E" := (Inter E) (at level 35, right associativity) : ens_scope.
Notation "{| x |}" := (Sing x) (at level 0) : ens_scope.
Notation "{| x ; y |}" := (Paire x y) (at level 0) : ens_scope.
Notation "⟨ x , y ⟩" := (Couple x y) (at level 0) : ens_scope.

(* Triple notation: [⟨ a , b , c ⟩ = ⟨ a , ⟨ b , c ⟩ ⟩]. *)
Notation "⟨ a , b , c ⟩" :=
  (Couple a (Couple b c)) (at level 0) : ens_scope.

(* TODO: this notation is confusing when P is a membership test.
   Change to [x <- E] instead? *)
Notation "⦃ x ∈ E | P ⦄" := (Comp E (fun x => P))
  (at level 0, x name, E at level 90, P at level 99) : ens_scope.

Notation "x ∩ y" :=
  (BinInter x y) (at level 50, left associativity) : ens_scope.
Notation "x ∪ y" :=
  (BinUnion x y) (at level 50, left associativity) : ens_scope.

Notation "x ← E ;; F" := (iUnion E (fun x => F))
  (at level 61, E at next level, right associativity) : ens_scope.

Notation "'Π[' x ∈ A ] B" := (Pi A (fun x => B))
  (at level 200, x ident, A at level 90, right associativity) : ens_scope.

(* Pair-pattern do-notation: when the bound element is a Werner
   couple, ['⟨ x , y ⟩ ← E ;; F] binds [x := pfst _] and
   [y := psnd _]. The leading apostrophe distinguishes it from the
   plain ordered-pair notation, which the parser cannot tell apart
   by lookahead alone. *)
Notation "''⟨' x , y '⟩' ← E ;; F" := (iUnion_pat E (fun x y => F))
  (at level 61, x name, y name, E at next level, right associativity)
  : ens_scope.
