(* ZFNotation.v: [zf_scope] syntax for the ZFSet-level operations
   defined in [ZFSet.v].

   Mirrors the symbols in [EnsNotation.v] but targets the quotient
   [ZFSet] instead of [Ens]; equality on [ZFSet] is Coq's Leibniz
   [=], so there is no separate [≃] notation. *)

(* Depends only on [ZFSetCore] (the operations the notations denote), so
   that [ZFSetFacts] may import it and state its lemmas with this syntax
   without creating an import cycle. *)
Require Export ZFSetCore.

From Stdlib Require Import Setoid Morphisms.


Declare Scope zf_scope.
Delimit Scope zf_scope with zf.
Bind Scope zf_scope with ZFSet.
Open Scope zf_scope.

Notation "∅"   := Empty   : zf_scope.
Notation "'ω'" := Omega   : zf_scope.

Notation "x ∈ y" := (In x y)
  (at level 70, no associativity) : zf_scope.
Notation "x ⊆ y" := (Inc x y)
  (at level 70, no associativity) : zf_scope.

Notation "⋃ E"    := (Union E)
  (at level 35, right associativity) : zf_scope.
Notation "⋂ E"    := (Inter E)
  (at level 35, right associativity) : zf_scope.

Notation "{| x |}"     := (Sing x)      (at level 0) : zf_scope.
Notation "{| x ; y |}" := (Paire x y)   (at level 0) : zf_scope.
Notation "⟨ x , y ⟩"    := (Couple x y)  (at level 0) : zf_scope.

(* Triple notation: [⟨ a , b , c ⟩ = ⟨ a , ⟨ b , c ⟩ ⟩]. *)
Notation "⟨ a , b , c ⟩" :=
  (Couple a (Couple b c)) (at level 0) : zf_scope.

Notation "⦃ x ∈ E | P ⦄" := (Comp E (fun x => P))
  (at level 0, x name, E at level 90, P at level 99) : zf_scope.

Notation "x ∩ y" := (BinInter x y)
  (at level 50, left associativity) : zf_scope.
Notation "x ∪ y" := (BinUnion x y)
  (at level 50, left associativity) : zf_scope.

Notation "x ← E ;; F" := (iUnion E (fun x => F))
  (at level 61, E at next level, right associativity) : zf_scope.

(* Pattern-binding [iUnion]: when each iterated element is a (Werner)
   pair ⟨x,y⟩, ['⟨ x , y ⟩ ← E ;; F] binds [x := pfst _], [y := psnd _];
   for a triple ⟨x,y,z⟩, ['⟨ x , y , z ⟩ ← E ;; F] binds [x := proj1 _],
   [y := proj2 _], [z := proj3 _].  The leading apostrophe distinguishes
   these from the plain ordered-pair / triple notations. *)
Notation "''⟨' x , y '⟩' ← E ;; F" := (iUnion_pat E (fun x y => F))
  (at level 61, x name, y name, E at next level, right associativity)
  : zf_scope.

Notation "''⟨' x , y , z '⟩' ← E ;; F" := (iUnion_pat3 E (fun x y z => F))
  (at level 61, x name, y name, z name, E at next level, right associativity)
  : zf_scope.

Notation "'Π[' x ∈ A ] B" := (Pi A (fun x => B))
  (at level 200, x name, A at level 90, right associativity) : zf_scope.

Coercion natZ : nat >-> ZFSet.

Notation "E + F" := (natZAdd E F) : zf_scope.

Notation "E [ F ]" := (applyFun E F) : zf_scope.
