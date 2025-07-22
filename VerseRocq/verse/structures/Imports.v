Require Export ssreflect.
Require Export Monad.
From Stdlib Require Export Program.Equality.
From Stdlib Require Export Classes.RelationClasses.
From Stdlib Require Export Relations.Relation_Definitions.
From Stdlib Require Export Classes.Morphisms.

Ltac inv H := inversion H; try subst; clear H.

Ltac crunch :=
  repeat match goal with
          | [ H : exists X, _ |- _ ] => destruct H
          | [ H : _ /\ _ |- _ ] => destruct H
          | [ H : _ \/ _ |- _ ] => destruct H
          | [ |- _ /\ _ ] => split
          end.
