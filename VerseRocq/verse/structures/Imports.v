Require Export ssreflect.
From Stdlib Require Export Program.Equality.
From Stdlib Require Export Classes.RelationClasses.
From Stdlib Require Export Relations.Relation_Definitions.
From Stdlib Require Export Classes.Morphisms.
From Stdlib Require Import Classes.EquivDec.

(* playing with fire. *)
From Stdlib Require Export Sets.Classical_sets.
From Stdlib Require Export Logic.PropExtensionality.
From Stdlib Require Export Logic.FunctionalExtensionality.



Ltac inv H := inversion H; try subst; clear H.

Ltac crunch :=
  repeat match goal with
          | [ H : exists X, _ |- _ ] => destruct H
          | [ H : _ /\ _ |- _ ] => destruct H
          | [ H : _ \/ _ |- _ ] => destruct H
          | [ |- _ /\ _ ] => split
          end.
