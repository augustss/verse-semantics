(** * Domains.Tactics: utility tactics *)
From Ltac2 Require Import Ltac2 Control Constr Constructor Ind Notations.
From smpl Require Export Smpl.

(** ** Hints *)

(** Solves the following issue: [Hint Extern]'s patterns only match syntactically:
  contrarily to the standard [Instances] it does not try to reduce at all before
  matching. *)
Hint Extern 500 => (progress (cbn beta delta zeta iota)) : typeclass_instances.

#[global]Hint Unfold notT: core.
#[global] Hint Resolve eq_refl eq_sym : core.
#[global] Hint Constructors and : core. 
#[global]Hint Extern 10 =>
  match goal with
    | H : False |- _ => destruct H
    | H : _ /\ _ |- _ => destruct H
    | H : exists _, _ |- _ => destruct H
    | H : ~ _ |- False => apply H
  end : core.

(** To use in intro patterns, similar to SSReflects' /dup view *)
Definition dup {A : Type} : A -> A * A := fun x => (x,x).

(** ** Automation *)

Ltac tea := try eassumption.
#[global] Ltac easy ::= solve [eauto 3 with core relations].

#[global] Ltac Tauto.intuition_solver ::= auto.

(** ** Extensionality *)

(** A tactic to use extensionality of equality, extended on the fly using the Smpl plugin. *)

(** *** Testing whether a goal is of the form {| … |} = {| … |} to apply extensionality
  of records. *)

Ltac2 is_record_constr (c : constructor) : bool :=
  match get_projections (data (inductive c)) with | Some _ => true | None => false end.

Ltac2 test_constr (c : constr) : unit :=
  match (Unsafe.kind c) with
  | Unsafe.App c _ =>
    match (Unsafe.kind c) with
    | Unsafe.Constructor _ _ => ()
    | _ => zero Assertion_failure
    end
  | _ => zero Assertion_failure
  end.

Ltac2 constr_ext () : unit :=
  match! goal with
  | [ |- ?t = ?u] => test_constr t ; test_constr u ; progress f_equal
  end.

Smpl Create extensionality.

Ltac ext := intros ; repeat (smpl extensionality ; intros).

Smpl Add (ltac2:(constr_ext ())) : extensionality.