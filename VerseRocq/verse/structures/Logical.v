From Stdlib Require Import Decidable DecidableTypeEx.

Module Facts.

    (** ** Lemmas and Tactics About Decidable Propositions *)

    (** ** Propositional Equivalences Involving Negation
        These are all written with the unfolded form of
        negation, since I am not sure if setoid rewriting will
        always perform conversion. *)

    (** ** Tactics for Negations *)

    Tactic Notation "fold" "any" "not" :=
      repeat (
        match goal with
        | H: context [?P -> False] |- _ =>
          fold (~ P) in H
        | |- context [?P -> False] =>
          fold (~ P)
        end).

    (** [push not using db] will pushes all negations to the
        leaves of propositions in the goal, using the lemmas in
        [db] to assist in checking the decidability of the
        propositions involved.  If [using db] is omitted, then
        [core] will be used.  Additional versions are provided
        to manipulate the hypotheses or the hypotheses and goal
        together.

        XXX: This tactic and the similar subsequent ones should
        have been defined using [autorewrite]. However, dealing
        with multiples rewrite sites and side-conditions is
        done more cleverly with the following explicit
        analysis of goals. *)

    Ltac or_not_l_iff P Q tac :=
      (rewrite (or_not_l_iff_1 P Q) by tac) ||
      (rewrite (or_not_l_iff_2 P Q) by tac).

    Ltac or_not_r_iff P Q tac :=
      (rewrite (or_not_r_iff_1 P Q) by tac) ||
      (rewrite (or_not_r_iff_2 P Q) by tac).

    Ltac or_not_l_iff_in P Q H tac :=
      (rewrite (or_not_l_iff_1 P Q) in H by tac) ||
      (rewrite (or_not_l_iff_2 P Q) in H by tac).

    Ltac or_not_r_iff_in P Q H tac :=
      (rewrite (or_not_r_iff_1 P Q) in H by tac) ||
      (rewrite (or_not_r_iff_2 P Q) in H by tac).

    Tactic Notation "push" "not" "using" ident(db) :=
      let dec := solve_decidable using db in
      unfold not, iff;
      repeat (
        match goal with
        | |- context [True -> False] => rewrite not_true_iff
        | |- context [False -> False] => rewrite not_false_iff
        | |- context [(?P -> False) -> False] => rewrite (not_not_iff P) by dec
        | |- context [(?P -> False) -> (?Q -> False)] =>
            rewrite (contrapositive P Q) by dec
        | |- context [(?P -> False) \/ ?Q] => or_not_l_iff P Q dec
        | |- context [?P \/ (?Q -> False)] => or_not_r_iff P Q dec
        | |- context [(?P -> False) -> ?Q] => rewrite (imp_not_l P Q) by dec
        | |- context [?P \/ ?Q -> False] => rewrite (not_or_iff P Q)
        | |- context [?P /\ ?Q -> False] => rewrite (not_and_iff P Q)
        | |- context [(?P -> ?Q) -> False] => rewrite (not_imp_iff P Q) by dec
        end);
      fold any not.

    Tactic Notation "push" "not" :=
      push not using core.

    Tactic Notation
      "push" "not" "in" "*" "|-" "using" ident(db) :=
      let dec := solve_decidable using db in
      unfold not, iff in * |-;
      repeat (
        match goal with
        | H: context [True -> False] |- _ => rewrite not_true_iff in H
        | H: context [False -> False] |- _ => rewrite not_false_iff in H
        | H: context [(?P -> False) -> False] |- _ =>
          rewrite (not_not_iff P) in H by dec
        | H: context [(?P -> False) -> (?Q -> False)] |- _ =>
          rewrite (contrapositive P Q) in H by dec
        | H: context [(?P -> False) \/ ?Q] |- _ => or_not_l_iff_in P Q H dec
        | H: context [?P \/ (?Q -> False)] |- _ => or_not_r_iff_in P Q H dec
        | H: context [(?P -> False) -> ?Q] |- _ =>
          rewrite (imp_not_l P Q) in H by dec
        | H: context [?P \/ ?Q -> False] |- _ => rewrite (not_or_iff P Q) in H
        | H: context [?P /\ ?Q -> False] |- _ => rewrite (not_and_iff P Q) in H
        | H: context [(?P -> ?Q) -> False] |- _ =>
          rewrite (not_imp_iff P Q) in H by dec
        end);
      fold any not.

    Tactic Notation "push" "not" "in" "*" "|-"  :=
      push not in * |- using core.

    Tactic Notation "push" "not" "in" "*" "using" ident(db) :=
      push not using db; push not in * |- using db.
    Tactic Notation "push" "not" "in" "*" :=
      push not in * using core.

    (** A simple test case to see how this works.  *)
    Lemma test_push : forall P Q R : Prop,
      decidable P ->
      decidable Q ->
      (~ True) ->
      (~ False) ->
      (~ ~ P) ->
      (~ (P /\ Q) -> ~ R) ->
      ((P /\ Q) \/ ~ R) ->
      (~ (P /\ Q) \/ R) ->
      (R \/ ~ (P /\ Q)) ->
      (~ R \/ (P /\ Q)) ->
      (~ P -> R) ->
      (~ ((R -> P) \/ (Q -> R))) ->
      (~ (P /\ R)) ->
      (~ (P -> R)) ->
      True.
    Proof.
      intros. push not in *.
       (* note that ~(R->P) remains (since R isnt decidable) *)
      tauto.
    Qed.

    (** [pull not using db] will pull as many negations as
        possible toward the top of the propositions in the goal,
        using the lemmas in [db] to assist in checking the
        decidability of the propositions involved.  If [using
        db] is omitted, then [core] will be used.  Additional
        versions are provided to manipulate the hypotheses or
        the hypotheses and goal together. *)

    Tactic Notation "pull" "not" "using" ident(db) :=
      let dec := solve_decidable using db in
      unfold not, iff;
      repeat (
        match goal with
        | |- context [True -> False] => rewrite not_true_iff
        | |- context [False -> False] => rewrite not_false_iff
        | |- context [(?P -> False) -> False] => rewrite (not_not_iff P) by dec
        | |- context [(?P -> False) -> (?Q -> False)] =>
          rewrite (contrapositive P Q) by dec
        | |- context [(?P -> False) \/ ?Q] => or_not_l_iff P Q dec
        | |- context [?P \/ (?Q -> False)] => or_not_r_iff P Q dec
        | |- context [(?P -> False) -> ?Q] => rewrite (imp_not_l P Q) by dec
        | |- context [(?P -> False) /\ (?Q -> False)] =>
          rewrite <- (not_or_iff P Q)
        | |- context [?P -> ?Q -> False] => rewrite <- (not_and_iff P Q)
        | |- context [?P /\ (?Q -> False)] => rewrite <- (not_imp_iff P Q) by dec
        | |- context [(?Q -> False) /\ ?P] =>
          rewrite <- (not_imp_rev_iff P Q) by dec
        end);
      fold any not.

    Tactic Notation "pull" "not" :=
      pull not using core.

    Tactic Notation
      "pull" "not" "in" "*" "|-" "using" ident(db) :=
      let dec := solve_decidable using db in
      unfold not, iff in * |-;
      repeat (
        match goal with
        | H: context [True -> False] |- _ => rewrite not_true_iff in H
        | H: context [False -> False] |- _ => rewrite not_false_iff in H
        | H: context [(?P -> False) -> False] |- _ =>
          rewrite (not_not_iff P) in H by dec
        | H: context [(?P -> False) -> (?Q -> False)] |- _ =>
          rewrite (contrapositive P Q) in H by dec
        | H: context [(?P -> False) \/ ?Q] |- _ => or_not_l_iff_in P Q H dec
        | H: context [?P \/ (?Q -> False)] |- _ => or_not_r_iff_in P Q H dec
        | H: context [(?P -> False) -> ?Q] |- _ =>
          rewrite (imp_not_l P Q) in H by dec
        | H: context [(?P -> False) /\ (?Q -> False)] |- _ =>
          rewrite <- (not_or_iff P Q) in H
        | H: context [?P -> ?Q -> False] |- _ =>
          rewrite <- (not_and_iff P Q) in H
        | H: context [?P /\ (?Q -> False)] |- _ =>
          rewrite <- (not_imp_iff P Q) in H by dec
        | H: context [(?Q -> False) /\ ?P] |- _ =>
          rewrite <- (not_imp_rev_iff P Q) in H by dec
        end);
      fold any not.

    Tactic Notation "pull" "not" "in" "*" "|-"  :=
      pull not in * |- using core.

    Tactic Notation "pull" "not" "in" "*" "using" ident(db) :=
      pull not using db; pull not in * |- using db.
    Tactic Notation "pull" "not" "in" "*" :=
      pull not in * using core.

    (** A simple test case to see how this works.  *)
    Lemma test_pull : forall P Q R : Prop,
      decidable P ->
      decidable Q ->
      (~ True) ->
      (~ False) ->
      (~ ~ P) ->
      (~ (P /\ Q) -> ~ R) ->
      ((P /\ Q) \/ ~ R) ->
      (~ (P /\ Q) \/ R) ->
      (R \/ ~ (P /\ Q)) ->
      (~ R \/ (P /\ Q)) ->
      (~ P -> R) ->
      (~ (R -> P) /\ ~ (Q -> R)) ->
      (~ P \/ ~ R) ->
      (P /\ ~ R) ->
      (~ R /\ P) ->
      True.
    Proof.
      intros. pull not in *. tauto.
    Qed.

End Facts.
