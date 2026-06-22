(** * ZFSet.v — umbrella for the quotient set theory.

    [ZFSet] is split into two layers:

      - [ZFSetCore.v] — the quotient construction and the *Ens-lifted*
        interface (definitions and lemmas proved by descending to the
        underlying [Ens] representation; the only layer that unfolds [mk]);

      - [ZFSetFacts.v] — the *pure* ZFSet theory, reasoning only through the
        core interface, plus the [zf_reduce] step-tactic infrastructure.

    Downstream code keeps [Require Import ZFSet]; this file re-exports both
    layers so nothing else needs to change. *)

Require Export ZFSetCore.
Require Export ZFSetFacts.
