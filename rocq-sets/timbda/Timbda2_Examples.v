From Stdlib Require Import ssreflect.
From Stdlib Require Import ClassicalEpsilon.

Require Import ZFSet.
Require Import ZFNotation.
Require Import Diagonal.
Require Import Syntax.
Require Import Timbda2.

(** Examples. ***)

(* [Econ]: a singleton triple. *)
Example example_con :
  eval (Econ 7) env_empty = {| ⟨ env_empty , natZ 7 , natZ 7 ⟩ |}.
Proof. reflexivity. Qed.

(* [natZAdd] computes correctly on encoded naturals. *)
Example example_natZAdd_1_2 : natZAdd (natZ 1) (natZ 2) = natZ 3.
Proof. apply natZAdd_natZ. Qed.

(* [Efail]: empty set of triples. *)
Example example_fail :
  forall env : Env, eval Efail env = ∅.
Proof. reflexivity. Qed.

(* [Echoice] of two [Efail]s collapses to the union of two empty
   iterations (no triples to lift on either side). *)
Example example_choice_fail :
  forall env : Env,
  eval (Echoice Efail Efail) env =
    ('⟨ _ , a , b ⟩ ← ∅ ;; {| ⟨ env , a , b ⟩ |})
    ∪ ('⟨ _ , a , b ⟩ ← ∅ ;; {| ⟨ env , a , b ⟩ |}).
Proof. reflexivity. Qed.


Ltac step3 := cbn [eval]; zf_reduce; try reflexivity.


(** * Evaluation of the [Syntax] example expressions.

    [Timbda2] is a *triple* semantics: a value is a set of triples
    ⟨env,a,b⟩.  The ground-fragment denotations (constants, addition,
    unification, choice) are recorded just below; the sequencing / binder /
    application examples ([Eseq] / [Eassign] / [Elet] / [Ebind] / [Eapp] /
    [Elam] — all interpreted here, unlike [Timbda1]) are at the *end* of
    the file, after the helpers they need.

    One subtlety governs the binder examples.  Here [Elam e1 e2] runs its
    body [e2] under [proj1 t] of each domain triple [t ∈ eval e1 env], so a
    binder only binds when the *domain extends the environment*.  [Ebind],
    [Eassign] and [Elet] do extend it (via [env_cons]), so their [Syntax]
    examples ([ex_T56], [ex_EV2], …) read as in Verse.  But the [Eimg]-
    domain [Elam] examples ([ex_int_id], [ex_succ], [ex_poly_id], …) are
    written for [Timbda0]/[Timbda1]'s substitution-style binding: their
    domains do *not* extend the environment, so under [Timbda2] the body
    reads the ambient slot 0 rather than the bound value.  The faithful
    [Timbda2] reading of a dependent function uses an environment-extending
    domain telescope — see [poly_t] / [poly_t_5_eval] / [const0_in_eval]
    above, which are the worked [Elam] (+ [Eapp]) examples. *)

(* The triple-pattern reduction [iUnion_pat3_Sing], the diagonal-triple
   distinctness lemma [Triple_snd_neq], and the [zf_reduce] normaliser are
   shared across the interpreters and live in [ZFSetFacts.v]. *)

(* ex_EV1 = Econ 2 *)
Example ev_ex_EV1 env : eval ex_EV1 env = {| ⟨ env , 2 , 2 ⟩ |}.
Proof. reflexivity. Qed.

(* ex_Arith1 = 3+4 *)
Example ev_ex_Arith1 env : eval ex_Arith1 env = {| ⟨ env , 7 , 7 ⟩ |}.
Proof. unfold ex_Arith1. step3. Qed.

(* ex_multiline = 1+2+3 *)
Example ev_ex_multiline env : eval ex_multiline env = {| ⟨ env , 6 , 6 ⟩ |}.
Proof. unfold ex_multiline. step3. Qed.

(* ex_Unif16 = (1=1) *)
Example ev_ex_Unif16 env : eval ex_Unif16 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_Unif16. step3. Qed.

(* ex_Unif = (1=2) -> ∅ *)
Example ev_ex_Unif env : eval ex_Unif env = ∅.
Proof.
  unfold ex_Unif. cbn [eval].
  apply BinInter_Sing_diff, Triple_snd_neq, natZ_neq. discriminate.
Qed.

(* ex_Cmp17 = (3=3) *)
Example ev_ex_Cmp17 env : eval ex_Cmp17 env = {| ⟨ env , 3 , 3 ⟩ |}.
Proof. unfold ex_Cmp17. step3. Qed.

(* ex_Cmp16 = (3=4) -> ∅ *)
Example ev_ex_Cmp16 env : eval ex_Cmp16 env = ∅.
Proof.
  unfold ex_Cmp16. cbn [eval].
  apply BinInter_Sing_diff, Triple_snd_neq, natZ_neq. discriminate.
Qed.

(* ex_Cmp18 = (3=2) -> ∅ *)
Example ev_ex_Cmp18 env : eval ex_Cmp18 env = ∅.
Proof.
  unfold ex_Cmp18. cbn [eval].
  apply BinInter_Sing_diff, Triple_snd_neq, natZ_neq. discriminate.
Qed.

(* ex_plus1 = (1+2 = 3) *)
Example ev_ex_plus1 env : eval ex_plus1 env = {| ⟨ env , 3 , 3 ⟩ |}.
Proof. unfold ex_plus1. step3. Qed.

(* ex_plus2 = (1+2 = 4) -> ∅ *)
Example ev_ex_plus2 env : eval ex_plus2 env = ∅.
Proof.
  unfold ex_plus2. cbn [eval].
  rewrite !iUnion_pat3_Sing !natZAdd_natZ. cbn [Nat.add].
  apply BinInter_Sing_diff, Triple_snd_neq, natZ_neq. discriminate.
Qed.

(* ex_choice_1_2 = (1 | 2) *)
Example ev_ex_choice env :
  eval ex_choice_1_2 env = {| ⟨ env , 1 , 1 ⟩ |} ∪ {| ⟨ env , 2 , 2 ⟩ |}.
Proof. unfold ex_choice_1_2. step3. Qed.

(* [Eany] / [Etype] are interpreted as the polymorphic identity values
   ⟨env, anyId, anyId⟩ / ⟨env, is_type, is_type⟩. *)
Example ev_any env : eval Eany env = {| ⟨ env , anyId , anyId ⟩ |}.
Proof. reflexivity. Qed.

Example ev_type env : eval Etype env = {| ⟨ env , is_type , is_type ⟩ |}.
Proof. reflexivity. Qed.

(** * More [Syntax] example denotations: sequencing, binders, application.

    These complete the [Syntax] test-case suite for the constructors that
    [Timbda2] interprets but that the ground-fragment section above did not
    reach.  (See the section header for why the [Eimg]-domain [Elam]
    examples are not faithful here.) *)

(** ** [Eseq] / [Eassign] / [Elet]. *)

(* ex_T24 = (2 = 2; 2): the unification succeeds, sequencing yields 2. *)
Example ev_ex_T24 env : eval ex_T24 env = {| ⟨ env , 2 , 2 ⟩ |}.
Proof. unfold ex_T24. step3. Qed.

(* ex_Pat1 = (x := 1): binds [x := 1], consing it onto the environment. *)
Example ev_ex_Pat1 env :
  eval ex_Pat1 env = {| ⟨ env_cons 1 env , 1 , 1 ⟩ |}.
Proof.
  unfold ex_Pat1. rewrite eval_assign eval_con iUnion_pat3_Sing. reflexivity.
Qed.

(* (x := 1; x): assign then read back the variable, yielding 1.  ([Elet]
   is no longer interpreted in [Timbda2] — folded into [Eseq]/[Eassign] —
   so we use the [Eseq (Eassign 0 _) (Evar 0)] form directly.) *)
Example ev_ex_EV2 env :
  eval (Eseq (Eassign 0 (Econ 1)) (Evar 0)) env = {| ⟨ env_cons 1 env , 1 , 1 ⟩ |}.
Proof.
  rewrite eval_seq eval_assign eval_con !iUnion_pat3_Sing eval_var
          env_lookup_cons_zero pfst_Couple.
  reflexivity.
Qed.

(** ** [Eapp]: applying the [nat] predicate to a numeral. *)

(* [nat[k]] returns [k] (the analogue of [eval_var1_app_con] for the
   primitive type [Enat], whose forward graph is [natId]). *)
Lemma eval_nat_app_con :
  forall (env : Env) (k : nat),
  eval (Eapp Enat (Econ k)) env = {| ⟨ env , natZ k , natZ k ⟩ |}.
Proof.
  intros env k.
  rewrite eval_app.
  change (eval Enat env) with ({| ⟨ env , natId , natId ⟩ |}).
  rewrite iUnion_pat3_Sing.
  unfold iUnion_pat.
  rewrite (iUnion_ext_mem natId _
             (fun p => ⦃ _ ∈ {| ⟨ env , psnd p , psnd p ⟩ |} | pfst p = natZ k ⦄)).
  - intros p Hp. cbv beta. rewrite eval_con iUnion_pat3_Sing. reflexivity.
  - apply natId_apply.
Qed.

(* ex_isnat = nat[5]: the nat predicate applied to 5 returns 5. *)
Example ev_ex_isnat env : eval ex_isnat env = {| ⟨ env , 5 , 5 ⟩ |}.
Proof. unfold ex_isnat. apply eval_nat_app_con. Qed.

(** ** [Ebind]: a logical variable declared at a type.

    [Ebind] extends the environment with the bound value, so [x:int; ...]
    reads as in Verse.  We show the intended Verse value is *among* the
    results (the binder ranges over the whole [nat] type; the unification
    in the body selects the witness shown). *)

(* ex_T56 = (x:int; x = 3): the witness [x = 3] is a value. *)
Example ev_ex_T56 env :
  In (⟨ env_cons 3 env , 3 , 3 ⟩) (eval ex_T56 env).
Proof.
  unfold ex_T56. rewrite eval_bind. unfold iUnion_pat3.
  apply IN_iUnion with (y := ⟨ env , 3 , 3 ⟩).
  - apply con_in_nat.
  - cbv beta.
    rewrite proj1_triple proj3_triple eval_equal eval_var eval_con
            env_lookup_cons_zero pfst_Couple BinInter_Sing_same.
    apply IN_Sing.
Qed.

(* ex_TB1 = ((x:int) = 5): the witness [x = 5] is a value. *)
Example ev_ex_TB1 env :
  In (⟨ env_cons 5 env , 5 , 5 ⟩) (eval ex_TB1 env).
Proof.
  unfold ex_TB1. rewrite eval_bind. unfold iUnion_pat3.
  apply IN_iUnion with (y := ⟨ env , 5 , 5 ⟩).
  - apply con_in_nat.
  - cbv beta.
    rewrite proj1_triple proj3_triple eval_equal eval_var eval_con
            env_lookup_cons_zero pfst_Couple BinInter_Sing_same.
    apply IN_Sing.
Qed.

(** ** The EV30–EV50 family: every [=] / [;] / [+] combination of the
       ground subexpressions [(1=1)], [(2;1)], [(+1)], each denoting [1]. *)

Ltac stepEV := cbv [e_eq11 e_seq21 e_plus1]; step3.

Example ev_ex_EV30 env : eval ex_EV30 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV30. stepEV. Qed.
Example ev_ex_EV31 env : eval ex_EV31 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV31. stepEV. Qed.
Example ev_ex_EV32 env : eval ex_EV32 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV32. stepEV. Qed.
Example ev_ex_EV33 env : eval ex_EV33 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV33. stepEV. Qed.
Example ev_ex_EV34 env : eval ex_EV34 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV34. stepEV. Qed.
Example ev_ex_EV35 env : eval ex_EV35 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV35. stepEV. Qed.
Example ev_ex_EV36 env : eval ex_EV36 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV36. stepEV. Qed.
Example ev_ex_EV37 env : eval ex_EV37 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV37. stepEV. Qed.
Example ev_ex_EV38 env : eval ex_EV38 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV38. stepEV. Qed.
Example ev_ex_EV39 env : eval ex_EV39 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV39. stepEV. Qed.
Example ev_ex_EV40 env : eval ex_EV40 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV40. stepEV. Qed.
Example ev_ex_EV41 env : eval ex_EV41 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV41. stepEV. Qed.
Example ev_ex_EV42 env : eval ex_EV42 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV42. stepEV. Qed.
Example ev_ex_EV43 env : eval ex_EV43 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV43. stepEV. Qed.
Example ev_ex_EV44 env : eval ex_EV44 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV44. stepEV. Qed.
Example ev_ex_EV45 env : eval ex_EV45 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV45. stepEV. Qed.
Example ev_ex_EV46 env : eval ex_EV46 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV46. stepEV. Qed.
Example ev_ex_EV47 env : eval ex_EV47 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV47. stepEV. Qed.
Example ev_ex_EV48 env : eval ex_EV48 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV48. stepEV. Qed.
Example ev_ex_EV49 env : eval ex_EV49 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV49. stepEV. Qed.
Example ev_ex_EV50 env : eval ex_EV50 env = {| ⟨ env , 1 , 1 ⟩ |}.
Proof. unfold ex_EV50. stepEV. Qed.
