(* Timbda2.v:

   A triple-valued bidirectional evaluator over the shared source
   language [Syntax.Expr], retargeted to the quotient set theory [ZFSet]
   (where equality is Leibniz [=]), mirroring [Timbda0]/[Timbda1].  Each
   [Expr] evaluates, in a given environment, to a [ZFSet] of triples
   ⟨env, a, b⟩.

   Unlike [Timbda0]/[Timbda1], the [env] component is itself a ZF value
   (a cons-list), because the evaluator pulls environments out of triples
   and passes them back into recursive [eval] calls.  So here [Env] is a
   [ZFSet], not the Coq function [nat -> ZFSet] of [Syntax].

   The iterations over sets of triples / pairs use the pattern-binding
   [iUnion] notations from [ZFNotation]: ['⟨ r , a , b ⟩ ← E ;; F] binds
   the three triple projections, ['⟨ u , v ⟩ ← E ;; F] the two pair
   projections.
*)

From Stdlib Require Import ssreflect.
From Stdlib Require Import ClassicalEpsilon.

Require Import ZFSet.
Require Import ZFNotation.
Require Import Diagonal.
Require Import Syntax.

(** ** Environments as ZF cons-lists.

   The evaluator pulls environments out of values and passes them back to
   recursive eval calls, so envs must be ZF values, not Coq functions.
   Standard right-nested encoding: [cons v env = ⟨v, env⟩]. ***)

Definition Env := ZFSet.

Definition env_empty : Env := ∅.

Definition env_cons (v : ZFSet) (env : Env) : Env := ⟨ v , env ⟩.

Fixpoint env_lookup (env : Env) (n : nat) : ZFSet :=
  match n with
  | O   => pfst env
  | S m => env_lookup (psnd env) m
  end.

(* The set-theoretic type machinery — [anyId] (the diagonal on [Big]), the
   partial functions [pFun], the partial identities [pIdFun], and the
   identity relation [is_type] on [pIdFun Big] — is provided by
   [lib.Diagonal].

   As a [Timbda2] function value, a relation [r] is packed into a triple
   ⟨env, r, r⟩ (forward graph and cograph both [r]); so [Eany] (resp.
   [Etype]) denotes the polymorphic identity function on [Big] (resp. on
   the partial identities), and applying it returns its argument
   unchanged.  Restricting [is_type] to partial *identity* functions (was:
   on all partial functions [pFun Big Big]) is what makes [poly_ty] behave
   like the identity on the diagonal — see [poly_ty_5_contains_5]. *)

(** **  Evaluator.

   [Etyp] (type projection [:e]) of the original development is [Eimg]
   in [Syntax]; [Ebind] ([e1 :>= e2]) was added to [Syntax] for this
   evaluator.  [Eany] / [Etype] denote the polymorphic identity function
   value ⟨env, anyId, anyId⟩ / ⟨env, is_type, is_type⟩.  The remaining
   [Syntax] constructors that this triple semantics does not interpret
   fall through to [∅]. ***)

Fixpoint eval (e : Expr) (env : Env) {struct e} : ZFSet :=
  match e with
  | Econ k =>
      {| ⟨ env , natZ k , natZ k ⟩ |}
  | Evar x =>
      {| ⟨ env , env_lookup env x , env_lookup env x ⟩ |}
  | Enat =>
      {| ⟨ env , natId , natId ⟩ |}

  (* Elam e1 e2  should be the same as Elamb (x:=e1) e2 *)
  | Elam e1 e2 =>
      h ← Π[ t ∈ eval e1 env ] eval e2 (env_cons (proj3 t) env) ;;
        let f := '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj3 ab , proj2 cd ⟩ |} in
        let g := '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj2 ab , proj3 cd ⟩ |} in
         ⦃ _ ∈ {| ⟨ env , f , g ⟩ |} | isFunction f /\ isFunction g ⦄

  | Elamb e1 e2 =>
      h ← Π[ t ∈ eval e1 env ] (eval e2 (proj1 t)) ;;
        let f := '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj3 ab , proj2 cd ⟩ |} in
        let g := '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj2 ab , proj3 cd ⟩ |} in
         ⦃ _ ∈ {| ⟨ env , f , g ⟩ |} | isFunction f /\ isFunction g ⦄

  | Eapp e1 e2 =>
      '⟨ r , _ , b ⟩  ← eval e1 env ;;
      '⟨ ka , va ⟩    ← b ;;
      '⟨ rs , sa , _ ⟩ ← eval e2 r ;;
        ⦃ _ ∈ {| ⟨ rs , va , va ⟩ |} | ka = sa ⦄
  | Ebind e1 e2 =>
      '⟨ r , _ , b ⟩ ← eval e1 env ;; eval e2 (env_cons b r)
  | Eimg e1 =>
      '⟨ r , _ , b ⟩ ← eval e1 env ;;
      '⟨ ka , va ⟩   ← b ;;
        {| ⟨ r , ka , va ⟩ |}
  | Efail => ∅
  | Echoice e1 e2 =>
      ('⟨ _ , a , b ⟩ ← eval e1 env ;; {| ⟨ env , a , b ⟩ |})
      ∪ ('⟨ _ , a , b ⟩ ← eval e2 env ;; {| ⟨ env , a , b ⟩ |})
  | Eadd e1 e2 =>
      '⟨ _ , _ , b1 ⟩ ← eval e1 env ;;
      '⟨ _ , _ , b2 ⟩ ← eval e2 env ;;
        let s := natZAdd b1 b2 in
        {| ⟨ env , s , s ⟩ |}
  | Eequal e1 e2 =>
      eval e1 env ∩ eval e2 env

  | Eany => {| ⟨ env , anyId , anyId ⟩ |}

  (* *)
  | Etype => {| ⟨ env , is_type , is_type ⟩ |}

  (* [x := e]: each result of [e] re-conses its value [b] onto its own
     environment (binding [x]); the [a]/[b] components are kept. *)
  | Eassign _ e =>
      '⟨ r , a , b ⟩ ← eval e env ;; {| ⟨ env_cons b r , a , b ⟩ |}

  (* [e1 ; e2]: run [e1] for its environments, then [e2] under each. *)
  | Eseq e1 e2 =>
      '⟨ r , _ , _ ⟩ ← eval e1 env ;; eval e2 r

  | _ => ∅
  end.

(** Equation lemmas (one per interpreted [Expr] case). ***)

Theorem eval_con :
  forall (k : nat) (env : Env),
  eval (Econ k) env = {| ⟨ env , natZ k , natZ k ⟩ |}.
Proof. reflexivity. Qed.

Theorem eval_var :
  forall (x : nat) (env : Env),
  eval (Evar x) env = {| ⟨ env , env_lookup env x , env_lookup env x ⟩ |}.
Proof. reflexivity. Qed.

Theorem eval_nat :
  forall env : Env, eval Enat env = {| ⟨ env , natId , natId ⟩ |}.
Proof. reflexivity. Qed.

Theorem eval_bind :
  forall (e1 e2 : Expr) (env : Env),
  eval (Ebind e1 e2) env =
    ('⟨ r , _ , b ⟩ ← eval e1 env ;; eval e2 (env_cons b r)).
Proof. reflexivity. Qed.

Theorem eval_img :
  forall (e : Expr) (env : Env),
  eval (Eimg e) env =
    ('⟨ r , _ , b ⟩ ← eval e env ;;
     '⟨ ka , va ⟩   ← b ;;
       {| ⟨ r , ka , va ⟩ |}).
Proof. reflexivity. Qed.

Theorem eval_fail : forall env : Env, eval Efail env = ∅.
Proof. reflexivity. Qed.

Theorem eval_choice :
  forall (e1 e2 : Expr) (env : Env),
  eval (Echoice e1 e2) env =
    ('⟨ _ , a , b ⟩ ← eval e1 env ;; {| ⟨ env , a , b ⟩ |})
    ∪ ('⟨ _ , a , b ⟩ ← eval e2 env ;; {| ⟨ env , a , b ⟩ |}).
Proof. reflexivity. Qed.

Theorem eval_equal :
  forall (e1 e2 : Expr) (env : Env),
  eval (Eequal e1 e2) env = eval e1 env ∩ eval e2 env.
Proof. reflexivity. Qed.

Theorem eval_any :
  forall env : Env, eval Eany env = {| ⟨ env , anyId , anyId ⟩ |}.
Proof. reflexivity. Qed.

Theorem eval_type :
  forall env : Env, eval Etype env = {| ⟨ env , is_type , is_type ⟩ |}.
Proof. reflexivity. Qed.

Theorem eval_app :
  forall (e1 e2 : Expr) (env : Env),
  eval (Eapp e1 e2) env =
    ('⟨ r , _ , b ⟩  ← eval e1 env ;;
     '⟨ ka , va ⟩    ← b ;;
     '⟨ rs , sa , _ ⟩ ← eval e2 r ;;
       ⦃ _ ∈ {| ⟨ rs , va , va ⟩ |} | ka = sa ⦄).
Proof. reflexivity. Qed.

Theorem eval_lam :
  forall (e1 e2 : Expr) (env : Env),
  eval (Elam e1 e2) env =
    (h ← Π[ t ∈ eval e1 env ] eval e2 (env_cons (proj3 t) env)  ;;
       let f := '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj3 ab , proj2 cd ⟩ |} in
       let g := '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj2 ab , proj3 cd ⟩ |} in
         ⦃ _ ∈ {| ⟨ env , f , g ⟩ |} | isFunction f /\ isFunction g ⦄).
Proof. reflexivity. Qed.

Theorem eval_lamb :
  forall (e1 e2 : Expr) (env : Env),
  eval (Elamb e1 e2) env =
    (h ← Π[ t ∈ eval e1 env ] eval e2 (proj1 t) ;;
       let f := '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj3 ab , proj2 cd ⟩ |} in
       let g := '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj2 ab , proj3 cd ⟩ |} in
         ⦃ _ ∈ {| ⟨ env , f , g ⟩ |} | isFunction f /\ isFunction g ⦄).
Proof. reflexivity. Qed.

Theorem eval_add :
  forall (e1 e2 : Expr) (env : Env),
  eval (Eadd e1 e2) env =
    ('⟨ _ , _ , b1 ⟩ ← eval e1 env ;;
     '⟨ _ , _ , b2 ⟩ ← eval e2 env ;;
       {| ⟨ env , natZAdd b1 b2 , natZAdd b1 b2 ⟩ |}).
Proof. reflexivity. Qed.

Theorem eval_assign :
  forall (n : nat) (e : Expr) (env : Env),
  eval (Eassign n e) env =
    ('⟨ r , a , b ⟩ ← eval e env ;; {| ⟨ env_cons b r , a , b ⟩ |}).
Proof. reflexivity. Qed.

Theorem eval_seq :
  forall (e1 e2 : Expr) (env : Env),
  eval (Eseq e1 e2) env = ('⟨ r , _ , _ ⟩ ← eval e1 env ;; eval e2 r).
Proof. reflexivity. Qed.

Theorem eval_bind_seq_assign :
  forall (e1 e2 : Expr) (env : Env),
  eval (Ebind e1 e2) env = eval (Eseq (Eassign 0 e1) e2) env.
Proof.
  intros e1 e2 env.
  rewrite eval_bind eval_seq eval_assign.
  unfold iUnion_pat3. rewrite iUnion_assoc.
  apply iUnion_ext_mem. intros t Ht.
  rewrite iUnion_Sing_l. rewrite proj1_triple. reflexivity.
Qed.


(** [x := a ; x] re-reads the just-bound variable.  The terminal [Evar 0]
    returns the value [b] that [Eassign] consed onto the environment, in
    *both* payload slots — so its triples are ⟨env', b, b⟩, whereas plain
    [x := a] keeps [a]'s original cograph component [a'] (its triples are
    ⟨env', a', b⟩).  The two therefore agree exactly when [a]'s values are
    *diagonal* ([proj2 t = proj3 t], i.e. [a'] = [b]); under that
    hypothesis [x := a ; x] = [x := a]. *)
Theorem eval_assign_seq_var0 :
  forall (n : nat) (a : Expr) (env : Env),
  (forall t, t ∈ eval a env -> proj2 t = proj3 t) ->
  eval (Eseq (Eassign n a) (Evar 0)) env = eval (Eassign n a) env.
Proof.
  intros n a env Hdiag.
  rewrite eval_seq eval_assign.
  unfold iUnion_pat3.
  rewrite iUnion_assoc.
  apply iUnion_ext_mem. intros t Ht.
  rewrite iUnion_Sing_l. cbv beta.
  rewrite proj1_triple eval_var.
  cbn [env_lookup]. unfold env_cons. rewrite pfst_Couple.
  rewrite (Hdiag t Ht). reflexivity.
Qed.

(** Environment lemmas. ***)

Theorem env_lookup_cons_zero :
  forall (v : ZFSet) (env : Env),
  env_lookup (env_cons v env) 0 = pfst ⟨ v , env ⟩.
Proof. reflexivity. Qed.

Theorem env_lookup_cons_succ :
  forall (v : ZFSet) (env : Env) (n : nat),
  env_lookup (env_cons v env) (S n) = env_lookup (psnd ⟨ v , env ⟩) n.
Proof. reflexivity. Qed.


Ltac step3 := cbn [eval]; zf_reduce; try reflexivity.


(** * Polymorphic application: [any] / [type] return their argument.

    [Eapp Eany e] reads the forward graph [anyId] from the function value,
    iterates its diagonal edges, and matches the edge source against the
    argument payload — returning the argument unchanged.  [Etype] behaves
    the same way. *)

(** Core reduction: iterating [anyId] and keeping the edge whose source is
    the argument payload [s0 ∈ Big] yields the value triple ⟨r0,s0,s0⟩. *)
Lemma anyId_apply3 :
  forall r0 s0 : ZFSet, s0 ∈ Big ->
  ('⟨ ka , va ⟩ ← anyId ;; ⦃ _ ∈ {| ⟨ r0 , va , va ⟩ |} | ka = s0 ⦄)
  = {| ⟨ r0 , s0 , s0 ⟩ |}.
Proof.
  intros r0 s0 Hs0. unfold iUnion_pat, anyId, diag.
  apply set_ext; apply Inc_def; intros x Hx.
  - apply iUnion_IN in Hx. destruct Hx as [p [Hp Hx]].
    apply iUnion_IN in Hp. destruct Hp as [a [Ha Hpa]].
    apply IN_Sing_EQ in Hpa. subst p.
    assert (Hpred : pfst (Couple a a) = s0) by exact (In_Comp_P _ _ _ Hx).
    pose proof (Inc_IN _ _ _ (Comp_Inc _ _) Hx) as Hmem.
    rewrite pfst_Couple in Hpred.
    rewrite !psnd_Couple in Hmem.
    apply IN_Sing_EQ in Hmem. subst x a. apply IN_Sing.
  - apply IN_Sing_EQ in Hx. subst x.
    apply IN_iUnion with (y := ⟨ s0 , s0 ⟩).
    + apply IN_iUnion with (y := s0); [ exact Hs0 | apply IN_Sing ].
    + apply In_P_Comp.
      * rewrite !psnd_Couple. apply IN_Sing.
      * rewrite pfst_Couple. reflexivity.
Qed.

(** [Eany] applied to a value [⟨r0,s0,b0⟩] with payload [s0 ∈ Big] returns
    the value [⟨r0,s0,s0⟩]. *)
Lemma eval_any_app :
  forall (e : Expr) (env : Env) (r0 s0 b0 : ZFSet),
  eval e env = {| ⟨ r0 , s0 , b0 ⟩ |} -> s0 ∈ Big ->
  eval (Eapp Eany e) env = {| ⟨ r0 , s0 , s0 ⟩ |}.
Proof.
  intros e env r0 s0 b0 He Hs0.
  rewrite eval_app.
  change (eval Eany env) with ({| ⟨ env , anyId , anyId ⟩ |}).
  rewrite iUnion_pat3_Sing.
  unfold iUnion_pat.
  rewrite (iUnion_ext_mem anyId _
             (fun p => ⦃ _ ∈ {| ⟨ r0 , psnd p , psnd p ⟩ |} | pfst p = s0 ⦄)).
  - intros p Hp. cbv beta. rewrite He iUnion_pat3_Sing. reflexivity.
  - apply anyId_apply3. exact Hs0.
Qed.

(* Applying [any] to a numeral returns it, for every numeral [k]. *)
Example ev_any_app_con (k : nat) env :
  eval (Eapp Eany (Econ k)) env = {| ⟨ env , natZ k , natZ k ⟩ |}.
Proof. apply eval_any_app with (b0 := natZ k); [ reflexivity | apply natZ_small ]. Qed.

(** ** [type] applied to a function value returns it. *)

(** Core reduction for [is_type], identical in shape to [anyId_apply3] but
    over the partial identities [pIdFun Big]. *)
Lemma is_type_apply3 :
  forall r0 s0 : ZFSet, s0 ∈ pIdFun Big ->
  ('⟨ ka , va ⟩ ← is_type ;; ⦃ _ ∈ {| ⟨ r0 , va , va ⟩ |} | ka = s0 ⦄)
  = {| ⟨ r0 , s0 , s0 ⟩ |}.
Proof.
  intros r0 s0 Hs0. unfold iUnion_pat, is_type, diag.
  apply set_ext; apply Inc_def; intros x Hx.
  - apply iUnion_IN in Hx. destruct Hx as [p [Hp Hx]].
    apply iUnion_IN in Hp. destruct Hp as [a [Ha Hpa]].
    apply IN_Sing_EQ in Hpa. subst p.
    assert (Hpred : pfst (Couple a a) = s0) by exact (In_Comp_P _ _ _ Hx).
    pose proof (Inc_IN _ _ _ (Comp_Inc _ _) Hx) as Hmem.
    rewrite pfst_Couple in Hpred.
    rewrite !psnd_Couple in Hmem.
    apply IN_Sing_EQ in Hmem. subst x a. apply IN_Sing.
  - apply IN_Sing_EQ in Hx. subst x.
    apply IN_iUnion with (y := ⟨ s0 , s0 ⟩).
    + apply IN_iUnion with (y := s0); [ exact Hs0 | apply IN_Sing ].
    + apply In_P_Comp.
      * rewrite !psnd_Couple. apply IN_Sing.
      * rewrite pfst_Couple. reflexivity.
Qed.

Lemma eval_type_app :
  forall (e : Expr) (env : Env) (r0 s0 b0 : ZFSet),
  eval e env = {| ⟨ r0 , s0 , b0 ⟩ |} -> s0 ∈ pIdFun Big ->
  eval (Eapp Etype e) env = {| ⟨ r0 , s0 , s0 ⟩ |}.
Proof.
  intros e env r0 s0 b0 He Hs0.
  rewrite eval_app.
  change (eval Etype env) with ({| ⟨ env , is_type , is_type ⟩ |}).
  rewrite iUnion_pat3_Sing.
  unfold iUnion_pat.
  rewrite (iUnion_ext_mem is_type _
             (fun p => ⦃ _ ∈ {| ⟨ r0 , psnd p , psnd p ⟩ |} | pfst p = s0 ⦄)).
  - intros p Hp. cbv beta. rewrite He iUnion_pat3_Sing. reflexivity.
  - apply is_type_apply3. exact Hs0.
Qed.

(** The [any] value [anyId] is itself a (single-valued) partial function on
    [Big], i.e. a member of [pFun Big Big] — so it is a value of type
    [type]. *)
Lemma anyId_in_pFun : anyId ∈ pFun Big Big.
Proof.
  unfold pFun. apply In_P_Comp.
  - apply Inc_IN_Power, Inc_def. intros x Hx.
    unfold anyId, diag in Hx. apply iUnion_IN in Hx. destruct Hx as [a [Ha Hxa]].
    apply IN_Sing_EQ in Hxa. subst x. apply Couple_IN_Prod; exact Ha.
  - intros a b1 b2 H1 H2. unfold anyId, diag in H1, H2.
    apply iUnion_IN in H1. destruct H1 as [c1 [_ H1]]. apply IN_Sing_EQ in H1.
    apply iUnion_IN in H2. destruct H2 as [c2 [_ H2]]. apply IN_Sing_EQ in H2.
    pose proof (Couple_inj_left  _ _ _ _ H1) as A1.
    pose proof (Couple_inj_right _ _ _ _ H1) as B1.
    pose proof (Couple_inj_left  _ _ _ _ H2) as A2.
    pose proof (Couple_inj_right _ _ _ _ H2) as B2.
    congruence.
Qed.

(* [anyId] — the full diagonal — is itself a partial identity function
   (it is trivially a subset of the diagonal), so it is a value of the
   restricted type [type]. *)
Lemma anyId_in_pIdFun : anyId ∈ pIdFun Big.
Proof.
  unfold pIdFun. apply Inc_IN_Power. apply Inc_def. intros x Hx. exact Hx.
Qed.

(* [type] applied to the [any] value returns it (because [any] is a
   partial identity, hence a member of [type]). *)
Example ev_type_app_any env :
  eval (Eapp Etype Eany) env = {| ⟨ env , anyId , anyId ⟩ |}.
Proof. apply eval_type_app with (b0 := anyId); [ reflexivity | apply anyId_in_pIdFun ]. Qed.

(* Saved: the earlier curried encoding (two nested [Elam]s), kept for
   reference. *)
Definition poly_tm_curried : Expr :=
  Elam (Ebind (Eimg Etype) (Evar 0))
       (Elam (Ebind (Eimg (Evar 0)) (Evar 0))
             (Evar 0)).

(** * The polymorphic identity term [fun(t:type; x:t) := x].

    A *single* [Elamb] whose domain sequences the two binders with
    [Eassign] and [Eseq] (i.e. [Elet]; see [eval_let_seq_assign]): the
    domain is [t := :type ; x := :t ; x].  Each [Eassign] conses the bound
    value onto the environment; [Eseq] threads that environment to the
    next stage.  The body is [x] = [Evar 0]; [t] sits at index [1].

    Because the body [x] is a single value, [poly_tm] is single-valued and
    its forward graph is the *identity relation* (see [poly_tm_only_id]). *)


(* The domain telescope [t := :type ; x := :t ; x] built from [Eassign]
   and [Eseq] (= [Elet]).  Its triples bind [x] at slot 0 and [t] at slot
   1, with domain value [proj3 = x]. *)
Definition dom_flat : Expr :=
  Eseq (Eassign 0 (Eimg Etype))           (* t := :type ; *)
    (Eseq (Eassign 0 (Eimg (Evar 0)))     (* x := :t ;    *)
       (Evar 0)).                         (* x            *)

      (* x := e ; x   /=   x:=e *)
Definition poly_tm : Expr := Elamb dom_flat (Evar 0).

(** A relation [r] is an *identity / diagonal* when every edge is [⟨a,a⟩]. *)
Definition is_diag (r : ZFSet) : Prop := forall a b, ⟨ a , b ⟩ ∈ r -> a = b.

(** A diagonal relation is single-valued (a function): both [b1] and [b2]
    equal the common source [a].  Used to discharge the [isFunction] filter
    that [eval_lam] / [eval_lamb] now place on the function value. *)
Lemma isFunction_of_diag : forall r : ZFSet, is_diag r -> isFunction r.
Proof.
  intros r Hd a b1 b2 H1 H2.
  pose proof (Hd a b1 H1) as E1. pose proof (Hd a b2 H2) as E2. congruence.
Qed.

(** Comprehending a singleton by a predicate that already holds of its
    element leaves it unchanged. *)
Lemma Comp_Sing_true : forall (p : ZFSet) (P : ZFSet -> Prop),
  P p -> Comp {| p |} P = {| p |}.
Proof.
  intros p P HP. apply set_ext.
  - apply Comp_Inc.
  - apply Sing_Inc_IN. apply In_P_Comp; [ apply IN_Sing | exact HP ].
Qed.

(** A *constant* graph [{⟨k s, c⟩ : s ∈ A}] (every edge has target [c]) is
    single-valued. *)
Lemma isFunction_const_graph (A : ZFSet) (k : ZFSet -> ZFSet) (c : ZFSet) :
  isFunction (s ← A ;; {| ⟨ k s , c ⟩ |}).
Proof.
  intros a b1 b2 H1 H2.
  apply iUnion_IN in H1. destruct H1 as [s1 [_ H1]]. apply IN_Sing_EQ in H1.
  apply iUnion_IN in H2. destruct H2 as [s2 [_ H2]]. apply IN_Sing_EQ in H2.
  pose proof (Couple_inj_right _ _ _ _ H1) as E1.
  pose proof (Couple_inj_right _ _ _ _ H2) as E2. congruence.
Qed.

(** A graph [{⟨k s, v s⟩ : s ∈ A}] is single-valued whenever the key [k s]
    determines the value [v s]. *)
Lemma isFunction_graph (A : ZFSet) (k v : ZFSet -> ZFSet) :
  (forall s s', s ∈ A -> s' ∈ A -> k s = k s' -> v s = v s') ->
  isFunction (s ← A ;; {| ⟨ k s , v s ⟩ |}).
Proof.
  intros Hdet a b1 b2 H1 H2.
  apply iUnion_IN in H1. destruct H1 as [s1 [Hs1 H1]]. apply IN_Sing_EQ in H1.
  apply iUnion_IN in H2. destruct H2 as [s2 [Hs2 H2]]. apply IN_Sing_EQ in H2.
  pose proof (Couple_inj_left  _ _ _ _ H1) as A1.
  pose proof (Couple_inj_right _ _ _ _ H1) as B1.
  pose proof (Couple_inj_left  _ _ _ _ H2) as A2.
  pose proof (Couple_inj_right _ _ _ _ H2) as B2.
  assert (k s1 = k s2) by congruence.
  pose proof (Hdet s1 s2 Hs1 Hs2 H) as Hv. congruence.
Qed.

(** A domain ending in [Eseq _ (Evar 0)] is *proper*: each of its triples
    [ab] has its bound value [env_lookup (proj1 ab) 0] equal to its domain
    value [proj3 ab] (the terminal [Evar 0] returns the just-bound
    variable). *)
Lemma Eseq_var0_proper :
  forall (e : Expr) (env : Env) (ab : ZFSet),
  ab ∈ eval (Eseq e (Evar 0)) env ->
  env_lookup (proj1 ab) 0 = proj3 ab.
Proof.
  intros e env ab Hab.
  rewrite eval_seq in Hab. unfold iUnion_pat3 in Hab.
  apply iUnion_IN in Hab. destruct Hab as [t [_ Hab]].
  cbv beta in Hab. apply IN_Sing_EQ in Hab. subst ab.
  rewrite proj1_triple proj3_triple. reflexivity.
Qed.

(** [poly_tm]'s domain [dom_flat] is proper (it ends in [Eseq _ (Evar 0)]
    under the outer [Eseq]). *)
Lemma dom_flat_proper :
  forall (env : Env) (ab : ZFSet),
  ab ∈ eval dom_flat env -> env_lookup (proj1 ab) 0 = proj3 ab.
Proof.
  intros env ab Hab. unfold dom_flat in Hab.
  rewrite eval_seq in Hab. unfold iUnion_pat3 in Hab.
  apply iUnion_IN in Hab. destruct Hab as [t [_ Hab]].
  cbv beta in Hab.
  apply (Eseq_var0_proper _ _ _ Hab).
Qed.

(** A single [Elamb] with identity body [Evar 0] over any *proper* domain
    evaluates to function values whose forward graph is an identity
    relation: every element's [proj2] is a diagonal. *)
Lemma id_body_diag :
  forall (dom : Expr) (env : Env),
  (forall ab, ab ∈ eval dom env -> env_lookup (proj1 ab) 0 = proj3 ab) ->
  forall elt, elt ∈ eval (Elamb dom (Evar 0)) env -> is_diag (proj2 elt).
Proof.
  intros dom env Hproper elt Helt.
  rewrite eval_lamb in Helt.
  apply iUnion_IN in Helt. destruct Helt as [h [Hh Helt]].
  cbv beta zeta in Helt.
  apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Helt.
  apply IN_Sing_EQ in Helt. subst elt.
  rewrite proj2_triple. intros a b Hab.
  unfold iUnion_pat in Hab.
  apply iUnion_IN in Hab. destruct Hab as [p [Hp Hab]].
  cbv beta in Hab. apply IN_Sing_EQ in Hab.
  pose proof (Couple_inj_left  _ _ _ _ Hab) as Ha.
  pose proof (Couple_inj_right _ _ _ _ Hab) as Hb.
  pose proof Hh as HhPi.
  apply In_Pi_inv in Hh. destruct Hh as [Hsub _].
  pose proof (Inc_IN _ _ _ Hsub Hp) as HpProd.
  apply IN_Prod_EX in HpProd. destruct HpProd as [a' [c' [Ha' [_ Hpeq]]]].
  subst p.
  rewrite pfst_Couple in Ha. rewrite psnd_Couple in Hb.
  pose proof (Pi_edge_codomain _ _ _ _ _ HhPi Ha' Hp) as Hc'B.
  cbv beta in Hc'B. apply IN_Sing_EQ in Hc'B. subst c'.
  rewrite proj2_triple in Hb.
  pose proof (Hproper _ Ha') as Hprop.
  congruence.
Qed.

(** [poly_tm]'s evaluation contains only identity functions: every value it
    produces has the *identity relation* as its forward graph [proj2]. *)
Lemma poly_tm_only_id :
  forall (env : Env) (elt : ZFSet),
  elt ∈ eval poly_tm env -> is_diag (proj2 elt).
Proof.
  intros env elt Helt. unfold poly_tm in Helt.
  exact (id_body_diag dom_flat env (dom_flat_proper env) elt Helt).
Qed.

(** The nat type, [natId] (the identity relation on [ω]), is a
    single-valued partial function on [Big], i.e. a member of
    [pFun Big Big] — so [nat] is a value of type [type]. *)
Lemma natId_in_pFun : natId ∈ pFun Big Big.
Proof.
  unfold pFun. apply In_P_Comp.
  - apply Inc_IN_Power, Inc_def. intros x Hx.
    destruct (IN_natId_EXType _ Hx) as [n Hn]. subst x.
    apply Couple_IN_Prod; apply natZ_small.
  - intros a b1 b2 H1 H2.
    pose proof (natId_pair_diagonal _ _ H1) as E1.
    pose proof (natId_pair_diagonal _ _ H2) as E2.
    congruence.
Qed.

(** [natId] (the identity on [ω]) is a partial identity function — its
    edges ⟨n,n⟩ are diagonal, so it is a subset of the diagonal on [Big]. *)
Lemma natId_in_pIdFun : natId ∈ pIdFun Big.
Proof.
  unfold pIdFun, diag. apply Inc_IN_Power. apply Inc_def. intros x Hx.
  destruct (IN_natId_EXType _ Hx) as [n Hn]. subst x.
  apply IN_iUnion with (y := natZ n); [ apply natZ_small | apply IN_Sing ].
Qed.

(** Hence the nat type ⟨env, natId, natId⟩ is a member of [:type], so it
    is a legal argument to [poly_tm]'s first parameter [t : type]. *)
Lemma nat_in_type :
  forall env : Env, ⟨ env , natId , natId ⟩ ∈ eval (Eimg Etype) env.
Proof.
  intro env. rewrite eval_img.
  change (eval Etype env) with ({| ⟨ env , is_type , is_type ⟩ |}).
  rewrite iUnion_pat3_Sing.
  unfold iUnion_pat, is_type, diag.
  apply IN_iUnion with (y := ⟨ natId , natId ⟩).
  - apply IN_iUnion with (y := natId); [ apply natId_in_pIdFun | apply IN_Sing ].
  - cbv beta. rewrite pfst_Couple psnd_Couple. apply IN_Sing.
Qed.

(** The *singleton* identity [{⟨a,a⟩}] (for any small [a]) is a partial
    identity, hence a type. *)
Lemma sing_in_pIdFun : forall a : ZFSet, a ∈ Big -> {| ⟨ a , a ⟩ |} ∈ pIdFun Big.
Proof.
  intros a Ha. unfold pIdFun, diag. apply Inc_IN_Power. apply Inc_def. intros x Hx.
  apply IN_Sing_EQ in Hx. subst x.
  apply IN_iUnion with (y := a); [ exact Ha | apply IN_Sing ].
Qed.

Lemma sing_in_type : forall (env : Env) (a : ZFSet), a ∈ Big ->
  ⟨ env , {| ⟨ a , a ⟩ |} , {| ⟨ a , a ⟩ |} ⟩ ∈ eval (Eimg Etype) env.
Proof.
  intros env a Ha. rewrite eval_img.
  change (eval Etype env) with ({| ⟨ env , is_type , is_type ⟩ |}).
  rewrite iUnion_pat3_Sing. unfold iUnion_pat, is_type, diag.
  apply IN_iUnion with (y := ⟨ {| ⟨ a , a ⟩ |} , {| ⟨ a , a ⟩ |} ⟩).
  - apply IN_iUnion with (y := {| ⟨ a , a ⟩ |});
      [ apply sing_in_pIdFun; exact Ha | apply IN_Sing ].
  - cbv beta. rewrite pfst_Couple psnd_Couple. apply IN_Sing.
Qed.

(* [poly_tm] applied to the nat type.  By [nat_in_type] the argument lies
   in [poly_tm]'s domain. *)
(* Definition poly_tm_nat : Expr := Eapp poly_tm Enat. *)

(** Each numeral [k] inhabits the nat type [:nat], so [k] is a legal
    argument to [poly_tm]'s second parameter [x : t] once [t = nat]. *)
Lemma con_in_nat :
  forall (k : nat) (env : Env),
  ⟨ env , natZ k , natZ k ⟩ ∈ eval (Eimg Enat) env.
Proof.
  intros k env. rewrite eval_img.
  change (eval Enat env) with ({| ⟨ env , natId , natId ⟩ |}).
  rewrite iUnion_pat3_Sing.
  unfold iUnion_pat.
  apply IN_iUnion with (y := ⟨ natZ k , natZ k ⟩).
  - apply pair_self_mem_natId.
  - cbv beta. rewrite pfst_Couple psnd_Couple. apply IN_Sing.
Qed.

(* [poly_tm] applied to the  numeral [5]:
   [(fun(t:type; x:t) := x)[5]], which returns [5].  Proved exactly equal
   to [5] in [poly_tm_5_eval] at the end of this file (it relies on the
   domain-membership/diagonality helpers developed below). *)
Definition poly_tm_5 : Expr := Eapp poly_tm (Econ 5).


(** * Concretely: the constant-0 function is a *value* of the multi-valued
    [fun(x:nat) := :nat].

    Because the body [:nat] returns the whole type [nat], this function
    ranges over *all* selections [nat -> :nat].  We exhibit one of them —
    the constant-0 function [{⟨v,0⟩ : v}] — as a member of the result set,
    witnessing that the [eval] is not a singleton. *)
Definition nat_to_nat : Expr := Elamb (Ebind (Eimg Enat) (Evar 0)) (Eimg Enat).

(* iterate a pattern-union over a graph [{⟨s, k s⟩ : s ∈ A}] *)
Lemma iUnion_pat_graph (A : ZFSet) (k : ZFSet -> ZFSet) (F : ZFSet -> ZFSet -> ZFSet) :
  iUnion_pat (s ← A ;; {| ⟨ s , k s ⟩ |}) F = (s ← A ;; F s (k s)).
Proof.
  unfold iUnion_pat. rewrite iUnion_assoc.
  apply iUnion_ext_mem. intros s Hs.
  rewrite iUnion_Sing_l. cbv beta. rewrite pfst_Couple psnd_Couple. reflexivity.
Qed.

(** The constant-0 function (over the [nat] domain [A], sending every
    domain value [proj3 s] to [0]) is a member of [eval nat_to_nat env]:
    it is the value produced by the choice that returns [0] for every
    argument. *)
Lemma const0_in_eval (env : Env) :
  ⟨ env ,
        (s ← eval (Ebind (Eimg Enat) (Evar 0)) env ;; {| ⟨ proj3 s , natZ 0 ⟩ |}) ,
        (s ← eval (Ebind (Eimg Enat) (Evar 0)) env ;; {| ⟨ proj2 s , natZ 0 ⟩ |}) ⟩ ∈ eval nat_to_nat env.
Proof.
  unfold nat_to_nat. rewrite eval_lamb.
  set (A := eval (Ebind (Eimg Enat) (Evar 0)) env).
  apply IN_iUnion with
    (y := iUnion A (fun s => {| ⟨ s , ⟨ proj1 s , natZ 0 , natZ 0 ⟩ ⟩ |})).
  - apply (iUnion_graph_mem_pi A (fun t => eval (Eimg Enat) (proj1 t))
             (fun s => ⟨ proj1 s , natZ 0 , natZ 0 ⟩)).
    intros s Hs. apply con_in_nat.
  - cbv beta zeta.
    rewrite !(iUnion_pat_graph A (fun s => ⟨ proj1 s , natZ 0 , natZ 0 ⟩)).
    cbv beta.
    assert (Hf : (s ← A ;; {| ⟨ proj3 s , proj2 (⟨ proj1 s , natZ 0 , natZ 0 ⟩ : ZFSet) ⟩ |})
               = (s ← A ;; {| ⟨ proj3 s , natZ 0 ⟩ |})).
    { apply iUnion_ext_mem. intros s Hs. rewrite proj2_triple. reflexivity. }
    assert (Hg : (s ← A ;; {| ⟨ proj2 s , proj3 (⟨ proj1 s , natZ 0 , natZ 0 ⟩ : ZFSet) ⟩ |})
               = (s ← A ;; {| ⟨ proj2 s , natZ 0 ⟩ |})).
    { apply iUnion_ext_mem. intros s Hs. rewrite proj3_triple. reflexivity. }
    rewrite Hf Hg.
    apply In_P_Comp; [ apply IN_Sing | split; apply isFunction_const_graph ].
Qed.

(** * The dependent function [fun(t:type; x:t) := :t].

    Same single-[Elam] / [Eassign];[Eseq] domain [dom_flat] as [poly_tm],
    but the body is the *type* [:t] = [Eimg (Evar 1)] (with [t] at index
    1) instead of [x].

    Unlike [poly_tm], the body [:t] returns the *whole* type — a
    multi-valued result.  So [poly_ty] does NOT only contain identity
    functions — [const0_in_eval] exhibits the constant-0 function (a
    non-identity) as one of the values of a [:t]-body function. *)

(* Saved: the earlier curried encoding. *)
Definition poly_ty_curried : Expr :=
  Elam (Ebind (Eimg Etype) (Evar 0))
       (Elam (Ebind (Eimg (Evar 0)) (Evar 0))
             (Eimg (Evar 1))).

Definition poly_ty : Expr := Elamb dom_flat (Eimg (Evar 1)).
(** * A *partial* witness: [poly_ty] produces the function [{⟨0,1⟩}]
    (maps [0] to [1], undefined elsewhere).

    Total selections like the constant-0 function arise at types with many
    inhabitants (e.g. [nat]).  A genuinely *partial* function appears at a
    *singleton* type: at [t0 = {⟨1,0⟩}] (whose sole inhabitant is [0]), the
    [:t]-body function [fun(x:t0) := :t0] has the one-point domain [{0}]
    and must return [t0]'s only first component [1] — so its forward graph
    is exactly the partial function [{⟨0,1⟩}]. *)
Definition t0 : ZFSet := {| ⟨ natZ 1 , natZ 0 ⟩ |}.
Definition inner_ty : Expr := Elamb (Ebind (Eimg (Evar 0)) (Evar 0)) (Eimg (Evar 1)).

(* membership in a type-variable projection [:(Evar i)] *)
Lemma In_Eimg_Evar :
  forall (i : nat) (env' : Env) (a b : ZFSet),
  ⟨ a , b ⟩ ∈ env_lookup env' i ->
  ⟨ env' , a , b ⟩ ∈ eval (Eimg (Evar i)) env'.
Proof.
  intros i env' a b Hab.
  rewrite eval_img.
  change (eval (Evar i) env')
    with ({| ⟨ env' , env_lookup env' i , env_lookup env' i ⟩ |}).
  rewrite iUnion_pat3_Sing. unfold iUnion_pat.
  apply IN_iUnion with (y := ⟨ a , b ⟩); [ exact Hab |].
  cbv beta. rewrite pfst_Couple psnd_Couple. apply IN_Sing.
Qed.

(* the [Eimg (Evar 0)] projection keeps the ambient env as its [proj1] *)
Lemma Eimg_Evar0_proj1 :
  forall (env' : Env) (tp : ZFSet),
  tp ∈ eval (Eimg (Evar 0)) env' -> proj1 tp = env'.
Proof.
  intros env' tp Htp. rewrite eval_img in Htp.
  change (eval (Evar 0) env')
    with ({| ⟨ env' , env_lookup env' 0 , env_lookup env' 0 ⟩ |}) in Htp.
  rewrite iUnion_pat3_Sing in Htp. unfold iUnion_pat in Htp.
  apply iUnion_IN in Htp. destruct Htp as [p [_ Htp]].
  cbv beta in Htp. apply IN_Sing_EQ in Htp. subst tp.
  apply proj1_triple.
Qed.

(* hence the inner domain places the ambient slot 0 at slot 1 of each
   domain triple's environment (this is where the type [t] is found) *)
Lemma inner_dom_lookup1 :
  forall (env' : Env) (s : ZFSet),
  s ∈ eval (Ebind (Eimg (Evar 0)) (Evar 0)) env' ->
  env_lookup (proj1 s) 1 = env_lookup env' 0.
Proof.
  intros env' s Hs.
  rewrite eval_bind in Hs. unfold iUnion_pat3 in Hs.
  apply iUnion_IN in Hs. destruct Hs as [tp [Htp Hs]].
  cbv beta in Hs. apply IN_Sing_EQ in Hs. subst s.
  rewrite proj1_triple.
  rewrite env_lookup_cons_succ. rewrite psnd_Couple.
  rewrite (Eimg_Evar0_proj1 _ _ Htp). reflexivity.
Qed.

(** The [:t]-body function [fun(x:t0):= :t0] (the body of [poly_ty] at the
    singleton type [t0]) is the partial function [{⟨0,1⟩}]: its forward
    graph maps the one domain value [0] to [1].  (As in [const0_in_eval]
    the graph is written over the domain; here that domain is the
    singleton [{0}], so the graph is [{⟨0,1⟩}].) *)
Lemma f01_in_eval (env : Env) :
  ⟨ env_cons t0 env ,
        (s ← eval (Ebind (Eimg (Evar 0)) (Evar 0)) (env_cons t0 env) ;; {| ⟨ proj3 s , natZ 1 ⟩ |}) ,
        (s ← eval (Ebind (Eimg (Evar 0)) (Evar 0)) (env_cons t0 env) ;; {| ⟨ proj2 s , natZ 0 ⟩ |}) ⟩ ∈ eval inner_ty (env_cons t0 env).
Proof.
  unfold inner_ty. rewrite eval_lamb.
  set (A := eval (Ebind (Eimg (Evar 0)) (Evar 0)) (env_cons t0 env)).
  apply IN_iUnion with
    (y := iUnion A (fun s => {| ⟨ s , ⟨ proj1 s , natZ 1 , natZ 0 ⟩ ⟩ |})).
  - apply (iUnion_graph_mem_pi A (fun t => eval (Eimg (Evar 1)) (proj1 t))
             (fun s => ⟨ proj1 s , natZ 1 , natZ 0 ⟩)).
    intros s Hs. apply In_Eimg_Evar.
    rewrite (inner_dom_lookup1 _ _ Hs).
    rewrite env_lookup_cons_zero. rewrite pfst_Couple.
    unfold t0. apply IN_Sing.
  - cbv beta zeta.
    rewrite !(iUnion_pat_graph A (fun s => ⟨ proj1 s , natZ 1 , natZ 0 ⟩)).
    cbv beta.
    assert (Hf : (s ← A ;; {| ⟨ proj3 s , proj2 (⟨ proj1 s , natZ 1 , natZ 0 ⟩ : ZFSet) ⟩ |})
               = (s ← A ;; {| ⟨ proj3 s , natZ 1 ⟩ |})).
    { apply iUnion_ext_mem. intros s Hs. rewrite proj2_triple. reflexivity. }
    assert (Hg : (s ← A ;; {| ⟨ proj2 s , proj3 (⟨ proj1 s , natZ 1 , natZ 0 ⟩ : ZFSet) ⟩ |})
               = (s ← A ;; {| ⟨ proj2 s , natZ 0 ⟩ |})).
    { apply iUnion_ext_mem. intros s Hs. rewrite proj3_triple. reflexivity. }
    rewrite Hf Hg.
    apply In_P_Comp; [ apply IN_Sing | split; apply isFunction_const_graph ].
Qed.

(** * [poly_tm] applied to [5] returns exactly [5].

    Each fibre of [poly_tm]'s domain [dom_flat] is a *singleton* (the body
    [Evar 0] reads back a single value), so by [Pi_Sing] the function space
    [Π[t ∈ dom_flat] (Evar 0)] is a singleton: [eval poly_tm env] is the one
    identity function on the domain values.  Applying it to [5] keeps the
    single diagonal edge ⟨5,5⟩, giving [{⟨env,5,5⟩}]. *)

(** ** Generic membership-introduction and reduction helpers. *)

(** A value ⟨r,a,b⟩ of [e] yields ⟨env_cons b r, a, b⟩ of [Eassign n e]
    (the bound value [b] is consed onto its own environment). *)
Lemma In_assign_intro :
  forall (n : nat) (e : Expr) (env : Env) (r a b : ZFSet),
  ⟨ r , a , b ⟩ ∈ eval e env ->
  ⟨ env_cons b r , a , b ⟩ ∈ eval (Eassign n e) env.
Proof.
  intros n e env r a b H.
  rewrite eval_assign. unfold iUnion_pat3.
  apply IN_iUnion with (y := ⟨ r , a , b ⟩); [ exact H |].
  rewrite proj1_triple proj2_triple proj3_triple. apply IN_Sing.
Qed.

(** [Eseq] threads [e1]'s environment [proj1 t1] into [e2]: any value of
    [e2] under that environment is a value of [Eseq e1 e2]. *)
Lemma In_seq_intro :
  forall (e1 e2 : Expr) (env : Env) (t1 x : ZFSet),
  t1 ∈ eval e1 env -> x ∈ eval e2 (proj1 t1) ->
  x ∈ eval (Eseq e1 e2) env.
Proof.
  intros e1 e2 env t1 x H1 H2.
  rewrite eval_seq. unfold iUnion_pat3.
  apply IN_iUnion with (y := t1); [ exact H1 | exact H2 ].
Qed.

(** Iterate a pair-pattern union over a graph [{| h s |} : s ∈ A] whose
    points [h s] need not be of the form ⟨s, _⟩ (generalises
    [iUnion_pat_graph], which fixes [h s = ⟨s, k s⟩]). *)
Lemma iUnion_pat_iUnion (A : ZFSet) (h : ZFSet -> ZFSet) (F : ZFSet -> ZFSet -> ZFSet) :
  iUnion_pat (iUnion A (fun s => {| h s |})) F
  = iUnion A (fun s => F (pfst (h s)) (psnd (h s))).
Proof.
  unfold iUnion_pat. rewrite iUnion_assoc.
  apply iUnion_ext_mem. intros s Hs. rewrite iUnion_Sing_l. reflexivity.
Qed.

(** A domain ending in [Eseq _ (Evar 0)] is *diagonal*: its triples have
    [proj2 = proj3] (the terminal [Evar] returns ⟨env, v, v⟩). *)
Lemma Eseq_var0_diag :
  forall (e : Expr) (env : Env) (ab : ZFSet),
  ab ∈ eval (Eseq e (Evar 0)) env -> proj2 ab = proj3 ab.
Proof.
  intros e env ab Hab.
  rewrite eval_seq in Hab. unfold iUnion_pat3 in Hab.
  apply iUnion_IN in Hab. destruct Hab as [t [_ Hab]].
  cbv beta in Hab. rewrite eval_var in Hab.
  apply IN_Sing_EQ in Hab. subst ab.
  rewrite proj2_triple proj3_triple. reflexivity.
Qed.

Lemma dom_flat_diag :
  forall (env : Env) (ab : ZFSet),
  ab ∈ eval dom_flat env -> proj2 ab = proj3 ab.
Proof.
  intros env ab Hab. unfold dom_flat in Hab.
  rewrite eval_seq in Hab. unfold iUnion_pat3 in Hab.
  apply iUnion_IN in Hab. destruct Hab as [t [_ Hab]].
  cbv beta in Hab. apply (Eseq_var0_diag _ _ _ Hab).
Qed.

(** A concrete inhabitant of [poly_tm]'s domain: the selection [t := nat ;
    x := 5] (so [t] sits at slot 1 as [natId], [x] at slot 0 as [5], and
    the domain value is [5]). *)
Lemma dom_flat_has_5 :
  forall env : Env,
  ⟨ env_cons (natZ 5) (env_cons natId env) , natZ 5 , natZ 5 ⟩ ∈ eval dom_flat env.
Proof.
  intro env. unfold dom_flat.
  apply (In_seq_intro _ _ _ (⟨ env_cons natId env , natId , natId ⟩)).
  - apply (In_assign_intro 0 (Eimg Etype) env env natId natId).
    apply nat_in_type.
  - rewrite proj1_triple.
    apply (In_seq_intro _ _ _
             (⟨ env_cons (natZ 5) (env_cons natId env) , natZ 5 , natZ 5 ⟩)).
    + apply (In_assign_intro 0 (Eimg (Evar 0)) (env_cons natId env)
               (env_cons natId env) (natZ 5) (natZ 5)).
      apply In_Eimg_Evar.
      rewrite env_lookup_cons_zero pfst_Couple.
      apply pair_self_mem_natId.
    + rewrite proj1_triple eval_var.
      rewrite env_lookup_cons_zero pfst_Couple.
      apply IN_Sing.
Qed.

(** A [dom_flat] domain point at the *singleton* type [{⟨a,a⟩}] with value
    [a] (for any small [a]).  As in [dom_flat_has_5] but with the singleton
    identity type instead of [nat]. *)
Lemma dom_flat_has_sing : forall (env : Env) (a : ZFSet), a ∈ Big ->
  ⟨ env_cons a (env_cons {| ⟨ a , a ⟩ |} env) , a , a ⟩ ∈ eval dom_flat env.
Proof.
  intros env a Ha. unfold dom_flat.
  apply (In_seq_intro _ _ _
           (⟨ env_cons {| ⟨ a , a ⟩ |} env , {| ⟨ a , a ⟩ |} , {| ⟨ a , a ⟩ |} ⟩)).
  - apply (In_assign_intro 0 (Eimg Etype) env env {| ⟨ a , a ⟩ |} {| ⟨ a , a ⟩ |}).
    apply sing_in_type; exact Ha.
  - rewrite proj1_triple.
    apply (In_seq_intro _ _ _
             (⟨ env_cons a (env_cons {| ⟨ a , a ⟩ |} env) , a , a ⟩)).
    + apply (In_assign_intro 0 (Eimg (Evar 0)) (env_cons {| ⟨ a , a ⟩ |} env)
               (env_cons {| ⟨ a , a ⟩ |} env) a a).
      apply In_Eimg_Evar.
      rewrite env_lookup_cons_zero pfst_Couple. apply IN_Sing.
    + rewrite proj1_triple eval_var.
      rewrite env_lookup_cons_zero pfst_Couple. apply IN_Sing.
Qed.

(** The type projection [:(Evar 1)] at an environment whose slot 1 holds a
    singleton type [{⟨a,a⟩}] is the singleton value [⟨env', a, a⟩]. *)
Lemma body_at_sing : forall (env' : Env) (a : ZFSet),
  env_lookup env' 1 = {| ⟨ a , a ⟩ |} ->
  eval (Eimg (Evar 1)) env' = {| ⟨ env' , a , a ⟩ |}.
Proof.
  intros env' a Hlook. rewrite eval_img.
  change (eval (Evar 1) env')
    with ({| ⟨ env' , env_lookup env' 1 , env_lookup env' 1 ⟩ |}).
  rewrite Hlook iUnion_pat3_Sing. unfold iUnion_pat.
  rewrite iUnion_Sing_l. cbv beta. rewrite pfst_Couple psnd_Couple. reflexivity.
Qed.

(** ** [eval poly_tm env] is the single identity function on its domain. *)

(** The per-fibre choice for [poly_tm]: at domain triple [s], the body
    [Evar 0] reads back the slot-0 value, giving ⟨proj1 s, v, v⟩. *)
Definition dom_choice (s : ZFSet) : ZFSet :=
  ⟨ proj1 s , env_lookup (proj1 s) 0 , env_lookup (proj1 s) 0 ⟩.

(** The identity relation on [poly_tm]'s domain values. *)
Definition idDom (env : Env) : ZFSet :=
  t ← eval dom_flat env ;; {| ⟨ proj3 t , proj3 t ⟩ |}.

(** [idDom] is a diagonal relation, hence a function. *)
Lemma is_diag_idDom : forall env : Env, is_diag (idDom env).
Proof.
  intros env a b Hab. unfold idDom in Hab.
  apply iUnion_IN in Hab. destruct Hab as [t [_ Hab]].
  cbv beta in Hab. apply IN_Sing_EQ in Hab.
  pose proof (Couple_inj_left _ _ _ _ Hab) as Ha.
  pose proof (Couple_inj_right _ _ _ _ Hab) as Hb. congruence.
Qed.

Lemma isFunction_idDom : forall env : Env, isFunction (idDom env).
Proof. intro env. apply isFunction_of_diag, is_diag_idDom. Qed.

(** Both the forward graph and cograph of [poly_tm]'s unique function value
    collapse to [idDom] (using domain properness and diagonality). *)
Lemma poly_tm_graph_reduce :
  forall (env : Env) (F : ZFSet -> ZFSet -> ZFSet),
  (forall s, s ∈ eval dom_flat env ->
     F s (dom_choice s) = {| ⟨ proj3 s , proj3 s ⟩ |}) ->
  iUnion_pat (iUnion (eval dom_flat env) (fun a => {| ⟨ a , dom_choice a ⟩ |})) F
  = idDom env.
Proof.
  intros env F HF.
  rewrite (iUnion_pat_graph (eval dom_flat env) dom_choice F).
  unfold idDom. apply iUnion_ext_mem. intros s Hs. apply HF. exact Hs.
Qed.

Lemma poly_tm_eval :
  forall env : Env, eval poly_tm env = {| ⟨ env , idDom env , idDom env ⟩ |}.
Proof.
  intro env. unfold poly_tm. rewrite eval_lamb.
  assert (Hfib : forall t, In t (eval dom_flat env) ->
            eval (Evar 0) (proj1 t) = {| dom_choice t |}).
  { intros t Ht. rewrite eval_var. unfold dom_choice. reflexivity. }
  rewrite (Pi_Sing (eval dom_flat env) (fun t => eval (Evar 0) (proj1 t))
             dom_choice Hfib).
  rewrite iUnion_Sing_l. cbv beta zeta.
  assert (Hf : iUnion_pat
                 (iUnion (eval dom_flat env) (fun a => {| ⟨ a , dom_choice a ⟩ |}))
                 (fun ab cd => {| ⟨ proj3 ab , proj2 cd ⟩ |}) = idDom env).
  { apply poly_tm_graph_reduce. intros s Hs. unfold dom_choice.
    rewrite proj2_triple (dom_flat_proper env s Hs). reflexivity. }
  assert (Hg : iUnion_pat
                 (iUnion (eval dom_flat env) (fun a => {| ⟨ a , dom_choice a ⟩ |}))
                 (fun ab cd => {| ⟨ proj2 ab , proj3 cd ⟩ |}) = idDom env).
  { apply poly_tm_graph_reduce. intros s Hs. unfold dom_choice.
    rewrite proj3_triple (dom_flat_proper env s Hs) (dom_flat_diag env s Hs).
    reflexivity. }
  rewrite Hf Hg.
  apply Comp_Sing_true. split; apply isFunction_idDom.
Qed.

(** ** The application: [poly_tm [5]] reduces to exactly [{⟨env,5,5⟩}]. *)
Lemma poly_tm_5_eval :
  forall env : Env, eval poly_tm_5 env = {| ⟨ env , natZ 5 , natZ 5 ⟩ |}.
Proof.
  intro env. unfold poly_tm_5. rewrite eval_app poly_tm_eval.
  rewrite iUnion_pat3_Sing.
  unfold idDom.
  rewrite (iUnion_pat_iUnion (eval dom_flat env) (fun t => ⟨ proj3 t , proj3 t ⟩)).
  (* Reduce each fibre to a singleton filtered by [proj3 t = 5]. *)
  transitivity (iUnion (eval dom_flat env)
     (fun t => ⦃ _ ∈ {| ⟨ env , proj3 t , proj3 t ⟩ |} | proj3 t = natZ 5 ⦄)).
  - apply iUnion_ext_mem. intros t Ht. cbv beta.
    rewrite !pfst_Couple !psnd_Couple eval_con iUnion_pat3_Sing. reflexivity.
  - apply set_ext.
    + apply Inc_def. intros x Hx.
      apply iUnion_IN in Hx. destruct Hx as [t [Ht Hx]].
      pose proof (In_Comp_P _ _ _ Hx) as Hpred.
      pose proof (Inc_IN _ _ _ (Comp_Inc _ _) Hx) as Hmem.
      apply IN_Sing_EQ in Hmem. subst x. rewrite !Hpred. apply IN_Sing.
    + apply Inc_def. intros x Hx. apply IN_Sing_EQ in Hx. subst x.
      apply IN_iUnion with
        (y := ⟨ env_cons (natZ 5) (env_cons natId env) , natZ 5 , natZ 5 ⟩).
      * apply dom_flat_has_5.
      * rewrite !proj3_triple. apply In_P_Comp; [ apply IN_Sing | reflexivity ].
Qed.

(** [poly_tm_5] *only* contains [5]: every value it produces is the single
    triple ⟨env, 5, 5⟩ (immediate from the exact denotation
    [poly_tm_5_eval]).  In particular its payload is always [5]. *)
Corollary poly_tm_5_only_5 :
  forall (env : Env) (x : ZFSet),
  x ∈ eval poly_tm_5 env -> x = ⟨ env , natZ 5 , natZ 5 ⟩.
Proof.
  intros env x Hx. rewrite poly_tm_5_eval in Hx.
  apply IN_Sing_EQ in Hx. exact Hx.
Qed.

(** * [fun(t:type; x:t) := (t[0]; 0) | x].

    Same domain telescope [dom_flat] as [poly_tm]/[poly_ty] ([t] at de
    Bruijn index [1], [x] at [0]); the body is a *choice* [(t[0]; 0) | x]:
      - left:  [t[0]; 0] — apply the type [t] to [0] (for its effect),
               discard the result, and yield [0];
      - right: [x].
    Concrete Verse syntax: [fun(t:type; x:t) := (t[0]; 0) | x]. *)
Definition poly_choice : Expr :=
  Elam dom_flat
    (Echoice (Eseq (Eapp (Evar 1) (Econ 0)) (Econ 0)) (Evar 0)).

(** Applying the identity relation [natId] (the [nat] type) to a numeral
    [k] returns [k]: iterating [natId]'s diagonal edges and keeping the one
    whose source is [k] yields the value triple ⟨r0,k,k⟩.  (Shape mirrors
    [anyId_apply3], but over [natId] rather than the diagonal on [Big].) *)
Lemma natId_apply :
  forall (r0 : ZFSet) (k : nat),
  ('⟨ ka , va ⟩ ← natId ;; ⦃ _ ∈ {| ⟨ r0 , va , va ⟩ |} | ka = natZ k ⦄)
  = {| ⟨ r0 , natZ k , natZ k ⟩ |}.
Proof.
  intros r0 k. unfold iUnion_pat.
  apply set_ext; apply Inc_def; intros x Hx.
  - apply iUnion_IN in Hx. destruct Hx as [p [Hp Hx]].
    assert (Hpred : pfst p = natZ k) by exact (In_Comp_P _ _ _ Hx).
    pose proof (Inc_IN _ _ _ (Comp_Inc _ _) Hx) as Hmem.
    apply IN_Sing_EQ in Hmem. subst x.
    destruct (IN_natId_EXType _ Hp) as [n Hn]. subst p.
    rewrite pfst_Couple in Hpred. rewrite !psnd_Couple Hpred. apply IN_Sing.
  - apply IN_Sing_EQ in Hx. subst x.
    apply IN_iUnion with (y := ⟨ natZ k , natZ k ⟩).
    + apply pair_self_mem_natId.
    + apply In_P_Comp.
      * rewrite !psnd_Couple. apply IN_Sing.
      * rewrite pfst_Couple. reflexivity.
Qed.

(** [t[k]] when [t] (at slot [1]) is the [nat] type [natId]: the
    application returns the numeral [k]. *)
Lemma eval_var1_app_con :
  forall (env_b : Env) (k : nat),
  env_lookup env_b 1 = natId ->
  eval (Eapp (Evar 1) (Econ k)) env_b = {| ⟨ env_b , natZ k , natZ k ⟩ |}.
Proof.
  intros env_b k Hlook.
  rewrite eval_app.
  change (eval (Evar 1) env_b)
    with ({| ⟨ env_b , env_lookup env_b 1 , env_lookup env_b 1 ⟩ |}).
  rewrite Hlook iUnion_pat3_Sing.
  unfold iUnion_pat.
  rewrite (iUnion_ext_mem natId _
             (fun p => ⦃ _ ∈ {| ⟨ env_b , psnd p , psnd p ⟩ |} | pfst p = natZ k ⦄)).
  - intros p Hp. cbv beta. rewrite eval_con iUnion_pat3_Sing. reflexivity.
  - apply natId_apply.
Qed.

(** The representative domain environment [t := nat, x := 5]. *)
Definition env_nat5 (env : Env) : Env :=
  env_cons (natZ 5) (env_cons natId env).

(** Evaluating [poly_choice]'s body under [env_nat5]: the choice yields
    [0] (left branch: [nat[0]; 0] succeeds because [0 ∈ nat]) *and* [5]
    (right branch: [x]).  So the body denotes the two-value set
    {⟨env,0,0⟩, ⟨env,5,5⟩}. *)
Example eval_poly_choice_body :
  forall env : Env,
  eval (Echoice (Eseq (Eapp (Evar 1) (Econ 0)) (Econ 0)) (Evar 0)) (env_nat5 env)
  = {| ⟨ env_nat5 env , natZ 0 , natZ 0 ⟩ |}
    ∪ {| ⟨ env_nat5 env , natZ 5 , natZ 5 ⟩ |}.
Proof.
  intro env.
  assert (Hlook : env_lookup (env_nat5 env) 1 = natId).
  { unfold env_nat5.
    rewrite env_lookup_cons_succ psnd_Couple env_lookup_cons_zero pfst_Couple.
    reflexivity. }
  assert (HL : eval (Eseq (Eapp (Evar 1) (Econ 0)) (Econ 0)) (env_nat5 env)
               = {| ⟨ env_nat5 env , natZ 0 , natZ 0 ⟩ |}).
  { rewrite eval_seq (eval_var1_app_con (env_nat5 env) 0 Hlook)
            iUnion_pat3_Sing eval_con. reflexivity. }
  rewrite eval_choice. f_equal.
  - rewrite HL iUnion_pat3_Sing. reflexivity.
  - rewrite eval_var iUnion_pat3_Sing.
    unfold env_nat5. rewrite env_lookup_cons_zero pfst_Couple. reflexivity.
Qed.


(** * The [Elamb] (environment-extending) function examples.

    [Elamb] runs its body in [env_cons (proj3 t) (proj1 t)], so the body's
    [Evar 0] is the bound domain value — the binding that the [Eimg]-domain
    [Elam] examples ([ex_succ], [ex_int_id], …) assume but that plain
    [Elam] does not provide in [Timbda2].  These [_b] variants therefore
    compute as in Verse. *)

(** When every fibre of an [Elamb]'s domain is a singleton [{| cd t |}],
    the function space is a singleton (by [Pi_Sing]): [eval (Elamb e1 e2)]
    is the single function value whose forward graph and cograph read off
    [cd] over the domain.  (The [Elamb] analogue of [poly_tm_eval].) *)
Lemma eval_lam_sing :
  forall (e1 e2 : Expr) (env : Env) (cd : ZFSet -> ZFSet),
  (forall t, t ∈ eval e1 env ->
     eval e2 (env_cons (proj3 t) env) = {| cd t |}) ->
  isFunction (t ← eval e1 env ;; {| ⟨ proj3 t , proj2 (cd t) ⟩ |}) ->
  isFunction (t ← eval e1 env ;; {| ⟨ proj2 t , proj3 (cd t) ⟩ |}) ->
  eval (Elam e1 e2) env
  = {| ⟨ env ,
         (t ← eval e1 env ;; {| ⟨ proj3 t , proj2 (cd t) ⟩ |}) ,
         (t ← eval e1 env ;; {| ⟨ proj2 t , proj3 (cd t) ⟩ |}) ⟩ |}.
Proof.
  intros e1 e2 env cd Hfib HfF HfG.
  rewrite eval_lam.
  rewrite (Pi_Sing (eval e1 env)
             (fun t => eval e2 (env_cons (proj3 t) env)) cd Hfib).
  rewrite iUnion_Sing_l. cbv beta zeta.
  rewrite (iUnion_pat_graph (eval e1 env) cd
             (fun ab cd0 => {| ⟨ proj3 ab , proj2 cd0 ⟩ |})).
  rewrite (iUnion_pat_graph (eval e1 env) cd
             (fun ab cd0 => {| ⟨ proj2 ab , proj3 cd0 ⟩ |})).
  apply Comp_Sing_true. split; assumption.
Qed.

(** Every value of the [nat] type [:nat = Eimg Enat] is a diagonal triple
    [⟨env, n, n⟩] (its [proj2] and [proj3] coincide). *)
Lemma Eimg_Enat_diag : forall (env : Env) (t : ZFSet),
  t ∈ eval (Eimg Enat) env -> proj2 t = proj3 t.
Proof.
  intros env t Ht. rewrite eval_img in Ht.
  change (eval Enat env) with ({| ⟨ env , natId , natId ⟩ |}) in Ht.
  rewrite iUnion_pat3_Sing in Ht. unfold iUnion_pat in Ht.
  apply iUnion_IN in Ht. destruct Ht as [p [Hp Ht]].
  cbv beta in Ht. apply IN_Sing_EQ in Ht. subst t.
  destruct (IN_natId_EXType _ Hp) as [n Hn]. subst p.
  rewrite proj2_triple proj3_triple pfst_Couple psnd_Couple. reflexivity.
Qed.

(** ** [ex_int_id_b]: the int identity returns its argument.

    Applying [(x:int => x)] to a numeral [k] yields [k] — confirming the
    body's [Evar 0] is bound to the argument. *)
Example ev_app_int_id_b env k :
  ⟨ env , natZ k , natZ k ⟩ ∈ eval (Eapp ex_int_id_b (Econ k)) env.
Proof.
  unfold ex_int_id_b. rewrite eval_app.
  assert (Hfib : forall t, In t (eval (Eimg Enat) env) ->
            eval (Evar 0) (env_cons (proj3 t) env)
            = {| ⟨ env_cons (proj3 t) env , proj3 t , proj3 t ⟩ |}).
  { intros t Ht. rewrite eval_var env_lookup_cons_zero pfst_Couple. reflexivity. }
  assert (HfF : isFunction (t ← eval (Eimg Enat) env ;;
              {| ⟨ proj3 t , proj2 (⟨ env_cons (proj3 t) env , proj3 t , proj3 t ⟩ : ZFSet) ⟩ |})).
  { apply isFunction_graph. intros s s' Hs Hs' Heq. rewrite !proj2_triple. exact Heq. }
  assert (HfG : isFunction (t ← eval (Eimg Enat) env ;;
              {| ⟨ proj2 t , proj3 (⟨ env_cons (proj3 t) env , proj3 t , proj3 t ⟩ : ZFSet) ⟩ |})).
  { apply isFunction_graph. intros s s' Hs Hs' Heq. rewrite !proj3_triple.
    rewrite -(Eimg_Enat_diag env s Hs) -(Eimg_Enat_diag env s' Hs'). exact Heq. }
  rewrite (eval_lam_sing (Eimg Enat) (Evar 0) env
             (fun t => ⟨ env_cons (proj3 t) env , proj3 t , proj3 t ⟩) Hfib HfF HfG).
  rewrite iUnion_pat3_Sing. unfold iUnion_pat.
  apply IN_iUnion with (y := ⟨ natZ k , natZ k ⟩).
  - apply IN_iUnion with (y := ⟨ env , natZ k , natZ k ⟩).
    + apply con_in_nat.
    + cbv beta. rewrite proj2_triple !proj3_triple. apply IN_Sing.
  - cbv beta. rewrite pfst_Couple !psnd_Couple eval_con iUnion_pat3_Sing.
    apply In_P_Comp; [ apply IN_Sing | reflexivity ].
Qed.

(** ** [ex_Fun1_b = (x:int => x+1)[2]] evaluates to (contains) [3]. *)
Example ev_ex_Fun1_b env :
  ⟨ env , natZ 3 , natZ 3 ⟩ ∈ eval ex_Fun1_b env.
Proof.
  unfold ex_Fun1_b, ex_succ_b. rewrite eval_app.
  assert (Hfib : forall t, In t (eval (Eimg Enat) env) ->
            eval (Eadd (Evar 0) (Econ 1)) (env_cons (proj3 t) env)
            = {| ⟨ env_cons (proj3 t) env ,
                  natZAdd (proj3 t) (natZ 1) , natZAdd (proj3 t) (natZ 1) ⟩ |}).
  { intros t Ht.
    rewrite eval_add eval_var eval_con !iUnion_pat3_Sing
            env_lookup_cons_zero pfst_Couple. reflexivity. }
  assert (HfF : isFunction (t ← eval (Eimg Enat) env ;;
              {| ⟨ proj3 t , proj2 (⟨ env_cons (proj3 t) env ,
                     natZAdd (proj3 t) (natZ 1) , natZAdd (proj3 t) (natZ 1) ⟩ : ZFSet) ⟩ |})).
  { apply isFunction_graph. intros s s' Hs Hs' Heq. rewrite !proj2_triple Heq. reflexivity. }
  assert (HfG : isFunction (t ← eval (Eimg Enat) env ;;
              {| ⟨ proj2 t , proj3 (⟨ env_cons (proj3 t) env ,
                     natZAdd (proj3 t) (natZ 1) , natZAdd (proj3 t) (natZ 1) ⟩ : ZFSet) ⟩ |})).
  { apply isFunction_graph. intros s s' Hs Hs' Heq. rewrite !proj3_triple.
    rewrite -(Eimg_Enat_diag env s Hs) -(Eimg_Enat_diag env s' Hs') Heq. reflexivity. }
  rewrite (eval_lam_sing (Eimg Enat) (Eadd (Evar 0) (Econ 1)) env
             (fun t => ⟨ env_cons (proj3 t) env ,
                        natZAdd (proj3 t) (natZ 1) , natZAdd (proj3 t) (natZ 1) ⟩) Hfib HfF HfG).
  rewrite iUnion_pat3_Sing. unfold iUnion_pat.
  apply IN_iUnion with (y := ⟨ natZ 2 , natZ 3 ⟩).
  - apply IN_iUnion with (y := ⟨ env , natZ 2 , natZ 2 ⟩).
    + apply con_in_nat.
    + cbv beta. rewrite proj2_triple !proj3_triple natZAdd_natZ. cbn [Nat.add].
      apply IN_Sing.
  - cbv beta. rewrite pfst_Couple !psnd_Couple eval_con iUnion_pat3_Sing.
    apply In_P_Comp; [ apply IN_Sing | reflexivity ].
Qed.

(** * [poly_choice] vs [poly_tm] (RETRACTED under the [isFunction] filter).

    With the [Elam]/[Elamb] [isFunction] filter added in this refactor, a
    function value's forward graph must be single-valued.  Every
    function-valued cograph of [poly_choice] is then forced to the
    *diagonal*: at any [x]-value there is a singleton identity type
    [{⟨x,x⟩}] whose [t[0]] fails (it lacks [0]) so the [x] branch fires,
    pinning [x ↦ x]; hence no single-valued cograph can carry the [5 ↦ 0]
    edge.  So [poly_choice ⊆ poly_tm] now holds and
    [poly_choice_not_subset_poly_tm] is FALSE — the witness below is no
    longer a member of [eval poly_choice] (it is filtered out).  The proof
    is commented out pending a re-formulation (e.g. proving the subset).
*)
(*

(** The body of [poly_choice]. *)
Definition pc_body : Expr :=
  Echoice (Eseq (Eapp (Evar 1) (Econ 0)) (Econ 0)) (Evar 0).

(** The right branch [x] is always available: at any environment the
    diagonal value ⟨env', v, v⟩ (with [v] the slot-0 binding) is in the
    body. *)
Lemma pc_xbranch_in_body :
  forall env' : Env,
  ⟨ env' , env_lookup env' 0 , env_lookup env' 0 ⟩ ∈ eval pc_body env'.
Proof.
  intro env'. unfold pc_body. rewrite eval_choice. apply IN_BinUnion_r.
  rewrite eval_var iUnion_pat3_Sing. apply IN_Sing.
Qed.

(** At the [t := nat, x := 5] domain point the left branch [t[0]; 0]
    fires, so the non-identity value [0] is in the body. *)
Lemma pc_zerobranch_at_t0 :
  forall env : Env,
  ⟨ env_nat5 env , natZ 0 , natZ 0 ⟩ ∈ eval pc_body (env_nat5 env).
Proof.
  intro env. unfold pc_body. rewrite eval_poly_choice_body.
  apply IN_BinUnion_l. apply IN_Sing.
Qed.

Theorem poly_choice_not_subset_poly_tm :
  forall env : Env, ~ (eval poly_choice env ⊆ eval poly_tm env).
Proof.
  intro env. intro Hsub.
  (* Biased selection: pick the [0] body value where it exists (in
     particular at the [x=5] point), else the diagonal [x] value. *)
  pose (g := fun a : ZFSet =>
    match excluded_middle_informative
            (In (⟨ proj1 a , natZ 0 , natZ 0 ⟩) (eval pc_body (proj1 a))) with
    | left _  => ⟨ proj1 a , natZ 0 , natZ 0 ⟩
    | right _ => ⟨ proj1 a , env_lookup (proj1 a) 0 , env_lookup (proj1 a) 0 ⟩
    end).
  assert (Hg : forall a, In a (eval dom_flat env) ->
                 In (g a) (eval pc_body (proj1 a))).
  { intros a Ha. unfold g.
    destruct (excluded_middle_informative
                (In (⟨ proj1 a , natZ 0 , natZ 0 ⟩) (eval pc_body (proj1 a))))
      as [HP | HP].
    - exact HP.
    - apply pc_xbranch_in_body. }
  (* The corresponding [Pi] member and the witness function value. *)
  pose (h := iUnion (eval dom_flat env) (fun a => {| ⟨ a , g a ⟩ |})).
  assert (HhPi : In h (Pi (eval dom_flat env) (fun t => eval pc_body (proj1 t)))).
  { unfold h. apply iUnion_graph_mem_pi. exact Hg. }
  pose (v := ⟨ env ,
              iUnion_pat h (fun ab cd => {| ⟨ proj3 ab , proj2 cd ⟩ |}) ,
              iUnion_pat h (fun ab cd => {| ⟨ proj2 ab , proj3 cd ⟩ |}) ⟩ : ZFSet).
  assert (Hv : In v (eval poly_choice env)).
  { unfold v, poly_choice. rewrite eval_lam.
    apply IN_iUnion with (y := h).
    - exact HhPi.
    - cbv beta zeta. apply IN_Sing. }
  (* The witness's forward graph [proj2 v] has the non-diagonal edge ⟨5,0⟩. *)
  assert (Hedge : In (⟨ natZ 5 , natZ 0 ⟩) (proj2 v)).
  { unfold v. rewrite proj2_triple. unfold h.
    rewrite (iUnion_pat_graph (eval dom_flat env) g
               (fun ab cd => {| ⟨ proj3 ab , proj2 cd ⟩ |})).
    apply IN_iUnion with
      (y := ⟨ env_cons (natZ 5) (env_cons natId env) , natZ 5 , natZ 5 ⟩).
    - apply dom_flat_has_5.
    - cbv beta. rewrite proj3_triple.
      assert (Hgt0 : g (⟨ env_cons (natZ 5) (env_cons natId env) , natZ 5 , natZ 5 ⟩)
                     = ⟨ env_cons (natZ 5) (env_cons natId env) , natZ 0 , natZ 0 ⟩).
      { unfold g. rewrite proj1_triple.
        destruct (excluded_middle_informative _) as [HP | HP].
        - reflexivity.
        - exfalso. apply HP.
          change (env_cons (natZ 5) (env_cons natId env)) with (env_nat5 env).
          apply pc_zerobranch_at_t0. }
      rewrite Hgt0 proj2_triple. apply IN_Sing. }
  (* If [v] were a [poly_tm] value its forward graph would be diagonal. *)
  pose proof (Inc_IN _ _ _ Hsub Hv) as Hvt.
  pose proof (poly_tm_only_id env v Hvt) as Hdiag.
  pose proof (Hdiag _ _ Hedge) as Heq.
  exact (natZ_neq 5 0 ltac:(discriminate) Heq).
Qed.
*)


(** * With partial-identity types, [poly_ty] applied to [5] contains [5].

    Restricting [is_type] to partial *identity* functions (above) makes the
    type projection [:t] in [poly_ty]'s body well-behaved: at every domain
    point the type [t] is a subset of the diagonal and contains the bound
    value's reflexive pair ⟨x,x⟩.  The uniform *diagonal* selection then
    makes [poly_ty] produce the very same identity function [idDom] as
    [poly_tm], so [⟨env,5,5⟩] is among the results of [poly_ty[5]].

    (It is only *among* the results — not the whole result — because a
    non-singleton identity type like [natId] still lets [:t] range over its
    whole diagonal; see the discussion that motivated this restriction.) *)

(** Inversion for [:type]: a value of [Eimg Etype] is ⟨env, f, f⟩ for some
    partial identity [f]. *)
Lemma In_Eimg_Etype_inv :
  forall (env : Env) (u : ZFSet),
  u ∈ eval (Eimg Etype) env ->
  exists f, f ∈ pIdFun Big /\ u = ⟨ env , f , f ⟩.
Proof.
  intros env u Hu. rewrite eval_img in Hu.
  change (eval Etype env) with ({| ⟨ env , is_type , is_type ⟩ |}) in Hu.
  rewrite iUnion_pat3_Sing in Hu. unfold iUnion_pat, is_type, diag in Hu.
  apply iUnion_IN in Hu. destruct Hu as [p [Hp Hu]].
  apply iUnion_IN in Hp. destruct Hp as [f [Hf Hpf]].
  cbv beta in Hpf. apply IN_Sing_EQ in Hpf. subst p.
  cbv beta in Hu. rewrite pfst_Couple psnd_Couple in Hu.
  apply IN_Sing_EQ in Hu. subst u.
  exists f. split; [ exact Hf | reflexivity ].
Qed.

(** Inversion for [:(Evar i)] when the projected type is a partial
    identity: the value is diagonal and its reflexive pair is in the type. *)
Lemma In_Eimg_pId_diag :
  forall (env' : Env) (i : nat) (u : ZFSet),
  env_lookup env' i ⊆ anyId ->
  u ∈ eval (Eimg (Evar i)) env' ->
  proj1 u = env' /\ ⟨ proj3 u , proj3 u ⟩ ∈ env_lookup env' i.
Proof.
  intros env' i u Hpid Hu. rewrite eval_img in Hu.
  change (eval (Evar i) env')
    with ({| ⟨ env' , env_lookup env' i , env_lookup env' i ⟩ |}) in Hu.
  rewrite iUnion_pat3_Sing in Hu. unfold iUnion_pat in Hu.
  apply iUnion_IN in Hu. destruct Hu as [p [Hp Hu]].
  cbv beta in Hu. apply IN_Sing_EQ in Hu. subst u.
  pose proof (Inc_IN _ _ _ Hpid Hp) as Hpany.
  unfold anyId, diag in Hpany. apply iUnion_IN in Hpany. destruct Hpany as [c [_ Hpc]].
  apply IN_Sing_EQ in Hpc. subst p.
  split.
  - rewrite proj1_triple. reflexivity.
  - rewrite !proj3_triple !psnd_Couple. exact Hp.
Qed.

(** Every [dom_flat] domain point's type (slot 1) is a partial identity
    containing the reflexive pair of the bound value [proj3 a]. *)
Lemma dom_flat_type_diag :
  forall (env : Env) (a : ZFSet),
  a ∈ eval dom_flat env ->
  ⟨ proj3 a , proj3 a ⟩ ∈ env_lookup (proj1 a) 1.
Proof.
  intros env a Ha. unfold dom_flat in Ha.
  rewrite eval_seq in Ha. unfold iUnion_pat3 in Ha.
  apply iUnion_IN in Ha. destruct Ha as [t1 [Ht1 Ha]]. cbv beta in Ha.
  rewrite eval_assign in Ht1. unfold iUnion_pat3 in Ht1.
  apply iUnion_IN in Ht1. destruct Ht1 as [u1 [Hu1 Ht1]]. cbv beta in Ht1.
  apply IN_Sing_EQ in Ht1.
  destruct (In_Eimg_Etype_inv _ _ Hu1) as [f [Hf Hu1eq]]. subst u1.
  rewrite proj1_triple proj2_triple proj3_triple in Ht1. subst t1.
  rewrite proj1_triple in Ha.
  rewrite eval_seq in Ha. unfold iUnion_pat3 in Ha.
  apply iUnion_IN in Ha. destruct Ha as [t2 [Ht2 Ha]]. cbv beta in Ha.
  rewrite eval_assign in Ht2. unfold iUnion_pat3 in Ht2.
  apply iUnion_IN in Ht2. destruct Ht2 as [u2 [Hu2 Ht2]]. cbv beta in Ht2.
  apply IN_Sing_EQ in Ht2.
  assert (Hfid : Inc (env_lookup (env_cons f env) 0) anyId).
  { rewrite env_lookup_cons_zero pfst_Couple.
    unfold pIdFun in Hf. exact (IN_Power_Inc _ _ Hf). }
  destruct (In_Eimg_pId_diag _ 0 _ Hfid Hu2) as [Hu2p1 Hu2diag].
  rewrite env_lookup_cons_zero pfst_Couple in Hu2diag.
  rewrite Hu2p1 in Ht2. subst t2.
  rewrite proj1_triple in Ha. rewrite eval_var in Ha.
  apply IN_Sing_EQ in Ha. subst a.
  rewrite proj1_triple !proj3_triple.
  rewrite env_lookup_cons_succ psnd_Couple.
  rewrite !env_lookup_cons_zero !pfst_Couple.
  exact Hu2diag.
Qed.

(** Hence the *diagonal* selection is a valid [Pi] member for [poly_ty]'s
    body, and the resulting function value is exactly [poly_tm]'s identity
    [⟨env, idDom, idDom⟩]. *)
Lemma idDom_in_poly_ty :
  forall env : Env, ⟨ env , idDom env , idDom env ⟩ ∈ eval poly_ty env.
Proof.
  intro env. unfold poly_ty. rewrite eval_lamb.
  apply IN_iUnion with
    (y := iUnion (eval dom_flat env)
            (fun a => {| ⟨ a , ⟨ proj1 a , proj3 a , proj3 a ⟩ ⟩ |})).
  - apply (iUnion_graph_mem_pi (eval dom_flat env)
             (fun t => eval (Eimg (Evar 1)) (proj1 t))
             (fun a => ⟨ proj1 a , proj3 a , proj3 a ⟩)).
    intros a Ha. apply In_Eimg_Evar. exact (dom_flat_type_diag env a Ha).
  - cbv beta zeta.
    rewrite !(iUnion_pat_graph (eval dom_flat env)
                (fun a => ⟨ proj1 a , proj3 a , proj3 a ⟩)).
    cbv beta.
    assert (Hf : (a ← eval dom_flat env ;;
                    {| ⟨ proj3 a , proj2 (⟨ proj1 a , proj3 a , proj3 a ⟩ : ZFSet) ⟩ |})
                 = idDom env).
    { unfold idDom. apply iUnion_ext_mem. intros a Ha. rewrite proj2_triple. reflexivity. }
    assert (Hg : (a ← eval dom_flat env ;;
                    {| ⟨ proj2 a , proj3 (⟨ proj1 a , proj3 a , proj3 a ⟩ : ZFSet) ⟩ |})
                 = idDom env).
    { unfold idDom. apply iUnion_ext_mem. intros a Ha.
      rewrite proj3_triple (dom_flat_diag env a Ha). reflexivity. }
    rewrite Hf Hg.
    apply In_P_Comp; [ apply IN_Sing | split; apply isFunction_idDom ].
Qed.

Definition poly_ty_5 : Expr := Eapp poly_ty (Econ 5).

(** [poly_ty] applied to [5] *contains* [5]. *)
Theorem poly_ty_5_contains_5 :
  forall env : Env, ⟨ env , natZ 5 , natZ 5 ⟩ ∈ eval poly_ty_5 env.
Proof.
  intro env. unfold poly_ty_5. rewrite eval_app. unfold iUnion_pat3.
  apply IN_iUnion with (y := ⟨ env , idDom env , idDom env ⟩).
  - apply idDom_in_poly_ty.
  - cbv beta. rewrite proj1_triple proj3_triple.
    unfold iUnion_pat.
    apply IN_iUnion with (y := ⟨ natZ 5 , natZ 5 ⟩).
    + unfold idDom.
      apply IN_iUnion with
        (y := ⟨ env_cons (natZ 5) (env_cons natId env) , natZ 5 , natZ 5 ⟩).
      * apply dom_flat_has_5.
      * cbv beta. rewrite proj3_triple. apply IN_Sing.
    + cbv beta. rewrite pfst_Couple !psnd_Couple.
      change (eval (Econ 5) env) with ({| ⟨ env , natZ 5 , natZ 5 ⟩ |}).
      rewrite iUnion_Sing_l proj1_triple proj2_triple.
      apply In_P_Comp; [ apply IN_Sing | reflexivity ].
Qed.

(** * [poly_ty[5]] also contains [3] (RETRACTED under the [isFunction]
    filter).

    Before the filter, [poly_ty]'s body [:t] was multi-valued, so a
    selection could map [5 ↦ 3] and [⟨env,3,3⟩] was reachable.  With the
    [isFunction] filter every function value is single-valued, and a
    singleton identity type [{⟨5,5⟩}] (which does not contain [3]) forces
    any value's graph to map [5 ↦ 5]; hence no function value maps [5 ↦ 3]
    and [poly_ty[5] = {5}].  So this theorem is now FALSE; its proof is
    commented out.  (Dually, [poly_ty_5_contains_5] above still holds.)
*)
(*
(** * [poly_ty[5]] also contains [3] — it is NOT just {5}.

    The negation "⟨env,3,3⟩ ∉ poly_ty_5" is FALSE: [natId] is a valid
    partial-identity type, and at the domain point [t := natId, x := 5] the
    body [:t] projects the whole of [natId], so it can output [3].  A
    selection that takes that output yields the forward-graph edge ⟨5,3⟩,
    hence the result ⟨env,3,3⟩.  (Witness to the "contains, not equals"
    remark on [poly_ty_5_contains_5].) *)
Theorem poly_ty_5_contains_3 :
  forall env : Env, ⟨ env , natZ 3 , natZ 3 ⟩ ∈ eval poly_ty_5 env.
Proof.
  intro env.
  (* Biased selection: output [3] where the type admits it (the nat-typed
     [x=5] point), else the always-available diagonal output. *)
  pose (g := fun a : ZFSet =>
    match excluded_middle_informative
            (In (⟨ proj1 a , natZ 3 , natZ 3 ⟩) (eval (Eimg (Evar 1)) (proj1 a))) with
    | left _  => ⟨ proj1 a , natZ 3 , natZ 3 ⟩
    | right _ => ⟨ proj1 a , proj3 a , proj3 a ⟩
    end).
  assert (Hg : forall a, In a (eval dom_flat env) ->
                 In (g a) (eval (Eimg (Evar 1)) (proj1 a))).
  { intros a Ha. unfold g.
    destruct (excluded_middle_informative
                (In (⟨ proj1 a , natZ 3 , natZ 3 ⟩) (eval (Eimg (Evar 1)) (proj1 a))))
      as [HP | HP].
    - exact HP.
    - apply In_Eimg_Evar. exact (dom_flat_type_diag env a Ha). }
  pose (h := iUnion (eval dom_flat env) (fun a => {| ⟨ a , g a ⟩ |})).
  assert (HhPi : In h (Pi (eval dom_flat env) (fun t => eval (Eimg (Evar 1)) (proj1 t)))).
  { unfold h. apply iUnion_graph_mem_pi. exact Hg. }
  pose (gH := iUnion_pat h (fun ab cd => {| ⟨ proj2 ab , proj3 cd ⟩ |})).
  assert (Hv : In (⟨ env ,
                    iUnion_pat h (fun ab cd => {| ⟨ proj3 ab , proj2 cd ⟩ |}) ,
                    gH ⟩) (eval poly_ty env)).
  { unfold poly_ty. rewrite eval_lam. apply IN_iUnion with (y := h).
    - exact HhPi.
    - cbv beta zeta. apply IN_Sing. }
  assert (Hgt0 : g (⟨ env_cons (natZ 5) (env_cons natId env) , natZ 5 , natZ 5 ⟩)
                 = ⟨ env_cons (natZ 5) (env_cons natId env) , natZ 3 , natZ 3 ⟩).
  { unfold g. rewrite proj1_triple.
    destruct (excluded_middle_informative _) as [HP | HP].
    - reflexivity.
    - exfalso. apply HP. apply In_Eimg_Evar.
      rewrite env_lookup_cons_succ psnd_Couple env_lookup_cons_zero pfst_Couple.
      apply pair_self_mem_natId. }
  assert (Hedge : In (⟨ natZ 5 , natZ 3 ⟩) gH).
  { unfold gH, h.
    rewrite (iUnion_pat_graph (eval dom_flat env) g
               (fun ab cd => {| ⟨ proj2 ab , proj3 cd ⟩ |})).
    apply IN_iUnion with
      (y := ⟨ env_cons (natZ 5) (env_cons natId env) , natZ 5 , natZ 5 ⟩).
    - apply dom_flat_has_5.
    - cbv beta. rewrite proj2_triple Hgt0 proj3_triple. apply IN_Sing. }
  unfold poly_ty_5. rewrite eval_app. unfold iUnion_pat3.
  apply IN_iUnion with
    (y := ⟨ env ,
            iUnion_pat h (fun ab cd => {| ⟨ proj3 ab , proj2 cd ⟩ |}) , gH ⟩).
  - exact Hv.
  - cbv beta. rewrite proj1_triple proj3_triple.
    unfold iUnion_pat.
    apply IN_iUnion with (y := ⟨ natZ 5 , natZ 3 ⟩).
    + exact Hedge.
    + cbv beta. rewrite pfst_Couple !psnd_Couple.
      change (eval (Econ 5) env) with ({| ⟨ env , natZ 5 , natZ 5 ⟩ |}).
      rewrite iUnion_Sing_l proj1_triple proj2_triple.
      apply In_P_Comp; [ apply IN_Sing | reflexivity ].
Qed.
*)

(* ================================================================== *)
(** * Now-true replacement: [poly_ty[5] = {5}] exactly. *)
(* ================================================================== *)

(** Any selection [h] over [poly_ty]'s domain has the diagonal edge ⟨5,5⟩
    in its forward graph: the singleton identity type [{⟨5,5⟩}] is a valid
    domain point, where the body [:t] is forced to the single value [5]. *)
Lemma poly_ty_g_has : forall (env : Env) (h : ZFSet),
  h ∈ Pi (eval dom_flat env) (fun t => eval (Eimg (Evar 1)) (proj1 t)) ->
  ⟨ natZ 5 , natZ 5 ⟩ ∈ '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj2 ab , proj3 cd ⟩ |}.
Proof.
  intros env h Hh.
  pose proof (dom_flat_has_sing env (natZ 5) (natZ_small 5)) as Hab.
  apply In_Pi_inv in Hh. destruct Hh as [_ [Htot _]].
  destruct (Htot _ Hab) as [cd [Hcd Hedge]].
  assert (Hl : env_lookup
      (env_cons (natZ 5) (env_cons {| ⟨ natZ 5 , natZ 5 ⟩ |} env)) 1
      = {| ⟨ natZ 5 , natZ 5 ⟩ |}).
  { rewrite env_lookup_cons_succ psnd_Couple env_lookup_cons_zero pfst_Couple. reflexivity. }
  rewrite proj1_triple in Hcd. rewrite (body_at_sing _ _ Hl) in Hcd.
  apply IN_Sing_EQ in Hcd. subst cd.
  unfold iUnion_pat.
  apply IN_iUnion with
    (y := ⟨ ⟨ env_cons (natZ 5) (env_cons {| ⟨ natZ 5 , natZ 5 ⟩ |} env) , natZ 5 , natZ 5 ⟩ ,
            ⟨ env_cons (natZ 5) (env_cons {| ⟨ natZ 5 , natZ 5 ⟩ |} env) , natZ 5 , natZ 5 ⟩ ⟩).
  - exact Hedge.
  - cbv beta. rewrite pfst_Couple psnd_Couple proj2_triple proj3_triple. apply IN_Sing.
Qed.

(** Soundness: every value of [poly_ty[5]] is [⟨env,5,5⟩].  A [poly_ty]
    value's forward graph [g] is single-valued (the [isFunction] filter)
    and contains ⟨5,5⟩ ([poly_ty_g_has]); so its only edge out of [5] is
    [5]. *)
Lemma poly_ty_5_only_5 :
  forall (env : Env) (x : ZFSet),
  x ∈ eval poly_ty_5 env -> x = ⟨ env , natZ 5 , natZ 5 ⟩.
Proof.
  intros env x Hx. unfold poly_ty_5 in Hx. rewrite eval_app in Hx.
  unfold iUnion_pat3 in Hx.
  apply iUnion_IN in Hx. destruct Hx as [V [HV Hx]]. cbv beta in Hx.
  unfold poly_ty in HV. rewrite eval_lamb in HV.
  apply iUnion_IN in HV. destruct HV as [h [Hh HV]]. cbv beta zeta in HV.
  pose proof (In_Comp_P _ _ _ HV) as Hfuns. destruct Hfuns as [_ HgFun].
  apply (Inc_IN _ _ _ (Comp_Inc _ _)) in HV. apply IN_Sing_EQ in HV. subst V.
  rewrite proj1_triple proj3_triple in Hx.
  unfold iUnion_pat in Hx. apply iUnion_IN in Hx. destruct Hx as [p [Hp Hx]].
  cbv beta in Hx. rewrite eval_con iUnion_Sing_l proj1_triple proj2_triple in Hx.
  pose proof (In_Comp_P _ _ _ Hx) as Hka.
  apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hx. apply IN_Sing_EQ in Hx. subst x.
  (* recover the edge [p] as a pair *)
  apply iUnion_IN in Hp. destruct Hp as [pr [Hpr Hp]].
  cbv beta in Hp. apply IN_Sing_EQ in Hp. subst p.
  rewrite pfst_Couple in Hka. rewrite psnd_Couple.
  (* goal: ⟨env, proj3 (psnd pr), proj3 (psnd pr)⟩ = ⟨env,5,5⟩ *)
  assert (Hpg : In (⟨ natZ 5 , proj3 (psnd pr) ⟩)
                   ('⟨ ab , cd ⟩ ← h ;; {| ⟨ proj2 ab , proj3 cd ⟩ |})).
  { rewrite <- Hka. unfold iUnion_pat.
    apply IN_iUnion with (y := pr); [ exact Hpr |].
    cbv beta. apply IN_Sing. }
  pose proof (poly_ty_g_has env h Hh) as Hg55.
  pose proof (HgFun (natZ 5) (proj3 (psnd pr)) (natZ 5) Hpg Hg55) as Hva.
  rewrite Hva. reflexivity.
Qed.

(** Hence [poly_ty[5]] is exactly [{⟨env,5,5⟩}] — the [isFunction] filter
    collapses [poly_ty] to the identity, so it agrees with [poly_tm[5]]. *)
Theorem poly_ty_5_eval :
  forall env : Env, eval poly_ty_5 env = {| ⟨ env , natZ 5 , natZ 5 ⟩ |}.
Proof.
  intro env. apply set_ext; apply Inc_def; intros x Hx.
  - rewrite (poly_ty_5_only_5 env x Hx). apply IN_Sing.
  - apply IN_Sing_EQ in Hx. subst x. apply poly_ty_5_contains_5.
Qed.

(* ================================================================== *)
(** * [poly_tm = poly_ty]: both collapse to the identity. *)
(* ================================================================== *)

(** The type at slot 1 of a [dom_flat] domain point is a partial identity
    (a subset of the diagonal [anyId]). *)
Lemma dom_flat_type_pId :
  forall (env : Env) (a : ZFSet),
  a ∈ eval dom_flat env -> env_lookup (proj1 a) 1 ⊆ anyId.
Proof.
  intros env a Ha. unfold dom_flat in Ha.
  rewrite eval_seq in Ha. unfold iUnion_pat3 in Ha.
  apply iUnion_IN in Ha. destruct Ha as [t1 [Ht1 Ha]]. cbv beta in Ha.
  rewrite eval_assign in Ht1. unfold iUnion_pat3 in Ht1.
  apply iUnion_IN in Ht1. destruct Ht1 as [u1 [Hu1 Ht1]]. cbv beta in Ht1.
  apply IN_Sing_EQ in Ht1.
  destruct (In_Eimg_Etype_inv _ _ Hu1) as [f [Hf Hu1eq]]. subst u1.
  rewrite proj1_triple proj2_triple proj3_triple in Ht1. subst t1.
  rewrite proj1_triple in Ha.
  rewrite eval_seq in Ha. unfold iUnion_pat3 in Ha.
  apply iUnion_IN in Ha. destruct Ha as [t2 [Ht2 Ha]]. cbv beta in Ha.
  rewrite eval_assign in Ht2. unfold iUnion_pat3 in Ht2.
  apply iUnion_IN in Ht2. destruct Ht2 as [u2 [Hu2 Ht2]]. cbv beta in Ht2.
  apply IN_Sing_EQ in Ht2.
  assert (Hfid : Inc (env_lookup (env_cons f env) 0) anyId).
  { rewrite env_lookup_cons_zero pfst_Couple.
    unfold pIdFun in Hf. exact (IN_Power_Inc _ _ Hf). }
  destruct (In_Eimg_pId_diag _ 0 _ Hfid Hu2) as [Hu2p1 _].
  rewrite Hu2p1 in Ht2. subst t2.
  rewrite proj1_triple in Ha. rewrite eval_var in Ha.
  apply IN_Sing_EQ in Ha. subst a.
  rewrite proj1_triple. rewrite env_lookup_cons_succ psnd_Couple. exact Hfid.
Qed.

(** Hence every [dom_flat] domain value is small. *)
Lemma dom_flat_val_small :
  forall (env : Env) (a : ZFSet),
  a ∈ eval dom_flat env -> proj3 a ∈ Big.
Proof.
  intros env a Ha.
  pose proof (Inc_IN _ _ _ (dom_flat_type_pId env a Ha) (dom_flat_type_diag env a Ha)) as Hany.
  unfold anyId, diag in Hany. apply iUnion_IN in Hany. destruct Hany as [c [Hc Hcc]].
  apply IN_Sing_EQ in Hcc.
  pose proof (Couple_inj_left _ _ _ _ Hcc) as E. rewrite E. exact Hc.
Qed.

(** For any small [x], both projected relations of any selection over
    [poly_ty]'s domain contain the diagonal edge ⟨x,x⟩ — read off the
    singleton-type point [{⟨x,x⟩}], where the body [:t] is forced to [x]. *)
Lemma poly_ty_fg_diag : forall (env : Env) (h : ZFSet) (x : ZFSet),
  x ∈ Big ->
  h ∈ Pi (eval dom_flat env) (fun t => eval (Eimg (Evar 1)) (proj1 t)) ->
  ⟨ x , x ⟩ ∈ '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj3 ab , proj2 cd ⟩ |} /\
  ⟨ x , x ⟩ ∈ '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj2 ab , proj3 cd ⟩ |}.
Proof.
  intros env h x Hx Hh.
  pose proof (dom_flat_has_sing env x Hx) as Hab.
  apply In_Pi_inv in Hh. destruct Hh as [_ [Htot _]].
  destruct (Htot _ Hab) as [cd [Hcd Hedge]].
  assert (Hl : env_lookup (env_cons x (env_cons {| ⟨ x , x ⟩ |} env)) 1
               = {| ⟨ x , x ⟩ |}).
  { rewrite env_lookup_cons_succ psnd_Couple env_lookup_cons_zero pfst_Couple. reflexivity. }
  rewrite proj1_triple in Hcd. rewrite (body_at_sing _ _ Hl) in Hcd.
  apply IN_Sing_EQ in Hcd. subst cd.
  split; unfold iUnion_pat;
    apply IN_iUnion with
      (y := ⟨ ⟨ env_cons x (env_cons {| ⟨ x , x ⟩ |} env) , x , x ⟩ ,
              ⟨ env_cons x (env_cons {| ⟨ x , x ⟩ |} env) , x , x ⟩ ⟩);
    try exact Hedge; cbv beta;
    rewrite pfst_Couple psnd_Couple proj2_triple proj3_triple; apply IN_Sing.
Qed.

(** Soundness for [poly_ty]: every value collapses to the polymorphic
    identity.  The body [:t] only ever yields diagonal pairs (types are
    partial identities), so the [isFunction] filter pins both projected
    relations to [idDom]. *)
Lemma poly_ty_value_idDom :
  forall (env : Env) (elt : ZFSet),
  elt ∈ eval poly_ty env -> elt = ⟨ env , idDom env , idDom env ⟩.
Proof.
  intros env elt Helt.
  unfold poly_ty in Helt. rewrite eval_lamb in Helt.
  apply iUnion_IN in Helt. destruct Helt as [h [Hh Helt]]. cbv beta zeta in Helt.
  pose proof (Inc_IN _ _ _ (Comp_Inc _ _) Helt) as HeltS.
  apply In_Comp_P in Helt. destruct Helt as [HfFun HgFun].
  apply IN_Sing_EQ in HeltS. subst elt.
  assert (Hfid : ('⟨ ab , cd ⟩ ← h ;; {| ⟨ proj3 ab , proj2 cd ⟩ |}) = idDom env).
  { apply set_ext; apply Inc_def; intros e He.
    - pose proof He as Hecopy.
      unfold iUnion_pat in He. apply iUnion_IN in He. destruct He as [pr [Hpr He]].
      cbv beta in He. apply IN_Sing_EQ in He.
      pose proof Hh as HhPi. apply In_Pi_inv in HhPi. destruct HhPi as [Hsub _].
      pose proof (Inc_IN _ _ _ Hsub Hpr) as HprP.
      apply IN_Prod_EX in HprP. destruct HprP as [pa [pc [Hpa [_ Hpreq]]]].
      subst pr. rewrite pfst_Couple psnd_Couple in He. subst e.
      destruct (poly_ty_fg_diag env h (proj3 pa) (dom_flat_val_small env pa Hpa) Hh) as [Hd _].
      pose proof (HfFun (proj3 pa) (proj2 pc) (proj3 pa) Hecopy Hd) as Heq.
      rewrite Heq. unfold idDom. apply IN_iUnion with (y := pa); [exact Hpa | apply IN_Sing].
    - unfold idDom in He. apply iUnion_IN in He. destruct He as [t [Ht He]].
      cbv beta in He. apply IN_Sing_EQ in He. subst e.
      destruct (poly_ty_fg_diag env h (proj3 t) (dom_flat_val_small env t Ht) Hh) as [Hd _].
      exact Hd. }
  assert (Hgid : ('⟨ ab , cd ⟩ ← h ;; {| ⟨ proj2 ab , proj3 cd ⟩ |}) = idDom env).
  { apply set_ext; apply Inc_def; intros e He.
    - pose proof He as Hecopy.
      unfold iUnion_pat in He. apply iUnion_IN in He. destruct He as [pr [Hpr He]].
      cbv beta in He. apply IN_Sing_EQ in He.
      pose proof Hh as HhPi. apply In_Pi_inv in HhPi. destruct HhPi as [Hsub _].
      pose proof (Inc_IN _ _ _ Hsub Hpr) as HprP.
      apply IN_Prod_EX in HprP. destruct HprP as [pa [pc [Hpa [_ Hpreq]]]].
      subst pr. rewrite pfst_Couple psnd_Couple in He. subst e.
      pose proof (dom_flat_diag env pa Hpa) as Hdg.
      rewrite Hdg in Hecopy |- *.
      destruct (poly_ty_fg_diag env h (proj3 pa) (dom_flat_val_small env pa Hpa) Hh) as [_ Hd].
      pose proof (HgFun (proj3 pa) (proj3 pc) (proj3 pa) Hecopy Hd) as Heq.
      rewrite Heq. unfold idDom. apply IN_iUnion with (y := pa); [exact Hpa | apply IN_Sing].
    - unfold idDom in He. apply iUnion_IN in He. destruct He as [t [Ht He]].
      cbv beta in He. apply IN_Sing_EQ in He. subst e.
      destruct (poly_ty_fg_diag env h (proj3 t) (dom_flat_val_small env t Ht) Hh) as [_ Hd].
      exact Hd. }
  rewrite Hfid Hgid. reflexivity.
Qed.

(** Hence [poly_ty] denotes exactly the polymorphic identity. *)
Lemma poly_ty_eval :
  forall env : Env, eval poly_ty env = {| ⟨ env , idDom env , idDom env ⟩ |}.
Proof.
  intros env. apply set_ext; apply Inc_def; intros e He.
  - rewrite (poly_ty_value_idDom env e He). apply IN_Sing.
  - apply IN_Sing_EQ in He. subst e. apply idDom_in_poly_ty.
Qed.

(** [poly_tm] and [poly_ty] denote the same set: under the [isFunction]
    filter both the term-level and type-level polymorphic identities
    collapse to the single function [idDom]. *)
Theorem poly_tm_eq_poly_ty :
  forall env : Env, eval poly_tm env = eval poly_ty env.
Proof.
  intros env. rewrite poly_tm_eval poly_ty_eval. reflexivity.
Qed.
