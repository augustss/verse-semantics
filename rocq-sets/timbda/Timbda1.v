(* Timbda1.v

   A pair-set semantics over [Syntax.Expr] in the quotient set theory
   [ZFSet].  Each [Expr] evaluates, in a given environment, to a set of
   ordered pairs ⟨a,b⟩.  This mirrors the original [Ens]-level
   development but is retargeted to [ZFSet] (where equality is Leibniz
   [=]) and to the shared source language of [Syntax.v], so that the
   STLC typing judgement can be interpreted against it. *)

From Stdlib Require Import ssreflect.

Require Import ZFSet.
Require Import ZFNotation.
Require Import Syntax.


(* graph h reads h as a relation: each element of h is a pair ⟨ab, cd⟩
   where ab, cd are themselves pairs; graph extracts ⟨psnd ab, pfst cd⟩
   for each such element.  This is exactly the [sndRel] computed inside
   the [Elam] arm of [eval]. *)

Definition graph (h : ZFSet) : ZFSet :=
  '⟨ ab , cd ⟩ ← h ;; {| ⟨ psnd ab , pfst cd ⟩ |}.

(* The set of partial functions from [A] to [B]: the single-valued
   ([isFunction]) subsets of the product [A × B]. *)
Definition pFun (A B : ZFSet) : ZFSet :=
  ⦃ G ∈ Power (Prod A B) | isFunction G ⦄.

(* The identity relation on [pFun Big Big], the partial functions on the
   universe: the diagonal {⟨f,f⟩ : f ∈ pFun Big Big}.  It is to the
   function space what [anyId] is to [Big].  [Etype] denotes the *value*
   ⟨is_type, is_type⟩, and the projection [:type] = [Eimg Etype] denotes
   [is_type] itself — the set of (diagonally-packed) function values. *)
Definition is_type : ZFSet :=
  f ← pFun Big Big ;; {| ⟨ f , f ⟩ |}.

(* The identity relation on the universe [Big]: the diagonal
   {⟨a,a⟩ : a ∈ Big}.  Read as a relation it sends each [a ∈ Big] to
   itself, so it is the graph of the total identity function on [Big].
   [Eany] denotes the *value* ⟨anyId, anyId⟩ (forward graph and cograph
   both [anyId]); applying it to any value whose payload lies in [Big]
   returns that value unchanged (see [eval_any_app]). *)
Definition anyId : ZFSet :=
  f ← Big ;; {| ⟨ f , f ⟩ |}.


(** ** Evaluator  ***)

Fixpoint eval (e : Expr) (rho : Env) {struct e} : ZFSet :=
  match e with
  | Econ k =>
      {| ⟨ k , k ⟩ |}
  | Evar x =>
      {| ⟨ rho x , rho x ⟩ |}
  | Enat =>
      {| ⟨ natId , natId ⟩ |}
  | Eimg e1 =>
      (* projection: flatten the second component of each value *)
      '⟨ _ , f ⟩ ← eval e1 rho ;; f
  | Elam e1 e2 =>
      h ← Π[ ab ∈ eval e1 rho ] eval e2 (env_ext rho (psnd ab)) ;;
      let f := '⟨ ab , cd ⟩ ← h ;; {| ⟨ psnd ab , pfst cd ⟩ |} in
      let g := '⟨ ab , cd ⟩ ← h ;; {| ⟨ pfst ab , psnd cd ⟩ |} in
      ⦃ _ ∈ {| ⟨ f , g ⟩ |} | isFunction f /\ isFunction g ⦄
  | Eapp e1 e2 =>
      '⟨ _ , f ⟩  ← eval e1 rho ;;
      '⟨ a , b ⟩  ← f ;;
      '⟨ _ , a1 ⟩ ← eval e2 rho ;;
        ⦃ _ ∈ {| ⟨ b , b ⟩ |} | a = a1 ⦄
  | Eadd e1 e2 =>
      '⟨ _ , v1 ⟩ ← eval e1 rho ;;
      '⟨ _ , v2⟩ ← eval e2 rho ;;
        {| ⟨ v1 + v2 , v1 + v2 ⟩ |}
  | Efail =>
      ∅
  | Echoice e1 e2 =>
      eval e1 rho ∪ eval e2 rho
  | Eequal e1 e2 =>
      eval e1 rho ∩ eval e2 rho
  
  | Eany =>  {| ⟨ anyId , anyId ⟩ |}

  | Etype => {| ⟨ is_type , is_type ⟩ |}

  | _ => ∅
  end.

(*** Equation lemmas ***)

Theorem eval_con :
  forall (k : nat) (rho : Env),
  eval (Econ k) rho = {| ⟨ k , k ⟩ |}.
Proof. reflexivity. Qed.

Theorem eval_var :
  forall (x : nat) (rho : Env),
  eval (Evar x) rho = {| ⟨ rho x , rho x ⟩ |}.
Proof. reflexivity. Qed.

Theorem eval_nat :
  forall rho : Env, eval Enat rho = {| ⟨ natId , natId ⟩ |}.
Proof. reflexivity. Qed.

Theorem eval_img :
  forall (e : Expr) (rho : Env),
  eval (Eimg e) rho = '⟨ _ , f ⟩ ← eval e rho ;; f.
Proof. reflexivity. Qed.

Theorem eval_lam :
  forall (e1 e2 : Expr) (rho : Env),
  eval (Elam e1 e2) rho
  = h ← Π[ ab ∈ eval e1 rho ] eval e2 (env_ext rho (psnd ab)) ;;
    let f := '⟨ ab , cd ⟩ ← h ;; {| ⟨ psnd ab , pfst cd ⟩ |} in
    let g := '⟨ ab , cd ⟩ ← h ;; {| ⟨ pfst ab , psnd cd ⟩ |} in
    ⦃ _ ∈ {| ⟨ f , g ⟩ |} | isFunction f /\ isFunction g ⦄.
Proof. reflexivity. Qed.

Theorem eval_app :
  forall (e1 e2 : Expr) (rho : Env),
  eval (Eapp e1 e2) rho
  = '⟨ _ , f ⟩  ← eval e1 rho ;;
    '⟨ a , b ⟩  ← f ;;
    '⟨ _ , a1 ⟩ ← eval e2 rho ;;
      ⦃ _ ∈ {| ⟨ b , b ⟩ |} | a = a1 ⦄.
Proof. reflexivity. Qed.

Theorem eval_add :
  forall (e1 e2 : Expr) (rho : Env),
  eval (Eadd e1 e2) rho
  = v1 ← eval e1 rho ;;
    v2 ← eval e2 rho ;;
      {| ⟨ psnd v1 + psnd v2 , psnd v1 + psnd v2 ⟩ |}.
Proof. reflexivity. Qed.

Theorem eval_fail : forall rho : Env, eval Efail rho = ∅.
Proof. reflexivity. Qed.

Theorem eval_choice :
  forall (e1 e2 : Expr) (rho : Env),
  eval (Echoice e1 e2) rho = eval e1 rho ∪ eval e2 rho.
Proof. reflexivity. Qed.

Theorem eval_equal :
  forall (e1 e2 : Expr) (rho : Env),
  eval (Eequal e1 e2) rho = eval e1 rho ∩ eval e2 rho.
Proof. reflexivity. Qed.

Theorem eval_any :
  forall rho : Env, eval Eany rho = {| ⟨ anyId , anyId ⟩ |}.
Proof. reflexivity. Qed.

Theorem eval_type :
  forall rho : Env, eval Etype rho = {| ⟨ is_type , is_type ⟩ |}.
Proof. reflexivity. Qed.

(** * Evaluation of the [Syntax] example expressions.

    [Timbda1] is a *pair* semantics: a value is a set of pairs ⟨a,b⟩.  We
    record the denotations of the [Syntax] examples whose constructors are
    interpreted here — the ground fragment (constants, addition,
    unification, choice).  The binder/application examples ([Elam] /
    [Eapp]) have intricate denotations in this semantics (the [Elam] arm
    builds a graph/cograph pair guarded by [isFunction]) and are proved
    only in [Timbda0].  [Eany] and [Etype] *are* interpreted (the
    polymorphic identity on [Big], resp. on the function space; see the
    sections below); [Ebind] / [Elet] / [Eassign] / [Eseq] are
    uninterpreted here and denote [∅]. *)

(* The pair-pattern reduction [iUnion_pat_Sing_Couple], the diagonal-pair
   distinctness lemma [Couple_diag_neq], and the [zf_reduce] normaliser are
   shared across the interpreters and live in [ZFSetFacts.v]. *)
Ltac step1 := cbn [eval]; zf_reduce; try reflexivity.

(* ex_EV1 = Econ 2 *)
Example ev_ex_EV1 rho : eval ex_EV1 rho = {| ⟨ 2 , 2 ⟩ |}.
Proof. reflexivity. Qed.

(* ex_Arith1 = 3+4 *)
Example ev_ex_Arith1 rho : eval ex_Arith1 rho = {| ⟨ 7 , 7 ⟩ |}.
Proof. unfold ex_Arith1. step1. Qed.

(* ex_multiline = 1+2+3 *)
Example ev_ex_multiline rho : eval ex_multiline rho = {| ⟨ 6 , 6 ⟩ |}.
Proof. unfold ex_multiline. step1. Qed.

(* ex_Unif16 = (1=1) *)
Example ev_ex_Unif16 rho : eval ex_Unif16 rho = {| ⟨ 1 , 1 ⟩ |}.
Proof. unfold ex_Unif16. step1. Qed.

(* ex_Unif = (1=2) -> ∅ *)
Example ev_ex_Unif rho : eval ex_Unif rho = ∅.
Proof.
  unfold ex_Unif. cbn [eval].
  apply BinInter_Sing_diff, Couple_diag_neq, natZ_neq. discriminate.
Qed.

(* ex_Cmp17 = (3=3) *)
Example ev_ex_Cmp17 rho : eval ex_Cmp17 rho = {| ⟨ 3 , 3 ⟩ |}.
Proof. unfold ex_Cmp17. step1. Qed.

(* ex_Cmp16 = (3=4) -> ∅ *)
Example ev_ex_Cmp16 rho : eval ex_Cmp16 rho = ∅.
Proof.
  unfold ex_Cmp16. cbn [eval].
  apply BinInter_Sing_diff, Couple_diag_neq, natZ_neq. discriminate.
Qed.

(* ex_Cmp18 = (3=2) -> ∅ *)
Example ev_ex_Cmp18 rho : eval ex_Cmp18 rho = ∅.
Proof.
  unfold ex_Cmp18. cbn [eval].
  apply BinInter_Sing_diff, Couple_diag_neq, natZ_neq. discriminate.
Qed.

(* ex_plus1 = (1+2 = 3) *)
Example ev_ex_plus1 rho : eval ex_plus1 rho = {| ⟨ 3 , 3 ⟩ |}.
Proof. unfold ex_plus1. step1. Qed.

(* ex_plus2 = (1+2 = 4) -> ∅ *)
Example ev_ex_plus2 rho : eval ex_plus2 rho = ∅.
Proof.
  unfold ex_plus2. cbn [eval].
  rewrite !iUnion_pat_Sing_Couple !natZAdd_natZ. cbn [Nat.add].
  apply BinInter_Sing_diff, Couple_diag_neq, natZ_neq. discriminate.
Qed.

(* ex_choice_1_2 = (1 | 2) *)
Example ev_ex_choice rho : eval ex_choice_1_2 rho = {| ⟨ 1 , 1 ⟩ |} ∪ {| ⟨ 2 , 2 ⟩ |}.
Proof. reflexivity. Qed.

(** * Polymorphism: [Eany] as the identity on the universe.

    [Eany] denotes the value ⟨anyId, anyId⟩ whose forward graph [anyId] is
    the identity relation on [Big].  Applying it (via [Eapp]) to any value
    [⟨w,w⟩] whose payload [w] lies in [Big] selects the diagonal edge
    ⟨w,w⟩ and returns the value [w] — so [Eany] behaves as a *polymorphic
    identity function*, accepting an argument of any (small) type. *)

(** The application reduction at the heart of it: iterating [anyId] and
    filtering for the edge whose source is the argument [w ∈ Big] returns
    exactly the value [⟨w,w⟩]. *)
Lemma anyId_apply :
  forall w : ZFSet, w ∈ Big ->
  ('⟨ a , b ⟩ ← anyId ;; ⦃ _ ∈ {| ⟨ b , b ⟩ |} | a = w ⦄) = {| ⟨ w , w ⟩ |}.
Proof.
  intros w Hw. unfold iUnion_pat, anyId.
  apply set_ext; apply Inc_def; intros x Hx.
  - apply iUnion_IN in Hx. destruct Hx as [p [Hp Hx]].
    apply iUnion_IN in Hp. destruct Hp as [c [Hc Hpc]].
    apply IN_Sing_EQ in Hpc. subst p.
    assert (Hpred : pfst (Couple c c) = w) by exact (In_Comp_P _ _ _ Hx).
    pose proof (Inc_IN _ _ _ (Comp_Inc _ _) Hx) as Hmem.
    rewrite pfst_Couple in Hpred.
    rewrite !psnd_Couple in Hmem.
    apply IN_Sing_EQ in Hmem. subst x c. apply IN_Sing.
  - apply IN_Sing_EQ in Hx. subst x.
    apply IN_iUnion with (y := ⟨ w , w ⟩).
    + apply IN_iUnion with (y := w); [ exact Hw | apply IN_Sing ].
    + apply In_P_Comp.
      * rewrite !psnd_Couple. apply IN_Sing.
      * rewrite pfst_Couple. reflexivity.
Qed.

(** [Eany] applied to a value [⟨w,w⟩] with [w ∈ Big] returns [⟨w,w⟩]. *)
Lemma eval_any_app :
  forall (e : Expr) (rho : Env) (w : ZFSet),
  eval e rho = {| ⟨ w , w ⟩ |} -> w ∈ Big ->
  eval (Eapp Eany e) rho = {| ⟨ w , w ⟩ |}.
Proof.
  intros e rho w He Hw.
  rewrite eval_app.
  change (eval Eany rho) with ({| ⟨ anyId , anyId ⟩ |}).
  rewrite iUnion_pat_Sing_Couple.   (* outer iteration: the function graph is [anyId] *)
  (* rewrite the (under-binder) argument iteration over [eval e rho] = {|⟨w,w⟩|}
     into the single edge-source [w], fibre by fibre, via [iUnion_ext_mem] *)
  unfold iUnion_pat at 1.
  rewrite (iUnion_ext_mem anyId _
             (fun p => ⦃ _ ∈ {| ⟨ psnd p , psnd p ⟩ |} | pfst p = w ⦄)).
  - intros p Hp. cbv beta. rewrite He iUnion_pat_Sing_Couple. reflexivity.
  - apply anyId_apply. exact Hw.
Qed.

(* [eval Eany] is the polymorphic identity value. *)
Example ev_any rho : eval Eany rho = {| ⟨ anyId , anyId ⟩ |}.
Proof. reflexivity. Qed.

(* Applying [any] to a numeral returns it — for *any* numeral [k]
   (polymorphism: the same value works at every argument). *)
Example ev_any_app_con (k : nat) rho :
  eval (Eapp Eany (Econ k)) rho = {| ⟨ k , k ⟩ |}.
Proof. apply eval_any_app; [ reflexivity | apply natZ_small ]. Qed.

(* Applying [any] to a *computed* value [(2+3)] returns [5]. *)
Example ev_any_app_add rho :
  eval (Eapp Eany (Eadd (Econ 2) (Econ 3))) rho = {| ⟨ 5 , 5 ⟩ |}.
Proof.
  apply eval_any_app; [ cbn [eval]; zf_reduce; reflexivity | apply natZ_small ].
Qed.

(** * The type of functions via [Etype] / [:type].

    [Etype] denotes the value ⟨is_type, is_type⟩ whose forward graph
    [is_type] is the identity relation on [pFun Big Big].  The projection
    [:type] (= [Eimg Etype]) flattens the value, yielding [is_type] itself
    — the *type of (partial) functions on the universe*, as a set of
    function values. *)

(* [:type] projects out the function-space identity relation. *)
Example ev_img_type rho : eval (Eimg Etype) rho = is_type.
Proof.
  rewrite eval_img.
  change (eval Etype rho) with ({| ⟨ is_type , is_type ⟩ |}).
  rewrite iUnion_pat_Sing_Couple. reflexivity.
Qed.
