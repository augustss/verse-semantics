(* lang/STLC.v

   A self-contained simply-typed lambda calculus with a set-theoretic
   semantics in the quotient set theory [ZFSet]:

     - types denote sets        ([evalTy]),
     - terms denote elements    ([evalTm]),

   and the model is sound: a well-typed term denotes an element of the
   denotation of its type ([soundness]). *)

Require Import ZFSet.
Require Import ZFNotation.
From Stdlib Require Import PeanoNat Lia.
From Stdlib Require Import FunctionalExtensionality.
From Stdlib Require Import RelationClasses.

(** * Syntax *)

(** Types: a base type of naturals and function types. *)
Inductive Ty : Set :=
  | TNat : Ty
  | TArr : Ty -> Ty -> Ty.

(** Terms, in de Bruijn form. *)
Inductive Tm : Set :=
  | tvar : nat -> Tm            (* de Bruijn variable    *)
  | tlam : Ty -> Tm -> Tm       (* λ:T. e                *)
  | tapp : Tm -> Tm -> Tm       (* e1 e2                 *)
  | tcon : nat -> Tm            (* numeral               *)
  | tadd : Tm -> Tm -> Tm.      (* e1 + e2               *)

(** * Typing *)

(** Contexts map de Bruijn indices to types. *)
Definition Ctx := nat -> Ty.

Definition ctx_cons (T : Ty) (Γ : Ctx) : Ctx :=
  fun n => match n with O => T | S m => Γ m end.

Inductive has_type : Ctx -> Tm -> Ty -> Prop :=
  | T_var : forall Γ n,
      has_type Γ (tvar n) (Γ n)
  | T_lam : forall Γ T1 T2 e,
      has_type (ctx_cons T1 Γ) e T2 ->
      has_type Γ (tlam T1 e) (TArr T1 T2)
  | T_app : forall Γ T1 T2 e1 e2,
      has_type Γ e1 (TArr T1 T2) ->
      has_type Γ e2 T1 ->
      has_type Γ (tapp e1 e2) T2
  | T_con : forall Γ n,
      has_type Γ (tcon n) TNat
  | T_add : forall Γ e1 e2,
      has_type Γ e1 TNat ->
      has_type Γ e2 TNat ->
      has_type Γ (tadd e1 e2) TNat.

(** * Set-theoretic semantics *)

(** A type denotes a set: [TNat] the naturals [ω = Omega], an arrow the
    full set-theoretic function space [Pi A (fun _ => B)]. *)
Fixpoint evalTy (T : Ty) : ZFSet :=
  match T with
  | TNat       => Omega
  | TArr T1 T2 => Pi (evalTy T1) (fun _ => evalTy T2)
  end.

(** Value environments map de Bruijn indices to sets. *)
Definition Env := nat -> ZFSet.

Definition env_cons (v : ZFSet) (ρ : Env) : Env :=
  fun n => match n with O => v | S m => ρ m end.

(** A term denotes an element.  A [tlam] denotes the *graph* of the
    function it computes — [{ ⟨a, ⟦e⟧(a·ρ)⟩ | a ∈ ⟦T1⟧ }] — and
    application is set-theoretic function application. *)
Fixpoint evalTm (e : Tm) (ρ : Env) : ZFSet :=
  match e with
  | tvar n     => ρ n
  | tlam T1 e  => iUnion (evalTy T1) (fun a => {| ⟨ a , evalTm e (env_cons a ρ) ⟩ |})
  | tapp e1 e2 => applyFun (evalTm e1 ρ) (evalTm e2 ρ)
  | tcon n     => natZ n
  | tadd e1 e2 => natZAdd (evalTm e1 ρ) (evalTm e2 ρ)
  end.

(** * Soundness of the model *)

(** [ρ] models [Γ] when every variable's value lies in its type. *)
Definition models (Γ : Ctx) (ρ : Env) : Prop :=
  forall n, ρ n ∈ evalTy (Γ n).

Lemma models_cons Γ ρ v T :
  models Γ ρ -> v ∈ evalTy T -> models (ctx_cons T Γ) (env_cons v ρ).
Proof. intros HM Hv [|n]; cbn; [ exact Hv | apply HM ]. Qed.

(** Every well-typed term denotes an element of (the denotation of) its
    type. *)
Theorem soundness :
  forall Γ e T,
  has_type Γ e T ->
  forall ρ, models Γ ρ -> evalTm e ρ ∈ evalTy T.
Proof.
  induction 1; intros ρ HM; cbn [evalTm evalTy].

  - (* T_var *)
    apply HM.

  - (* T_lam: the graph belongs to the function space *)
    apply (iUnion_graph_mem_pi (evalTy T1) (fun _ => evalTy T2)
             (fun a => evalTm e (env_cons a ρ))).
    intros a Ha. apply IHhas_type. apply models_cons; assumption.

  - (* T_app: apply a function value to an argument value *)
    apply (applyFun_mem_of_pi (evalTy T1) (fun _ => evalTy T2)
             (evalTm e1 ρ) (evalTm e2 ρ)).
    + apply IHhas_type1; exact HM.
    + apply IHhas_type2; exact HM.

  - (* T_con *)
    apply natZ_mem_omega.

  - (* T_add: the sum of two naturals is a natural *)
    pose proof (IHhas_type1 ρ HM) as H1. pose proof (IHhas_type2 ρ HM) as H2.
    cbn [evalTy] in H1, H2.
    destruct (In_omega _ H1) as [n1 E1]. destruct (In_omega _ H2) as [n2 E2].
    rewrite <- E1, <- E2, natZAdd_natZ. apply natZ_mem_omega.
Qed.

(* ================================================================== *)
(** * Call-by-name small-step reduction. *)
(* ================================================================== *)

(** A substitution-based, call-by-name small-step semantics.  An
    application reduces its *function* to a λ ([S_app1]) and then β fires on
    the *unevaluated* argument ([S_beta], no value premise).  Addition is a
    strict primitive: it reduces its left operand, then (once that is a
    numeral) its right operand, then computes ([S_add1]/[S_add2]/[S_add]). *)

(** de Bruijn shift and single-variable substitution. *)
Fixpoint lift (c : nat) (e : Tm) : Tm :=
  match e with
  | tvar n     => match Nat.compare n c with Lt => tvar n | _ => tvar (S n) end
  | tlam T e1  => tlam T (lift (S c) e1)
  | tapp a b   => tapp (lift c a) (lift c b)
  | tcon n     => tcon n
  | tadd a b   => tadd (lift c a) (lift c b)
  end.

Fixpoint subst (k : nat) (s : Tm) (e : Tm) : Tm :=
  match e with
  | tvar n     => match Nat.compare n k with
                  | Lt => tvar n
                  | Eq => s
                  | Gt => tvar (Nat.pred n)
                  end
  | tlam T e1  => tlam T (subst (S k) (lift 0 s) e1)
  | tapp a b   => tapp (subst k s a) (subst k s b)
  | tcon n     => tcon n
  | tadd a b   => tadd (subst k s a) (subst k s b)
  end.

Reserved Notation "e '-->' e'" (at level 70, no associativity).

Inductive step : Tm -> Tm -> Prop :=
  | S_beta : forall T body e2,
      tapp (tlam T body) e2 --> subst 0 e2 body
  | S_app1 : forall e1 e1' e2,
      e1 --> e1' ->
      tapp e1 e2 --> tapp e1' e2
  | S_add1 : forall e1 e1' e2,
      e1 --> e1' ->
      tadd e1 e2 --> tadd e1' e2
  | S_add2 : forall n e2 e2',
      e2 --> e2' ->
      tadd (tcon n) e2 --> tadd (tcon n) e2'
  | S_add : forall n1 n2,
      tadd (tcon n1) (tcon n2) --> tcon (n1 + n2)

where "e '-->' e'" := (step e e').

(** Reflexive–transitive closure. *)
Reserved Notation "e '-->*' e'" (at level 70, no associativity).
Inductive multistep : Tm -> Tm -> Prop :=
  | ms_refl : forall e, e -->* e
  | ms_step : forall e1 e2 e3, e1 --> e2 -> e2 -->* e3 -> e1 -->* e3
where "e '-->*' e'" := (multistep e e').

(* ================================================================== *)
(** * Each reduction rule is sound for [evalTm]. *)
(* ================================================================== *)

(** Inserting a value [a] at de Bruijn position [k] in the environment —
    the semantic counterpart of [subst k] / [lift k]. *)
Definition env_ins (k : nat) (a : ZFSet) (ρ : Env) : Env :=
  fun n => match Nat.compare n k with
           | Lt => ρ n
           | Eq => a
           | Gt => ρ (Nat.pred n)
           end.

Lemma env_ins_0 a ρ : env_ins 0 a ρ = env_cons a ρ.
Proof. apply functional_extensionality. intros [|n]; reflexivity. Qed.

Lemma env_ins_S_cons d b c ρ :
  env_ins (S d) b (env_cons c ρ) = env_cons c (env_ins d b ρ).
Proof.
  apply functional_extensionality. intro n. unfold env_ins, env_cons.
  destruct n as [|n]; cbn [Nat.compare].
  - reflexivity.
  - destruct (Nat.compare n d) eqn:E; cbn [Nat.pred]; try reflexivity.
    destruct n as [|m]; [ apply Nat.compare_gt_iff in E; lia | reflexivity ].
Qed.

(** Lifting and inserting a dummy slot cancel. *)
Lemma evalTm_lift e : forall d b ρ,
  evalTm (lift d e) (env_ins d b ρ) = evalTm e ρ.
Proof.
  induction e; intros d b ρ; cbn [lift evalTm].
  - (* tvar *)
    destruct (Nat.compare n d) eqn:E; cbn [evalTm].
    + apply Nat.compare_eq_iff in E. subst n. unfold env_ins.
      assert (E2 : Nat.compare (S d) d = Gt) by (apply Nat.compare_gt_iff; lia).
      rewrite E2. cbn [Nat.pred]. reflexivity.
    + unfold env_ins. rewrite E. reflexivity.
    + apply Nat.compare_gt_iff in E. unfold env_ins.
      assert (E2 : Nat.compare (S n) d = Gt) by (apply Nat.compare_gt_iff; lia).
      rewrite E2. cbn [Nat.pred]. reflexivity.
  - (* tlam *)
    f_equal. apply functional_extensionality. intro a. f_equal. f_equal.
    rewrite <- (env_ins_S_cons d b a ρ). apply IHe.
  - (* tapp *) rewrite IHe1, IHe2. reflexivity.
  - (* tcon *) reflexivity.
  - (* tadd *) rewrite IHe1, IHe2. reflexivity.
Qed.

(** Substitution lemma: substituting [s] for index [k] is inserting its
    value [evalTm s ρ] at slot [k].  Unconditional — this model is
    single-valued. *)
Lemma evalTm_subst e : forall k s ρ,
  evalTm (subst k s e) ρ = evalTm e (env_ins k (evalTm s ρ) ρ).
Proof.
  induction e; intros k s ρ; cbn [subst evalTm].
  - (* tvar *)
    destruct (Nat.compare n k) eqn:E; cbn [evalTm].
    + apply Nat.compare_eq_iff in E. subst n.
      unfold env_ins. rewrite Nat.compare_refl. reflexivity.
    + unfold env_ins. rewrite E. reflexivity.
    + unfold env_ins. rewrite E. reflexivity.
  - (* tlam *)
    f_equal. apply functional_extensionality. intro a. f_equal. f_equal.
    rewrite (IHe (S k) (lift 0 s) (env_cons a ρ)).
    replace (evalTm (lift 0 s) (env_cons a ρ)) with (evalTm s ρ).
    2:{ rewrite <- (env_ins_0 a ρ). symmetry. apply (evalTm_lift s 0 a ρ). }
    rewrite (env_ins_S_cons k (evalTm s ρ) a ρ). reflexivity.
  - (* tapp *) rewrite IHe1, IHe2. reflexivity.
  - (* tcon *) reflexivity.
  - (* tadd *) rewrite IHe1, IHe2. reflexivity.
Qed.

(** Applying the λ-graph [{ ⟨a, g a⟩ | a ∈ A }] at an in-domain point [v]
    returns the fibre [g v]. *)
Lemma applyFun_graph A (g : ZFSet -> ZFSet) v :
  v ∈ A -> applyFun (iUnion A (fun a => {| ⟨ a , g a ⟩ |})) v = g v.
Proof.
  intro Hv. unfold applyFun.
  assert (Himg : image (iUnion A (fun a => {| ⟨ a , g a ⟩ |})) {| v |} = {| g v |}).
  { apply set_ext; apply Inc_def; intros x Hx.
    - apply image_elim in Hx. destruct Hx as [a [Ha Hedge]].
      apply IN_Sing_EQ in Ha. subst a.
      apply iUnion_IN in Hedge. destruct Hedge as [b [_ Hedge]].
      apply IN_Sing_EQ in Hedge.
      pose proof (Couple_inj_left _ _ _ _ Hedge) as Hb.
      pose proof (Couple_inj_right _ _ _ _ Hedge) as Hx.
      subst b. rewrite Hx. apply IN_Sing.
    - apply IN_Sing_EQ in Hx. subst x.
      apply image_intro with (a := v); [ apply IN_Sing | ].
      apply IN_iUnion with (y := v); [ exact Hv | apply IN_Sing ]. }
  rewrite Himg, Union_Sing. reflexivity.
Qed.

(** **Soundness of each rule**: a well-typed [e --> e'] preserves the
    denotation, [evalTm e ρ = evalTm e' ρ], in every model [ρ].  The proof
    is by cases on the reduction, i.e. one case per rule.

    The interesting rule is β ([S_beta]): the λ-graph is applied at the
    argument's value, which lands in the domain because the argument is
    well-typed ([soundness]); the result is then the body's value, which
    the substitution lemma identifies with the reduct.  [S_app1]/[S_add1]/
    [S_add2] are congruences (use the induction hypothesis), and [S_add]
    is the numeral computation [natZAdd (natZ n1) (natZ n2) = natZ (n1+n2)]. *)
Theorem step_sound : forall e e',
  e --> e' ->
  forall Γ T, has_type Γ e T ->
  forall ρ, models Γ ρ -> evalTm e ρ = evalTm e' ρ.
Proof.
  induction 1; intros Γ U HT ρ HM.

  - (* S_beta *)
    inversion HT as [| | ?Γ T1 T2 ?e1 ?e2 Hf Ha | |]; subst.
    inversion Hf; subst.
    cbn [evalTm].
    rewrite (applyFun_graph (evalTy T1) (fun a => evalTm body (env_cons a ρ))
               (evalTm e2 ρ) (soundness Γ e2 T1 Ha ρ HM)).
    rewrite (evalTm_subst body 0 e2 ρ), env_ins_0. reflexivity.

  - (* S_app1 *)
    inversion HT as [| | ?Γ T1 T2 ?a ?b Hf Ha | |]; subst.
    cbn [evalTm]. rewrite (IHstep Γ (TArr T1 U) Hf ρ HM). reflexivity.

  - (* S_add1 *)
    inversion HT as [| | | | ?Γ ?a ?b H1 H2]; subst.
    cbn [evalTm]. rewrite (IHstep Γ TNat H1 ρ HM). reflexivity.

  - (* S_add2 *)
    inversion HT as [| | | | ?Γ ?a ?b H1 H2]; subst.
    cbn [evalTm]. rewrite (IHstep Γ TNat H2 ρ HM). reflexivity.

  - (* S_add *)
    cbn [evalTm]. rewrite natZAdd_natZ. reflexivity.
Qed.

(** Subject reduction (type preservation).  This is the standard syntactic
    property of STLC — provable from a typing-substitution lemma and
    inversion on [step] — which we take here as an axiom rather than develop
    the substitution metatheory.  It is what lets [step_sound] be threaded
    along a reduction sequence. *)
Axiom preservation : forall Γ e e' T,
  has_type Γ e T -> e --> e' -> has_type Γ e' T.

(** With preservation, [step_sound] closes under [multistep]: every term in
    the reduction sequence stays well-typed, so each step preserves the
    denotation. *)
Theorem multistep_sound : forall e e',
  e -->* e' ->
  forall Γ T, has_type Γ e T -> forall ρ, models Γ ρ -> evalTm e ρ = evalTm e' ρ.
Proof.
  induction 1 as [e | e1 e2 e3 Hstep _ IH]; intros Γ T HT ρ HM.
  - reflexivity.
  - rewrite (step_sound _ _ Hstep Γ T HT ρ HM).
    exact (IH Γ T (preservation _ _ _ _ HT Hstep) ρ HM).
Qed.

(* ================================================================== *)
(** * β-equivalence (convertibility). *)
(* ================================================================== *)

(** [e1 <--> e2]: the least *equivalence* relation containing the small-step
    reduction [-->].  Since [-->] is the compatible closure of β (and the
    strict primitive rules), its reflexive–symmetric–transitive closure is
    β-equivalence / convertibility — terms inter-convertible by reducing
    redexes anywhere, in either direction.  (The symmetric counterpart of
    [-->] / [-->*].) *)
Reserved Notation "e1 '<-->' e2" (at level 70, no associativity).

Inductive beta_equiv : Tm -> Tm -> Prop :=
  | be_step  : forall e e', e --> e' -> e <--> e'
  | be_refl  : forall e, e <--> e
  | be_sym   : forall e e', e <--> e' -> e' <--> e
  | be_trans : forall e1 e2 e3, e1 <--> e2 -> e2 <--> e3 -> e1 <--> e3

where "e1 '<-->' e2" := (beta_equiv e1 e2).

(** It is, by construction, an equivalence relation. *)
#[export] Instance beta_equiv_Equivalence : Equivalence beta_equiv.
Proof.
  constructor.
  - exact be_refl.
  - exact be_sym.
  - exact be_trans.
Qed.

(** Reduction — single- and multi-step — implies β-equivalence. *)
Lemma step_beta_equiv e e' : e --> e' -> e <--> e'.
Proof. apply be_step. Qed.

Lemma multistep_beta_equiv e e' : e -->* e' -> e <--> e'.
Proof.
  induction 1 as [e | e1 e2 e3 Hstep _ IH].
  - apply be_refl.
  - exact (be_trans _ _ _ (be_step _ _ Hstep) IH).
Qed.

(** The defining example: a β-redex is convertible with its reduct. *)
Example beta_equiv_beta T body e2 :
  tapp (tlam T body) e2 <--> subst 0 e2 body.
Proof. apply be_step, S_beta. Qed.

(** Soundness for whole β-*equivalence* (not just reduction) needs more than
    [preservation]: a [<-->] chain mixes forward and backward steps, and
    subject *expansion* fails, so an intermediate term in the chain can be
    ill-typed and have a different (junk, off-domain) denotation.  The clean
    route is confluence (Church–Rosser): two convertible *well-typed* terms
    share a common reduct, and [multistep_sound] equates each with it.  That
    needs a confluence proof, not pursued here; [multistep_sound] above is the
    one-directional fragment that [preservation] already delivers. *)

(* ================================================================== *)
(** * βη-equivalence. *)
(* ================================================================== *)

(** β-equivalence is *incomplete* for [evalTm]: the model is **extensional**
    (a [tlam] denotes its function graph), so it validates **η**, which
    [<-->] does not — e.g. [λf. f] and [λf. λx. f x] have equal denotations
    but are not β-convertible.  We close that gap by extending the theory
    with η (and packaging it as a genuine congruence).

    [e1 <==> e2] is the least *congruence* containing β, η, and the [+]
    computation rule — i.e. βη-equivalence (with δ for the one primitive). *)
Reserved Notation "e1 '<==>' e2" (at level 70, no associativity).

Inductive beta_eta_equiv : Tm -> Tm -> Prop :=
  (* the rewrite axioms: β, η, and δ (for [+]) *)
  | bqe_beta  : forall T body e2, tapp (tlam T body) e2 <==> subst 0 e2 body
  | bqe_eta   : forall T e, tlam T (tapp (lift 0 e) (tvar 0)) <==> e
  | bqe_delta : forall m n, tadd (tcon m) (tcon n) <==> tcon (m + n)
  (* equivalence *)
  | bqe_refl  : forall e, e <==> e
  | bqe_sym   : forall e e', e <==> e' -> e' <==> e
  | bqe_trans : forall e1 e2 e3, e1 <==> e2 -> e2 <==> e3 -> e1 <==> e3
  (* congruence (compatible closure under each constructor) *)
  | bqe_lam   : forall T e e', e <==> e' -> tlam T e <==> tlam T e'
  | bqe_app   : forall e1 e1' e2 e2',
      e1 <==> e1' -> e2 <==> e2' -> tapp e1 e2 <==> tapp e1' e2'
  | bqe_add   : forall e1 e1' e2 e2',
      e1 <==> e1' -> e2 <==> e2' -> tadd e1 e2 <==> tadd e1' e2'

where "e1 '<==>' e2" := (beta_eta_equiv e1 e2).

#[export] Instance beta_eta_equiv_Equivalence : Equivalence beta_eta_equiv.
Proof. constructor; [ exact bqe_refl | exact bqe_sym | exact bqe_trans ]. Qed.

(** βη-equivalence really extends β-equivalence: every reduction step, and
    hence all of [<-->], is contained in [<==>]. *)
Lemma step_beta_eta e e' : e --> e' -> e <==> e'.
Proof.
  induction 1.
  - apply bqe_beta.
  - apply bqe_app; [ assumption | apply bqe_refl ].
  - apply bqe_add; [ assumption | apply bqe_refl ].
  - apply bqe_add; [ apply bqe_refl | assumption ].
  - apply bqe_delta.
Qed.

Lemma beta_equiv_beta_eta e e' : e <--> e' -> e <==> e'.
Proof.
  induction 1.
  - apply step_beta_eta. assumption.
  - apply bqe_refl.
  - apply bqe_sym. assumption.
  - eapply bqe_trans; eassumption.
Qed.

(** The η witness that refuted β-completeness is now an equation: [λf. f]
    and [λf. λx. f x] are βη-equivalent (η under the outer λ). *)
Example eta_example :
  tlam (TArr TNat TNat) (tvar 0)
  <==> tlam (TArr TNat TNat) (tlam TNat (tapp (tvar 1) (tvar 0))).
Proof. apply bqe_sym, bqe_lam. exact (bqe_eta TNat (tvar 0)). Qed.

(* ------------------------------------------------------------------ *)
(** ** The model validates η.

    Two [Pi] facts STLC's library lacks: in [f ∈ Pi A B], the edge over an
    in-domain point is [⟨a, f a⟩], and conversely every edge's target is the
    applied value. *)
Lemma pi_value_edge A B f a :
  f ∈ Pi A B -> a ∈ A -> ⟨ a , applyFun f a ⟩ ∈ f.
Proof.
  intros Hf Ha.
  destruct (In_Pi_inv A B f Hf) as [_ [Htot _]].
  destruct (Htot a Ha) as [b [_ Hedge]].
  assert (Hval : b = applyFun f a).
  { assert (Hbin : b ∈ image f {| a |})
      by (apply image_intro with (a := a); [ apply IN_Sing | exact Hedge ]).
    rewrite (image_Sing_of_pi A B f a Hf Ha) in Hbin.
    apply IN_Sing_EQ in Hbin. exact Hbin. }
  rewrite <- Hval. exact Hedge.
Qed.

Lemma pi_edge_value A B f a b :
  f ∈ Pi A B -> ⟨ a , b ⟩ ∈ f -> b = applyFun f a.
Proof.
  intros Hf Hedge.
  destruct (In_Pi_inv A B f Hf) as [Hsub _].
  pose proof (Inc_IN _ _ _ Hsub Hedge) as Hprod.
  apply Couple_Prod_IN in Hprod. destruct Hprod as [Ha _].
  assert (Hbin : b ∈ image f {| a |})
    by (apply image_intro with (a := a); [ apply IN_Sing | exact Hedge ]).
  rewrite (image_Sing_of_pi A B f a Hf Ha) in Hbin.
  apply IN_Sing_EQ in Hbin. exact Hbin.
Qed.

(** η-soundness: an η-expansion [λx. (lift e) x] of a well-typed function
    [e : T1 → T2] has the same denotation as [e].  (This is the new axiom
    [bqe_eta]; combined with [step_sound] for β and δ, [evalTm] validates
    every rule of [<==>].  Equating η-distinct terms is exactly why the
    model is *complete* for βη — Friedman's theorem, since [evalTy ι] is
    infinite and arrows are the full function space — not formalized here.) *)
Lemma eta_sound Γ e T1 T2 ρ :
  has_type Γ e (TArr T1 T2) -> models Γ ρ ->
  evalTm (tlam T1 (tapp (lift 0 e) (tvar 0))) ρ = evalTm e ρ.
Proof.
  intros HT HM.
  pose proof (soundness Γ e (TArr T1 T2) HT ρ HM) as HF. cbn [evalTy] in HF.
  assert (Hbody : forall a, a ∈ evalTy T1 ->
            evalTm (tapp (lift 0 e) (tvar 0)) (env_cons a ρ)
            = applyFun (evalTm e ρ) a).
  { intros a Ha. cbn [evalTm env_cons].
    f_equal. rewrite <- (env_ins_0 a ρ). apply (evalTm_lift e 0 a ρ). }
  change (evalTm (tlam T1 (tapp (lift 0 e) (tvar 0))) ρ)
    with (iUnion (evalTy T1)
            (fun a => {| ⟨ a, evalTm (tapp (lift 0 e) (tvar 0)) (env_cons a ρ) ⟩ |})).
  apply set_ext; apply Inc_def; intros x Hx.
  - apply iUnion_IN in Hx. destruct Hx as [a [Ha Hx]].
    rewrite (Hbody a Ha) in Hx. apply IN_Sing_EQ in Hx. subst x.
    exact (pi_value_edge (evalTy T1) (fun _ => evalTy T2) (evalTm e ρ) a HF Ha).
  - pose proof (In_Pi_inv _ _ _ HF) as [Hsub _].
    pose proof (Inc_IN _ _ _ Hsub Hx) as Hprod.
    apply IN_Prod_EX in Hprod. destruct Hprod as [a [b [Ha [_ Heq]]]]. subst x.
    pose proof (pi_edge_value (evalTy T1) (fun _ => evalTy T2) (evalTm e ρ) a b HF Hx)
      as Hb.
    apply IN_iUnion with (y := a); [ exact Ha | ].
    rewrite (Hbody a Ha), <- Hb. apply IN_Sing.
Qed.

(* ================================================================== *)
(** * Completeness for βη: Friedman's theorem (proof sketch).

    The soundness direction is (modulo the chain-typing issue noted above)
    [step_sound]+[eta_sound]: βη-equal terms have equal denotations.  The
    *converse* — equal denotation implies βη-equal — is **completeness**, and
    for this model it holds by a classical theorem of H. Friedman ("Equality
    between functionals", LNM 453, 1975):

      In the *full* set-theoretic type hierarchy over an *infinite* base set,
      two typed λ-terms have the same denotation iff they are βη-equal.

    Both hypotheses are met here: [evalTy TNat = ω] is infinite, and
    [evalTy (TArr A B) = Pi (evalTy A) (fun _ => evalTy B)] is the *full*
    function space (every total functional graph), not just the definable
    elements.  So the STLC model of this file is complete for βη.

    A formalisation is not attempted; the argument runs as follows.

    (1) NORMALISATION.  Every well-typed term has a βη-(long-)normal form, and
        [<==>] equates a term with its normal form.  So completeness reduces
        to: terms with equal denotation have *equal* βη-normal forms.

    (2) A LOGICAL RELATION between the model and the syntax.  Define, by
        induction on the type [T], a relation
            R_T ⊆ evalTy T × { terms of type T }
        with the usual arrow clause
            R_{A→B}(f, M)  :=  ∀ a N, R_A(a, N) → R_B(applyFun f a, tapp M N),
        and, at the base type, a clause that ties a semantic natural to a
        term using an *injection of the (countable) set of normal forms into
        the infinite base set* [ω].  Infinitude of the base is exactly what
        makes room to encode syntax inside the model — the crux of the proof,
        and the reason the theorem needs an infinite base.

    (3) FUNDAMENTAL LEMMA (definability/"glueing").  Every term is related to
        its own denotation: under R-related environments, [R_T(evalTm M ρ, M)]
        by induction on [M].  (Friedman's "partial homomorphism" / back-and-
        forth between the standard model and the term model.)

    (4) READ-BACK.  The base-type clause is injective enough that from a
        semantic value one recovers the βη-normal form of any term related to
        it: applying a function to enough "generic" arguments coded by
        distinct base elements exposes its normal form.

    (5) COMPLETENESS.  If [evalTm M ρ = evalTm N ρ] for closed [M, N : T],
        then by (3) both are R_T-related to the *same* semantic value; by (4)
        they have the same βη-normal form; by (1) [M <==> N].

    The role of each ingredient is visible in this file: [evalTy] gives the
    full hierarchy over the infinite [ω] (so (2)/(4) have room to encode
    syntax), [eta_sound]/[step_sound] are the soundness half, and
    [beta_eta_equiv] is the theory shown complete. *)
