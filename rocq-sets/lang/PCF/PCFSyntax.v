(* lang/PCF/PCFSyntax.v

   The *syntax* of Plotkin's PCF (Programming Computable Functions; "LCF
   considered as a programming language", 1977): the simply-typed lambda
   calculus over two ground types — naturals and booleans — with the
   arithmetic/boolean primitives and a fixpoint operator.

   This file collects the parts shared verbatim by the two denotational
   developments [PCFStrict.v] (the strict relational model) and [PCFLifted.v]
   (the ⊥-lifted model): the types and terms, the typing relation, and the
   substitution-based call-by-name small-step reduction.  It deliberately
   depends on nothing set-theoretic — it is pure syntax — so both models can
   [Require Import PCFSyntax] and then give their own [evalTm]. *)

From Stdlib Require Import PeanoNat.

(** * Syntax *)

(** Types: the ground types [ι] (naturals) and [o] (booleans), and
    function types. *)
Inductive Ty : Set :=
  | TNat  : Ty
  | TBool : Ty
  | TArr  : Ty -> Ty -> Ty.

(** Terms, in de Bruijn form.  Numerals are built from [tzero]/[tsucc]
    ([tsucc] is the successor primitive; the numeral [n] is [tsucc^n
    tzero]).  [tfix e] is the fixpoint of [e : σ → σ] (Plotkin's [Y_σ]). *)
Inductive Tm : Set :=
  | tvar    : nat -> Tm               (* de Bruijn variable        *)
  | tlam    : Ty -> Tm -> Tm         (* λ:T. e                    *)
  | tapp    : Tm -> Tm -> Tm         (* e1 e2                     *)
  | tzero   : Tm                   (* 0                         *)
  | tsucc   : Tm -> Tm              (* succ e                    *)
  | tpred   : Tm -> Tm              (* pred e                    *)
  | tiszero : Tm -> Tm              (* zero? e                   *)
  | ttrue   : Tm                   (* true                      *)
  | tfalse  : Tm                   (* false                     *)
  | tif     : Tm -> Tm -> Tm -> Tm    (* if e0 then e1 else e2     *)
  | tfix    : Tm -> Tm.             (* fix e   (Y_σ)             *)

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
  | T_zero : forall Γ,
      has_type Γ tzero TNat
  | T_succ : forall Γ e,
      has_type Γ e TNat ->
      has_type Γ (tsucc e) TNat
  | T_pred : forall Γ e,
      has_type Γ e TNat ->
      has_type Γ (tpred e) TNat
  | T_iszero : forall Γ e,
      has_type Γ e TNat ->
      has_type Γ (tiszero e) TBool
  | T_true : forall Γ,
      has_type Γ ttrue TBool
  | T_false : forall Γ,
      has_type Γ tfalse TBool
  | T_if : forall Γ T e0 e1 e2,
      has_type Γ e0 TBool ->
      has_type Γ e1 T ->
      has_type Γ e2 T ->
      has_type Γ (tif e0 e1 e2) T
  | T_fix : forall Γ T e,
      has_type Γ e (TArr T T) ->
      has_type Γ (tfix e) T.

(* ================================================================== *)
(** * Small-step call-by-name reduction. *)
(* ================================================================== *)

(** A substitution-based small-step semantics, call-by-name (matching the
    big-step semantics of [PCFStrict.v] and Plotkin's original PCF).  An
    application reduces its *function* to a λ ([S_app1]) and then β fires on
    the *unevaluated* argument ([S_beta], no [value] premise) — there is no
    rule that reduces an argument before the call.  The ground primitives
    remain strict (their arguments are reduced before they fire).
    [fix (λ:T. e)] unrolls by substituting itself for the bound variable. *)

(** de Bruijn shift: increment the free indices [≥ c]. *)
Fixpoint lift (c : nat) (e : Tm) : Tm :=
  match e with
  | tvar n     => match Nat.compare n c with Lt => tvar n | _ => tvar (S n) end
  | tlam T e1  => tlam T (lift (S c) e1)
  | tapp a b   => tapp (lift c a) (lift c b)
  | tzero      => tzero
  | tsucc e1   => tsucc (lift c e1)
  | tpred e1   => tpred (lift c e1)
  | tiszero e1 => tiszero (lift c e1)
  | ttrue      => ttrue
  | tfalse     => tfalse
  | tif a b d  => tif (lift c a) (lift c b) (lift c d)
  | tfix e1    => tfix (lift c e1)
  end.

(** Substitute [s] for index [k], decrementing the higher indices (so
    that, with [k = 0], it implements the β/fix substitution that removes
    one binder). *)
Fixpoint subst (k : nat) (s : Tm) (e : Tm) : Tm :=
  match e with
  | tvar n     => match Nat.compare n k with
                  | Lt => tvar n
                  | Eq => s
                  | Gt => tvar (Nat.pred n)
                  end
  | tlam T e1  => tlam T (subst (S k) (lift 0 s) e1)
  | tapp a b   => tapp (subst k s a) (subst k s b)
  | tzero      => tzero
  | tsucc e1   => tsucc (subst k s e1)
  | tpred e1   => tpred (subst k s e1)
  | tiszero e1 => tiszero (subst k s e1)
  | ttrue      => ttrue
  | tfalse     => tfalse
  | tif a b d  => tif (subst k s a) (subst k s b) (subst k s d)
  | tfix e1    => tfix (subst k s e1)
  end.

(** Syntactic values: λs, numerals ([succ^n 0]) and booleans. *)
Inductive value : Tm -> Prop :=
  | v_lam   : forall T e, value (tlam T e)
  | v_zero  : value tzero
  | v_succ  : forall e, value e -> value (tsucc e)
  | v_true  : value ttrue
  | v_false : value tfalse.

Reserved Notation "e '-->' e'" (at level 70, no associativity).

Inductive step : Tm -> Tm -> Prop :=
  | S_beta : forall T body e2,
      tapp (tlam T body) e2 --> subst 0 e2 body
  | S_app1 : forall e1 e1' e2,
      e1 --> e1' ->
      tapp e1 e2 --> tapp e1' e2
  | S_succ : forall e e',
      e --> e' ->
      tsucc e --> tsucc e'
  | S_pred_zero :
      tpred tzero --> tzero
  | S_pred_succ : forall v,
      value v ->
      tpred (tsucc v) --> v
  | S_pred : forall e e',
      e --> e' ->
      tpred e --> tpred e'
  | S_iszero_zero :
      tiszero tzero --> ttrue
  | S_iszero_succ : forall v,
      value v ->
      tiszero (tsucc v) --> tfalse
  | S_iszero : forall e e',
      e --> e' ->
      tiszero e --> tiszero e'
  | S_if_true : forall e1 e2,
      tif ttrue e1 e2 --> e1
  | S_if_false : forall e1 e2,
      tif tfalse e1 e2 --> e2
  | S_if : forall e0 e0' e1 e2,
      e0 --> e0' ->
      tif e0 e1 e2 --> tif e0' e1 e2
  | S_fix : forall T body,
      tfix (tlam T body) --> subst 0 (tfix (tlam T body)) body
  | S_fix_cong : forall e e',
      e --> e' ->
      tfix e --> tfix e'

where "e '-->' e'" := (step e e').

(** Reflexive–transitive closure. *)
Reserved Notation "e '-->*' e'" (at level 70, no associativity).
Inductive multistep : Tm -> Tm -> Prop :=
  | ms_refl : forall e, e -->* e
  | ms_step : forall e1 e2 e3, e1 --> e2 -> e2 -->* e3 -> e1 -->* e3
where "e '-->*' e'" := (multistep e e').
