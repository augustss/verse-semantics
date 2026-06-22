From Stdlib Require Import ssreflect.

Require Import Sets.
Require Import ZFNotation.
Require Lang.PolyF.


(** Timbda language: shared for Timbda0 and Timbda1 ***)

Inductive Expr : Set :=
  | Econ    : nat -> Expr                  (* k in 0,1,... *)
  | Evar    : nat -> Expr                  (* x            *) 
  | Enat    : Expr                         (* nat          *)
  | Elam    : Expr -> Expr -> Expr         (* fun(x:=e1)e2 *)
  | Elamp   : Expr -> Expr -> Expr         (* fun(x:=e1)e2, partial in e1 *)
  | Elamb   : Expr -> Expr -> Expr         (* fun(x:=e1)e2, env-extending binder (Timbda2) *)
  | Eapp    : Expr -> Expr -> Expr         (* e1[e2]       *)
  | Ebind   : Expr -> Expr -> Expr         (* e1 :>= e2    *)
  | Eany    : Expr                         (* the universe *)
  | Etype   : Expr                         (* the type of types *)
  | Efail   : Expr                         (* fail         *)
  | Eimg    : Expr -> Expr                 (* :e           *)
  | Echoice : Expr -> Expr -> Expr         (* e1 | e2      *)
  | Eequal  : Expr -> Expr -> Expr         (* e1 = e2      *)
  | Eadd    : Expr -> Expr -> Expr         (* e1 + e2      *)
  | Efix    : Expr -> Expr
  | Eassign : nat -> Expr -> Expr          (* x := e *)
  | Elet    : nat -> Expr -> Expr -> Expr  (* x := e; e *)
  | Eseq    : Expr -> Expr -> Expr         (* e1 ; e2 *)
  .





  
(** ** step relation *)
Inductive value : Expr -> Prop := 
  | value_con n : value (Econ n)
  | value_nats  : value Enat
  | value_lam a b : value (Elam a b)
.

(* just a few for now *)
Inductive step : Expr -> Expr -> Prop := 
  | step_add n m : 
    step (Eadd (Econ n) (Econ m)) (Econ (n + m))
  | step_is_nat n :
    step (Eapp Enat (Econ n)) (Econ n)
  | step_isnt_nat a b :
    step (Eapp Enat (Elam a b)) Efail
  | step_unify_con n : 
    step (Eequal (Econ n) (Econ n)) (Econ n)
  | step_unify_lam a1 a2 b1 b2 : 
    step (Eequal (Elam a1 b1) (Elam a2 b2)) 
         (Elam (Eequal a2 a1) (Eequal b1 b2))
  | step_unify_fail n a b: 
    step (Eequal (Econ n) (Elam a b)) Efail.


(**** Types ****)

Definition Ctx := nat -> Expr.

Definition ctx_ext (Gamma : Ctx) (T : Expr) : Ctx :=
  fun n => match n with O => T | S m => Gamma m end.

Theorem ctx_ext_zero : forall (Gamma : Ctx) (T : Expr), ctx_ext Gamma T O = T.
Proof. reflexivity. Qed.

Theorem ctx_ext_succ :
  forall (Gamma : Ctx) (T : Expr) (n : nat),
  ctx_ext Gamma T (S n) = Gamma n.
Proof. reflexivity. Qed.


(** Typing judgement that defines an embedding of STLC in Timbda0/1 ***)

Module STLC.

Inductive IsType : Expr -> Prop := 
  | IT_nat : 
    IsType (Eimg Enat)
  | IT_lam T T' : 
    IsType T -> IsType T' -> 
    IsType (Elam T T').

Definition IsCtx (Γ : Ctx) := 
  forall i, IsType (Γ i).

Inductive HasType : Ctx -> Expr -> Expr -> Prop :=
  | HT_con : forall Gamma n, 
    IsCtx Gamma ->
    HasType Gamma (Econ n) (Eimg Enat)
  | HT_var : forall Gamma i, 
    IsCtx Gamma ->
    HasType Gamma (Evar i) (Gamma i)
  | HT_lam : forall Gamma T T' e,
    IsType T ->
    HasType (ctx_ext Gamma T) e T' ->
    HasType Gamma (Elam T e) (Elam T T')
  | HT_app : forall Gamma T1 T2 e1 e2,
      HasType Gamma e1 (Elam T1 T2) ->
      HasType Gamma e2 T1 ->
      HasType Gamma (Eapp e1 e2) T2
  | HT_add : forall Gamma e1 e2,
      HasType Gamma e1 (Eimg Enat) ->
      HasType Gamma e2 (Eimg Enat) ->
      HasType Gamma (Eadd e1 e2) (Eimg Enat).

Notation "Gamma ⊢ e ⦂ T" := (HasType Gamma e T)
  (at level 40, e at next level) : zf_scope.

Lemma HasType_IsType Γ e T : 
  Γ ⊢ e ⦂ T -> IsType T.
Proof.
  induction 1; eauto using IsType.
  inversion IHHasType1. eauto.
Qed.  

Lemma IsCtx_inv Gamma T : IsCtx (ctx_ext Gamma T) -> IsCtx Gamma.
intros h i. 
specialize (h (S i)). 
unfold ctx_ext in h.
auto.
Qed.

Lemma HasType_IsCtx : forall G e T, HasType G e T -> IsCtx G.
induction 1; eauto. eapply IsCtx_inv; eauto.
Qed.

End STLC.


(* Coppo-Dezani type system *)
Module IntersectionTypes.

Inductive IsType : Expr -> Prop := 
  | IT_nat : IsType (Eimg Enat)
  | IT_lam T T' : 
    IsDomType T -> IsType T' -> 
    IsType (Elam T T')
with IsDomType : Expr -> Prop := 
  | IDT_one T : IsType T -> IsDomType T
  | IDT_inter T TS :
    IsType T -> IsDomType TS -> 
    IsDomType (Eequal T TS).

Definition IsCtx (Γ : Ctx) := 
  forall i, IsDomType (Γ i).

Inductive InDomType (T : Expr) : Expr -> Prop := 
  | InDT_one : InDomType T T
  | InDT_inter_here TS :
    InDomType T (Eequal T TS)
  | InDT_inter_there T' TS :
    InDomType T TS ->
    InDomType T (Eequal T' TS).

Inductive HasType : Ctx -> Expr -> Expr -> Prop :=
  | HT_con : forall Gamma n, 
    IsCtx Gamma ->
    HasType Gamma (Econ n) (Eimg Enat)
  | HT_var : forall Gamma i T, 
    IsCtx Gamma ->
    InDomType T (Gamma i) ->
    HasType Gamma (Evar i) T
  | HT_lam : forall Gamma T T' e,
    IsDomType T ->
    HasType (ctx_ext Gamma T) e T' ->
    HasType Gamma (Elam T e) (Elam T T')
  | HT_app : forall Gamma T1 T2 e1 e2,
      HasType Gamma e1 (Elam T1 T2) ->
      HasDomType Gamma e2 T1 ->
      HasType Gamma (Eapp e1 e2) T2
  | HT_add : forall Gamma e1 e2,
      HasType Gamma e1 (Eimg Enat) ->
      HasType Gamma e2 (Eimg Enat) ->
      HasType Gamma (Eadd e1 e2) (Eimg Enat)
with HasDomType : Ctx -> Expr -> Expr -> Prop := 
  | HDT_one  Γ e T : 
    HasType Γ e T -> HasDomType Γ e T
  | HDT_inter Γ e T TS : 
    HasType Γ e T -> HasDomType Γ e TS -> HasDomType Γ e (Eequal T TS).

End IntersectionTypes.


(** Environments ***)

Definition Env := nat -> ZFSet.

Definition env_ext (rho : Env) (v : ZFSet) : Env :=
  fun n => match n with O => v | S m => rho m end.


(** env_ext lookup laws ***)

Theorem env_ext_zero : forall (rho : Env) (v : ZFSet), env_ext rho v O = v.
Proof. reflexivity. Qed.

Theorem env_ext_succ :
  forall (rho : Env) (v : ZFSet) (n : nat), env_ext rho v (S n) = rho n.
Proof. reflexivity. Qed.


(** * Example expressions drawn from the Verse test suite.

    The definitions below translate the Verse tests that can be written
    using the [Expr] constructors above, drawn from all three test files
    in [VersePrototypes/versetests/]:
      - [tests.versetest]    (runtime [testeq] tests),
      - [verify.versetest]   (verifier [verify] tests), and
      - [meetings.versetest] (a mix of both).
    Each definition is preceded by a comment giving the original Verse
    source verbatim (with its test name).  Goal: express *everything that
    the syntax can represent*, even where the current evaluators do not
    yet give those constructors semantics.

    Constructs with no [Expr] form, and hence omitted, are: tuples/arrays
    [(a,b)] / [array{...}] (and [Length], splicing [..]); the comparison
    operators [<], [<=], [>], [>=], [<>]; the aggregation forms [all],
    [for], [one], [first], [exists]; conditionals [if]/[then]/[else];
    subtraction, multiplication and division; character/string literals
    ['m'] / ["monkey"]; and [type{...}], [truth{}], [option{}], pointers,
    [where], [&]-patterns and [check]/[option] forms.  (Effect annotations
    [<closed>] / [<decides>] / [<succeeds>] carry no value and are simply
    dropped.)

    Translation conventions:
      - a Verse integer literal [k] is [Econ k];
      - the integer type [int] used as a *type/domain* (i.e. [:int]) is
        [Eimg Enat];
      - the type used as a *predicate / coercion*, [int[e]] or [nat[e]],
        is [Eapp Enat e] — cf. the [step_is_nat] rule above, in which
        [Eapp Enat (Econ n)] steps to [Econ n];
      - unification [e1 = e2] is [Eequal] (set intersection), so a clashing
        unification denotes the empty set, i.e. Verse's [:false];
      - choice [e1 | e2] is [Echoice] (set union);
      - sequencing [e1 ; e2] is [Eseq]; the binding forms [x := e] and
        [x := e1; e2] are [Eassign] / [Elet]; and a logical-variable
        declaration [x : T; rest] (an existential of type [T]) is
        [Ebind T rest] — cf. the [Ebind] arm of Timbda2's evaluator.

    Variable convention: indices are de Bruijn.  Every binder — [Elam],
    [Ebind], and (here) [Elet] / [Eassign] — introduces a fresh variable
    at index [0] and shifts the existing variables up by one; the [nat]
    argument of [Elet] / [Eassign] is taken to be that fresh index [0]. *)

(** ** A note on [Elam] binding, which differs across the evaluators.

    The body of [Elam e1 e2] is interpreted differently by the three
    evaluators, and in two of them the *domain* [e1] may bind more than
    one variable:

      - Timbda0: [eval (Elam e1 e2) ρ = Π[a ∈ eval e1 ρ] eval e2 (env_ext ρ a)].
        The body sees the single domain element [a] at index [0] — an
        ordinary one-argument function.

      - Timbda1: the body binds [psnd ab] (the value component of a domain
        *pair* [ab]) at index [0] — still one bound value, but projected
        out of a pair.

      - Timbda2: [eval (Elam e1 e2) env = Π[t ∈ eval e1 env] eval e2 (proj1 t)].
        The body runs under the whole *environment* [proj1 t] produced by
        the domain.  Because the domain [e1] is itself an expression that
        threads/extends the environment (e.g. a telescope of [Ebind]s),
        it can introduce *several* variables that are all in scope in the
        body — modelling a multi-argument function such as
        [f(x:int, y:int) := x+y].

    The single-argument examples below read the same under all three; the
    multi-argument example [ex_multiarg_dom] only makes sense under the
    Timbda1/Timbda2 reading and is annotated accordingly. *)

(** ** Core values and addition. *)

(* testeq("EV1",pass) {2}                                  {2} *)
Definition ex_EV1 : Expr := Econ 2.

(* testeq("Arith1",pass) {3+4}                             {7} *)
Definition ex_Arith1 : Expr := Eadd (Econ 3) (Econ 4).

(* verify("multiline", pass){ 1 + 2 + 3 } *)
Definition ex_multiline : Expr := Eadd (Eadd (Econ 1) (Econ 2)) (Econ 3).

(** ** Unification ([=] is intersection; a clash denotes [:false] = [∅]). *)

(* testeq("Unif16",pass) {1=1}                             {1} *)
Definition ex_Unif16 : Expr := Eequal (Econ 1) (Econ 1).

(* testeq("Unif",pass) {1=2}                               {:false} *)
Definition ex_Unif : Expr := Eequal (Econ 1) (Econ 2).

(* testeq("Cmp17",pass) {3=3}                              {3} *)
Definition ex_Cmp17 : Expr := Eequal (Econ 3) (Econ 3).

(* testeq("Cmp16",pass) {3=4}                              {:false} *)
Definition ex_Cmp16 : Expr := Eequal (Econ 3) (Econ 4).

(* testeq("Cmp18",pass) {3=2}                              {:false} *)
Definition ex_Cmp18 : Expr := Eequal (Econ 3) (Econ 2).

(* verify("plus1", pass){ main()<closed> := { 1 + 2 = 3 } }   (the body 1+2=3) *)
Definition ex_plus1 : Expr := Eequal (Eadd (Econ 1) (Econ 2)) (Econ 3).

(* verify("plus2", fail){ main()<closed> := { 1 + 2 = 4 } }   (the body 1+2=4) *)
Definition ex_plus2 : Expr := Eequal (Eadd (Econ 1) (Econ 2)) (Econ 4).

(** ** Choice ([|] is union). *)

(* the choice subexpression [1|2], e.g. inside testeq("Array10a") {all{1|2}} *)
Definition ex_choice_1_2 : Expr := Echoice (Econ 1) (Econ 2).

(** ** Sequencing, assignment, and let ([;], [x:=e], [x:=e1; e2]). *)

(* testeq("EV2",pass) {x:=1; x}                            {1} *)
Definition ex_EV2 : Expr := Elet 0 (Econ 1) (Evar 0).

(* testeq("Pat1",pass) {x:=1}                              {1} *)
Definition ex_Pat1 : Expr := Eassign 0 (Econ 1).

(* verify("T24",pass) { g()<closed> := (2 = 2; 2) }        (the body 2=2; 2) *)
Definition ex_T24 : Expr := Eseq (Eequal (Econ 2) (Econ 2)) (Econ 2).

(** ** The [nat]/[int] type as a predicate: [int[e]] = [Eapp Enat e]
       (cf. [step_is_nat] / [step_isnt_nat] above). *)

(* [int[5]] / [nat[5]]: applying the nat predicate to a constant returns it.
   (cf. step_is_nat; appears e.g. in verify("T1") / verify("Tricky:L1").) *)
Definition ex_isnat : Expr := Eapp Enat (Econ 5).

(* [int[(x:int => x)]]: the nat predicate on a lambda fails (cf. step_isnt_nat). *)
Definition ex_isnt_nat : Expr := Eapp Enat (Elam (Eimg Enat) (Evar 0)).

(** ** Logical-variable declarations [x:T; rest] via [Ebind]. *)

(* verify("T56",pass) {(x:int, x=3)} — the conjunct {x:int; x=3}, result 3 *)
Definition ex_T56 : Expr := Ebind (Eimg Enat) (Eequal (Evar 0) (Econ 3)).

(* testeq("TB1",pass) {(x:int) = 5}                        {5} *)
Definition ex_TB1 : Expr := Ebind (Eimg Enat) (Eequal (Evar 0) (Econ 5)).

(* testeq("Unif20",pass) {x:any; y:any; x=y; y=1}          {1}   (x=Evar 1, y=Evar 0) *)
Definition ex_Unif20 : Expr :=
  Ebind Eany (Ebind Eany
    (Eseq (Eequal (Evar 1) (Evar 0)) (Eequal (Evar 0) (Econ 1)))).

(* testeq("Unif21",pass) {x:any; y:any; x=1; y=x}          {1} *)
Definition ex_Unif21 : Expr :=
  Ebind Eany (Ebind Eany
    (Eseq (Eequal (Evar 1) (Econ 1)) (Eequal (Evar 0) (Evar 1)))).

(* testeq("JB3",pass) {x:any; y:any; x = y; x = y; x = 1}  {1} *)
Definition ex_JB3 : Expr :=
  Ebind Eany (Ebind Eany
    (Eseq (Eequal (Evar 1) (Evar 0))
      (Eseq (Eequal (Evar 1) (Evar 0)) (Eequal (Evar 1) (Econ 1))))).

(** ** Functions: abstraction [(x:T => body)] = [Elam T body] and
       application [f[a]] = [Eapp f a]. *)

(* the polymorphic identity {(x:any => x)} (Unif3/Unif19; verify T25, M20Jan25-1) *)
Definition ex_poly_id : Expr := Elam Eany (Evar 0).

(* verify("T26",pass) {g(x:int)<closed> := x}              (the int identity) *)
Definition ex_int_id : Expr := Elam (Eimg Enat) (Evar 0).

(* verify("T3",pass) {f(x:int)<closed>:=x+1}               (the successor function) *)
Definition ex_succ : Expr := Elam (Eimg Enat) (Eadd (Evar 0) (Econ 1)).

(* testeq("Fun1",pass) {(x:int => x+1)[2]}                 {3} *)
Definition ex_Fun1 : Expr := Eapp ex_succ (Econ 2).

(** ** [Elamb] versions of the function examples (Timbda2 binding).

    The [Elam] examples above use [Timbda0]/[Timbda1]'s substitution-style
    binding, where the domain payload becomes the bound variable.  Under
    [Timbda2], [Elam] runs the body at the domain triple's *environment*
    ([proj1 t]) without extending it, so those examples do not bind.  The
    [Elamb] constructor *does* extend the environment with the bound value
    (it runs the body at [env_cons (proj3 t) (proj1 t)]), so these [_b]
    variants read in [Timbda2] exactly as the originals do in [Timbda1].
    See [Timbda2.eval_lamb] and the worked denotations there. *)

(* {(x:any => x)} with Timbda2 binding *)
Definition ex_poly_id_b : Expr := Elam Eany (Evar 0).

(* {g(x:int) := x} (the int identity), Timbda2 binding *)
Definition ex_int_id_b : Expr := Elam (Eimg Enat) (Evar 0).

(* {f(x:int) := x+1} (the successor function), Timbda2 binding *)
Definition ex_succ_b : Expr := Elam (Eimg Enat) (Eadd (Evar 0) (Econ 1)).

(* {(x:int => x+1)[2]} = 3, Timbda2 binding *)
Definition ex_Fun1_b : Expr := Eapp ex_succ_b (Econ 2).

(* verify("ReflInt1",pass) {foo(z:int)<closed> := { z = z }} *)
Definition ex_ReflInt1 : Expr := Elam (Eimg Enat) (Eequal (Evar 0) (Evar 0)).

(* verify("ReflInt2",pass) {f(x:int)<closed>:={x=x;x}} *)
Definition ex_ReflInt2 : Expr :=
  Elam (Eimg Enat) (Eseq (Eequal (Evar 0) (Evar 0)) (Evar 0)).

(* verify("Curry1",pass) {f(x:int)<closed>(y:int)<closed>:int := x+y}  (curried add) *)
Definition ex_Curry1 : Expr :=
  Elam (Eimg Enat) (Elam (Eimg Enat) (Eadd (Evar 1) (Evar 0))).

(* verify("M20Jan25-5",pass) {f(x:int)(y:int)(z:int):int := x+y+z}  (3-level curry) *)
Definition ex_curry3 : Expr :=
  Elam (Eimg Enat) (Elam (Eimg Enat) (Elam (Eimg Enat)
    (Eadd (Eadd (Evar 2) (Evar 1)) (Evar 0)))).

(* testeq("Pat9",pass) {succ(x:int)<closed>:int:=x+1; succ[5]}   {6}
   succ bound by [Elet] at index 0, then applied. *)
Definition ex_succ_app : Expr := Elet 0 ex_succ (Eapp (Evar 0) (Econ 5)).

(* testeq("jan1",pass) {(f(x:int)<succeeds>:= x); y:int; f[y]=3}  {3}
   f := (x:int => x) at index 0; then y:int (f shifts to 1); then f[y]=3. *)
Definition ex_jan1 : Expr :=
  Elet 0 ex_int_id
    (Ebind (Eimg Enat) (Eequal (Eapp (Evar 1) (Evar 0)) (Econ 3))).

(** ** Functions used as (non-retraction) types: [a : succ] is a domain
       whose "type" is the function [succ]. *)

(* testeq("EV10a",pass) {succ(x:int)<closed>:=x+1; f(a:succ)<closed> := a; f[3]}  {4}
   succ at index 0; f := (a:succ => a) (domain [succ] = [Evar 0]); then f[3]. *)
Definition ex_EV10a : Expr :=
  Elet 0 ex_succ
    (Elet 0 (Elam (Evar 0) (Evar 0)) (Eapp (Evar 0) (Econ 3))).

(** ** A multi-argument domain (Timbda1/Timbda2 reading — see the note above).

    [verify("T17") { g(x:int, y:int)<closed> := {a := x; b := a; b = y} }]
    and [testeq("Fun8") { f(x:int, y:int) := 2*x+y; ... }] both use a
    function whose *domain binds two variables*.  Taking the simpler body
    [x + y] (Fun8 without the unexpressible [2*]), the function
    [(x:int, y:int) => x+y] is an [Elam] whose domain is a telescope of
    two binders.  Under Timbda2 the body then sees both [x] (index 1) and
    [y] (index 0).  The precise environment threaded by the domain is
    evaluator-specific; this records the *shape*. *)
Definition ex_multiarg_dom : Expr :=
  Elam (Ebind (Eimg Enat) (Eimg Enat)) (Eadd (Evar 1) (Evar 0)).

(** ** The EV30–EV50 family: every combination of the three subexpressions
       [(1=1)], [(2;1)] and [(+1)] under [=] and [;].  ([+e] is [0+e].) *)

Definition e_eq11  : Expr := Eequal (Econ 1) (Econ 1).   (* (1=1) *)
Definition e_seq21 : Expr := Eseq   (Econ 2) (Econ 1).   (* (2;1) *)
Definition e_plus1 : Expr := Eadd   (Econ 0) (Econ 1).   (* (+1)  *)

(* testeq("EV30",pass) {(1=1) = (1=1)}   {1} *) Definition ex_EV30 : Expr := Eequal e_eq11  e_eq11.
(* testeq("EV31",pass) {(1=1) = (2;1)}   {1} *) Definition ex_EV31 : Expr := Eequal e_eq11  e_seq21.
(* testeq("EV32",pass) {(1=1) = (+1)}    {1} *) Definition ex_EV32 : Expr := Eequal e_eq11  e_plus1.
(* testeq("EV33",pass) {(2;1) = (1=1)}   {1} *) Definition ex_EV33 : Expr := Eequal e_seq21 e_eq11.
(* testeq("EV34",pass) {(2;1) = (2;1)}   {1} *) Definition ex_EV34 : Expr := Eequal e_seq21 e_seq21.
(* testeq("EV35",pass) {(2;1) = (+1)}    {1} *) Definition ex_EV35 : Expr := Eequal e_seq21 e_plus1.
(* testeq("EV36",pass) {(+1) = (1=1)}    {1} *) Definition ex_EV36 : Expr := Eequal e_plus1 e_eq11.
(* testeq("EV37",pass) {(+1) = (2;1)}    {1} *) Definition ex_EV37 : Expr := Eequal e_plus1 e_seq21.
(* testeq("EV38",pass) {(+1) = (+1)}     {1} *) Definition ex_EV38 : Expr := Eequal e_plus1 e_plus1.
(* testeq("EV39",pass) {(1=1) ; (1=1)}   {1} *) Definition ex_EV39 : Expr := Eseq e_eq11  e_eq11.
(* testeq("EV40",pass) {(1=1) ; (2;1)}   {1} *) Definition ex_EV40 : Expr := Eseq e_eq11  e_seq21.
(* testeq("EV41",pass) {(1=1) ; (+1)}    {1} *) Definition ex_EV41 : Expr := Eseq e_eq11  e_plus1.
(* testeq("EV42",pass) {(2;1) ; (1=1)}   {1} *) Definition ex_EV42 : Expr := Eseq e_seq21 e_eq11.
(* testeq("EV43",pass) {(2;1) ; (2;1)}   {1} *) Definition ex_EV43 : Expr := Eseq e_seq21 e_seq21.
(* testeq("EV44",pass) {(2;1) ; (+1)}    {1} *) Definition ex_EV44 : Expr := Eseq e_seq21 e_plus1.
(* testeq("EV45",pass) {(+1) ; (1=1)}    {1} *) Definition ex_EV45 : Expr := Eseq e_plus1 e_eq11.
(* testeq("EV46",pass) {(+1) ; (2;1)}    {1} *) Definition ex_EV46 : Expr := Eseq e_plus1 e_seq21.
(* testeq("EV47",pass) {(+1) ; (+1)}     {1} *) Definition ex_EV47 : Expr := Eseq e_plus1 e_plus1.
(* testeq("EV48",pass) {  + (1=1) }      {1} *) Definition ex_EV48 : Expr := Eadd (Econ 0) e_eq11.
(* testeq("EV49",pass) {  + (2;1) }      {1} *) Definition ex_EV49 : Expr := Eadd (Econ 0) e_seq21.
(* testeq("EV50",pass) {  + (+1) }       {1} *) Definition ex_EV50 : Expr := Eadd (Econ 0) e_plus1.


(** ** Translation of predicative System F ([Lang.PolyF]) into [Expr].

    System F has two de Bruijn namespaces — term variables and type
    variables — but [Expr] has a single environment.  A *binder stack*
    records, innermost-first, whether each enclosing binder is a term
    binder [bTm] or a type binder [bTy]; both push one slot onto the
    Timbda environment (a [Λ] and a [λ] alike), so a PolyF variable maps to
    its absolute Timbda slot via [tmIx] / [tyIx].

    The encoding follows the [poly_t] conventions of [Timbda2]: a type is
    an [Expr] denoting a (partial-identity) type-value ([Enat], a variable,
    or an arrow/∀ former); a binder over a type [σ] ranges over its
    *inhabitants* [:σ = Eimg σ] (so binder domains are [Eimg]-wrapped),
    using the environment-extending binder [Elamb]; a type variable is
    bound over the type universe [:type = Eimg Etype].  Type application
    feeds the translated monotype (a type-value) directly, as in
    [Eapp poly_t Enat]. *)
Module PolyF.

Module Src := Lang.PolyF.
Local Open Scope list_scope.

Inductive bnd := bTm | bTy.

(** Timbda slot of the [n]-th term variable under stack [Δ]. *)
Fixpoint tmIx (Δ : list bnd) (n : nat) : nat :=
  match Δ with
  | nil       => n
  | bTm :: Δ' => match n with O => O | S m => S (tmIx Δ' m) end
  | bTy :: Δ' => S (tmIx Δ' n)
  end.

(** Timbda slot of the [n]-th type variable under stack [Δ]. *)
Fixpoint tyIx (Δ : list bnd) (n : nat) : nat :=
  match Δ with
  | nil       => n
  | bTy :: Δ' => match n with O => O | S m => S (tyIx Δ' m) end
  | bTm :: Δ' => S (tyIx Δ' n)
  end.

(** Monotypes: [MNat] to the nat type-value [Enat], a type variable to its
    Timbda slot, an arrow to a function former over the source's
    inhabitants. *)
Fixpoint trMono (Δ : list bnd) (t : Src.Mono) : Expr :=
  match t with
  | Src.MNat     => Enat
  | Src.MVar n   => Evar (tyIx Δ n)
  | Src.MArr a b => Elam (trMono Δ a) (trMono (bTm :: Δ) b)
  end.

(** Polytypes: monotypes, arrows, and [∀] (a function over the type
    universe [:type]). *)
Fixpoint trPoly (Δ : list bnd) (s : Src.Poly) : Expr :=
  match s with
  | Src.PMono t  => trMono Δ t
  | Src.PArr a b => Elam (trPoly Δ a) (trPoly (bTm :: Δ) b)
  | Src.PAll a   => Elam (Eimg Etype) (trPoly (bTy :: Δ) a)
  end.

(** Terms: term abstraction binds an inhabitant of its domain type; type
    abstraction binds a type over [:type]; type application applies to the
    translated monotype (a type-value). *)
Fixpoint trTm (Δ : list bnd) (e : Src.Tm) : Expr :=
  match e with
  | Src.tvar n     => Evar (tmIx Δ n)
  | Src.tlam s e   => Elam (trPoly Δ s) (trTm (bTm :: Δ) e)
  | Src.tapp e1 e2 => Eapp (trTm Δ e1) (trTm Δ e2)
  | Src.tTlam e    => Elam (Eimg Etype) (trTm (bTy :: Δ) e)
  | Src.tTapp e τ  => Eapp (trTm Δ e) (trMono Δ τ)
  | Src.tcon n     => Econ n
  | Src.tadd e1 e2 => Eadd (trTm Δ e1) (trTm Δ e2)
  end.

(** Top-level translations (empty binder stack). *)
Definition trMono0 (t : Src.Mono) : Expr := trMono nil t.
Definition trPoly0 (s : Src.Poly) : Expr := trPoly nil s.
Definition trTm0   (e : Src.Tm)   : Expr := trTm   nil e.

End PolyF.
