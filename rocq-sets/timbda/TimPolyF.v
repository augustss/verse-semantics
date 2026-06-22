(* TimPolyF.v:

   A *Timbda2*-flavoured set-theoretic model of predicative System F
   ([Lang.PolyF]), expressed by **translating** the source language into
   [Syntax.Expr] and interpreting the translated term with the Timbda2
   *triple* evaluator [Timbda2.eval].

   Unlike the earlier direct-semantics version of this file (which defined a
   bespoke [evalTm'] fixpoint over [Src.Tm]), the term semantics here is
   literally

       evalTm' Δ e env := eval (trTm Δ e) env

   where [trTm] is the translation defined below and [eval] is the Timbda2
   evaluator, whose values are sets of triples ⟨env, a, b⟩.

   A *type* [σ] denotes the set of *value triples* of that type, namely the
   image projection [:trPoly Δ σ = Eimg (trPoly Δ σ)] — Timbda2's own
   "inhabitants of a type" construct.  Soundness is therefore a set
   *containment*:

       has_type Γ e s  ->  WellTyped Γ Δ env  ->
         eval (trTm Δ e) env ⊆ eval (Eimg (trPoly Δ s)) env.

   STATUS: work in progress.  The translation, the redefined term
   semantics, the triple-valued type denotation, and the soundness
   *statement* are complete and compile; the soundness *proof* discharges
   the immediately tractable cases ([T_var], [T_con]) and [admit]s the
   remaining cases ([T_lam], [T_app], [T_Tlam], [T_Tapp], [T_add]), which
   require a general arrow-/∀-type story for the triple semantics that the
   Timbda2 development does not yet provide.  The theorem is therefore
   closed with [Admitted]. *)

Require Import ZFSet.
Require Import ZFNotation.
Require Import Diagonal.
Require Import Syntax.
Require Import Timbda2.
Require Lang.PolyF.

Module Src := Lang.PolyF.
Module Tr  := Syntax.PolyF.

Local Open Scope list_scope.

(* ================================================================== *)
(** * A fresh translation [Src.Tm -> Syntax.Expr] for the triple model. *)
(* ================================================================== *)

(** System F has two de Bruijn namespaces (term and type variables); the
    Timbda environment has one.  A *binder stack* [Δ : list Tr.bnd]
    records, innermost-first, whether each enclosing binder is a term
    binder [bTm] or a type binder [bTy]; both push one slot onto the
    Timbda environment.  A source term/type variable maps to its absolute
    Timbda slot via [Tr.tmIx] / [Tr.tyIx] (reused from [Syntax.PolyF]).

    Conventions for the Timbda2 (partial-identity) view of types:
      - a *type* translates to an [Expr] that denotes a (partial-identity)
        *type value* ([Enat], a type variable, an arrow/∀ former);
      - a binder over a type [σ] ranges over the *inhabitants* of [σ],
        i.e. its domain is the image projection [Eimg (trPoly Δ σ)];
      - a type variable binder ([Λ] / [∀]) ranges over the type universe
        [:type = Eimg Etype];
      - type application feeds the translated monotype's *type value*
        [trMono Δ τ] directly (cf. [Eapp Etype]/[poly_t] in Timbda2). *)

(** Monotypes: [MNat] is the nat type value [Enat]; a type variable reads
    its Timbda slot; an arrow is a function former whose domain ranges over
    the inhabitants of [a]. *)
Fixpoint trMono (Δ : list Tr.bnd) (t : Src.Mono) : Expr :=
  match t with
  | Src.MNat     => Enat
  | Src.MVar n   => Evar (Tr.tyIx Δ n)
  | Src.MArr a b => Elam (Eimg (trMono Δ a)) (trMono (Tr.bTm :: Δ) b)
  end.

(** Polytypes: monotypes; arrows (domain ranges over inhabitants of [a]);
    and [∀] (a function over the type universe [:type]). *)
Fixpoint trPoly (Δ : list Tr.bnd) (s : Src.Poly) : Expr :=
  match s with
  | Src.PMono t  => trMono Δ t
  | Src.PArr a b => Elam (Eimg (trPoly Δ a)) (trPoly (Tr.bTm :: Δ) b)
  | Src.PAll a   => Elam (Eimg Etype) (trPoly (Tr.bTy :: Δ) a)
  end.

(** Terms: a term abstraction binds an inhabitant of its domain type; a
    type abstraction binds a type over [:type]; an application is [Eapp];
    a type application applies to the translated monotype's type value. *)
Fixpoint trTm (Δ : list Tr.bnd) (e : Src.Tm) : Expr :=
  match e with
  | Src.tvar n     => Evar (Tr.tmIx Δ n)
  | Src.tlam s e   => Elam (Eimg (trPoly Δ s)) (trTm (Tr.bTm :: Δ) e)
  | Src.tapp e1 e2 => Eapp (trTm Δ e1) (trTm Δ e2)
  | Src.tTlam e    => Elam (Eimg Etype) (trTm (Tr.bTy :: Δ) e)
  | Src.tTapp e τ  => Eapp (trTm Δ e) (trMono Δ τ)
  | Src.tcon n     => Econ n
  | Src.tadd e1 e2 => Eadd (trTm Δ e1) (trTm Δ e2)
  end.

(** Top-level translations (empty binder stack). *)
Definition trMono0 (t : Src.Mono) : Expr := trMono nil t.
Definition trPoly0 (s : Src.Poly) : Expr := trPoly nil s.
Definition trTm0   (e : Src.Tm)   : Expr := trTm   nil e.

(* ================================================================== *)
(** * The triple semantics, via the translation. *)
(* ================================================================== *)

(** The term semantics is the Timbda2 [eval] of the translated term. *)
Definition evalTm' (Δ : list Tr.bnd) (e : Src.Tm) (env : Env) : ZFSet :=
  eval (trTm Δ e) env.

(** A type [σ] denotes the set of its *value triples* in [env]: the image
    projection [:trPoly Δ σ] = [Eimg (trPoly Δ σ)].  (For [σ = MNat] this is
    [{⟨env, natZ k, natZ k⟩ : k}], for an arrow it is the function values,
    etc.) *)
Definition denot (Δ : list Tr.bnd) (s : Src.Poly) (env : Env) : ZFSet :=
  eval (Eimg (trPoly Δ s)) env.

(* ================================================================== *)
(** * Well-typed Timbda environments. *)
(* ================================================================== *)

(** [env] models [Γ] under the binder stack [Δ] when each source term
    variable's Timbda slot holds a value triple of its type. *)
Definition WellTyped (Γ : Src.Ctx) (Δ : list Tr.bnd) (env : Env) : Prop :=
  forall n,
    ⟨ env , env_lookup env (Tr.tmIx Δ n) , env_lookup env (Tr.tmIx Δ n) ⟩
      ∈ eval (Eimg (trPoly Δ (Γ n))) env.

(* ================================================================== *)
(** * Soundness (set containment). *)
(* ================================================================== *)

(** A well-typed term's value triples all lie in the value triples of its
    type.

    WIP: only [T_var] and [T_con] are discharged; the remaining cases are
    [admit]ted (see the file header). *)
Theorem soundness :
  forall Γ e s,
  Src.has_type Γ e s ->
  forall Δ env, WellTyped Γ Δ env ->
    evalTm' Δ e env ⊆ denot Δ s env.
Proof.
  unfold evalTm', denot.
  induction 1; intros Δ env Hwt; cbn [trTm trPoly trMono].

  - (* T_var: the variable's slot holds a value triple of its type. *)
    rewrite eval_var. apply Sing_Inc_IN. apply (Hwt n).

  - (* T_lam *)
    admit.

  - (* T_app *)
    admit.

  - (* T_Tlam *)
    admit.

  - (* T_Tapp *)
    admit.

  - (* T_con: a numeral is a value triple of the nat type. *)
    rewrite eval_con. apply Sing_Inc_IN. apply con_in_nat.

  - (* T_add *)
    admit.
Admitted.
