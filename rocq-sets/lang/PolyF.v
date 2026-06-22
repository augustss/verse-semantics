(* lang/PolyF.v

   Predicative System F: a polymorphic lambda calculus with a syntactic
   distinction between *monotypes* (small, instantiable) and *polytypes*
   (which may contain quantifiers).  Set-theoretically:

     - a monotype denotes a *small* set (a member of the universe [Big]);
     - type variables range over [Big];
     - a quantifier [∀.σ] denotes the *predicative* product
       [Π_{X ∈ Big} ⟦σ⟧(X·δ)] — a function from small types to values.

   Type application instantiates a quantifier with a *monotype* only
   (predicativity), which is exactly what keeps the argument a member of
   [Big].

   The model is sound: a well-typed term denotes an element of the
   denotation of its type. *)

Require Import ZFSet.
Require Import ZFNotation.
From Stdlib Require Import PeanoNat.
From Stdlib Require Import Lia.
From Stdlib Require Import FunctionalExtensionality.

(** * Universe closure (Grothendieck / inaccessibility properties).

    [Big] is closed under the type-formers used by monotypes: it contains
    [ω] (the lemma [ZFSet.Omega_small]) and is closed under function
    space.  A function space [A → B] is realised set-theoretically as the
    product [Pi A (fun _ => B)], whose elements are functional graphs:
    subsets of [A × B].  Hence [Pi A (fun _ => B) ⊆ Power (A × B)], and
    [Big] — being transitive and closed under products ([Prod_small]) and
    power sets ([In_Power_Big]) — contains it. *)
Lemma Arr_small : forall A B : ZFSet,
  In A Big -> In B Big -> In (Pi A (fun _ => B)) Big.
Proof.
  intros A B HA HB.
  apply small_of_Inc with (V := Power (Prod A (iUnion A (fun _ => B)))).
  - (* a [Pi] is carved by separation out of [Power (A × ⋃B)] *)
    unfold Pi. apply Comp_Inc.
  - apply In_Power_Big. apply Prod_small.
    + exact HA.
    + (* [⋃_{a∈A} B ⊆ B], so it is small *)
      apply small_of_Inc with (V := B).
      * apply iUnion_Inc. intros y _. apply Inc_refl.
      * exact HB.
Qed.

(** * Syntax *)

(** Monotypes: a base type, type variables (de Bruijn), and arrows. *)
Inductive Mono : Set :=
  | MNat : Mono
  | MVar : nat -> Mono
  | MArr : Mono -> Mono -> Mono.

(** Polytypes: monotypes, arrows, and universal quantification. *)
Inductive Poly : Set :=
  | PMono : Mono -> Poly
  | PArr  : Poly -> Poly -> Poly
  | PAll  : Poly -> Poly.

(** Terms (de Bruijn for both term and type variables). *)
Inductive Tm : Set :=
  | tvar  : nat -> Tm
  | tlam  : Poly -> Tm -> Tm     (* λ:σ. e         *)
  | tapp  : Tm -> Tm -> Tm       (* e1 e2          *)
  | tTlam : Tm -> Tm            (* Λ. e           *)
  | tTapp : Tm -> Mono -> Tm     (* e [τ]          *)
  | tcon  : nat -> Tm
  | tadd  : Tm -> Tm -> Tm.

(** * de Bruijn type-variable shifting and (monotype) substitution. *)

Fixpoint shiftMono (c : nat) (t : Mono) : Mono :=
  match t with
  | MNat     => MNat
  | MVar n   => MVar (match Nat.compare n c with Lt => n | _ => S n end)
  | MArr a b => MArr (shiftMono c a) (shiftMono c b)
  end.

Fixpoint shiftPoly (c : nat) (s : Poly) : Poly :=
  match s with
  | PMono t  => PMono (shiftMono c t)
  | PArr a b => PArr (shiftPoly c a) (shiftPoly c b)
  | PAll a   => PAll (shiftPoly (S c) a)
  end.

Fixpoint substMono (j : nat) (s : Mono) (t : Mono) : Mono :=
  match t with
  | MNat     => MNat
  | MVar n   => match Nat.compare n j with
                | Eq => s
                | Lt => MVar n
                | Gt => MVar (Nat.pred n)
                end
  | MArr a b => MArr (substMono j s a) (substMono j s b)
  end.

Fixpoint substPoly (j : nat) (s : Mono) (sg : Poly) : Poly :=
  match sg with
  | PMono t  => PMono (substMono j s t)
  | PArr a b => PArr (substPoly j s a) (substPoly j s b)
  | PAll a   => PAll (substPoly (S j) (shiftMono 0 s) a)
  end.

(** * Typing *)

Definition Ctx := nat -> Poly.

Definition pcons (s : Poly) (Γ : Ctx) : Ctx :=
  fun n => match n with O => s | S m => Γ m end.

(* introducing a type variable shifts the free type variables of the
   whole term context *)
Definition shiftCtx (Γ : Ctx) : Ctx := fun n => shiftPoly 0 (Γ n).

Inductive has_type : Ctx -> Tm -> Poly -> Prop :=
  | T_var : forall Γ n,
      has_type Γ (tvar n) (Γ n)
  | T_lam : forall Γ s1 s2 e,
      has_type (pcons s1 Γ) e s2 ->
      has_type Γ (tlam s1 e) (PArr s1 s2)
  | T_app : forall Γ s1 s2 e1 e2,
      has_type Γ e1 (PArr s1 s2) ->
      has_type Γ e2 s1 ->
      has_type Γ (tapp e1 e2) s2
  | T_Tlam : forall Γ s e,
      has_type (shiftCtx Γ) e s ->
      has_type Γ (tTlam e) (PAll s)
  | T_Tapp : forall Γ s τ e,
      has_type Γ e (PAll s) ->
      has_type Γ (tTapp e τ) (substPoly 0 τ s)
  | T_con : forall Γ n,
      has_type Γ (tcon n) (PMono MNat)
  | T_add : forall Γ e1 e2,
      has_type Γ e1 (PMono MNat) ->
      has_type Γ e2 (PMono MNat) ->
      has_type Γ (tadd e1 e2) (PMono MNat).

(** * Set-theoretic semantics *)

(** Type environments map type variables to (small) sets. *)
Definition tenv := nat -> ZFSet.

Definition tcons (X : ZFSet) (δ : tenv) : tenv :=
  fun n => match n with O => X | S m => δ m end.

Fixpoint evalMono (t : Mono) (δ : tenv) : ZFSet :=
  match t with
  | MNat     => ω
  | MVar n   => δ n
  | MArr a b => Π[ _ ∈ evalMono a δ ] evalMono b δ
  end.

Fixpoint evalPoly (s : Poly) (δ : tenv) : ZFSet :=
  match s with
  | PMono t  => evalMono t δ
  | PArr a b => Π[ _ ∈ evalPoly a δ ] evalPoly b δ
  | PAll a   => Π[ X ∈ Big ] evalPoly a (tcons X δ)
  end.

Definition Env := nat -> ZFSet.

Definition env_cons (v : ZFSet) (ρ : Env) : Env :=
  fun n => match n with O => v | S m => ρ m end.

Fixpoint evalTm (e : Tm) (ρ : Env) (δ : tenv) : ZFSet :=
  match e with
  | tvar n     => ρ n
  | tlam s e   => a ← evalPoly s δ ;; 
                 {| ⟨ a , evalTm e (env_cons a ρ) δ ⟩ |}
  | tapp e1 e2 => (evalTm e1 ρ δ) [ evalTm e2 ρ δ ]
  | tTlam e    => X ← Big ;; {| ⟨ X , evalTm e ρ (tcons X δ) ⟩ |}
  | tTapp e τ  => (evalTm e ρ δ) [ evalMono τ δ ]
  | tcon n     => natZ n
  | tadd e1 e2 => evalTm e1 ρ δ + evalTm e2 ρ δ
  end.

(** * de Bruijn insertion and the shift / substitution semantic lemmas. *)

(** [insert c X δ] inserts [X] at de Bruijn slot [c]. *)
Definition insert (c : nat) (X : ZFSet) (δ : tenv) : tenv :=
  fun n => match Nat.compare n c with
           | Lt => δ n
           | Eq => X
           | Gt => δ (Nat.pred n)
           end.

Lemma insert0 X δ : insert 0 X δ = tcons X δ.
Proof.
  apply functional_extensionality. intros [|n]; reflexivity.
Qed.

Lemma insert_cons c X Y δ :
  insert (S c) X (tcons Y δ) = tcons Y (insert c X δ).
Proof.
  apply functional_extensionality. intros [|n]; [ reflexivity | ].
  unfold insert, tcons. cbn [Nat.compare].
  destruct (Nat.compare_spec n c) as [->| |]; cbn; try reflexivity.
  destruct n; [ lia | reflexivity ].
Qed.

(** Shifting at cutoff [c] is cancelled by inserting at slot [c]. *)
Lemma shiftMono_eval t : forall c X δ,
  evalMono (shiftMono c t) (insert c X δ) = evalMono t δ.
Proof.
  induction t as [| n | a IHa b IHb]; intros c X δ; cbn.
  - reflexivity.
  - destruct (Nat.compare_spec n c) as [->| Hlt | Hgt]; unfold insert.
    + (* n = c: shift gives [S c], compare (S c) c = Gt *)
      replace (Nat.compare (S c) c) with Gt by (symmetry; apply Nat.compare_gt_iff; lia).
      reflexivity.
    + (* n < c *)
      replace (Nat.compare n c) with Lt by (symmetry; apply Nat.compare_lt_iff; lia).
      reflexivity.
    + (* n > c: shift gives [S n], compare (S n) c = Gt, pred (S n) = n *)
      replace (Nat.compare (S n) c) with Gt by (symmetry; apply Nat.compare_gt_iff; lia).
      reflexivity.
  - rewrite IHa. f_equal. apply functional_extensionality. intro. apply IHb.
Qed.

Lemma shiftMono_eval0 m Y δ :
  evalMono (shiftMono 0 m) (tcons Y δ) = evalMono m δ.
Proof. rewrite <- (insert0 Y δ). apply shiftMono_eval. Qed.

Lemma shiftPoly_eval s : forall c X δ,
  evalPoly (shiftPoly c s) (insert c X δ) = evalPoly s δ.
Proof.
  induction s as [t | a IHa b IHb | a IHa]; intros c X δ; cbn.
  - apply shiftMono_eval.
  - rewrite IHa. f_equal. apply functional_extensionality. intro. apply IHb.
  - f_equal. apply functional_extensionality. intro Y.
    rewrite <- insert_cons. apply IHa.
Qed.

(** Substituting a monotype [s] for slot [j] amounts to inserting its
    denotation at slot [j]. *)
Lemma substMono_eval t : forall j s δ,
  evalMono (substMono j s t) δ = evalMono t (insert j (evalMono s δ) δ).
Proof.
  induction t as [| n | a IHa b IHb]; intros j s δ; cbn.
  - reflexivity.
  - unfold insert. destruct (Nat.compare_spec n j) as [->| Hlt | Hgt]; cbn.
    + reflexivity.
    + reflexivity.
    + reflexivity.
  - rewrite IHa. f_equal. apply functional_extensionality. intro. apply IHb.
Qed.

Lemma substPoly_eval s : forall j m δ,
  evalPoly (substPoly j m s) δ = evalPoly s (insert j (evalMono m δ) δ).
Proof.
  induction s as [t | a IHa b IHb | a IHa]; intros j m δ; cbn.
  - apply substMono_eval.
  - rewrite IHa. f_equal. apply functional_extensionality. intro. apply IHb.
  - f_equal. apply functional_extensionality. intro Y.
    rewrite IHa. f_equal.
    rewrite shiftMono_eval0. apply insert_cons.
Qed.

(** * Smallness of monotypes. *)

Definition tenv_ok (δ : tenv) : Prop := forall n, δ n ∈ Big.

Lemma tenv_ok_cons X δ : X ∈ Big -> tenv_ok δ -> tenv_ok (tcons X δ).
Proof. intros HX Hδ [|n]; [ exact HX | apply Hδ ]. Qed.

Lemma mono_small t : forall δ, tenv_ok δ -> evalMono t δ ∈ Big.
Proof.
  induction t as [| n | a IHa b IHb]; intros δ Hδ; cbn.
  - apply Omega_small.
  - apply Hδ.
  - apply Arr_small; [ apply IHa | apply IHb ]; exact Hδ.
Qed.

(** * Soundness *)

Definition models (Γ : Ctx) (ρ : Env) (δ : tenv) : Prop :=
  forall n, ρ n ∈ evalPoly (Γ n) δ.

Lemma models_cons Γ ρ δ v s :
  models Γ ρ δ -> v ∈ evalPoly s δ -> models (pcons s Γ) (env_cons v ρ) δ.
Proof. intros HM Hv [|n]; cbn; [ exact Hv | apply HM ]. Qed.

(** Under a fresh type variable [X], the shifted context has the same
    denotations, so a model is preserved. *)
Lemma models_tyshift Γ ρ δ X :
  models Γ ρ δ -> models (shiftCtx Γ) ρ (tcons X δ).
Proof.
  intros HM n. unfold shiftCtx.
  rewrite <- (insert0 X δ). rewrite shiftPoly_eval. apply HM.
Qed.

(** all types are inhabited in PolyF *)
Theorem soundness :
  forall Γ e s,
  has_type Γ e s ->
  forall ρ δ, tenv_ok δ -> models Γ ρ δ -> evalTm e ρ δ ∈ evalPoly s δ.
Proof.
  induction 1; intros ρ δ Hδ HM; cbn [evalTm evalPoly].

  - (* T_var *) apply HM.

  - (* T_lam *)
    apply (iUnion_graph_mem_pi (evalPoly s1 δ) (fun _ => evalPoly s2 δ)
             (fun a => evalTm e (env_cons a ρ) δ)).
    intros a Ha. apply IHhas_type; [ exact Hδ | ].
    apply models_cons; assumption.

  - (* T_app *)
    apply (applyFun_mem_of_pi (evalPoly s1 δ) (fun _ => evalPoly s2 δ)
             (evalTm e1 ρ δ) (evalTm e2 ρ δ)).
    + apply IHhas_type1; assumption.
    + apply IHhas_type2; assumption.

  - (* T_Tlam: the type-function graph over the universe [Big] *)
    apply (iUnion_graph_mem_pi Big (fun X => evalPoly s (tcons X δ))
             (fun X => evalTm e ρ (tcons X δ))).
    intros X HX. apply IHhas_type.
    + apply tenv_ok_cons; assumption.
    + apply models_tyshift; assumption.

  - (* T_Tapp: instantiate at the (small) monotype [τ] *)
    rewrite substPoly_eval.
    pose proof (mono_small τ δ Hδ) as Hsmall.
    pose proof (IHhas_type ρ δ Hδ HM) as Hfun. cbn [evalPoly] in Hfun.
    pose proof (applyFun_mem_of_pi Big (fun X => evalPoly s (tcons X δ))
                  (evalTm e ρ δ) (evalMono τ δ) Hfun Hsmall) as Happ.
    rewrite insert0. exact Happ.

  - (* T_con *) apply natZ_mem_omega.

  - (* T_add *)
    pose proof (IHhas_type1 ρ δ Hδ HM) as H1.
    pose proof (IHhas_type2 ρ δ Hδ HM) as H2. cbn [evalPoly evalMono] in H1, H2.
    destruct (In_omega _ H1) as [n1 E1]. destruct (In_omega _ H2) as [n2 E2].
    rewrite <- E1, <- E2, natZAdd_natZ. apply natZ_mem_omega.
Qed.
