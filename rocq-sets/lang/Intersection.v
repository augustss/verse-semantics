(* lang/Intersection.v

   A Coppo–Dezani intersection type system whose discipline matches
   [Syntax.IntersectionTypes]: intersections occur only in *domain*
   (argument) positions.  Types [Ty] and domain types [DomTy] are
   mutually inductive — so every type is well-formed by construction —
   and interpreted set-theoretically in [ZFSet], an arrow as a function
   space and an intersection as set intersection.

   The mutual structure (types/domain-types, has_type/has_dom_type,
   membership-in-a-domain) mirrors [Syntax.IntersectionTypes] so that the
   translation in [Timbda.TimIntersection] is a clean correspondence. *)

Require Import ZFSet.
Require Import ZFNotation.

(** * Syntax *)

(** A [Ty] is a base type or an arrow whose *domain* is an intersection
    ([DomTy]) and whose codomain is again a [Ty]; a [DomTy] is a nonempty
    intersection of [Ty]s. *)
Inductive Ty : Set :=
  | TNat : Ty
  | TArr : DomTy -> Ty -> Ty
with DomTy : Set :=
  | DOne   : Ty -> DomTy
  | DInter : Ty -> DomTy -> DomTy.

(** Terms (de Bruijn); a lambda's domain annotation is a [DomTy]. *)
Inductive Tm : Set :=
  | tvar : nat -> Tm
  | tlam : DomTy -> Tm -> Tm
  | tapp : Tm -> Tm -> Tm
  | tcon : nat -> Tm
  | tadd : Tm -> Tm -> Tm.

(** * Typing *)

Definition Ctx := nat -> DomTy.

Definition ctx_cons (D : DomTy) (Γ : Ctx) : Ctx :=
  fun n => match n with O => D | S m => Γ m end.

(** [InDom T D]: the type [T] is one of the components of the
    intersection [D]. *)
Inductive InDom (T : Ty) : DomTy -> Prop :=
  | InD_one   : InDom T (DOne T)
  | InD_here  : forall D, InDom T (DInter T D)
  | InD_there : forall T' D, InDom T D -> InDom T (DInter T' D).

Inductive has_type : Ctx -> Tm -> Ty -> Prop :=
  | T_con : forall Γ n,
      has_type Γ (tcon n) TNat
  | T_var : forall Γ i T,
      InDom T (Γ i) ->
      has_type Γ (tvar i) T
  | T_lam : forall Γ D T2 e,
      has_type (ctx_cons D Γ) e T2 ->
      has_type Γ (tlam D e) (TArr D T2)
  | T_app : forall Γ D T2 e1 e2,
      has_type Γ e1 (TArr D T2) ->
      has_dom_type Γ e2 D ->
      has_type Γ (tapp e1 e2) T2
  | T_add : forall Γ e1 e2,
      has_type Γ e1 TNat ->
      has_type Γ e2 TNat ->
      has_type Γ (tadd e1 e2) TNat
with has_dom_type : Ctx -> Tm -> DomTy -> Prop :=
  | TD_one : forall Γ e T,
      has_type Γ e T ->
      has_dom_type Γ e (DOne T)
  | TD_inter : forall Γ e T D,
      has_type Γ e T ->
      has_dom_type Γ e D ->
      has_dom_type Γ e (DInter T D).

Scheme has_type_min := Minimality for has_type Sort Prop
  with has_dom_type_min := Minimality for has_dom_type Sort Prop.
Combined Scheme has_type_has_dom_type_mut
  from has_type_min, has_dom_type_min.

(** * Set-theoretic semantics *)

Fixpoint evalTy (T : Ty) : ZFSet :=
  match T with
  | TNat      => Omega
  | TArr D T2 => Pi (evalDomTy D) (fun _ => evalTy T2)
  end
with evalDomTy (D : DomTy) : ZFSet :=
  match D with
  | DOne T      => evalTy T
  | DInter T D  => evalTy T ∩ evalDomTy D
  end.

Definition Env := nat -> ZFSet.

Definition env_cons (v : ZFSet) (ρ : Env) : Env :=
  fun n => match n with O => v | S m => ρ m end.

Fixpoint evalTm (e : Tm) (ρ : Env) : ZFSet :=
  match e with
  | tvar n     => ρ n
  | tlam D e   => iUnion (evalDomTy D) (fun a => {| ⟨ a , evalTm e (env_cons a ρ) ⟩ |})
  | tapp e1 e2 => applyFun (evalTm e1 ρ) (evalTm e2 ρ)
  | tcon n     => natZ n
  | tadd e1 e2 => natZAdd (evalTm e1 ρ) (evalTm e2 ρ)
  end.

(** * Soundness *)

(** A domain type's denotation is included in each component's. *)
Lemma InDom_Inc : forall T D, InDom T D -> Inc (evalDomTy D) (evalTy T).
Proof.
  intros T D H. induction H; cbn [evalDomTy].
  - apply Inc_refl.
  - apply BinInter_Inc_l.
  - eapply Inc_tran; [ apply BinInter_Inc_r | exact IHInDom ].
Qed.

Definition models (Γ : Ctx) (ρ : Env) : Prop :=
  forall n, ρ n ∈ evalDomTy (Γ n).

Lemma models_cons Γ ρ v D :
  models Γ ρ -> v ∈ evalDomTy D -> models (ctx_cons D Γ) (env_cons v ρ).
Proof. intros HM Hv [|n]; cbn; [ exact Hv | apply HM ]. Qed.

Theorem soundness_and :
  (forall Γ e T, has_type Γ e T ->
     forall ρ, models Γ ρ -> evalTm e ρ ∈ evalTy T) /\
  (forall Γ e D, has_dom_type Γ e D ->
     forall ρ, models Γ ρ -> evalTm e ρ ∈ evalDomTy D).
Proof.
  apply has_type_has_dom_type_mut.

  - (* T_con *) intros Γ n ρ HM. apply natZ_mem_omega.

  - (* T_var *) intros Γ i T Hin ρ HM. cbn [evalTm].
    apply (Inc_IN _ _ _ (InDom_Inc T (Γ i) Hin)). apply HM.

  - (* T_lam *) intros Γ D T2 e Hbody IH ρ HM. cbn [evalTm evalTy].
    apply (iUnion_graph_mem_pi (evalDomTy D) (fun _ => evalTy T2)
             (fun a => evalTm e (env_cons a ρ))).
    intros a Ha. apply IH. apply models_cons; assumption.

  - (* T_app *) intros Γ D T2 e1 e2 Hf IHf Ha IHa ρ HM. cbn [evalTm].
    apply (applyFun_mem_of_pi (evalDomTy D) (fun _ => evalTy T2)
             (evalTm e1 ρ) (evalTm e2 ρ)).
    + apply IHf; exact HM.
    + apply IHa; exact HM.

  - (* T_add *) intros Γ e1 e2 H1 IH1 H2 IH2 ρ HM. cbn [evalTm evalTy].
    pose proof (IH1 ρ HM) as Hv1. pose proof (IH2 ρ HM) as Hv2.
    cbn [evalTy] in Hv1, Hv2.
    destruct (In_omega _ Hv1) as [n1 E1]. destruct (In_omega _ Hv2) as [n2 E2].
    rewrite <- E1, <- E2, natZAdd_natZ. apply natZ_mem_omega.

  - (* TD_one *) intros Γ e T H IH ρ HM. cbn [evalDomTy]. apply IH; exact HM.

  - (* TD_inter *) intros Γ e T D H1 IH1 H2 IH2 ρ HM. cbn [evalDomTy].
    apply IN_BinInter; [ apply IH1 | apply IH2 ]; exact HM.
Qed.

Theorem soundness Γ e T :
  has_type Γ e T -> forall ρ, models Γ ρ -> evalTm e ρ ∈ evalTy T.
Proof. destruct soundness_and as [H _]. exact (H Γ e T). Qed.

Theorem soundness_dom Γ e D :
  has_dom_type Γ e D -> forall ρ, models Γ ρ -> evalTm e ρ ∈ evalDomTy D.
Proof. destruct soundness_and as [_ H]. exact (H Γ e D). Qed.
