(** * ZFSetFacts.v — pure ZFSet theory built on the core interface.

    Every lemma below reasons at the [ZFSet] level through the abstract
    interface exported by [ZFSetCore.v], and *states its result in the
    [zf_scope] set notations* of [ZFNotation.v] ([∈], [⊆], [{|·|}],
    [⟨·,·⟩], [∩], [∪], [⋃], [∅], the [·←·;;·] iterators, …).  (Two helpers,
    [BinUnion_eq_Union_Paire] and [Couple_unfold], still touch the [mk]
    representation in their *proofs*; every other proof is
    representation-independent.)

    Contents: the [Pi] / [applyFun] theory, the pure smallness lemmas
    ([small_of_Inc], [BinUnion_small], [Prod_small]), the general-purpose
    facts (intersection, image/range, iterated unions, singleton
    products), and the [zf_reduce] step tactic shared by the three
    interpreters. *)

Require Import ZFSetCore.
Require Import ZFNotation.
From Stdlib Require Import Logic.Epsilon.
Require Import utils.all.

(* State and reason in the ZFSet-level notations ([zf_scope]); the
   Ens-level [ens_scope] (opened transitively by the core) is closed so
   the syntax is unambiguous. *)
Close Scope ens_scope.
Open Scope zf_scope.

(* ===== Pi / applyFun theory ===== *)

(** Inversion: every [X ∈ Pi A B] is a subset of [A × ⋃B] that is
    total and functional. Packages the [Comp] / [Power] unfolding so
    callers don't have to repeat it. *)
Lemma In_Pi_inv :
  forall (A : ZFSet) (B : ZFSet -> ZFSet) (X : ZFSet),
  X ∈ Pi A B ->
  X ⊆ Prod A (iUnion A B) /\
  (forall a, a ∈ A -> exists b, b ∈ B a /\ ⟨ a , b ⟩ ∈ X) /\
  (forall a b1 b2, ⟨ a , b1 ⟩ ∈ X -> ⟨ a , b2 ⟩ ∈ X -> b1 = b2).
Proof.
  intros A B X HX.
  pose proof HX as HXP. unfold Pi in HXP.
  apply In_Comp_P in HXP. destruct HXP as [Htot Hfn].
  pose proof (Comp_Inc (Power (Prod A (iUnion A B)))
              (fun f =>
                (forall a, In a A -> exists b, In b (B a) /\ In (Couple a b) f)
                /\ (forall a b1 b2,
                      In (Couple a b1) f -> In (Couple a b2) f -> b1 = b2)))
    as Hsub.
  rewrite Inc_def in Hsub.
  apply Hsub in HX. apply IN_Power_Inc in HX.
  split; [|split]; assumption.
Qed.

Lemma iUnion_graph_mem_pi :
  forall (A : ZFSet) (B : ZFSet -> ZFSet) (g : ZFSet -> ZFSet),
  (forall a : ZFSet, a ∈ A -> g a ∈ B a) ->
  (a ← A ;; {| ⟨ a , g a ⟩ |}) ∈ Pi A B.
Proof.
  intros A B g hg.
  unfold Pi. apply In_P_Comp.
  - apply Inc_IN_Power, Inc_def. intros x Hx.
    destruct (iUnion_IN _ _ _ Hx) as [a [HaA HxSing]].
    apply IN_Sing_EQ in HxSing. subst x.
    apply Couple_IN_Prod; [exact HaA|].
    apply IN_iUnion with a; [exact HaA | apply hg; exact HaA].
  - split.
    + intros a HaA. exists (g a). split.
      * apply hg; exact HaA.
      * apply IN_iUnion with a; [exact HaA | apply IN_Sing].
    + intros a b1 b2 H1 H2.
      destruct (iUnion_IN _ _ _ H1) as [a1 [_ HS1]].
      destruct (iUnion_IN _ _ _ H2) as [a2 [_ HS2]].
      apply IN_Sing_EQ in HS1, HS2.
      assert (Ea1 : a = a1) by exact (Couple_inj_left _ _ _ _ HS1).
      assert (Eb1 : b1 = g a1) by exact (Couple_inj_right _ _ _ _ HS1).
      assert (Ea2 : a = a2) by exact (Couple_inj_left _ _ _ _ HS2).
      assert (Eb2 : b2 = g a2) by exact (Couple_inj_right _ _ _ _ HS2).
      congruence.
Qed.

(** The singleton graph [{|⟨a,b⟩|}] is a member of any [Pi] over the
    singleton domain [{|a|}] whose codomain at [a] contains [b].
    Specializes [iUnion_graph_mem_pi] to a one-point domain. *)
Lemma Sing_Couple_mem_Pi a b C :
  b ∈ C a -> {| ⟨ a , b ⟩ |} ∈ Pi {| a |} C.
Proof.
  intro h.
  rewrite <- (iUnion_Sing_l a (fun a' => Sing (Couple a' b))).
  apply iUnion_graph_mem_pi with (g := fun _ => b).
  intros a' Ha'. apply IN_Sing_EQ in Ha'. subst a'. exact h.
Qed.

(** A [Pi] over a singleton domain and a constant singleton codomain
    is included in the singleton of its unique graph: every function in
    [Pi {|a|} (fun _ => {|b|})] is [{|⟨a,b⟩|}].  The [Inc]-level
    eliminator for this shape, so equality goals against it close by
    [set_ext] without element-chasing. *)
Lemma Pi_Sing_Sing_Inc a b :
  Pi {| a |} (fun _ => {| b |}) ⊆ {| {| ⟨ a , b ⟩ |} |}.
Proof.
  apply Inc_def. intros X HX.
  apply In_Pi_inv in HX. destruct HX as [XPow [Htot _]].
  rewrite iUnion_Sing_l in XPow. rewrite Prod_Sing_Sing in XPow.
  assert (HC : In (Couple a b) X).
  { destruct (Htot a (IN_Sing a)) as [b' [Hb' HC]].
    apply IN_Sing_EQ in Hb'. subst b'. exact HC. }
  assert (HX : X = Sing (Couple a b)).
  { apply set_ext; [exact XPow|].
    apply Inc_def. intros x Hx. apply IN_Sing_EQ in Hx. subst x. exact HC. }
  subst X. apply IN_Sing.
Qed.

Lemma applyFun_mem_of_pi :
  forall (A : ZFSet) (B : ZFSet -> ZFSet) (f v : ZFSet),
  f ∈ Pi A B -> v ∈ A -> applyFun f v ∈ B v.
Proof.
  intros A B f v Hf Hv.
  unfold Pi in Hf. apply In_Comp_P in Hf.
  destruct Hf as [Htotal Hfn].
  destruct (Htotal v Hv) as [b [HbB Hbf]].
  unfold applyFun.
  assert (Himg : image f (Sing v) = Sing b).
  { apply set_ext; apply Inc_def; intros x Hx.
    - destruct (image_elim _ _ _ Hx) as [a [Ha Hax]].
      apply IN_Sing_EQ in Ha. subst a.
      specialize (Hfn v x b Hax Hbf). subst x. apply IN_Sing.
    - apply IN_Sing_EQ in Hx. subst x.
      apply image_intro with v; [apply IN_Sing | exact Hbf]. }
  rewrite Himg, Union_Sing. exact HbB.
Qed.

(** The relational image of a function [f ∈ Pi A B] at an in-domain point
    [v] is the singleton of the applied value [{f v}].  This is what makes
    a *relational* application [image f {|v|}] agree with the functional
    [applyFun f v] on in-domain arguments (and yield [∅] off-domain). *)
Lemma image_Sing_of_pi :
  forall (A : ZFSet) (B : ZFSet -> ZFSet) (f v : ZFSet),
  f ∈ Pi A B -> v ∈ A -> image f {| v |} = {| applyFun f v |}.
Proof.
  intros A B f v Hf Hv.
  unfold Pi in Hf. apply In_Comp_P in Hf. destruct Hf as [Htotal Hfn].
  destruct (Htotal v Hv) as [b [HbB Hbf]].
  assert (Himg : image f (Sing v) = Sing b).
  { apply set_ext; apply Inc_def; intros x Hx.
    - destruct (image_elim _ _ _ Hx) as [a [Ha Hax]].
      apply IN_Sing_EQ in Ha. subst a.
      specialize (Hfn v x b Hax Hbf). subst x. apply IN_Sing.
    - apply IN_Sing_EQ in Hx. subst x.
      apply image_intro with v; [apply IN_Sing | exact Hbf]. }
  rewrite Himg. unfold applyFun. rewrite Himg, Union_Sing. reflexivity.
Qed.

(** In a function [f ∈ Pi A B], an edge [⟨v,w⟩] over an in-domain point
    [v] lands its target in the fibre [B v].  (The functional version of
    [applyFun_mem_of_pi], usable when one has the edge directly.) *)
Lemma Pi_edge_codomain :
  forall (A : ZFSet) (B : ZFSet -> ZFSet) (f v w : ZFSet),
  f ∈ Pi A B -> v ∈ A -> ⟨ v , w ⟩ ∈ f -> w ∈ B v.
Proof.
  intros A B f v w Hf Hv Hedge.
  unfold Pi in Hf. apply In_Comp_P in Hf. destruct Hf as [Htotal Hfn].
  destruct (Htotal v Hv) as [b [HbB Hbf]].
  specialize (Hfn v w b Hedge Hbf). subst w. exact HbB.
Qed.



(* ----- monotonicity of [Pi] in its codomain ----- *)

Lemma Pi_Inc_Codomain A C D :
  (forall x, x ∈ A -> C x ⊆ D x) ->
  Pi A C ⊆ Pi A D.
Proof.
  intros h1.
  rewrite -> Inc_def in *.
  intros X XIn.
  apply In_Pi_inv in XIn.
  destruct XIn as [XIn [XdomB XisFun]].
  unfold Pi.
  eapply In_P_Comp.
  eapply Inc_IN_Power.
  - rewrite -> Inc_def in *.
    intros x xIn. specialize (XIn x xIn).
    eapply IN_Prod_EX in XIn.
    destruct XIn as [b [c [bIn [cIn ->]]]].
    eapply Couple_IN_Prod. auto.
    eapply iUnion_IN in cIn. destruct cIn as [a [aIn cIn]].
    eapply IN_iUnion; eauto. specialize (h1 a aIn).
    rewrite Inc_def in h1. auto.
  - split; auto.
    intros a aIn.
    specialize (XdomB a aIn).
    destruct XdomB as [c [bIn pIn]].
    specialize (h1 a aIn). rewrite -> Inc_def in h1.
    specialize (h1 _ bIn).
    exists c. split; auto.
Qed.

(* ===== pure smallness lemmas, and general-purpose facts ===== *)

(** A subset of a member of [Big] is again a member of [Big]: it lies in
    the (small) power set of that member, and [Big] is transitive. *)
Lemma small_of_Inc : forall X V : ZFSet, X ⊆ V -> V ∈ Big -> X ∈ Big.
Proof.
  intros X V HXV HV.
  apply Big_transitive with (Power V).
  - apply Inc_IN_Power; exact HXV.
  - apply In_Power_Big; exact HV.
Qed.

(** Binary union is [⋃ {A, B}], hence small whenever both arguments are. *)
Lemma BinUnion_eq_Union_Paire : forall A B : ZFSet,
  A ∪ B = ⋃ {| A ; B |}.
Proof.
  intros A B. pattern A; apply quot_ind; intros ea.
  pattern B; apply quot_ind; intros eb.
  rewrite BinUnion_mk, Paire_mk, Union_mk. reflexivity.
Qed.

Lemma BinUnion_small : forall A B : ZFSet,
  A ∈ Big -> B ∈ Big -> A ∪ B ∈ Big.
Proof.
  intros A B HA HB. rewrite BinUnion_eq_Union_Paire.
  apply Union_small, Paire_small; assumption.
Qed.

(** The Werner pair, unfolded with the [ZFSet] set-formers. *)
Lemma Couple_unfold : forall a b : ZFSet,
  ⟨ a , b ⟩ = {| {| a |} ; {| ∅ ; {| b |} |} |}.
Proof.
  intros a b. pattern a; apply quot_ind; intros ea.
  pattern b; apply quot_ind; intros eb.
  rewrite Couple_mk. unfold Empty.
  rewrite !Sing_mk, !Paire_mk. reflexivity.
Qed.

(** [Big] is closed under cartesian products.  Every element of
    [Prod A B] is a Werner pair [⟨a,b⟩ = {{a},{∅,{b}}}] with [a ∈ A] and
    [b ∈ B]; all of those components live two power-set levels above the
    small set [S := (A ∪ B) ∪ Power (A ∪ B)], so the whole product is a
    subset of [Power (Power S)], which is small. *)
Lemma Prod_small : forall A B : ZFSet,
  A ∈ Big -> B ∈ Big -> Prod A B ∈ Big.
Proof.
  intros A B HA HB.
  set (U := BinUnion A B).
  assert (HU : In U Big) by (apply BinUnion_small; assumption).
  set (S := BinUnion U (Power U)).
  assert (HS : In S Big)
    by (apply BinUnion_small; [ exact HU | apply In_Power_Big; exact HU ]).
  (* [a ∈ A] and [b ∈ B] both land in [U], so singletons of them and the
     empty set are subsets of [U], i.e. members of [Power U ⊆ S]. *)
  assert (HaU : forall a, In a A -> In a S).
  { intros a Ha. apply IN_BinUnion_l. apply IN_BinUnion_l. exact Ha. }
  assert (HSingS : forall x, In x U -> In (Sing x) S).
  { intros x Hx. apply IN_BinUnion_r. apply Inc_IN_Power.
    apply Inc_def. intros t Ht. apply IN_Sing_EQ in Ht. subst t. exact Hx. }
  apply small_of_Inc with (V := Power (Power S)).
  2:{ apply In_Power_Big, In_Power_Big; exact HS. }
  apply Inc_def. intros y Hy.
  apply IN_Prod_EX in Hy. destruct Hy as [a [b [Ha [Hb Heq]]]]. subst y.
  rewrite Couple_unfold.
  (* [{{a}, {∅,{b}}} ⊆ Power S]: each component is a subset of [S]. *)
  apply Inc_IN_Power. apply Inc_def. intros z Hz.
  apply Paire_IN in Hz. destruct Hz as [Hz | Hz]; subst z; apply Inc_IN_Power.
  - (* [{a} ⊆ S] *)
    apply Inc_def. intros w Hw. apply IN_Sing_EQ in Hw. subst w.
    apply HaU; exact Ha.
  - (* [{∅, {b}} ⊆ S] *)
    apply Inc_def. intros w Hw. apply Paire_IN in Hw. destruct Hw as [Hw | Hw]; subst w.
    + (* [∅ ∈ S]: the empty set is a subset of [U], hence in [Power U]. *)
      apply IN_BinUnion_r. apply Inc_IN_Power.
      apply Inc_def. intros t Ht. destruct (not_In_Empty _ Ht).
    + (* [{b} ∈ S] since [b ∈ B ⊆ U]. *)
      apply HSingS. apply IN_BinUnion_r. exact Hb.
Qed.


(** *** General-purpose lemmas collected from the Timbda / Lang
    developments.  These are pure [ZFSet] facts (binary intersection,
    image/range of concrete relations, iterated unions, and the
    singleton-product lemmas) with no dependence on any object language. *)

(** **** Binary intersection. *)

Lemma BinInter_Inc_l (A B : ZFSet) : A ∩ B ⊆ A.
Proof. unfold BinInter. apply Comp_Inc. Qed.

Lemma BinInter_Inc_r (A B : ZFSet) : A ∩ B ⊆ B.
Proof.
  apply Inc_def. intros x Hx. unfold BinInter in Hx.
  apply In_Comp_P in Hx. exact Hx.
Qed.

Lemma Inc_BinInter (C A B : ZFSet) : C ⊆ A -> C ⊆ B -> C ⊆ A ∩ B.
Proof.
  intros H1 H2. apply Inc_def. intros x Hx. unfold BinInter.
  apply In_P_Comp.
  - exact (Inc_IN _ _ _ H1 Hx).
  - exact (Inc_IN _ _ _ H2 Hx).
Qed.

Lemma IN_BinInter (x A B : ZFSet) : x ∈ A -> x ∈ B -> x ∈ A ∩ B.
Proof. intros HA HB. unfold BinInter. apply In_P_Comp; assumption. Qed.

Lemma BinInter_IN_l (x A B : ZFSet) : x ∈ A ∩ B -> x ∈ A.
Proof. intro H. exact (Inc_IN _ _ _ (Comp_Inc _ _) H). Qed.

Lemma BinInter_IN_r (x A B : ZFSet) : x ∈ A ∩ B -> x ∈ B.
Proof. intro H. unfold BinInter in H. apply In_Comp_P in H. exact H. Qed.

(** Intersection of a singleton with itself / with a distinct singleton —
    the two shapes produced when [Eequal] is applied to constants. *)
Lemma BinInter_Sing_same : forall a : ZFSet, {| a |} ∩ {| a |} = {| a |}.
Proof.
  intro a. apply set_ext; [ apply BinInter_Inc_l | apply Inc_BinInter; apply Inc_refl ].
Qed.

Lemma BinInter_Sing_diff :
  forall a b : ZFSet, a <> b -> {| a |} ∩ {| b |} = ∅.
Proof.
  intros a b Hab. apply set_ext; apply Inc_def; intros x Hx.
  - pose proof (BinInter_IN_l _ _ _ Hx) as Ha.
    pose proof (BinInter_IN_r _ _ _ Hx) as Hb.
    apply IN_Sing_EQ in Ha, Hb. subst a. exfalso. apply Hab. exact Hb.
  - destruct (not_In_Empty _ Hx).
Qed.

(** **** Image and range of concrete relations. *)

(** The image of the empty graph is empty. *)
Lemma image_Vide_l (X : ZFSet) : image ∅ X = ∅.
Proof.
  apply set_ext; apply Inc_def; intros y Hy.
  - apply image_elim in Hy. destruct Hy as [a [_ Hab]].
    destruct (not_In_Empty _ Hab).
  - destruct (not_In_Empty _ Hy).
Qed.

(** A one-edge graph applied (relationally) to its key yields the value. *)
Lemma image_Sing_Couple (a b : ZFSet) :
  image {| ⟨ a , b ⟩ |} {| a |} = {| b |}.
Proof.
  apply set_ext; apply Inc_def; intros y Hy.
  - apply image_elim in Hy. destruct Hy as [a' [Ha' Hab]].
    apply IN_Sing_EQ in Ha'. subst a'.
    apply IN_Sing_EQ in Hab. apply Couple_inj_right in Hab. subst y. apply IN_Sing.
  - apply IN_Sing_EQ in Hy. subst y. apply image_intro with a; apply IN_Sing.
Qed.

(** The image of a union of graphs is the union of the images. *)
Lemma image_BinUnion (R S X : ZFSet) :
  image (R ∪ S) X = image R X ∪ image S X.
Proof.
  apply set_ext; apply Inc_def; intros y Hy.
  - apply image_elim in Hy. destruct Hy as [a [HaX Hab]].
    apply BinUnion_IN in Hab. destruct Hab as [Hab | Hab].
    + apply IN_BinUnion_l. apply image_intro with a; assumption.
    + apply IN_BinUnion_r. apply image_intro with a; assumption.
  - apply BinUnion_IN in Hy. destruct Hy as [Hy | Hy].
    + apply image_elim in Hy. destruct Hy as [a [HaX Hab]].
      apply image_intro with a; [ exact HaX | apply IN_BinUnion_l; exact Hab ].
    + apply image_elim in Hy. destruct Hy as [a [HaX Hab]].
      apply image_intro with a; [ exact HaX | apply IN_BinUnion_r; exact Hab ].
Qed.

(** The range of a one-edge graph. *)
Lemma rng_Sing_Couple (a b : ZFSet) : rng {| ⟨ a , b ⟩ |} = {| b |}.
Proof.
  apply set_ext; apply Inc_def; intros y Hy.
  - apply rng_elim in Hy. destruct Hy as [a' Hab].
    apply IN_Sing_EQ in Hab. apply Couple_inj_right in Hab. subst y. apply IN_Sing.
  - apply IN_Sing_EQ in Hy. subst y. apply rng_intro with a. apply IN_Sing.
Qed.

(** **** Iterated unions. *)

(** Subset rule for a double iterated union of singletons: the union is
    included in [T] when [g y z ∈ T] for every pair of elements. *)
Lemma iUnion2_Sing_Inc
  (A B : ZFSet) (g : ZFSet -> ZFSet -> ZFSet) (T : ZFSet) :
  (forall y z, y ∈ A -> z ∈ B -> g y z ∈ T) ->
  (a ← A ;; b ← B ;; {| g a b |}) ⊆ T.
Proof.
  intro h. apply iUnion_Inc. intros y yIn.
  apply iUnion_Inc. intros z zIn.
  apply Sing_Inc_IN. now apply h.
Qed.

(** A union is monotone in its index set. *)
Lemma iUnion_Inc_index (E1 E2 : ZFSet) (F : ZFSet -> ZFSet) :
  E1 ⊆ E2 -> iUnion E1 F ⊆ iUnion E2 F.
Proof.
  intro h. apply iUnion_Inc. intros y yIn.
  apply Inc_def. intros x xIn.
  apply IN_iUnion with y; [ exact (Inc_IN _ _ _ h yIn) | exact xIn ].
Qed.

(** A union is unchanged when its body is replaced by a pointwise-equal
    body on the members of the index set. *)
Lemma iUnion_ext_mem (E : ZFSet) (F G : ZFSet -> ZFSet) :
  (forall y, y ∈ E -> F y = G y) -> iUnion E F = iUnion E G.
Proof.
  intro h. apply set_ext; apply iUnion_Inc; intros y yIn;
    apply Inc_def; intros x xIn; apply IN_iUnion with y; auto.
  - rewrite <- (h y yIn). exact xIn.
  - rewrite (h y yIn). exact xIn.
Qed.

(** **** Singleton products.

    A [Pi] whose every fibre is a singleton is itself a singleton: the
    unique inhabitant is the graph of the choice function [g]. *)
Lemma Pi_Sing (A : ZFSet) (B : ZFSet -> ZFSet) (g : ZFSet -> ZFSet) :
  (forall a, a ∈ A -> B a = {| g a |}) ->
  Pi A B = {| a ← A ;; {| ⟨ a , g a ⟩ |} |}.
Proof.
  intro hB.
  assert (hg : forall a, In a A -> In (g a) (B a)).
  { intros a Ha. rewrite (hB a Ha). apply IN_Sing. }
  apply set_ext.
  - apply Inc_def. intros f Hf.
    assert (Ef : f = iUnion A (fun a => Sing (Couple a (g a)))).
    { apply In_Pi_inv in Hf. destruct Hf as [Hsub [Htot Hfun]].
      apply set_ext.
      - apply Inc_def. intros p Hp.
        pose proof (Inc_IN _ _ _ Hsub Hp) as Hp'.
        apply IN_Prod_EX in Hp'. destruct Hp' as [a [b [Ha [_ Ep]]]].
        subst p.
        destruct (Htot a Ha) as [b' [Hb' Hcoup]].
        rewrite (hB a Ha) in Hb'. apply IN_Sing_EQ in Hb'. subst b'.
        pose proof (Hfun a b (g a) Hp Hcoup) as Eb. subst b.
        apply IN_iUnion with a; [exact Ha | apply IN_Sing].
      - apply Inc_def. intros p Hp.
        apply iUnion_IN in Hp. destruct Hp as [a [Ha HpS]].
        apply IN_Sing_EQ in HpS. subst p.
        destruct (Htot a Ha) as [b' [Hb' Hcoup]].
        rewrite (hB a Ha) in Hb'. apply IN_Sing_EQ in Hb'. subst b'.
        exact Hcoup. }
    rewrite Ef. apply IN_Sing.
  - apply Sing_Inc_IN. apply iUnion_graph_mem_pi. exact hg.
Qed.

(** Existential form: a [Pi] over pointwise-singleton fibres is a
    singleton (Hilbert choice picks the per-fibre witness). *)
Lemma Pi_Sing_ex (A : ZFSet) (B : ZFSet -> ZFSet) :
  (forall a, a ∈ A -> exists b, B a = {| b |}) ->
  exists f, Pi A B = {| f |}.
Proof.
  intro h.
  pose (g := fun a => epsilon (inhabits Empty) (fun b => B a = Sing b)).
  exists (iUnion A (fun a => Sing (Couple a (g a)))).
  apply Pi_Sing. intros a Ha.
  unfold g. apply epsilon_spec. apply h. exact Ha.
Qed.


(* ===== Reduction lemmas and the [zf_reduce] tactic =====

   These are the computation rules an interpreter's evaluator bottoms out
   in: iterating over a singleton (in single / pair / triple form),
   integer addition, and the reflexive-unification intersection.  They are
   collected here, with the [zf_reduce] tactic that applies them, so the
   three interpreters share one normalisation engine instead of each
   re-deriving it. *)

(** Iterate a pair-pattern union over a single pair. *)
Lemma iUnion_pat_Sing_Couple :
  forall (a b : ZFSet) (F : ZFSet -> ZFSet -> ZFSet),
  iUnion_pat {| ⟨ a , b ⟩ |} F = F a b.
Proof.
  intros a b F. unfold iUnion_pat. rewrite iUnion_Sing_l.
  cbv beta. rewrite pfst_Couple, psnd_Couple. reflexivity.
Qed.

(** Iterate a triple-pattern union over a single triple. *)
Lemma iUnion_pat3_Sing :
  forall (r a b : ZFSet) (F : ZFSet -> ZFSet -> ZFSet -> ZFSet),
  iUnion_pat3 {| ⟨ r , a , b ⟩ |} F = F r a b.
Proof.
  intros r a b F. unfold iUnion_pat3. rewrite iUnion_Sing_l. cbv beta.
  rewrite proj1_triple, proj2_triple, proj3_triple. reflexivity.
Qed.

(** Distinct diagonal pairs / triples — used to show a clashing
    unification denotes [∅] in the pair / triple semantics. *)
Lemma Couple_diag_neq : forall a b : ZFSet, a <> b -> ⟨ a , a ⟩ <> ⟨ b , b ⟩.
Proof. intros a b Hab H. apply Hab. exact (Couple_inj_left _ _ _ _ H). Qed.

Lemma Triple_snd_neq :
  forall env a b : ZFSet, a <> b -> ⟨ env , a , a ⟩ <> ⟨ env , b , b ⟩.
Proof.
  intros env a b Hab H. apply Hab.
  apply Couple_inj_right in H. exact (Couple_inj_left _ _ _ _ H).
Qed.

(** [zf_reduce] normalises a denotation that bottoms out in singleton
    iterations: it discharges single / pair / triple unions over
    singletons, integer additions, the reflexive intersection, and the
    image / range reductions, then renormalises nat additions.  An
    interpreter's step tactic is just [cbn [eval]; zf_reduce; try
    reflexivity] (see [Timbda0.step_eval], [Timbda1.step1],
    [Timbda2.step3]). *)
Ltac zf_reduce :=
  repeat (first
    [ rewrite iUnion_Sing_l
    | rewrite iUnion_pat_Sing_Couple
    | rewrite iUnion_pat3_Sing
    | rewrite iUnion_Vide
    | rewrite BinUnion_Vide_l
    | rewrite BinUnion_Vide_r
    | rewrite natZAdd_natZ
    | rewrite BinInter_Sing_same
    | rewrite image_Vide_l
    | rewrite image_Sing_Couple
    | rewrite image_BinUnion
    | rewrite rng_Sing_Couple
    | rewrite rng_natId
    | rewrite Union_Sing ]; cbn [Nat.add]).
