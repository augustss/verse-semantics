From Stdlib Require Import Morphisms.
From Stdlib Require Import ssreflect.
From Stdlib Require Import FunctionalExtensionality.

Require Import Axioms.
Require Import Cartesian.
Require Import Omega.

Require Import ZFSet.
Require Import ZFNotation.
Require Import Diagonal.
Require Import Projections.

Require Import Syntax.

Require Import utils.all.
From smpl Require Export Smpl.
Require Import Timbda0.

(* Set semantics. Simbda calculus *)

(* Identity relation *)
Definition ID (X : ZFSet) := 
  x ← X ;; {| ⟨ x , x ⟩ |}.
Lemma isFunctionID X: isFunction (ID X).
Proof. unfold isFunction. intros a b1 b2 In1 In2.
       unfold ID in *.
       eapply iUnion_IN in In1. destruct In1 as [y [Iny Inyy]].
       eapply iUnion_IN in In2. destruct In2 as [z [Inz Inzz]].
       apply IN_Sing_EQ in Inyy.
       apply IN_Sing_EQ in Inzz.
       move: (Couple_inj_left _ _ _ _ Inyy) => EQ1.
       move: (Couple_inj_right _ _ _ _ Inyy) => EQ2. subst.
       move: (Couple_inj_left _ _ _ _ Inzz) => EQ1.
       move: (Couple_inj_right _ _ _ _ Inzz) => EQ2.
       subst. done.
Qed.

Module S.

(* Set semantics. Simbda calculus *)


Fixpoint eval (e : Expr) (rho : Env) {struct e} : ZFSet :=
  match e with
  | Econ n        => {| n |}
  | Evar i        => {| rho i |}
  | Enat          => {| ID ω |}
  | Eany          => Big
  | Etype         => {| is_type |}
  | Elam e1 e2    => Π[ a ∈ eval e1 rho ] eval e2 (env_ext rho a)
  | Eapp e1 e2    =>
       f ← eval e1 rho ;;
       a ← eval e2 rho ;;
       image f {| a |}
  | Efail         => ∅
  | Eimg e        => f ← eval e rho ;; rng f 
  | Echoice e1 e2 => eval e1 rho ∪ eval e2 rho
  | Eequal e1 e2  => eval e1 rho ∩ eval e2 rho
  | Eadd e1 e2 =>
       v1 ← eval e1 rho ;;
       v2 ← eval e2 rho ;;
        {| v1 + v2 |} 
  | Efix e =>
       f ← eval e rho ;;
       ⦃ a ∈ dom f | ⟨ a , a ⟩ ∈ f ⦄
  | _ => ∅
  end.

End S.




(* Partial functions. We drop the totality requirement *)
Definition ParPi (A : ZFSet) (B : ZFSet -> ZFSet) : ZFSet :=
  Comp (Power (Prod A (iUnion A B)))
       (fun f =>
          (* (forall a, In a A -> exists b, In b (B a) /\ In (Couple a b) f)
          /\  *) 
          (forall a b1 b2,
                In (Couple a b1) f -> In (Couple a b2) f -> b1 = b2)).



(* [diag_not_couple], [FST_diag_isFunction], [SND_diag_isFunction] now live in
   [lib/Projections.v] (imported above). *)

Module R.

(* [FST] / [SND] and their unfolding lemmas now live in [lib/Projections.v]
   (imported above): [FST_Couple], [SND_Couple] for the base case and
   [FST_rel], [SND_rel] for the relational (recursive) clause. *)

(* Enat = Id (Id Nat)  *)

(* The relation [Enat] denotes is [Id (Id Nat)]; [R.eval] wraps a type's
   relation in a singleton (as for [Eany]/[Etype]), so the equation is
   [R.eval Enat = {| ID (ID ω) |}]. *)
Lemma Enat_equiv :
     {| k ← ω ;; {| ⟨ ⟨ k , k ⟩ , ⟨ k , k ⟩⟩ |} |} = {| ID (ID ω) |}.
Proof.
  f_equal. apply set_ext; apply Inc_def => z Hz.
  - (* {⟨⟨k,k⟩,⟨k,k⟩⟩ : k∈ω}  ⊆  ID (ID ω) *)
    apply iUnion_IN in Hz. move: Hz => [k [Hk Hz]]. apply IN_Sing_EQ in Hz. subst z.
    apply IN_iUnion with (y := ⟨ k , k ⟩).
    + apply IN_iUnion with (y := k); [ exact Hk | apply IN_Sing ].
    + apply IN_Sing.
  - (* ID (ID ω)  ⊆  {⟨⟨k,k⟩,⟨k,k⟩⟩ : k∈ω} *)
    apply iUnion_IN in Hz. move: Hz => [y [Hy Hz]]. apply IN_Sing_EQ in Hz. subst z.
    apply iUnion_IN in Hy. move: Hy => [k [Hk Hy]]. apply IN_Sing_EQ in Hy. subst y.
    apply IN_iUnion with (y := k); [ exact Hk | apply IN_Sing ].
Qed.





Fixpoint eval (e : Expr) (rho : Env) {struct e} : ZFSet :=
  match e with
  | Econ k        => {| ⟨ k , k ⟩ |}
  | Evar i        => {| rho i |}
  | Enat          => {| k ← ω ;; {| ⟨ ⟨ k , k ⟩ , ⟨ k , k ⟩⟩ |} |}
  | Eany          => {| x ← Big ;; {| ⟨ x ,  x ⟩ |} |}
  | Elam e1 e2    =>  ⦃ f ∈ Π[ a ∈ eval e1 rho ] eval e2 (env_ext rho a)
                      | isFunction (FST f) /\ isFunction (SND f) ⦄
  (* partial functions *)
  | Elamp e1 e2   =>  ⦃ f ∈ ParPi (eval e1 rho) (fun a => eval e2 (env_ext rho a))
                      | isFunction (FST f) /\ isFunction (SND f) ⦄
  (* forall *)
  (*
  R(\(t:=e0;x:=e1)e2)ρ	= { f
    | h ∈ PI { (u,a) | u∈R(e0)ρ, a∈R(e1)ρ[t⊦>u] } (\(u,a) -> R(e2)ρ[t⊦>u,x⊦>a])
	, let f = { (a,b) | ((_,a),b)∈h }
 	, isFunction(f)  ⟵ (maybe implied by the following two lines)
     , isFunction(LFT(f))
	, isFunction(RGT(f))
	}
    *)
  | Elamb (Eseq (Eassign 0 e0) (Eassign 1 e1)) e2 =>
     let A := u ← eval e0 rho ;;
              a ← eval e1 (env_ext rho u);;
              {| ⟨ u, a ⟩ |} in
     let B p := eval e2 (env_ext (env_ext rho (pfst p)) (psnd p))
     in
     h ← Pi A B ;;
     ⦃ f ∈  {| '⟨ p, b ⟩ ← h ;; {| ⟨ psnd p, b ⟩ |} |}
    | isFunction (FST f) /\ isFunction (SND f) ⦄
  | Eapp e1 e2    =>
       f ← eval e1 rho ;;
       a ← eval e2 rho ;;
       image f {| a |}
  | Efail         => ∅
  | Eimg e        => f ← eval e rho ;; rng f 
  | Echoice e1 e2 => eval e1 rho ∪ eval e2 rho
  | Eequal e1 e2  => eval e1 rho ∩ eval e2 rho
 
  | _ => ∅
  end.

(* ParPi : partial functions, not total functions *)

(* \( f := pfun (x:any){x}){f}*)
Definition Etype :=
   Elam (Elamp (Eimg Eany) (Evar 0)) (Evar 0).

(* The image of [any] is the whole universe [Big]: [:any = U]. *)
Lemma ImgAny rho : eval (Eimg Eany) rho = Big.
Proof.
  cbn [eval]. rewrite iUnion_Sing_l.
  change (iUnion Big (fun x => {| ⟨ x, x ⟩ |})) with (diag Big).
  apply rng_diag.
Qed.

(* The image of the type-of-types is the partial-function space that defines
   it: applying [:] to [Etype] returns its domain [Elamp (:any) x].  (Not the
   set of *all* small relations — [ParPi] keeps only the single-valued ones,
   so the earlier `a←Big;;b←Big;;R←Power(Prod a b);;{|R|}` is too big.) *)
Lemma ImgType rho :
   eval (Eimg Etype) rho = eval (Elamp (Eimg Eany) (Evar 0)) rho.
Proof.
  (* [Eimg] over the singleton [{| diag A |}] gives [rng (diag A) = A], using
     the top-level [FST_diag_isFunction] / [SND_diag_isFunction]. *)
  set (A := eval (Elamp (Eimg Eany) (Evar 0)) rho).
  change (eval (Eimg Etype) rho) with (f ← eval Etype rho ;; rng f).
  assert (HE : eval Etype rho = {| diag A |}).
  { unfold Etype.
    change (eval (Elam (Elamp (Eimg Eany) (Evar 0)) (Evar 0)) rho)
      with (⦃ g ∈ Π[ a ∈ eval (Elamp (Eimg Eany) (Evar 0)) rho ]
                   eval (Evar 0) (env_ext rho a)
             | isFunction (FST g) /\ isFunction (SND g) ⦄).
    fold A.
    assert (Hcod : forall a, eval (Evar 0) (env_ext rho a) = {| a |}).
    { intro a. cbn [eval]. rewrite env_ext_zero. reflexivity. }
    rewrite (Pi_Sing A (fun a => eval (Evar 0) (env_ext rho a)) (fun a => a) (fun a _ => Hcod a)).
    change (iUnion A (fun a => {| ⟨ a, a ⟩ |})) with (diag A).
    apply set_ext; apply Inc_def => x Hx.
    - exact (Inc_IN _ _ _ (Comp_Inc _ _) Hx).
    - apply IN_Sing_EQ in Hx. subst x.
      apply In_P_Comp;
        [ apply IN_Sing | split; [ apply FST_diag_isFunction | apply SND_diag_isFunction ] ]. }
  rewrite HE iUnion_Sing_l. apply rng_diag.
Qed.

(* An [eval]-free description of the image: it is the set of *single-valued*
   relations [f ⊆ Big × Big] whose left and right projections [FST f], [SND f]
   are themselves functions.  (Equivalently [⦃ f ∈ pFun Big Big | … ⦄], since
   [ParPi Big (fun a => {|a|})] is exactly the partial functions on [Big].) *)
Lemma ImgType' rho :
   eval (Eimg Etype) rho
   = ⦃ f ∈ ParPi Big (fun a => {| a |}) | isFunction (FST f) /\ isFunction (SND f) ⦄.
Proof.
  rewrite ImgType.
  change (eval (Elamp (Eimg Eany) (Evar 0)) rho)
    with (⦃ f ∈ ParPi (eval (Eimg Eany) rho) (fun a => eval (Evar 0) (env_ext rho a))
           | isFunction (FST f) /\ isFunction (SND f) ⦄).
  rewrite ImgAny.
  assert (Hcod : (fun a => eval (Evar 0) (env_ext rho a)) = (fun a => {| a |})).
  { apply functional_extensionality => a. cbn [eval]. by rewrite env_ext_zero. }
  rewrite Hcod. reflexivity.
Qed.

(* [Etype] itself (the type-of-types) denotes a singleton holding the identity
   function [diag (:type)] on the type universe, with [:type] spelled out
   eval-free (via [ImgType']) as the single-valued small relations whose [FST]
   and [SND] projections are functions. *)
Lemma eval_Etype rho :
   eval Etype rho =
     {| diag (⦃ f ∈ ParPi Big (fun a => {| a |}) | isFunction (FST f) /\ isFunction (SND f) ⦄) |}.
Proof.
  set (A := ⦃ f ∈ ParPi Big (fun a => {| a |}) | isFunction (FST f) /\ isFunction (SND f) ⦄).
  (* [:type], the domain of [Etype], is [A] (eval-free, via [ImgType]/[ImgType']). *)
  assert (HA : eval (Elamp (Eimg Eany) (Evar 0)) rho = A).
  { unfold A. rewrite <- ImgType. apply ImgType'. }
  unfold Etype.
  change (eval (Elam (Elamp (Eimg Eany) (Evar 0)) (Evar 0)) rho)
    with (⦃ g ∈ Π[ a ∈ eval (Elamp (Eimg Eany) (Evar 0)) rho ]
                 eval (Evar 0) (env_ext rho a)
           | isFunction (FST g) /\ isFunction (SND g) ⦄).
  rewrite HA.
  assert (Hcod : forall a, eval (Evar 0) (env_ext rho a) = {| a |}).
  { intro a. cbn [eval]. by rewrite env_ext_zero. }
  rewrite (Pi_Sing A (fun a => eval (Evar 0) (env_ext rho a)) (fun a => a) (fun a _ => Hcod a)).
  change (iUnion A (fun a => {| ⟨ a, a ⟩ |})) with (diag A).
  apply set_ext; apply Inc_def => x Hx.
  - exact (Inc_IN _ _ _ (Comp_Inc _ _) Hx).
  - apply IN_Sing_EQ in Hx. subst x.
    apply In_P_Comp;
      [ apply IN_Sing | split; [ apply FST_diag_isFunction | apply SND_diag_isFunction ] ].
Qed.

Definition pconst := 
   Elam (Eimg Enat) (Evar 0).

(* [pconst = \(x : :nat){x}] denotes the identity function on [:nat = ID ω],
   i.e. the graph [ID (ID ω)] (= [diag (ID ω)]). *)
Lemma eval_pconst rho :
    eval pconst rho = {| ID (ID ω) |}.
Proof.
  change (ID (ID ω)) with (diag (ID ω)).
  assert (HA : eval (Eimg Enat) rho = ID ω).
  { change (eval (Eimg Enat) rho) with (f ← eval Enat rho ;; rng f).
    change (eval Enat rho) with ({| k ← ω ;; {| ⟨ ⟨ k , k ⟩ , ⟨ k , k ⟩⟩ |} |}).
    rewrite Enat_equiv. rewrite iUnion_Sing_l.
    change (ID (ID ω)) with (diag (ID ω)). apply rng_diag. }
  change (eval pconst rho)
    with (⦃ f ∈ Π[ a ∈ eval (Eimg Enat) rho ] eval (Evar 0) (env_ext rho a)
           | isFunction (FST f) /\ isFunction (SND f) ⦄).
  rewrite HA.
  assert (Hcod : forall a, eval (Evar 0) (env_ext rho a) = {| a |}).
  { intro a. cbn [eval]. by rewrite env_ext_zero. }
  rewrite (Pi_Sing (ID ω) (fun a => eval (Evar 0) (env_ext rho a)) (fun a => a) (fun a _ => Hcod a)).
  change (a ← ID ω ;; {| ⟨ a, a ⟩ |}) with (diag (ID ω)).
  apply set_ext; apply Inc_def => z Hz.
  - exact (Inc_IN _ _ _ (Comp_Inc _ _) Hz).
  - apply IN_Sing_EQ in Hz. subst z.
    apply In_P_Comp;
      [ apply IN_Sing | split; [ apply FST_diag_isFunction | apply SND_diag_isFunction ] ].
Qed.

(* [pconst = \(x : :nat){x}] is the identity on [nat].  Its domain [:nat] is
   [ID ω] (the value relation of the naturals), which *does* contain the value
   [⟨x,x⟩] of [Econ x] — so, unlike [pid]/[Ebool] (whose binder ranges over the
   *type* universe), applying it to a numeral lands in the domain and returns
   that numeral. *)
Lemma eval_pconst_app rho x :
   eval (Eapp pconst(Econ x)) rho = {| ⟨ x, x ⟩ |}.
Proof.
  (* [:nat] denotes [ID ω]. *)
  assert (HA : eval (Eimg Enat) rho = ID ω).
  { change (eval (Eimg Enat) rho) with (f ← eval Enat rho ;; rng f).
    change (eval Enat rho) with ({| k ← ω ;; {| ⟨ ⟨ k , k ⟩ , ⟨ k , k ⟩⟩ |} |}).
    rewrite Enat_equiv. rewrite iUnion_Sing_l.
    change (ID (ID ω)) with (diag (ID ω)). apply rng_diag. }
  (* [pconst] denotes the identity graph on [ID ω], i.e. [diag (ID ω)]. *)
  assert (HP : eval pconst rho = {| diag (ID ω) |}).
  { change (eval pconst rho)
      with (⦃ f ∈ Π[ a ∈ eval (Eimg Enat) rho ] eval (Evar 0) (env_ext rho a)
             | isFunction (FST f) /\ isFunction (SND f) ⦄).
    rewrite HA.
    assert (Hcod : forall a, eval (Evar 0) (env_ext rho a) = {| a |}).
    { intro a. cbn [eval]. by rewrite env_ext_zero. }
    rewrite (Pi_Sing (ID ω) (fun a => eval (Evar 0) (env_ext rho a)) (fun a => a) (fun a _ => Hcod a)).
    change (a ← ID ω ;; {| ⟨ a, a ⟩ |}) with (diag (ID ω)).
    apply set_ext; apply Inc_def => z Hz.
    - exact (Inc_IN _ _ _ (Comp_Inc _ _) Hz).
    - apply IN_Sing_EQ in Hz. subst z.
      apply In_P_Comp;
        [ apply IN_Sing | split; [ apply FST_diag_isFunction | apply SND_diag_isFunction ] ]. }
  (* Apply the identity graph to the value [⟨x,x⟩], which is in its domain. *)
  change (eval (Eapp pconst (Econ x)) rho)
    with (f ← eval pconst rho ;; a ← eval (Econ x) rho ;; image f {| a |}).
  change (eval (Econ x) rho) with ({| ⟨ x, x ⟩ |}).
  rewrite HP. rewrite iUnion_Sing_l. rewrite iUnion_Sing_l.
  assert (Hc : ⟨ x, x ⟩ ∈ ID ω).
  { unfold ID. apply IN_iUnion with (y := natZ x); [ apply natZ_mem_omega | apply IN_Sing ]. }
  apply set_ext; apply Inc_def => z Hz.
  - apply image_elim in Hz. destruct Hz as [a [Ha Hcoup]].
    apply IN_Sing_EQ in Ha. subst a.
    unfold diag in Hcoup. apply iUnion_IN in Hcoup. destruct Hcoup as [w [Hw Hwz]].
    apply IN_Sing_EQ in Hwz.
    pose proof (Couple_inj_left _ _ _ _ Hwz) as E1.
    pose proof (Couple_inj_right _ _ _ _ Hwz) as E2.
    rewrite E2. rewrite <- E1. apply IN_Sing.
  - apply IN_Sing_EQ in Hz. subst z.
    apply image_intro with (a := ⟨ x, x ⟩); [ apply IN_Sing | ].
    unfold diag. apply IN_iUnion with (y := ⟨ x, x ⟩); [ exact Hc | apply IN_Sing ].
Qed.

(* \(t:type; x:t){t} *)
Definition pid :=
   Elamb (Eseq (Eassign 0 (Eimg Etype)) (Eassign 1 (Evar 0)))
         (Evar 1 ).

(* The set-theoretic function [pid] denotes: the identity function on the type
   universe [:type], i.e. the graph [{ ⟨u,u⟩ | u ∈ eval (:type) }].  ([pid =
   \(t:type; x:t){t}] returns the type [t]; but its domain [A = diag (:type)]
   forces [x = t] — the bound [x:t] collapses onto the diagonal — so returning
   [t] coincides with returning [x], and the denotation is the identity on the
   universe of types either way.) *)
Definition idType (rho : Env) : ZFSet := diag (eval (Eimg Etype) rho).

Lemma eval_pid rho : eval pid rho = {| idType rho |}.
Proof.
  unfold idType. set (P := eval (Eimg Etype) rho).
  (* Unfold the dependent λ one layer, keeping the subterms folded.  The body
     is [Evar 1 = t], so the codomain fibre is [{| pfst p |}]. *)
  change (eval pid rho) with
    (h ← Pi (u ← eval (Eimg Etype) rho ;; a ← eval (Evar 0) (env_ext rho u) ;; {| ⟨ u, a ⟩ |})
             (fun p => eval (Evar 1) (env_ext (env_ext rho (pfst p)) (psnd p)))
     ;; ⦃ f ∈ {| '⟨ p, b ⟩ ← h ;; {| ⟨ psnd p, b ⟩ |} |}
        | isFunction (FST f) /\ isFunction (SND f) ⦄).
  (* The domain is the diagonal on the type universe [P]; the codomain fibre
     over [p] is the singleton [{| pfst p |}] (the bound type [t]). *)
  assert (HA : (u ← eval (Eimg Etype) rho ;; a ← eval (Evar 0) (env_ext rho u) ;; {| ⟨ u, a ⟩ |}) = diag P).
  { unfold diag. fold P. f_equal. apply functional_extensionality => u.
    cbn [eval]. rewrite env_ext_zero. rewrite iUnion_Sing_l. reflexivity. }
  assert (HB : (fun p => eval (Evar 1) (env_ext (env_ext rho (pfst p)) (psnd p))) = (fun p : ZFSet => {| pfst p |})).
  { apply functional_extensionality => p. cbn [eval env_ext]. reflexivity. }
  rewrite HA HB.
  (* [Pi (diag P) (fun p => {|pfst p|})] is the single graph [{⟨a, pfst a⟩ | a ∈ diag P}]. *)
  rewrite (Pi_Sing (diag P) (fun p => {| pfst p |}) pfst (fun a _ => eq_refl)).
  rewrite iUnion_Sing_l.
  (* The constructed graph [{⟨psnd p, b⟩ | ⟨p,b⟩ ∈ h}] is exactly [diag P]:
     the domain is diagonal, so [pfst a = psnd a] and this is the same identity
     graph as for the [Evar 0] body. *)
  assert (Hf0 : ('⟨ p, b ⟩ ← (a ← diag P ;; {| ⟨ a, pfst a ⟩ |}) ;; {| ⟨ psnd p, b ⟩ |}) = diag P).
  { unfold iUnion_pat.
    apply set_ext; apply Inc_def => x Hx.
    - apply iUnion_IN in Hx. destruct Hx as [e [He Hx]].
      apply IN_Sing_EQ in Hx.
      apply iUnion_IN in He. destruct He as [a [Ha He]].
      apply IN_Sing_EQ in He. subst e.
      rewrite pfst_Couple psnd_Couple in Hx.
      unfold diag in Ha. apply iUnion_IN in Ha. destruct Ha as [u [Hu Ha]].
      apply IN_Sing_EQ in Ha. subst a. rewrite psnd_Couple pfst_Couple in Hx.
      subst x. unfold diag. apply IN_iUnion with (y := u); [ exact Hu | apply IN_Sing ].
    - unfold diag in Hx. apply iUnion_IN in Hx. destruct Hx as [u [Hu Hx]].
      apply IN_Sing_EQ in Hx. subst x.
      apply IN_iUnion with (y := ⟨ ⟨ u, u ⟩, u ⟩).
      + apply IN_iUnion with (y := ⟨ u, u ⟩).
        * unfold diag. apply IN_iUnion with (y := u); [ exact Hu | apply IN_Sing ].
        * rewrite pfst_Couple. apply IN_Sing.
      + rewrite pfst_Couple !psnd_Couple. apply IN_Sing. }
  rewrite Hf0.
  (* [diag P] passes the [FST]/[SND]-functional filter, so the comprehension
     over the singleton is the singleton. *)
  apply set_ext; apply Inc_def => z Hz.
  - exact (Inc_IN _ _ _ (Comp_Inc _ _) Hz).
  - apply IN_Sing_EQ in Hz. subst z.
    apply In_P_Comp;
      [ apply IN_Sing | split; [ apply FST_diag_isFunction | apply SND_diag_isFunction ] ].
Qed.

Lemma eval_pid_app rho x :
   eval (Eapp pid (Econ x)) rho = {| ⟨ x, x ⟩ |}.
Proof.
Admitted.
         
(* \(t:type; x:t) {\(y:t){:t} } *)
Definition Ebool :=
   Elamb (Eseq (Eassign 0 (Eimg Etype)) (Eassign 1 (Evar 0)))
         (Elam (Evar 1) (Eimg (Evar 2))).

Lemma eval_Ebool rho x y :
   eval (Eapp (Eapp Ebool (Econ x)) (Econ y)) rho
      =  {| ⟨ x, x ⟩ |} ∪ {| ⟨ y, y ⟩ |}.
Proof.
Admitted.

(*  Type is definable using partial functions on any

type (f := pfun(x:any){x}){f} 

Koen has proven that

:type is the set { <a,b,R> | a in U, b in U, R subset a x b } 

\(t:type; x:t, y:t){:t}  is  either fst or snd

R(\:any){nat}) has same cardinality as {nat}

*)



(* fun (x:any){ 3 | 4 } *)
Definition ex_fun_choice : Expr := Elam (Eimg Eany) (Echoice (Econ 3) (Econ 4)).

(* [fun (x : :any){3 | 4}] denotes: the *total* functions [f] from the whole
   universe [Big] into the two-element set [{⟨3,3⟩, ⟨4,4⟩}] (the body ignores
   [x] and offers a binary choice), kept only when both projections [FST f]
   and [SND f] are single-valued — i.e. [f] factors through [FST] and through
   [SND].  (The filter is non-trivial: the two constant functions pass, but an
   [f] sending [⟨0,5⟩↦⟨3,3⟩] and [⟨0,7⟩↦⟨4,4⟩] does not, since [FST] of both
   inputs is [0] but [FST] of the outputs are [3 ≠ 4].) *)
Lemma eval_ex_fun_choice rho :
  eval ex_fun_choice rho
  = ⦃ f ∈ Pi Big (fun _ => {| ⟨ 3, 3 ⟩ |} ∪ {| ⟨ 4, 4 ⟩ |})
      | isFunction (FST f) /\ isFunction (SND f) ⦄.
Proof.
  unfold ex_fun_choice.
  change (eval (Elam (Eimg Eany) (Echoice (Econ 3) (Econ 4))) rho)
    with (⦃ f ∈ Π[ a ∈ eval (Eimg Eany) rho ]
                 eval (Echoice (Econ 3) (Econ 4)) (env_ext rho a)
           | isFunction (FST f) /\ isFunction (SND f) ⦄).
  rewrite ImgAny.
  assert (Hcod : (fun a => eval (Echoice (Econ 3) (Econ 4)) (env_ext rho a))
                 = (fun _ => {| ⟨ 3, 3 ⟩ |} ∪ {| ⟨ 4, 4 ⟩ |})).
  { apply functional_extensionality => a. reflexivity. }
  rewrite Hcod. reflexivity.
Qed.

(* Applying [fun(x : :any){3|4}] to any numeral [n] yields the binary choice
   [{⟨3,3⟩} ∪ {⟨4,4⟩}]: every candidate function maps the in-domain point
   [⟨n,n⟩] into [{⟨3,3⟩, ⟨4,4⟩}] (⊆), and the two constant functions realise
   both outcomes (⊇). *)
Lemma eval_app_ex_fun_choice rho n :
  eval (Eapp ex_fun_choice (Econ n)) rho = {| ⟨ 3, 3 ⟩ |} ∪ {| ⟨ 4, 4 ⟩ |}.
Proof.
  change (eval (Eapp ex_fun_choice (Econ n)) rho)
    with (f ← eval ex_fun_choice rho ;; a ← eval (Econ n) rho ;; image f {| a |}).
  change (eval (Econ n) rho) with ({| ⟨ n, n ⟩ |}).
  assert (HG : (fun f => a ← {| ⟨ n, n ⟩ |} ;; image f {| a |})
               = (fun f => image f {| ⟨ n, n ⟩ |})).
  { apply functional_extensionality => f. by rewrite iUnion_Sing_l. }
  rewrite HG. rewrite eval_ex_fun_choice.
  assert (HnB : natZ n ∈ Big) by (apply (Big_transitive _ _ (natZ_mem_omega n) Omega_small)).
  assert (Hnn : ⟨ natZ n, natZ n ⟩ ∈ Big).
  { rewrite Couple_unfold. apply Paire_small; [ apply Sing_small; exact HnB | ].
    apply Paire_small; [ apply Empty_small | apply Sing_small; exact HnB ]. }
  apply set_ext; apply Inc_def => x Hx.
  - (* every candidate maps ⟨n,n⟩ into the codomain {⟨3,3⟩, ⟨4,4⟩} *)
    apply iUnion_IN in Hx. move: Hx => [f [Hf Hx]].
    apply (Inc_IN _ _ _ (Comp_Inc _ _)) in Hf.
    apply image_elim in Hx. move: Hx => [e [He Hedge]]. apply IN_Sing_EQ in He. subst e.
    exact (Pi_edge_codomain Big (fun _ => {| ⟨ 3, 3 ⟩ |} ∪ {| ⟨ 4, 4 ⟩ |})
             f ⟨ n, n ⟩ x Hf Hnn Hedge).
  - (* the constant function [fun _ => x] realises any [x ∈ {⟨3,3⟩, ⟨4,4⟩}] *)
    apply IN_iUnion with (y := iUnion Big (fun a => {| ⟨ a, x ⟩ |})).
    + apply In_P_Comp.
      * apply (iUnion_graph_mem_pi Big (fun _ => {| ⟨ 3, 3 ⟩ |} ∪ {| ⟨ 4, 4 ⟩ |}) (fun _ => x)).
        intros a _. exact Hx.
      * split.
        -- apply (FST_graph_isFunction Big (fun _ => x)). intros a b _ _ _. reflexivity.
        -- apply (SND_graph_isFunction Big (fun _ => x)). intros a b _ _ _. reflexivity.
    + apply (image_intro (iUnion Big (fun a => {| ⟨ a, x ⟩ |})) {| ⟨ n, n ⟩ |} x ⟨ n, n ⟩).
      * apply IN_Sing.
      * apply IN_iUnion with (y := ⟨ n, n ⟩); [ exact Hnn | apply IN_Sing ].
Qed.

End R.
