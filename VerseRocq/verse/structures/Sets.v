Require Import Imports.

Require Import Laws.

From Stdlib Require Export Ensembles.
From Stdlib Require Setoids.Setoid.
From Stdlib Require Lists.List.

(* Representing sets by their characteristic functions. 

   This is a wrapper for `Ensembles` from the Coq standard library.

   It includes notations, Relation classes instances, and a bridge to 
   finite sets represented by lists.

*)

(* Inspired by 
   https://github.com/jsiek/denotational_semantics/agda/SetsAsPredicates.agda *)


Declare Scope set_scope.
Delimit Scope set_scope with Ensemble.
Bind Scope set_scope with Ensemble.

Arguments In {_}.
Arguments Included {_}.
Arguments Union {_}.
Arguments Intersection {_}.
Arguments Same_set {_}.
Arguments Singleton {_}.
Arguments Empty_set {_}.
Arguments Inhabited {_}.
Arguments Full_set {_}.
Arguments Setminus {_}.

Definition P := Ensemble.

(* More operations on sets *)
Definition Total_set {A} := fun (x:A) => True.

(* Union of a set of sets (monadic join) *)
Definition UNION {A} (VS : P (P A)) : P A := 
  fun v => exists V, (In VS V) /\ (In V v).

Open Scope set_scope.

Module SetNotations. 
  Notation "⊤"  := Total_set : set_scope.
  Notation "∅"  := Empty_set : set_scope.
  Notation "⌈ x ⌉" := (Singleton x) : set_scope.
  Infix "∪"  := Union (at level 90) : set_scope.
  Infix "∩"  := Intersection (at level 90) : set_scope.
  Infix "-"  := Setminus : set_scope.

  Notation "x ∈ s"  := (In s x) (at level 90) : set_scope.
  Notation "a ⊆ b" := (forall x, (x ∈ a) -> (x ∈ b)) (at level 90) : set_scope.
  Notation "a ≃ b" := ((a ⊆ b) /\ (b ⊆ a)) (at level 90) : set_scope.

  Notation "⨃" := UNION : set_scope.
End SetNotations. 

Import SetNotations.


Definition map {A B} (f : A -> B) : P A -> P B := 
  fun s => fun y => exists x, f x = y /\ (x ∈ s).

Definition filter {A} (f : A -> bool) : P A -> P A := 
  fun s => fun x => (f x = true) /\ (x ∈ s).

(* define a set via set comprehension (i.e. monadic bind): {{ k x | x <- s }} *)
Definition bind_ {A B} (k : A -> P B) (s : P A) : P B := 
  fun b => exists a, (a ∈ s) /\ (b ∈ (k a)).

(* monadic sequence (>>): requires s1 to be inhabited *)
Definition seq {A B} (s1 : P A) (s2 : P B) := 
  fun b => Inhabited s1 /\ (b ∈ s2).

(* Total set if proposition holds, emptyset otherwise *)
Definition guard {A} (ϕ : Prop) : P A := fun _ => ϕ.

(* when ϕ s == guard ϕ >> s *)
Definition when {A} (ϕ : Prop) (s : P A) : P A := 
  fun ρ => ϕ /\ (ρ ∈ s).

(* if2 s1 s2 == guard (s1 = ∅) >> s *)
Definition If2 {A B} (s1 : P A) (s3 : P B) := 
  (when (~ (Same_set s1 Empty_set)) s3).

Definition If3 {A B} (s1 : P A) (s2 : P B) (s3 : P B) := 
  Union 
    (when (Same_set s1 Empty_set) s2) 
    (when (not (Same_set s1 Empty_set)) s3).



(* P is a monad *)

#[export] Instance Monad_P : Monad P :=
  { ret  := (fun A (x : A) => Singleton x);
     bind := fun A B x y => @bind_ A B y x
   }.

#[export] Instance Functor_P : Functor P :=
  { fmap := fun A => @map A
  }.

#[export] Instance Applicative_P : Applicative P :=
  { pure := @ret P _;
     ap   := fun A B (m1 :  P (A -> B)) (m2 : P A) => 
               bind m1 (fun x1 => 
                            bind m2 (fun x2 => 
                                         ret (x1 x2))) 
  }.

#[export] Instance Alternative_P : Alternative P :=
  { empty := fun A (x:A) => False ;
    choose := fun A (p1 p2 : P A) (y : A) => 
                 p1 y \/ p2 y
   }.


(* Test cases for notations *)
Check (1 ∈ ⊤).
Check (1 ∈ ⌈ 1 ⌉).
Check (∅ ⊆ ⌈ 1 ⌉).
Check (∅ ∪ ⌈ 1 ⌉).
Check (∅ ∪ ⌈ 1 ⌉ ≃ ∅).


(* A proposition that a set is inhabited. Due to the restrictions
   of Coq, the witness cannot be extracted except to produce a 
   proof of a different proposition. *)
Definition nonempty {A} : P A -> Prop := @Inhabited A.

(* This is in Type so that we can extract the witness *)
Definition nonemptyT {A} (s : P A) : Type := {x : A & x ∈ s}.

Arguments nonempty {_}.
Arguments nonemptyT {_}.

Lemma nonemptyT_nonempty {A}{S : P A} : 
  nonemptyT S -> nonempty S.
Proof. intros h. destruct h. econstructor; eauto. Qed.


(* Relation classes *)

#[export] Instance Refl_Incl {A} : Reflexive (@Included A).
intros x. unfold Included. eauto. Qed.

#[export] Instance Trans_Incl {A} : Transitive (@Included A).
intros x y z S1 S2. unfold Included. intro w. eauto. Qed.

#[export] Instance Equivalence_Same_set {A} : Equivalence (@Same_set A) .
constructor.
- unfold Reflexive, Same_set, Included in *. tauto. 
- unfold Symmetric, Same_set, Included in *. tauto.
- unfold Transitive, Same_set, Included in *. intros x y z [h1 h2] [k1 k2]. 
  split; eauto.
Qed.

(* Proper instances: allow rewriting *)

#[export] Instance Union_Included_Proper {A} : Proper (@Included A ==> @Included A ==> @Included A) Union.
Proof. intros a1 a2 Ea b1 b2 Eb.
unfold Included in *. intros x h. inversion h. subst. left. auto. right. auto.
Qed.

#[export] Instance Union_Same_set_Proper {A} : Proper (@Same_set A ==> @Same_set A ==> @Same_set A) Union.
Proof. intros a1 a2 Ea b1 b2 Eb.
unfold Same_set in Ea. unfold Same_set in Eb. move: Ea => [Sa1 Sa2]. move: Eb => [Sb1 Sb2].
split. rewrite -> Sa1. rewrite -> Sb1. reflexivity. rewrite -> Sa2. rewrite -> Sb2. reflexivity. Qed.

#[export] Instance Included_Same_set_Proper {A} : Proper (@Same_set A ==> @Same_set A ==> Logic.iff) Included.
Proof. intros a1 a2 Ea. intros b1 b2 Eb.
       unfold Same_set in Ea. unfold Same_set in Eb. move: Ea => [Sa1 Sa2]. move: Eb => [Sb1 Sb2].
       split. intro h. transitivity a1; auto. transitivity b1; auto. 
       intros h. transitivity a2; auto. transitivity b2; auto. Qed.

#[export] Instance In_Proper {A} : Proper (@Same_set A ==> Logic.eq ==> Logic.iff) (@In A).
Proof. 
  intros a1 a2 [E1 E2]. intros b1 b2 ->.
  split; intro x; eauto.
Qed.

(* ----------------------------------------- *)


(* facts about singleton sets *)

Lemma in_singleton {A:Type} {v : A} : 
  v ∈ ⌈ v ⌉.
Proof. unfold In. econstructor. Qed.

#[export] Hint Resolve in_singleton : sets.

Lemma in_singleton_sub {A}{v:A}{X} : v ∈ X -> ⌈ v ⌉ ⊆ X.
Proof.
  intros. inversion H0. subst. done.
Qed.

#[export] Hint Resolve in_singleton_sub : sets.

Lemma Singleton_inv A (x y : A) : ⌈ x ⌉ = ⌈ y ⌉ -> x = y.
Proof.  intros h. 
        have k: (x ∈ ⌈ x ⌉ <-> x ∈ ⌈ y ⌉). rewrite h.
        tauto.
        move: k => [h1 h2].
        specialize (h1 ltac:(eauto with sets)). inversion h1. auto.
Qed.

(* Facts about union *)

Lemma sub_union_left {A} (X Y : P A) : X ⊆ (X ∪ Y).
Proof. intros x I.  econstructor; eauto. Qed.

Lemma sub_union_right {A} (X Y : P A) : Y ⊆ (X ∪ Y).
Proof. intros x I. eapply Union_intror; eauto. Qed.

#[export] Hint Resolve sub_union_left sub_union_right : sets.

Lemma union_idem {A:Type}{E : P A} : (E ∪ E) ≃ E.
Proof.
  split. intros x h. inversion h; auto.
  intros x h. left. auto.
Qed.

#[export] Hint Resolve union_idem : sets.

Lemma union_left {A}{X Y Z: P A} : X ⊆ Z -> Y ⊆ Z -> X ∪ Y ⊆ Z.
Proof. intros h1 h2.
       intros x xIn. destruct xIn; eauto.
Qed.

Lemma union_left_inv1 {A}{X Y Z: P A} : X ∪ Y ⊆ Z -> X ⊆ Z.
Proof. 
  intros h x in1. apply h. econstructor; eauto.
Qed.

Lemma union_left_inv2 {A}{X Y Z: P A} : X ∪ Y ⊆ Z -> Y ⊆ Z.
Proof. 
  intros h x in1. apply h. eapply Union_intror; eauto.
Qed.


#[export] Hint Resolve union_left_inv1 union_left_inv2 : sets.


(* ----------------------------------------- *)


Definition Forall  : forall {A} (Pr : A -> Prop), P A -> Prop := 
  fun {A} Pr p => forall x, x ∈ p -> Pr x.

Definition Exists : forall {A} (Pr : A -> Prop), P A -> Prop :=
  fun {A} Pr p => exists x, (x ∈ p) /\ Pr x.

Definition  ForallT : forall {A} (Pr : A -> Type), P A -> Type := 
  fun {A} Pr p => forall x, x ∈ p -> Pr x.

Definition ExistsT :  forall {A} (Pr : A -> Type), P A -> Type :=
  fun {A} Pr p => { x & ((x ∈ p) * Pr x)%type }.

Definition Forall_forall : forall {A} (Pr : A -> Prop) (l : P A), 
      Forall Pr l <-> (forall x, x ∈ l -> Pr x).
Proof. intros. unfold Forall. reflexivity. Qed.

Definition Exists_exists : forall {A} (Pr : A -> Prop) (l : P A), 
      Exists Pr l <-> (exists x, (x ∈ l) /\ Pr x).
Proof. intros. unfold Exists. reflexivity. Qed.


(* -------------- some laws -------------- *)

Lemma bind_singleton_l {A B : Type}
  {f : A -> P B}{a : A} :
  bind_ f ⌈ a ⌉ = f a.
eapply Extensionality_Ensembles.
split. intros x xIn. inversion xIn.
destruct H. inversion H. subst. auto.
intros x xIn. cbv. exists a. split. 
econstructor. auto.
Qed.

#[export] Instance BindRetL_P : BindRetL (m:=P).
intros A B f a. eapply bind_singleton_l.
Qed.

Lemma bind_singleton_r  {A} {ma: P A} :
  bind_ (fun x : A => ⌈ x ⌉) ma = ma.
eapply Extensionality_Ensembles.
split. intros x xIn. inversion xIn.
destruct H. inversion H0. subst. auto.
intros x xIn.
exists  x. split. auto. econstructor.
Qed.

#[export] Instance BindRetR_P : BindRetR (m:=P).
intros A ma. eapply bind_singleton_r.
Qed.

Lemma bind_bind {A B C}{ma : P A}{f : A -> P B} {g : B -> P C} :
bind_ g (bind_ f ma) = 
  bind_ (fun x : A => bind_ g (f x)) ma.
Proof.
  unfold bind_.
  eapply Extensionality_Ensembles.
  split.
  intros c cIn. 
  destruct cIn as [b [[a [h1 h2]] h3]].
  exists a. split; auto. exists b. split; auto.
  intros c cIn. 
  destruct cIn as [b [h1 [a [h2 h3]]]]. 
  exists a. split; auto. exists b. split; auto.
Qed.

#[export] Instance BindBind_P : BindBind (m:=P).
intros A B C ma f g.
eapply bind_bind.
Qed.

#[export] Instance RetInv_P : RetInv (m:=P).
intros A a1 a2 h. cbn in h.
eapply Singleton_inv. auto.
Qed.

#[export] Instance BindRetInv_P : BindRetInv (m:=P).
Abort.

Lemma bind_singleton_fmap {A B} (f : A -> B) (ma : P A) :
   bind_ (fun x : A => ⌈f x⌉) ma = fmap f ma.
unfold bind_.
eapply Extensionality_Ensembles.
split.
+ intros x xIn. destruct xIn as [a [h1 h2]].
  cbn. unfold map. inversion h2. exists a. split. auto.
  auto.
+ intros b xIn. cbn in xIn.  unfold map in xIn.
  destruct xIn as [x [h1 h2]].
  exists x. split. auto. rewrite h1. econstructor.
Qed.

Lemma fmap_Included {A B}{f : A -> B}{s1}{s2} :
  s1 ⊆ s2 -> fmap f s1 ⊆ fmap f s2.
Proof. 
  cbv.
  intros h x [a [h1 h2]]. 
  exists a. split; auto.
Qed.

(*
#[export] Instance bind_Included_Proper {A B} :
  Proper (Included ==> (fun k1 k2 => forall x, Included (k1 x) (k2 x)) ==> Included) 
    (bind : P A -> (A -> P B) -> P B).
Proof.
  intros m1 m2 R k1 k2 S.
  cbv.
  intros x [a [h1 h2]].
  exists a. split; eauto. eapply R; eauto. eapply S; eauto.
Qed.

#[export] Instance bind_Same_set_Proper {A B} :
  Proper (Same_set ==> (fun k1 k2 => forall x, Same_set (k1 x) (k2 x)) ==> Same_set) 
    (bind : P A -> (A -> P B) -> P B).
Proof.
    intros m1 m2 R k1 k2 S.
    unfold bind, Monad_P.
    move: R => [M12 M21].
    split. 
    + intros b [a [h1 h2]].
      exists a. split. eauto. move: (S a) => [K12 K21]. eauto.
    + intros b [a [h1 h2]].
      exists a. split. eauto. move: (S a) => [K12 K21]. eauto.
Qed. *)

(* ------------------------------------------------------- *)


(* Finite lists `mem` as sets *)

Import List.

Definition mem {A} : list A -> P A :=
  fun ls x => List.In x ls.

Lemma mem_one_inv : forall A (h v : A),  
 h ∈ mem (v :: nil) -> h = v.
Proof. 
  intros. cbn in H. destruct H; try done.
Qed. 

(* E≢[]⇒nonempty-mem *)
Lemma nonnil_nonempty_mem : forall{T}{E : list T}, E <> nil -> nonemptyT (mem E).
Proof. intros. destruct E; cbv. done.
       econstructor.
       econstructor. eauto.
Qed.

Lemma mem_head {A} a (V : list A) :
   a ∈ mem (a :: V).
Proof. 
  unfold mem. 
  unfold Ensembles.In.
  econstructor. auto.
Qed.

Lemma mem_cons {A} d a (V : list A) :
    d ∈ mem V ->
    d ∈ mem (a :: V).
Proof. 
  unfold mem. 
  unfold Ensembles.In.
  eapply in_cons.
Qed.

#[export] Hint Resolve mem_head mem_cons mem_one_inv : sets.


Lemma In_Sub {A}{x:A}{D}: x ∈ D <-> mem (x :: nil) ⊆ D.
Proof. split. intros h y yIn. inversion yIn. subst; auto. inversion H. 
       intros h. cbv in h. specialize (h x). tauto.
Qed.

#[export] Hint Resolve In_Sub : sets.

Lemma union_mem {A:Type}{E1 E2 : list A} : mem (E1 ++ E2) = (mem E1 ∪ mem E2).
Proof. unfold mem. 
       eapply Extensionality_Ensembles.
       split.
       + intros x.
         induction E1. 
         ++ simpl. intro h. right. auto.
         ++ simpl. intro h. inversion h.
            left. unfold In. econstructor; eauto.
            apply IHE1 in H.
            inversion H. subst.
            left. unfold In. eapply in_cons. auto.
            subst. right. auto.
       + intros x.
         induction E1. 
         ++ intro h. inversion h. subst. done. subst. auto.
         ++ simpl. intro h. inversion h; subst.
            destruct H.  left. auto.
            lapply IHE1. intro h2. right. eauto.
            left. eauto. right. apply in_or_app. right. auto.
Qed.

Lemma singleton_mem {A} : forall v : A,  ⌈ v ⌉ ⊆ mem (v :: nil).
Proof. intro v. econstructor. inversion H. done. Qed.

Lemma mem_singleton {A} : forall v : A, mem (v :: nil) ⊆ ⌈ v ⌉.
Proof. intro v. cbv. intros. inversion H. subst. econstructor; eauto. done. Qed.

Lemma mem_singleton_eq {A} {x:A} : mem (x :: nil) ≃ ⌈ x ⌉.
Proof. split; eauto using mem_singleton, singleton_mem. Qed.

#[export] Hint Resolve singleton_mem mem_singleton : sets. 


Lemma mem_cons_inv {A} {a:A} {xs ys : list A} : 
  ~(List.In a xs) -> ~(List.In a ys) -> mem (a :: xs) ≃ mem (a :: ys) -> mem xs ≃ mem ys.
Proof. 
  intros nI1 nI2 [h1 h2].
  split.
  - intros x xIn.
    specialize (h1 x).
    have h: x ∈ mem (a :: xs). right. auto.
    specialize (h1 h). clear h2.
    destruct h1.
    -- subst. done.
    -- done.
  - intros x xIn.
    specialize (h2 x).
    have h: x ∈ mem (a :: ys). right. auto.
    specialize (h2 h). 
    destruct h2.
    -- subst. done.
    -- done.
Qed.




Lemma Forall_mem {A}{V : list A}{Pr} : List.Forall Pr V -> Sets.Forall Pr (mem V).
Proof.
  induction V; intro h; intros y yIn. 
  inversion yIn. 
  inversion h. subst.
  inversion yIn. subst. auto. eapply IHV; eauto.
Qed.

Lemma Forall_sub_mem : forall A (D:list A) X Pr, 
    mem D ⊆ X -> 
    Sets.Forall Pr X ->
    List.Forall Pr D.
Proof.
  intros A D.
  induction D; intros X Pr SUB F.
  eauto.
  econstructor; eauto. eapply F. 
  eapply SUB. cbn. eauto with sets.
  eapply IHD with (X:=X); auto. 
  intros x xIn. eapply SUB; cbn; eauto with sets. 
Qed.

Lemma mem_In : forall A (x:A) l, x ∈ mem l -> List.In x l.
Proof. intros. induction l. cbv in H. done.
       destruct H. subst. econstructor. auto. 
       simpl. right. eauto. Qed.



Lemma in_singleton' {A} (x y : A) : x = y -> x ∈ ⌈ y ⌉.
Proof. intros. subst. eapply in_singleton. Qed.


Lemma all_intersect {A} (s : P A) : (Total_set ∩ s) = s.
Proof.
  eapply Extensionality_Ensembles.
  split. intros x xIn. inversion xIn. auto.
  split. cbv. auto. auto.
Qed.

Lemma intersect_all {A} (s : P A) : (s ∩ Total_set) = s.
  eapply Extensionality_Ensembles.
  split. intros x xIn. inversion xIn. auto.
  split; cbv; auto. 
Qed.

Lemma Union_empty {A} (V : P A) : (∅ ∪ V) = V.
Admitted.

Lemma empty_Union {A} (V : P A) : (∅ ∪ V) = V.
Admitted.

Lemma map_union {A B} (f : A -> B) (s1 : P A) (s2 : P A) :
  (Sets.map f (s1 ∪ s2)) = ((Sets.map f s1) ∪ (Sets.map f s2)).
Proof.
  unfold Sets.map.
  eapply Extensionality_Ensembles.
  split.
  - intros x [a [h1 h2]]. subst. inversion h2. subst.
    left. exists a. split; auto.
    right. exists a. split; auto.
  - intros b1 h1. 
    inversion h1. subst. clear h1.
    + move: H => [ y [h2 h3]]. subst.
      exists y. split; auto. left. auto.
    + move: H => [ y [h2 h3]]. subst.
      exists y. split; auto. right. auto.
Qed.

Lemma map_singleton {A B} (f : A -> B) (a : A) :
  (Sets.map f ⌈ a ⌉) = ⌈ f a ⌉.
Admitted.


Lemma UNION_empty {A} (W : P (P A)) : (⨃ (∅ ∪ W )) = (⨃ W).
Admitted.

Lemma UNION_Singleton {A} (V : P A) W : (⨃ (⌈V ⌉ ∪ W )) = (V ∪ ⨃ W).
Proof.
eapply Extensionality_Ensembles.
split.
- intros x xIn. cbv in xIn. move: xIn => [v [h1 h2]].
  inversion h1. subst. inversion H. subst. left. auto.
  right.  exists v. split; eauto.
- intros x xIn. inversion xIn. subst.
  exists V. split; auto. left. eapply in_singleton.
  cbv in H. move: H => [w [h1 h2]].
  exists w. split. right. eauto. eauto.
Qed.

Lemma empty_is_empty {A} : forall (S : P A), S = ∅ -> forall x, not (x ∈ S).
Admitted.

Lemma not_Singleton_empty : forall A B (x:B), ⌈ x ⌉ ≃ ∅ -> A.
Admitted.
Lemma Singleton_not_empty {A}{v:A} : ⌈ v ⌉ <> ∅. 
Admitted.


Lemma Intersection_same {A}{v:P A} : (v ∩ v) = v.  Admitted.
Lemma Intersection_diff {A}{v1 v2:A} : v1 <> v2 -> (⌈v1⌉ ∩ ⌈v2⌉) = ∅. Admitted.
Lemma Intersection_commutes {A}{v1 v2:P A} : (v1 ∩ v2) = (v2 ∩ v1). Admitted.

Lemma SetMinus_empty {A} (s : P A) : s - ∅ = s. Admitted.
Lemma SetMinusUnion {A} (s1 s2 s3 : P A) : s1 - (s2 ∪ s3) = (s1 - s2) - s3.
Proof. 
Admitted.



Lemma set_extensionality {A} (s1 s2 : P A) :
  (forall x, x ∈ s1 <-> x ∈ s2) -> (s1 = s2).
Proof.
  intros h.
  eapply Extensionality_Ensembles.
  split. intros x. rewrite h. done. intros x. rewrite h. done.
Qed.

Lemma Union_empty_r {A} (s : P A) : 
  (s ∪ ∅) = s.
Admitted.

Lemma Union_empty_l {A} (s : P A) : 
  (∅ ∪ s) = s.
Admitted.

Lemma intersection_empty_l {A}(s2 : P A) : 
  (∅ ∩ s2) = ∅.
Admitted.

Lemma intersection_empty_r {A}(s2 : P A) : 
  (s2 ∩ ∅) = ∅.
Admitted.


#[export] Hint Rewrite 
  @intersection_empty_l
  @intersection_empty_r
  @Union_empty_r
  @Union_empty_l : sets.

Import MonadNotation.

Lemma in_bind {A B} (ma : P A) (k : A -> P B) (ρ : B) :
  (ρ ∈ (x <- ma ;; k x)) <->
  (exists x, (x ∈ ma) /\ (ρ ∈ (k x))).
Proof. cbn. unfold bind_. cbn. reflexivity. Qed.

Lemma in_ret {A} (x y :A) :
  x ∈ (ret y : P A) <-> x = y.
Proof.     
  cbn. split. intros h1; inversion h1. done.
  intros h. subst. done.
Qed.

Lemma in_intersection {A} (x : A) s1 s2 :
  x ∈ (s1 ∩ s2) <-> (x ∈ s1) /\ (x ∈ s2).
Proof.
  split. intros h1; inversion h1. split; done.
  intros [h1 h2]; econstructor; eauto.
Qed.

Lemma in_union {A} (x : A) s1 s2 :
  x ∈ (s1 ∪ s2) <-> (x ∈ s1) \/ (x ∈ s2).
Proof.
  split. intros [h1|h1]; [left; auto| right; auto].
  intros [h1|h1];  [left; auto| right; auto].
Qed.

#[export] Hint Rewrite @in_bind @in_ret @in_intersection @in_union : sets.

Lemma bind_union {A B} (s1 s2 : P A) (k : A -> P B) :
  (bind (s1 ∪ s2) k) = ((bind s1 k) ∪ (bind s2 k)).
Proof.
  apply set_extensionality. intros ρ.
  autorewrite with sets.
  split.
  + intro h.
    crunch.
    inv H.
    left. eexists. split; eauto.
    right. eexists. split; eauto.
  + intro h. crunch.
    eexists. split; eauto. left. auto.
    eexists. split; eauto. right. auto.
Qed.

#[export] Hint Rewrite @bind_union : sets.

Lemma bind_intersection {A B} (s1 s2 : P A) (k : A -> P B) :
  (bind (s1 ∩ s2) k) ⊆ ((bind s1 k) ∩ (bind s2 k)).
Proof.
  intros ρ.
  autorewrite with sets.
  intro h.
  crunch.
  inv H.
  eexists. split; eauto.
  inv H.
  eexists. split; eauto.
Qed. (* NB: converse is not true. *)


Lemma intersection_assoc
     : forall (A : Type) (l m n : P A), 
    (l ∩ (m ∩ n)) = ((l ∩ m) ∩ n).
Admitted.
Lemma union_assoc
     : forall (A : Type) (l m n : P A), 
    (l ∪ (m ∪ n)) = ((l ∪ m) ∪ n).
Admitted.
