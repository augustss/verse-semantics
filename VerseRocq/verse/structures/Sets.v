Require Import Imports.

From Stdlib Require Export Ensembles.
From Stdlib Require Setoids.Setoid.
From Stdlib Require Lists.List.

(* Representing sets by their characteristic functions.  *)

(* This file extends the usual operations on sets with 
   functorial map, monadic join and bind (set comprehension).
*)

(*
   The tactic 'set_simpl' rewrites expressions using sets 
   to simpler versions.
   
*)

Create HintDb set_simpl.

Ltac set_simpl := autorewrite with set_simpl.
Tactic Notation "set_simpl" "in" hyp(H) :=
  autorewrite with set_simpl in H.

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

Open Scope set_scope.

Module SetNotations. 
  Notation "∅"  := Empty_set : set_scope.
  Notation "⌈ x ⌉" := (Singleton x) : set_scope.
  Infix "∪"  := Union (at level 60, right associativity) : set_scope.
  Infix "∩"  := Intersection (at level 60, right associativity) : set_scope.
  Infix "-"  := Setminus : set_scope.
  Notation "x ∈ s" := (Ensembles.In s x) (at level 65) : set_scope.
  Notation "a ⊆ b" := (Included a b) (at level 70) : set_scope.
  Notation "a ≃ b" := (Same_set a b) (at level 70) : set_scope.
End SetNotations. 

Import SetNotations.

(* Test cases for notations *)
Check (1 ∈ ⌈ 1  ⌉ ∪ ⌈2 ⌉ /\ 2 ∈ ⌈ 2  ⌉).
Check (∅ ⊆ ⌈ 1  ⌉ \/ ⌈ 1  ⌉ ⊆ ⌈ 2  ⌉ ∩ ⌈3 ⌉ ).
Check (∅ ∪ ⌈ 1  ⌉).
Check (∅ ∪ ⌈ 1  ⌉ ≃ ∅).

(* ----------------------------------------------------- *)

(* Sets have extensional equality! *)
Lemma set_extensionality {A} (s1 s2 : P A) :
  (forall x, x ∈ s1 <-> x ∈ s2) -> (s1 = s2).
Proof.
  intros h.
  eapply Extensionality_Ensembles.
  split. intros x. rewrite h. done. intros x. rewrite h. done.
Qed.

Ltac set_ext x := (apply set_extensionality; intros x).

(* ----------------------------------------------------- *)


(** More operations on sets *)
Definition Total_set {A} := fun (x:A) => True.

Definition map {A B} (f : A -> B) : P A -> P B := 
  fun s => fun y => exists x, f x = y /\ (x ∈ s).

(* Union of a set of sets (monadic join) *)
Definition join {A} (VS : P (P A)) : P A := 
  fun v => exists V, (In VS V) /\ (In V v).

(* define a set via set comprehension (i.e. monadic bind): 
   {{ k x | x <- s }} *)
Definition bind {A B} (s : P A) (k : A -> P B)  : P B := 
  fun b => exists a, (a ∈ s) /\ (b ∈ (k a)).

(* monadic sequence (>>): requires s1 to be inhabited *)
Definition seq {A B} (S1 : P A) (S2 : P B) := 
  bind S1 (fun _ => S2).
(* equivalent to:
  fun b => Inhabited s1 /\ (b ∈ s2)
*)

(* Total set if proposition holds, emptyset otherwise *)
Definition guard {A} (ϕ : Prop) : P A := fun _ => ϕ.

(* when ϕ s == guard ϕ ∩ s *)
Definition when {A} (ϕ : Prop) (s : P A) : P A := guard ϕ ∩ s.

(* if2 s1 s2 == guard (not (s1 = ∅)) >> s 
   returns s2 when s1 is not empty. otherwise emptyset *)
Definition If2 {A B} (s1 : P A) (s2 : P B) := seq s1 s2.

(* returns s2 when s1 is not empty. otherwise returns s3 *)
Definition If3 {A B} (s1 : P A) (s2 : P B) (s3 : P B) := 
  (seq s1 s2) ∪ (guard (s1 = ∅) ∩ s3).


Module SetMonadNotation.
  (* LLeftArrow *)
  Notation "x ⭅ c1 ;; c2" := (@bind _ _ c1 (fun x => c2))
    (at level 61, c1 at next level, right associativity) : set_scope.
  Notation "' pat ⭅ c1 ;; c2" :=
    (@bind _ _ c1 (fun x => match x with pat => c2 end))
    (at level 61, pat pattern, c1 at next level, right associativity) : set_scope.
  (* \fcmp *)
  Notation "a ⨾ b"  := (seq a b) (at level 70) : set_scope.
  (* \bigcupdot *)
  Notation "⨃" := join : set_scope.
End SetMonadNotation.

Import SetMonadNotation.

Check (x ⭅ ⌈ 1 ⌉ ;; ⌈ x ⌉).


(* ------------------------------------------------------------- *)

(** tactics *)

Ltac unfoldIn :=  match goal with 
    | [ |- context[?ρ ∈ (fun ρ0 => @?f ρ0)] ] => 
  replace (ρ ∈ (fun ρ0 => f ρ0)) with (f ρ);[|auto] end.

Ltac foldInGoal x := 
  match goal with 
  |  [ |- context [?S x] ] => 
  replace (S x) with (x ∈ S); [|auto] end.

Tactic Notation "foldInH" constr(x) "in" hyp(H) := 
  match goal with 
  |  [ H : context [?S x] |- _ ] => 
  replace (S x) with (x ∈ S) in H; [|auto] end.

Ltac set_crunch :=
    crunch ; repeat match goal with 
    | [ H : ?ρ ∈ (Sets.bind ?ma ?k) |- _ ] =>
        let ρ1 := fresh ρ in
        move: H => [ρ1 H]; crunch
    | [ H : ?ρ ∈ (Sets.seq ?s1 ?s2) |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ (Sets.map ?f ?s) |- _ ] =>
        let ρ1 := fresh ρ in
        move: H => [ρ1 H]; crunch
    | [ H : ?ρ ∈ ⌈?v ⌉ |- _ ] =>
        inv H; crunch
    | [ H : ⌈?v ⌉ ?ρ |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ (?s1 ∩ ?s2) |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ (when ?x ?k) |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ ∅ |- _ ] =>
        inv H
    | [ H : ?ρ ∈ (fun ρ => _) |- _ ] => 
        inv H ; crunch
      end.

(** Simplification rules *)

(** ---- map: (functor identities) *)

Lemma map_id: forall {A : Type} (s : P A), 
    (Sets.map id s) = s.
Admitted.

Lemma map_map: forall {A B C: Type} (f : B -> C) (g : A -> B) (s : P A), 
    (Sets.map f (Sets.map g s)) = Sets.map (fun x => f ( g x)) s.
Admitted.

Lemma map_empty {A B} {f : A -> B} : 
  Sets.map f (∅ : P A) = ∅.
Admitted.

Lemma map_singleton {A B} (f : A -> B) (a : A) :
  map f ⌈ a ⌉ = ⌈ f a ⌉.
set_ext b.
split.
intro h. destruct h as [y [h1 h2]]. subst. inv h2. econstructor; eauto.
intro h. destruct h. exists a. split; eauto. econstructor; eauto.
Qed.

Lemma map_union {A B} (f : A -> B) (s1 : P A) (s2 : P A) :
  (map f (s1 ∪ s2)) = ((map f s1) ∪ (map f s2)).
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

#[export] Hint Rewrite @map_empty @map_singleton @map_union 
  @map_id @map_map : set_simpl.

(* join *)

Lemma join_empty {A} : 
  ⨃ (∅ : P (P A)) = (∅ : P A).
Admitted.

Lemma join_singleton {A} (S : (P A)) :
  ⨃ ⌈S⌉ = S.
Admitted.

Lemma join_union {A} (S1 S2 : P (P A)) :
  ⨃ (S1 ∪ S2) = (⨃ S1) ∪ (⨃ S2).
Admitted.

#[export] Hint Rewrite @join_union @join_singleton @join_empty : set_simpl.


(* bind: *)

Lemma bind_empty {A B}(k : A -> P B) : Sets.bind ∅ k = ∅.
unfold Sets.bind.
set_ext s.
split; intros h; set_crunch.
Qed.

Lemma bind_singleton_l {A B : Type}
  {f : A -> P B}{a : A} :
  bind ⌈ a ⌉ f = f a.
eapply Extensionality_Ensembles.
split. intros x xIn. inversion xIn.
destruct H. inversion H. subst. auto.
intros x xIn. cbv. exists a. split. 
econstructor. auto.
Qed.


Lemma bind_union {A B} (s1 s2 : P A) (k : A -> P B) :
  (bind (s1 ∪ s2) k) = ((bind s1 k) ∪ (bind s2 k)).
Proof.
  set_ext ρ.
  split.
  + intro h. inv h. crunch. inv H.
    left. eexists. split; eauto.
    right. eexists. split; eauto.
  + intro h. inv h; inv H; crunch.
    eexists. split; eauto. left. auto.
    eexists. split; eauto. right. auto.
Qed.

Lemma bind_singleton_r  {A} {ma: P A} :
  bind ma (fun x : A => ⌈ x ⌉) = ma.
eapply Extensionality_Ensembles.
split. intros x xIn. inversion xIn.
destruct H. inversion H0. subst. auto.
intros x xIn.
exists  x. split. auto. econstructor.
Qed.

(* Define map in terms of bind *)
Lemma bind_singleton_map {A B} (f : A -> B) (ma : P A) :
   bind ma (fun x : A => ⌈f x⌉) = map f ma.
unfold bind.
eapply Extensionality_Ensembles.
split.
+ intros x xIn. destruct xIn as [a [h1 h2]].
  cbn. unfold map. inversion h2. exists a. split. auto.
  auto.
+ intros b xIn. cbn in xIn.  unfold map in xIn.
  destruct xIn as [x [h1 h2]].
  exists x. split. auto. rewrite h1. econstructor.
Qed.

Lemma bind_bind {A B C}{ma : P A}{f : A -> P B} {g : B -> P C} :
bind (bind ma f) g = 
  bind ma (fun x : A => bind (f x) g).
Proof.
  unfold bind.
  eapply Extensionality_Ensembles.
  split.
  intros c cIn. 
  destruct cIn as [b [[a [h1 h2]] h3]].
  exists a. split; auto. exists b. split; auto.
  intros c cIn. 
  destruct cIn as [b [h1 [a [h2 h3]]]]. 
  exists a. split; auto. exists b. split; auto.
Qed.


#[export] Hint Rewrite @bind_singleton_r @bind_singleton_l @bind_singleton_map @bind_union @bind_bind @bind_empty : set_simpl.

(* define bind in terms of join (not a rewrite) *)
Lemma join_map {A B} (f : A -> P B) (S : P A) : 
  bind S f = ⨃ (map f S).
Admitted.


Lemma bind_map_l : forall {A B C : Type} (g : B -> P C) (f : A -> B) (xs : P A), 
    Sets.bind (Sets.map f xs) g =  Sets.bind xs (fun x : A => g (f x)).
Admitted.

Lemma bind_map_r : forall {A B C : Type} (g : A -> P B) (f : B -> C) (s : P A), 
    Sets.bind s (fun x => Sets.map f (g x)) = Sets.map f (Sets.bind s g).
Proof.
  intros.
  rewrite <- bind_singleton_map.
  set_simpl.
  f_equal.
  eapply functional_extensionality. intros x.
  rewrite <- bind_singleton_map. done.
Qed.

(* intersection *)

Lemma univ_intersection_l {A} (s : P A) : 
  (Total_set ∩ s) = s.
Proof.
  eapply Extensionality_Ensembles.
  split. intros x xIn. inversion xIn. auto.
  split. cbv. auto. auto.
Qed.

Lemma univ_intersection_r {A} (s : P A) : 
  (s ∩ Total_set) = s.
  eapply Extensionality_Ensembles.
  split. intros x xIn. inversion xIn. auto.
  split; cbv; auto. 
Qed.

Lemma intersection_same {A}{v:P A} : (v ∩ v) = v.  
Admitted.

Lemma intersection_empty_l {A}(s2 : P A) : 
  (∅ ∩ s2) = ∅.
Admitted.

Lemma intersection_empty_r {A}(s2 : P A) : 
  (s2 ∩ ∅) = ∅.
Admitted.

Lemma intersection_assoc
     : forall (A : Type) (l m n : P A), 
    ((l ∩ m) ∩ n) = (l ∩ (m ∩ n)).
Admitted.


#[export] Hint Rewrite @univ_intersection_l @univ_intersection_r @intersection_same @intersection_empty_l @intersection_empty_r @intersection_assoc : set_simpl.

(* union *)

Lemma union_empty_r {A} (s : P A) : 
  (s ∪ ∅) = s.
Admitted.

Lemma union_empty_l {A} (s : P A) : 
  (∅ ∪ s) = s.
Admitted.

Lemma union_univ_r {A} (s : P A) : 
  (s ∪ Total_set) = Total_set.
Admitted.

Lemma union_univ_l {A} (s : P A) : 
  (Total_set ∪ s) = Total_set.
Admitted.


Lemma union_assoc
     : forall (A : Type) (l m n : P A), 
    ((l ∪ m) ∪ n) = (l ∪ (m ∪ n)).
Admitted.

Lemma union_same {A:Type}{E : P A} : (E ∪ E) = E.
Proof.
  eapply Extensionality_Ensembles.  
  split. intros x h. inversion h; auto.
  intros x h. left. auto.
Qed.

#[export] Hint Rewrite @union_same @union_empty_r @union_empty_l @union_univ_l @union_univ_r @map_union @bind_union @union_assoc : set_simpl.

(** setminus *)

Lemma any_minus_empty {A} (s : P A) : s - ∅ = s.
set_ext x.
split.
intro h. inversion h. done.
intro h. econstructor. auto. intro j. inv j.
Qed.

Lemma any_minus_all {A} (s : P A) : s - Total_set = ∅.
set_ext x.
Admitted.


#[export] Hint Rewrite @any_minus_empty @any_minus_all : set_simpl.

(* These are prop rewrites. *)

Lemma in_bind {A B} (ma : P A) (k : A -> P B) (ρ : B) :
  (ρ ∈ (x ⭅ ma ;; k x)) =
  (exists x, (x ∈ ma) /\ (ρ ∈ (k x))).
Proof. cbn. unfold bind. cbn. reflexivity. Qed.

Lemma in_ret {A} (x y :A) :
  (x ∈ (⌈ y ⌉ : P A)) = (x = y).
Proof.     
  cbn. 
  eapply propositional_extensionality.
  split. intros h1; inversion h1. done.
  intros h. subst. done.
Qed.

Lemma in_intersection {A} (x : A) s1 s2 :
  (x ∈ (s1 ∩ s2)) = ((x ∈ s1) /\ (x ∈ s2)).
Proof.
  eapply propositional_extensionality.
  split. 
  intros h1; inversion h1. split; done.
  intros [h1 h2]; econstructor; eauto.
Qed.

Lemma in_union {A} (x : A) s1 s2 :
  (x ∈ (s1 ∪ s2)) = ((x ∈ s1) \/ (x ∈ s2)).
Proof.
  eapply propositional_extensionality.  
  split. intros [h1|h1]; [left; auto| right; auto].
  intros [h1|h1];  [left; auto| right; auto].
Qed.

#[export] Hint Rewrite @in_ret @in_intersection @in_union : set_simpl.

Lemma intersect_Setminus {A} (S : P A) : S ∩ (Total_set - S) = ∅.
  set_ext s. unfold Total_set, Setminus. rewrite in_intersection.
  intuition. inv H1. done. inv H. done. Qed.

(* SCW: I think this needs an axiom that set membership is decidable. *)
Lemma union_Setminus {A} (S : P A) : (S ∪ (Total_set - S)) = Total_set.
  set_ext s. unfold Total_set, Setminus. rewrite in_union.
  unfold In. split. auto. 
Admitted.

#[export] Hint Rewrite @intersect_Setminus @union_Setminus : set_simpl.



Lemma If3_empty {A B} (s2 s3 : P B) : 
  @If3 A B ∅ s2 s3 = s3.
Proof.
  unfold If3.
  cbn. set_simpl.
  set_ext z.
  split. intro h. inv h. inv H. set_crunch. 
  inv H. auto. 
  intro h.
  right.
  split. unfold guard. unfold In. auto. auto.
Qed.

Lemma Singleton_not_empty {A}{v:A} : ⌈ v ⌉ <> ∅. 
Admitted.

Lemma If3_nonempty {A B} (s1 : P A)(s2 s3 : P B) : 
  Inhabited s1 ->
  @If3 A B s1 s2 s3 = s2.
Proof.
  intro h1.
  unfold If3.
  unfold guard.
  set_ext b.
  split.
  + set_simpl. intro h. set_crunch. auto.
    apply Inhabited_not_empty in h1.
    contradiction.
  + intro h. left. inv h1. exists x. split; auto.    
Qed.

Lemma If3_ret {A B} v (s2 s3 : P B) : 
  If3 (⌈ v ⌉ : P A) s2 s3 = s2.
Proof.
  eapply If3_nonempty.
  eexists. econstructor; eauto.
Qed.

Lemma If3_union {A B} (s1 s1' : P A)
  (s2 s3 : P B) : 
  If3 (s1 ∪ s1') s2 s3 = If3 s1 s2 (If3 s1' s2 s3).
Proof.
  unfold If3. 
  apply set_extensionality. intros b.
  set_simpl.
  split.
  + intro h. set_crunch.
    ++ inv H.
       left. exists x. eauto.
       right.
       split.
       unfold guard.
Abort.

#[export] Hint Rewrite @If3_empty @If3_ret : set_simpl. 



(* ------------------------------------------------------------- *)

(** Relation classes *)

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

Lemma map_Included {A B}{f : A -> B}{s1}{s2} :
  s1 ⊆ s2 -> map f s1 ⊆ map f s2.
Proof. 
  cbv.
  intros h x [a [h1 h2]]. 
  exists a. split; auto.
Qed.


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
    move: R => [M12 M21].
    split. 
    + intros b [a [h1 h2]].
      exists a. split. eauto. move: (S a) => [K12 K21]. eauto.
    + intros b [a [h1 h2]].
      exists a. split. eauto. move: (S a) => [K12 K21]. eauto.
Qed.


(* ----------------------------------------- *)

(* These facts are not useful as simplifications, but are still 
   facts about the various operations *)

(* facts about singleton sets *)

Lemma in_singleton {A:Type} {v : A} : 
  v ∈ ⌈ v ⌉.
Proof. unfold In. econstructor. Qed.

#[export] Hint Resolve @in_singleton : sets.

Lemma in_singleton' {A} (x y : A) : x = y -> x ∈ ⌈ y ⌉.
Proof. intros. subst. eapply in_singleton. Qed.

Lemma in_singleton_sub {A}{v:A}{X} : v ∈ X -> ⌈ v ⌉ ⊆ X.
Proof.
  intros. intros x xIn. inv xIn. done.
Qed.

#[export] Hint Resolve in_singleton_sub : sets.

Lemma Singleton_inv A (x y : A) : ⌈ x ⌉ = ⌈ y ⌉ <-> x = y.
Proof.  
  split.
  intros h. 
  have k: (x ∈ ⌈ x ⌉ <-> x ∈ ⌈ y ⌉). rewrite h.
  tauto.
  move: k => [h1 h2].
  specialize (h1 ltac:(eauto with sets)). inversion h1. auto.
  intro h. subst. auto.
Qed.

#[export] Hint Rewrite Singleton_inv : set_simpl.

(* Facts about union *)

Lemma sub_union_left {A} (X Y : P A) : X ⊆ (X ∪ Y).
Proof. intros x I.  econstructor; eauto. Qed.

Lemma sub_union_right {A} (X Y : P A) : Y ⊆ (X ∪ Y).
Proof. intros x I. eapply Union_intror; eauto. Qed.

#[export] Hint Resolve sub_union_left sub_union_right : sets.

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


Lemma empty_is_empty {A} : forall (S : P A), S = ∅ -> forall x, not (x ∈ S).
Admitted.

Lemma not_Singleton_empty : forall A B (x:B), ⌈ x ⌉ ≃ ∅ -> A.
Admitted.

Lemma Intersection_diff {A}{v1 v2:A} : v1 <> v2 -> (⌈v1⌉ ∩ ⌈v2⌉) = ∅. Admitted.

Lemma Intersection_commutes {A}{v1 v2:P A} : (v1 ∩ v2) = (v2 ∩ v1). Admitted.

Lemma bind_intersection {A B} (s1 s2 : P A) (k : A -> P B) :
  (bind (s1 ∩ s2) k) ⊆ ((bind s1 k) ∩ (bind s2 k)).
Proof.
  intros ρ.
  set_simpl.
  intro h.
  set_crunch.
  eexists. split; eauto.
  eexists. split; eauto.
Qed. (* NB: converse is not true. *)


Lemma SetMinusUnion {A} (s1 s2 s3 : P A) : s1 - (s2 ∪ s3) = (s1 - s2) - s3.
Proof. 
Admitted.

Lemma distrib_union_l {A} (S S1 S2 : P A) : S ∪ (S1 ∩ S2) = (S ∪ S1) ∩ (S ∪ S2).
Admitted.
Lemma distrib_union_r {A} (S S1 S2 : P A) : (S1 ∩ S2) ∪ S = (S1 ∪ S) ∩ (S2 ∪ S).
Admitted.

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



Lemma when_is_true A ϕ (S : P A) :
  ϕ -> when ϕ S = S. 
Admitted.


Lemma when_is_false A ϕ (S : P A) :
  ~ ϕ -> when ϕ S = ∅. 
unfold when, guard. intro h.
set_ext a. 
split; intro h1; set_crunch. 
unfold Ensembles.In in H. done.
Qed.




