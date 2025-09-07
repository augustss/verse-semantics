Require Import Imports.

Import ssreflect.

Require Import syntax.common.
Require syntax.mini.
Require syntax.essential.
Require Import PFun.
Require Import structures.Sets.
Import structures.List.

Require Export densem.Dom.    (* values are finite *)
Require Export densem.tenv.   (* environments are total *)
Require Export densem.envSet. (* def of ENV , hide, constraints *)
Require Export densem.squash. (* definitions related to squashing *)

Import mini.MiniNotation.
Import SetNotations.
Import SetMonadNotation.
Import List.ListNotations.
Import ListMonadNotation.
Import EnvNotation.
Import envSetNotation.

Open Scope list_scope.
Open Scope mini_expr_scope.
Open Scope env_scope.
Open Scope set_scope.

Ltac bind_equal w := 
  match goal with 
  | [ |- Sets.bind ?x ?y = Sets.bind ?x ?z ] => 
      f_equal; extensionality w ; set_simpl
  | [ |- Sets.map ?f ?x = Sets.bind ?x ?z ] => 
      rewrite <- Sets.bind_singleton_map;
      f_equal; extensionality w ; set_simpl
  | [ |- Sets.bind ?x ?z = Sets.map ?f ?x ] => 
      rewrite <- Sets.bind_singleton_map;
      f_equal; extensionality w ; set_simpl
  | [ |- Sets.bind ?x ?z = ?x ] => 
      rewrite <- (@Sets.bind_singleton_r _ x) at 2;
      f_equal; extensionality w ; set_simpl
  end.



(* some list properties. *)
Lemma fold_right_map {A B C} (f : A -> C) g (b : B) (z : list A) :
  List.fold_right g b (List.map f z) = List.fold_right (fun x => g (f x)) b z.
Proof. induction z. cbn. done. cbn. rewrite IHz. f_equal. Qed.
Lemma flat_map_cons {A B} (f : A -> list B) (x : A) xs : 
  List.flat_map f (x :: xs) = (List.flat_map f [x]) ++ (List.flat_map f xs).
Proof. rewrite <- flat_map_app. cbn. done. Qed.





(** ------- operations on sets of lists ------ *)

(* The set of all first elements from a list. *)
(*   {{ v | (v :: _) <- V }}   *)
Definition Head {A} (V : P (list A)) : P A := 
  vs ⭅ V ;;
  match vs with 
  | v :: _ => ⌈ v ⌉
  | _ => ∅
  end. 

Definition Nil {A} : P (list A) := ⌈ [] ⌉.
Definition Cons {A} (x : P A) (xs : P (list A)) : P (list A) := 
   y1 ⭅ x ;; y2 ⭅ xs ;; ⌈ y1 :: y2 ⌉.  
Definition Append {A} (x1 : P (list A)) (x2: P (list A)) : P (list A) := 
   y1 ⭅ x1 ;; y2 ⭅ x2 ;; ⌈ y1 ++ y2 ⌉.
Definition Concat {A} (xss : P (list (list A))) : P (list A) := 
   xs ⭅ xss ;; ⌈ concat xs ⌉.

(** ---- Fold ??? ------- *)

Definition Fold {A B} (f : B -> A -> A) (bs : P A) (xs: P (list B)) : P A := 
  x ⭅ xs ;;
  b ⭅ bs ;;
  ⌈ List.fold_right f b x ⌉.

Lemma app_def {A} (xs ys : list A) :
 app xs ys = List.fold_right cons ys xs.
Proof. induction xs. cbn. done. cbn. rewrite IHxs. done. Qed.

Lemma Append_def {A} (xs ys : P (list A)) : 
  Append xs ys = Fold cons ys xs.
Proof. 
  unfold Append, Fold.
  bind_equal y1.
  f_equal.
  extensionality z.
  rewrite app_def.
  done.
Qed.

Lemma concat_def {A} (xss : list (list A)) : 
  concat xss = List.fold_right (@List.app A) nil xss.
Proof.
  induction xss. cbn. done.
  cbn. f_equal. Qed.

Lemma Concat_def {A} (xss : P (list (list A))) :
  Concat xss = Fold (@List.app A) Nil xss.
Proof.
  unfold Concat, Fold, Nil.
  bind_equal y1.
  rewrite concat_def.
  done.
Qed.

(* --------- pick/sequence ----------- *)

(* pick for sets: given a list of sets, make the set of 
   all lists where each element comes from each set. *)
Fixpoint pick {A} (xs : list (P A)) : P (list A) := 
  match xs with 
  | nil => Nil
  | V :: VS => Cons V (pick VS)
  end.

(* pick for lists: 
   NB: this is the list instance of 'sequence' from Haskell's 
   Traversable class *)
Fixpoint pickl {A} (xs : list (list A)) : list (list A) := 
  match xs with 
  | nil => [ [] ]
  | V :: VS =>  v <- V ;; vs <- pickl VS ;; [ v :: vs ] 
  end.

Definition Concat' {A} : list (P (list A)) -> P (list A) := 
  List.fold_right Append Nil.

Definition CP {A} : list (P (list A)) -> P (list A) := 
 List.fold_right Append Nil.

(** ------------- Functor/Applicative/Monad definitions ---------------- *)


Definition Pure {A} : A -> P (list A) := fun x =>  ⌈ [ x ] ⌉ .

Definition Map { A B} : (A -> B) -> P (list A) -> P (list B) := 
  fun op E => s ⭅ E ;; ⌈ List.map op s ⌉.

Definition Bind {A B} : P (list A) -> (A -> P (list B)) -> P (list B) := 
  fun S K => 
    s ⭅ S ;;
    let t : list (P (list B)) := List.map K s in
    Concat (pick t).


Definition liftA2 {A B C} : (A -> B -> C) -> P (list A) -> P (list B) -> P (list C) := 
  fun op E1 E2 => 
    s1 ⭅ E1 ;;
    let t : list (P (list C)) := 
      Δ <- s1 ;;
      [ Map (op Δ) E2 ] in
    CP t.


Definition liftA2' {A B C} : (A -> B -> C) -> P (list A) -> P (list B) -> P (list C) := 
  fun op E1 E2 => 
    s1 ⭅ E1 ;;
    s2 ⭅ E2 ;;
    ⌈ List.ap (List.map op s1) s2 ⌉.



(* --------------- Append/Concat Properties ------------------- *)


Lemma Append_Nil_l {A} (XS : P (list A)) : Append Nil XS = XS.
Proof. unfold Nil, Append. set_simpl. done. Qed.
Lemma Append_Nil_r {A} (XS : P (list A)) : Append XS Nil = XS.
Proof.
unfold Nil, Append. 
bind_equal z.
eapply List.app_nil_r.
Qed.
Lemma Append_Append {A} (XS YS ZS : P (list A)) : 
  Append (Append XS YS) ZS = Append XS (Append YS ZS).
Proof.
  unfold Append.
  set_simpl.
  bind_equal x0.
  bind_equal x1.
  bind_equal x2.
  rewrite app_assoc.
  done.
Qed.

Lemma Concat_Nil {A} : Concat (Nil : P (list (list A))) = Nil.
Proof. unfold Concat, Nil. set_simpl. reflexivity. Qed.
Lemma Concat_Cons {A} (x:P (list A)) xs : Concat (Cons x xs) = Append x (Concat xs).
Proof. unfold Concat, Cons, Append. 
       set_simpl.
       bind_equal y.
       bind_equal z.
       reflexivity.
Qed.
Lemma Concat_Append {A} (xs ys : P (list (list A))) : 
  Concat (Append xs ys) = Append (Concat xs) (Concat ys).
Proof. 
  unfold Concat,Append. set_simpl.
  bind_equal x.
  bind_equal y.
  rewrite List.concat_app.
  done.
Qed.

Lemma Concat_Map {A B} (f : A -> B) (l : P (list (list A))) :
  Map f (Concat l) = Concat (Map (map f) l).
Proof.
  unfold Map, Concat.
  set_simpl.
  bind_equal x0.
  rewrite concat_map. 
  done.
Qed.

Lemma Concat_singleton {A} (s : list A) : 
  Concat ⌈ (fun x => [x]) <$> s ⌉ = ⌈ s ⌉.
Proof.
  unfold Concat.
  set_simpl.
  rewrite <- flat_map_concat_map. list_simpl. done.
Qed.

Lemma Concat_pick {A} (xs : list (P (list A))) :
 Concat (pick xs) = List.fold_right Append Nil xs.
Proof.
  induction xs. simpl. rewrite Concat_Nil. done.
  cbn. rewrite Concat_Cons.
  f_equal. 
  unfold Concat' in IHxs. done.
Qed.


Lemma Map_Nil {A B} (f : A -> B) :
  Map f Nil = Nil.
Proof.
  unfold Map, Nil. set_simpl. cbn. done.
Qed.

Lemma Map_Cons {A B} (f : A -> B) (x : P A) (xs : P (list A)) :
  Map f (Cons x xs) = Cons (Sets.map f x) (Map f xs).
Proof.
  unfold Map, Cons.
  set_simpl.
  rewrite Sets.bind_map_l.
  bind_equal x0.
  bind_equal xs0.
  cbn.
  done.
Qed.

Lemma Map_Append {A B} (f : A -> B) (xs1 xs2 : P (list A)) :
  Map f (Append xs1 xs2) = Append (Map f xs1) (Map f xs2).
Proof.
  unfold Map,Append.
  set_simpl.
  bind_equal ys1.
  bind_equal ys2.
  rewrite map_app.
  done.
Qed.



(* ------ set bind Cons/Append ---------------- *)

Lemma Set_Bind_Cons {A B} (l1 : P (list A)) (l2 : P B) (k : list A -> P (list B)) :
 x1 ⭅ l1;; Cons l2 (k x1) =  Cons l2 (x1 ⭅ l1;; k x1).
Proof.
  unfold Cons.
  unfold Sets.bind.
  set_ext a. 
  split.
  - intro h. set_crunch. unfold Ensembles.In.
    exists x0. split; auto.
    exists x1. split; eauto. eapply in_singleton.
  - intro h. set_crunch. unfold Ensembles.In.
    exists x1. split; auto.
    exists x. split; eauto.
    exists x0. split; eauto.
    eapply in_singleton.
Qed.

Lemma Set_Bind_Append {A B} (l1 : P (list A)) (l2 : P (list B)) (k : list A -> P (list B)) :
 x1 ⭅ l1;; Append l2 (k x1) =  Append l2 (x1 ⭅ l1;; k x1).
Proof.
  unfold Append.
  unfold Sets.bind.
  set_ext a. 
  split.
  - intro h. set_crunch. unfold Ensembles.In.
    exists x0. split; auto.
    exists x1. split; eauto. eapply in_singleton.
  - intro h. set_crunch. unfold Ensembles.In.
    exists x1. split; auto.
    exists x. split; eauto.
    exists x0. split; eauto.
    eapply in_singleton.
Qed.

Lemma Set_Bind_Concat {A B} (S : P (list A)) (k : (list A) -> P (list (list B))) :
 s ⭅ S ;; Concat (k s) = Concat (s ⭅ S ;; k s).
Proof.
  unfold Concat.
  unfold Sets.bind.
  set_ext a.
  split.
  - intro h. set_crunch. unfold Ensembles.In.
    exists x0. split; auto.
    exists x. split; eauto. eapply in_singleton.
  - intro h. set_crunch. unfold Ensembles.In.
    exists x0. split; auto.
    exists x. split; eauto.
    eapply in_singleton.
Qed.


(** --------------- pick properties ---------------- *)


Lemma pick_nil {A} :
  pick ([] : list (P A)) = Nil. Proof. reflexivity. Qed.
Lemma pick_cons {A} (x:P A) (xs : list (P A)) :
  pick (x :: xs) = Cons x (pick xs). 
Proof. unfold pick, Cons. done. Qed.
Lemma pick_app {A} : forall (x1 : list (P A)) (x2 : list (P A)), 
  pick (x1 ++ x2) = Append (pick x1) (pick x2).
Proof.
  unfold Append.
  induction x1; intros x2.
  - cbn. unfold Nil. set_simpl. done.
  - cbn. 
    rewrite IHx1.
    unfold Cons. set_simpl.
    bind_equal z1.
    bind_equal z2.
    bind_equal z3.
    reflexivity.
Qed.

Lemma pick_def {A} (xs : list (P A)) :
  pick xs = List.fold_right Cons Nil xs.
induction xs; try done. rewrite pick_cons. cbn. f_equal. done. Qed.

Lemma pick_map {A B} (s : list (P A)) (f : A -> B) : 
  pick (Sets.map f <$> s) =  Map f (pick s).
Proof.
  induction s.
  - cbn. rewrite Map_Nil. done.
  - cbn. rewrite Map_Cons. rewrite IHs. done.
Qed.

Lemma pick_pure {A} (s : list A) : 
  pick (Pure <$> s) =  ⌈ List.map (fun x => [x]) s ⌉.
Proof.
  induction s.
  - cbn. done.
  - cbn. rewrite IHs.
    unfold Cons, Pure.
    set_simpl.
    done.
Qed.


(** --------------- Functor laws ------------------- *)

Lemma map_id {A} (x : P (list A)) :
  Map id x = x.
Proof.
  unfold Map.
  bind_equal y.
  rewrite List.map_id.
  done.
Qed.

Lemma map_map {A B C} (g : A -> B) (f : B -> C) x : 
  Map f (Map g x) = Map (fun y => f (g y)) x.
Proof. unfold Map.
  set_simpl.       
  bind_equal y.
  rewrite List.map_map.
  done.
Qed.

(** ---------------- *)

Definition comp {A B C}  (f : B -> C) (g : A -> B) := fun x => f ( g x).

Definition Comp {A B C} : P (list (B -> C)) -> P (list (A -> B)) -> P (list (A -> C))
  := liftA2 comp.

Lemma list_map_id {A} (x : list A) f :
  (forall y, f y = y) ->
  List.map f x = x. 
Proof. induction x; intros h; cbn.  done. rewrite h. rewrite IHx; auto. Qed.

Lemma Comp_left_id {A B} (f : P (list (A -> B))) : Comp (Pure id) f = f.
Proof.
  unfold Comp, liftA2, Pure.
  set_simpl.
  list_simpl.
  cbn.
  rewrite Append_Nil_r.
  unfold Map.
  bind_equal y.
  rewrite list_map_id; auto.
Qed.

Lemma comp_right_id {A B} (f : P (list (A -> B))) : 
  Comp f (Pure id) = f.
Proof.
  unfold Comp, liftA2, Pure.
  bind_equal y.
  unfold Map.
  replace (Δ <- y;; [s ⭅ ⌈ [id] ⌉;; ⌈ (fun (g : A -> A) (x : A) => Δ (g x)) <$> s ⌉]) with 
          (Δ <- y;; [ ⌈ [Δ] ⌉]  ).
  induction y. cbn. done.
  cbn. rewrite IHy.
  unfold Append.
  set_simpl.
  cbn.
  done.
  f_equal. extensionality Δ.
  set_simpl.
  f_equal.
Qed.

Lemma Comp_assoc {A B C D} (g : P (list (B -> C))) (h: P (list (A -> B))) (f : P (list (C -> D))) : 
  Comp f (Comp g h) = Comp (Comp f g) h.
Proof.
  unfold Comp, liftA2, Pure.
  set_simpl.
  bind_equal s1.
  induction s1.
  - cbn. repeat rewrite Concat_Nil. unfold Nil. set_simpl. cbn. done.
  - cbn.
    rewrite IHs1. clear IHs1.
    move: (@List.bind_bind) => h1.
    repeat rewrite <- Set_Bind_Append.
Abort.

(*
pure id <.> f = f
f <.> pure id = f
f <.> (g <.> h) = (f <.> g) <.> h
*)

(* 
pure id <*> v = v
Composition
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
Homomorphism
pure f <*> pure x = pure (f x)
Interchange
u <*> pure y = pure ($ y) <*> u
*)

(** ----------------- Monad laws (identity) ------------------  *)

Lemma bind_ret_l {A B} (x : A) (k : A -> P (list B))  : 
  Bind (Pure x) k = k x.
Proof.
  unfold Pure, Bind, Cons, Nil.
  set_simpl.
  cbn.
  rewrite Concat_Cons. 
  rewrite Concat_Nil.
  rewrite Append_Nil_r.
  done.
Qed.


Lemma bind_ret_r {A} (m : P (list A))  : 
  Bind m Pure = m.
Proof.
  unfold Bind, Pure.
  bind_equal s0.
  rewrite pick_pure.
  apply Concat_singleton.
Qed.


(** ---------- Monad/Functor law --------------- *)

(* bind_map: forall {A B C : Type} (g : B -> list C) (f : A -> B) (xs : list A), 
   x <- f <$> xs;; g x = x <- xs;; g (f x) *)

Lemma bind_map {A B C} (g : B -> P (list C)) (f : A -> B) (xs : P (list A)) : 
  Bind (Map f xs) g = Bind xs (fun x => g (f x)).
Proof.
  unfold Bind,Map.
  set_simpl.
  bind_equal y.
  rewrite List.map_map.
  done.
Qed.

Lemma bind_pure_map {A B} (f : A -> B) (xs : P (list A)) : 
  Bind xs (fun x => Pure (f x)) = Map f xs.
Proof.
  rewrite <- bind_map.
  rewrite bind_ret_r.
  done.
Qed.


Lemma Bind_Cons {A B} (f : A -> P (list B)) (x : P A) (l2 : P (list A)) : 
  Bind (Cons x l2) f = Append (Bind (Cons x Nil) f) (Bind l2 f).
Proof.
unfold Bind, Cons, Nil, Append.
set_simpl. 
bind_equal x0.
rewrite pick_cons. 
rewrite pick_nil.
rewrite Concat_Cons. 
rewrite Concat_Nil. 
rewrite Append_Nil_r. 

replace (x1 ⭅ l2;; s ⭅ ⌈ x0 :: x1 ⌉;; Concat (pick (f <$> s))) with 
        (x1 ⭅ l2;; Concat (pick (f x0 :: f <$> x1))).
2: { bind_equal x1. f_equal. }
replace (x1 ⭅ l2;; Concat (pick (f x0 :: f <$> x1))) with 
        (x1 ⭅ l2;; Concat (Cons (f x0) (pick (f <$> x1)))).
2: { reflexivity. } 
replace (x1 ⭅ l2;; Concat (Cons (f x0) (pick (f <$> x1)))) with 
        (x1 ⭅ l2;; Append (f x0) (Concat (pick (f <$> x1)))).
2: { bind_equal x1. rewrite Concat_Cons. done. } 
rewrite (Set_Bind_Append l2 (f x0) (fun x1 => Concat (pick (f <$> x1)))).
done.
Qed.

(*
Lemma foo {A} 
  (L1 : P (list A))
  (L2 : P (list (list A)) :
   s ⭅ Append L1 (Concat L2);; K s = Cons (s ⭅ L1;; Concat (K s)) (s ⭅ Concat L2;; K s).
  *)

Lemma Sets_bind_commute { A B C } (S1 : P A) (S2 : P B) (K : A -> B -> P C) :
(x ⭅ S1 ;; y ⭅ S2 ;; K x y = y ⭅ S2 ;; x ⭅ S1 ;; K x y).
Proof.
  set_ext c.
  split.
  - intro h. set_crunch. 
    unfold Ensembles.In, Sets.bind. 
    exists c1. split; auto. unfold Ensembles.In. exists c0. split; auto.
  - intro h. set_crunch.
    unfold Ensembles.In, Sets.bind. 
    exists c1. split; auto. unfold Ensembles.In. exists c0. split; auto.
Qed.

Lemma helper {A B C} 
  (f : A -> P (list B))
  (z : list A) 
  (K : list B -> P (list (list C))) 
  (P1 : K [] = Nil)
  (P2 : forall (x2 x3 : (list B)), 
      K (x2 ++ x3) = x ⭅ K x2;; y2 ⭅ K x3;; ⌈ concat x :: y2 ⌉) :

  s ⭅ Concat (pick (f <$> z));; K s = 
  pick ((fun x => s ⭅ f x;; Concat (K s)) <$> z).
Proof.
  induction z.
  - cbn. unfold Concat, Nil. set_simpl. rewrite concat_nil. apply P1.
  - cbn.
    rewrite Concat_Cons.
    rewrite <- IHz.
    remember (pick (f <$> z)) as L2.
    remember (f a) as L1.

    rewrite <- Concat_Cons.
    rewrite <- Set_Bind_Cons.
    unfold Concat , Cons.
    set_simpl. 

    replace (x ⭅ L1;; x0 ⭅ (y2 ⭅ L2;; ⌈ x :: y2 ⌉);; s ⭅ ⌈ concat x0 ⌉;; K s) with 
            (x ⭅ L1;; y2 ⭅ L2;; x0 ⭅ ⌈ x :: y2 ⌉;; K (concat x0)). 
    2: { bind_equal x. bind_equal y2. done. }

    rewrite Sets_bind_commute.
    bind_equal x1.
    bind_equal x2.
    replace (x ⭅ K x2;; y1 ⭅ ⌈ concat x ⌉;; y2 ⭅ K (concat x1);; ⌈ y1 :: y2 ⌉) with 
            (x ⭅ K x2;; y2 ⭅ K (concat x1);; ⌈ concat x :: y2 ⌉ ).
    2: { bind_equal x. done. }
    eapply P2.
Qed.

Lemma bind_bind: forall {A B C : Type} {ma : P (list A)} {f : A -> P (list B)} {g : B -> P (list C)}, 
   Bind (Bind ma f) g = Bind ma (fun x => Bind (f x) g).
Proof.
  intros.
  unfold Bind.
  set_simpl.
  bind_equal z.
  rewrite (Set_Bind_Concat _ (fun s => (pick (g <$> s)))).
  f_equal.
  eapply helper.
  - cbn. done.
  - intros x2 x3.
    rewrite map_app.
    rewrite pick_app.
    unfold Append.
    bind_equal y1.
    f_equal.
    (* here the goal is (append y1) = (cons (concat y1)) 
       which doesn't seem provable. I guess the assumption in the
       helper lemma is too strong.
     *)
Abort.

Module Examples.

Require Import densem.envSet.
Require Import syntax.common.

Import SetNotations.
Import SetMonadNotation.
Import List.ListNotations.
Import ListMonadNotation.
Import EnvNotation.
Import envSetNotation.

Import ConcreteVars.
Open Scope env_scope.
Open Scope list_scope.
Open Scope set_scope.

Definition S1 : P (list ENV) := 
  ⌈ [ {{ x ≈ 1 }} ;  {{ x ≈ 2 }} ] ⌉.

Definition S2 : P (list ENV) := 
  ⌈ [ {{ x ≈ 1 }} ] ⌉ ∪ ⌈ [ {{ x ≈ 2 }} ] ⌉.

Definition R : P (list ENV) := 
  ⌈ [ {{ x ≈ 1 }} ;  {{ x ≈ 2 }} ] ⌉.

Definition S : P (list ENV) := 
⌈ [{{x ≈ 1}}; ∅] ⌉ ∪ ⌈ [{{x ≈ 1}}; {{x ≈ 2}}] ⌉ ∪ ⌈ [∅; ∅] ⌉ ∪ ⌈ [∅; {{x ≈ 2}}] ⌉.

Definition UNIFY : P (list ENV) -> P (list ENV) -> P (list ENV) := 
  liftA2 Ensembles.Intersection.

Example example1 :
  UNIFY S1 S2 = S.
Proof.
  unfold UNIFY, liftA2, S1, S2.
  set_simpl.
  cbn.
  unfold Map.
  set_simpl.
  cbn.
  rewrite Append_Nil_r.
  unfold Append.
  set_simpl.
  cbn.
  repeat rewrite constrain_eq_intersection. cbn. done.
Qed.



Definition UNIFY' : P (list ENV) -> P (list ENV) -> P (list ENV) := 
  fun S1 S2 => 
    l1 ⭅ S1 ;;
    l2 ⭅ S2 ;;
    ⌈ Δ1 <- l1 ;; Δ2 <- l2 ;;  [Δ1 ∩ Δ2]  ⌉.

Example example2 :
  UNIFY' S1 S2 = ⌈ [{{x ≈ 1}}; ∅] ⌉ ∪ ⌈ [∅; {{x ≈ 2}}] ⌉.
Proof.
unfold UNIFY', S1, S2.
set_simpl.
cbn.
repeat rewrite constrain_eq_intersection. cbn. 
done.
Qed.

Definition UNIFY'' : P (list ENV) -> P (list ENV) -> P (list ENV) := 
  fun S1 S2 => Bind S1 (fun Δ1 => Bind S2 (fun Δ2 => Pure (Δ1 ∩ Δ2))).
Example example3 :
  UNIFY'' S1 S2 = S.
Proof.
unfold UNIFY'', S1, S2.
unfold Bind.
set_simpl.
cbn.
set_simpl.
cbn.
repeat rewrite Concat_Cons.
repeat rewrite Concat_Nil.
unfold Pure.
unfold Append.
unfold Nil.
set_simpl.
repeat rewrite List.app_nil_r.
cbn.
repeat rewrite constrain_eq_intersection. cbn. 
done.
Qed.

End Examples.
