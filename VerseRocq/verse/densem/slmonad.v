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

Definition ConcatPick {A} : list (P (list A)) -> P (list A) := 
 List.fold_right Append Nil.

(** ------------- Functor/Applicative/Monad definitions ---------------- *)


Definition Pure {A} : A -> P (list A) := fun x =>  ⌈ [ x ] ⌉ .

Definition Map { A B} : (A -> B) -> P (list A) -> P (list B) := 
  fun op E => s ⭅ E ;; ⌈ List.map op s ⌉.

Definition Bind {A B} : P (list A) -> (A -> P (list B)) -> P (list B) := 
  fun S K => 
    s ⭅ S ;;
    ConcatPick (List.map K s).

Definition liftA2 {A B C} : (A -> B -> C) -> P (list A) -> P (list B) -> P (list C) := 
  fun op E1 E2 => 
    s1 ⭅ E1 ;;
    let t : list (P (list C)) := 
      Δ <- s1 ;;
      [ Map (op Δ) E2 ] in
    ConcatPick t.

(* NOT correct *)
Definition liftA2' {A B C} : (A -> B -> C) -> P (list A) -> P (list B) -> P (list C) := 
  fun op E1 E2 => 
    s1 ⭅ E1 ;;
    s2 ⭅ E2 ;;
    ⌈ List.ap (List.map op s1) s2 ⌉.

(* ---------------------------------------------------------- *)

Module Notation.
Infix "++" := Append.
Notation "∅" := Nil.
Notation "⌊ x ⌋" := (Pure x).
Infix "★" := (liftA2 Ensembles.Intersection) (at level 70).
Infix ">>=" := Bind (at level 70).
End Notation.

Import Notation.

(* --------------- Append/Concat Properties ------------------- *)

Lemma Append_Nil_l {A} (XS : P (list A)) : ∅ ++ XS = XS.
Proof. unfold Nil, Append. set_simpl. done. Qed.
Lemma Append_Nil_r {A} (XS : P (list A)) : XS ++ ∅ = XS.
Proof.
unfold Nil, Append. 
bind_equal z.
eapply List.app_nil_r.
Qed.
Lemma Append_Append {A} (XS YS ZS : P (list A)) : 
  (XS ++ YS) ++ ZS = XS ++ (YS ++ ZS).
Proof.
  unfold Append.
  set_simpl.
  bind_equal x0.
  bind_equal x1.
  bind_equal x2.
  rewrite app_assoc.
  done.
Qed.

Lemma Concat_Nil {A} : Concat (∅ : P (list (list A))) = ∅.
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
 Concat (pick xs) = ConcatPick xs.
Proof.
  induction xs. simpl. rewrite Concat_Nil. done.
  cbn. rewrite Concat_Cons.
  f_equal. 
  unfold ConcatPick in IHxs. done.
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

Lemma Map_Union {A B} (f : A -> B) (S T : P (list A)):   
  Map f (S ∪ T) = (Map f S) ∪ (Map f T).
Proof. 
  unfold Map. set_simpl. done.
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

(** ---------Applicative laws ------- *)

  
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
  rewrite Append_Nil_r.
  done.
Qed.


Lemma bind_ret_r {A} (m : P (list A))  : 
  Bind m Pure = m.
Proof.
  unfold Bind, Pure.
  bind_equal s0.
  unfold ConcatPick.
  induction s0.
  - cbn. done.
  - cbn. rewrite IHs0. 
    unfold Append.
    set_simpl.
    cbn.
    done.
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
repeat rewrite <- Concat_pick.
rewrite pick_cons. 
rewrite pick_nil.
rewrite Concat_Cons. 
rewrite Concat_Nil. 
rewrite Append_Nil_r. 

replace (x1 ⭅ l2;; s ⭅ ⌈ x0 :: x1 ⌉;; ConcatPick (f <$> s)) with 
        (x1 ⭅ l2;; ConcatPick (f x0 :: f <$> x1)).
2: { bind_equal x1. f_equal. }
replace (x1 ⭅ l2;; ConcatPick (f x0 :: f <$> x1)) with 
        (x1 ⭅ l2;; Concat (Cons (f x0) (pick (f <$> x1)))).
2: { bind_equal x1. rewrite <- Concat_pick. reflexivity. } 
replace (x1 ⭅ l2;; Concat (Cons (f x0) (pick (f <$> x1)))) with 
        (x1 ⭅ l2;; Append (f x0) (ConcatPick (f <$> x1))).
2: { bind_equal x1. rewrite Concat_Cons. rewrite <- Concat_pick. done. } 
rewrite (Set_Bind_Append l2 (f x0) (fun x1 => ConcatPick (f <$> x1))).
done.
Qed.



Lemma Bind_Append {A B} (f : A -> P (list B)) (l1 l2 : P (list A)) : 
  Bind (Append l1 l2) f = Append (Bind l1 f) (Bind l2 f).
Proof.
  unfold Bind, Append.
  set_simpl.
  bind_equal x0.
  replace (x ⭅ l2;; s ⭅ ⌈ (x0 ++ x)%list ⌉;; ConcatPick (f <$> s) ) with 
          (x ⭅ l2;; ConcatPick (f <$> x0 ++ f <$> x)). 
  2: { bind_equal x1. rewrite map_app. done. }
  replace (y1 ⭅ ConcatPick (f <$> x0);; y2 ⭅ (s ⭅ l2;; ConcatPick (f <$> s));; ⌈ (y1 ++ y2)%list ⌉) with 
          (y1 ⭅ ConcatPick (f <$> x0);; s ⭅ l2;; y2 ⭅ (ConcatPick (f <$> s));; ⌈ (y1 ++ y2)%list ⌉).
  2: { bind_equal y1. done. } 
  replace (y1 ⭅ ConcatPick (f <$> x0);; s ⭅ l2;; y2 ⭅ (ConcatPick (f <$> s));; ⌈ (y1 ++ y2)%list ⌉) with 
          (s ⭅ l2;; y1 ⭅ ConcatPick (f <$> x0);;  y2 ⭅ (ConcatPick (f <$> s));; ⌈ (y1 ++ y2)%list ⌉).
  (* commute set bind *)
  2: { set_ext x1. repeat rewrite in_bind. split.
       intro h. set_crunch. exists x2. split; auto.
       rewrite in_bind. exists x. split; auto.
       rewrite in_bind. exists x3. split; auto. eapply in_singleton.
       intro h. set_crunch. exists x2. split; auto.
       rewrite in_bind. exists x. split; auto.
       rewrite in_bind. exists x3. split; auto. eapply in_singleton.
  } 
  bind_equal x1.
  repeat rewrite <- Concat_pick.
  rewrite pick_app.
  rewrite Concat_Append.
  done.
Qed.

Lemma liftA2_Bind {A B C} (f : A -> B -> C) (S : P (list A)) (T : P (list B)) :
    (liftA2 f S T : P (list C)) = 
      Bind S (fun (s : A) => (Bind T (fun t => Pure (f s t)))).
Proof.    
  unfold liftA2.
  unfold Bind.
  bind_equal a.
Admitted.


(* -------------------------------------------------------------------- *)

Section Axioms.

Variable (A : Type).

(* Already exists above *)
(*
Lemma Append_Nil_l (S : P (list A)) : ∅ ++ S = S.
unfold Nil, Append. set_simpl. done. Qed.
Lemma Append_Nil_r (S : P (list A)) : S ++ ∅ = S.
unfold Nil, Append. 
rewrite <- (@Sets.bind_singleton_r _ S) at 2.
f_equal. extensionality y1.
set_simpl.
eapply List.app_nil_r.
Qed.

Lemma Append_Append (S T R : P (list A)) : 
  (S ++ T) ++ R = S ++ (T ++ R).
Proof.
  unfold Append.
  set_simpl.
  f_equal. extensionality x0.
  set_simpl.
  f_equal. extensionality x1.
  set_simpl.
  rewrite <- Sets.bind_singleton_map.
  f_equal. extensionality x2.
  set_simpl.
  rewrite app_assoc.
  done.
Qed.
*)
Lemma Nil_Union_l (S : P (list A)) : ∅ ∪ S = S.
Proof. set_simpl. done. Qed.
Lemma Nil_Union_r (S : P (list A)) : S ∪ ∅ = S.
Proof. set_simpl. done. Qed.

Lemma Union_commutes (S T : P (list A)) : S ∪ T = T ∪ S.
Proof.
  set_ext x. split.
  intros h. inv h. right; auto. left; auto.
  intros h. inv h. right; auto. left; auto.
Qed.

Lemma Union_assoc (S T R : P (list A)) : (S ∪ T) ∪ R =  S ∪ (T ∪ R). Proof. eapply union_assoc. Qed.

Lemma distrib_l (R S T : P (list A)) : (S ∪ T) ++ R = (S ++ R) ∪ (T ++ R).
Proof.
  unfold Append.
  rewrite <- Sets.bind_union.
  done.
Qed.

Lemma distrib_r (R S T : P (list A)) : S ++ (T ∪ R) = (S ++ T) ∪ (S ++ R).
Proof.
  unfold Append.
  set_ext x.
  repeat rewrite in_bind.
  split.
  - intros h. set_crunch. inv H0. left. 
    rewrite in_bind. exists x0. split. auto. 
    rewrite in_bind. exists x1. split. auto.
    eapply in_singleton.
    right; rewrite in_bind. exists x0. split; auto.
    rewrite in_bind. exists x1. split; eauto. eapply in_singleton.
  - intros h. inv h. set_crunch.
    exists x0. split. auto. 
    rewrite in_bind. exists x1. split. left. auto.
    eapply in_singleton.
    set_crunch.
    exists x0. split; auto.
    rewrite in_bind. exists x1. split. right. eauto. eapply in_singleton.   
Qed.

End Axioms.

Section Canonicalisation.

Context {A : Type}.

Definition Canonical (S : P (list A)) : Prop := 
  forall xs, xs ∈ S -> xs <> nil.

Definition Canonical_eq (S T : P (list A)) := 
  forall xs, xs <> nil -> xs ∈ S <-> xs ∈ T.

Infix "≃" := Canonical_eq.

Lemma ce_reflexivity (S : P (list A)) : S ≃ S.
unfold Reflexive, Canonical_eq. intros xs h. done. Qed.
Lemma ce_symmetry (S T : P (list A)) : S ≃ T -> T ≃ S.
Admitted.
Lemma ce_transitivity (S T R : P (list A)) : S ≃ T -> T ≃ R -> S ≃ R.
Admitted.

Instance Eq_Can_eq : Equivalence Canonical_eq.
split.
- exact ce_reflexivity.
- exact ce_symmetry.
- exact ce_transitivity.
Qed.

End Canonicalisation.

Add Parametric Relation A : (P (list A)) Canonical_eq
reflexivity proved by ce_reflexivity 
symmetry proved by ce_symmetry
transitivity proved by ce_transitivity as ce.

Add Parametric Morphism A : (@Union (list A))
  with signature (@Canonical_eq A) ==> (@Canonical_eq A) ==> (@Canonical_eq A) as union_mor.
Proof.
  intros x1 y1 EQ1 x2 y2 EQ2.
  unfold Canonical_eq in *.
  intros xs h.
  specialize (EQ1 xs h).
  specialize (EQ2 xs h).
  repeat rewrite in_union.
  rewrite EQ1.
  rewrite EQ2.
  done.
Qed.

Add Parametric Morphism A : (@Append A)
  with signature (@Canonical_eq A) ==> (@Canonical_eq A) ==> (@Canonical_eq A) as append_mor.
Proof.
  intros x1 y1 EQ1 x2 y2 EQ2.
  unfold Canonical_eq in *.
  intros xs h.
  unfold Append.
  split; intro h1; set_crunch.
  - destruct xs0. destruct xs1. simpl in h. done.
    simpl in h. simpl.
    specialize (EQ2 (a :: xs1) ltac:(eauto)).
    rewrite in_bind. exists nil.
Abort.
(* If there is no nil in a set, ++ needs to pretend that there is *)

Create HintDb canon.
Hint Rewrite Nil_Union_l distrib_l Union_assoc : canon.




Section Sequencing.

Variable A:Type.

Variable S T R : P (list (P A)).
Variable Δ Δ1 Δ2 : P A.

Lemma Seq_Nil_l : ∅ ★ S = ∅.
Proof.
  unfold liftA2, Nil. set_simpl.
  list_simpl.
  done.
Qed.

(* If S is Canonical, then this holds *)
Lemma Seq_Nil_r : Canonical S -> S ★ Nil = Nil.
  intro h.
  unfold liftA2, Nil.
  unfold Map.
  transitivity (s1 ⭅ S;; @ConcatPick (P A) (Δ0 <- s1;; [ Nil ])).
   { bind_equal s. f_equal. 
       f_equal. extensionality PA.
       set_simpl. list_simpl. done. } 
  transitivity (s1 ⭅ S;; @ConcatPick (P A) [ Nil ]).
  { bind_equal s. f_equal. rewrite flat_map_concat_map.
    Search List.map.

Admitted. (* looks true. really annoying to prove. *)

Lemma Seq_Union_distrib_l : (S ∪ T) ★ R = (S ★ R) ∪ (T ★ R).
Proof. unfold liftA2.
rewrite <- Sets.bind_union. done.
Qed.

Lemma Seq_Append_distrib_l : (S ++ T) ★ R = (S ★ R) ++ (T ★ R).
Proof. unfold liftA2.
       set_ext x.
       repeat rewrite in_bind.
       split.
       - intro h. set_crunch.
         unfold Append in H.
         set_crunch.

         rewrite <- Concat_pick in H0.
Admitted. (* This seems difficult to prove. *)

Lemma Seq_Singleton_Singleton : ⌊ Δ1 ⌋ ★ ⌊ Δ2 ⌋ = ⌊ Δ1 ∩ Δ2 ⌋.
Proof.
  unfold liftA2, Pure, Nil, Map. set_simpl. list_simpl.
  set_simpl. list_simpl. 
  rewrite <- Concat_pick. rewrite pick_cons. rewrite pick_nil.
  rewrite Concat_Cons. rewrite Concat_Nil. rewrite Append_Nil_r.
  done.
Qed.

Lemma Seq_Singleton_Union : ⌊ Δ ⌋ ★ (S ∪ T) = (⌊ Δ ⌋ ★ S) ∪ (⌊ Δ ⌋ ★ T).
  unfold liftA2, Pure. set_simpl. list_simpl.
  rewrite Map_Union.
  repeat rewrite <- Concat_pick.
  repeat rewrite pick_cons. 
  repeat rewrite pick_nil. 
  repeat rewrite Concat_Cons.
  repeat rewrite Concat_Nil.
  repeat rewrite Append_Nil_r.
  done.
Qed.



Lemma Seq_Singleton_Append : ⌊ Δ ⌋ ★ (S ++ T) = (⌊ Δ ⌋ ★ S) ++ (⌊ Δ ⌋ ★ T).
Proof.
  unfold liftA2, Pure. set_simpl. list_simpl.
  rewrite Map_Append.
  repeat rewrite <- Concat_pick.
  repeat rewrite pick_cons. 
  repeat rewrite pick_nil. 
  repeat rewrite Concat_Cons.
  repeat rewrite Concat_Nil.
  repeat rewrite Append_Nil_r.
  done.
Qed.
  

End Sequencing.

Section Monad.

Variable A B : Type.
Variable S T : P (list A).
Variable h : A -> P (list B).

Lemma Bind_Nil_l : ∅ >>= h = ∅.
Proof.
  unfold Bind,Nil. set_simpl. list_simpl.
  rewrite <- Concat_pick.
  rewrite pick_nil.
  rewrite Concat_Nil.
  done.
Qed.

Lemma Bind_Singleton (a : A) : ⌊ a ⌋ >>= h = h a.
Proof. 
  rewrite bind_ret_l. done.
Qed.

Lemma Bind_Append : (S ++ T) >>= h = (S >>= h) ++ (T >>= h).
Proof.
  unfold Append, Bind.
  set_simpl.
  bind_equal x.
(* This looks really hard now. *)
Admitted.

End Monad.


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
  S1 ★ S2 = S.
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
