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


(** ------- operations on lists of sets and sets of lists ------ *)

(* --------- pick/sequence ----------- *)
   
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

(* pick for sets: given a list of sets, make the set of 
   all lists where each element comes from each set. *)
Fixpoint pick {A} (xs : list (P A)) : P (list A) := 
  match xs with 
  | nil => ⌈ [] ⌉
  | V :: VS => v  ⭅ V ;; vs ⭅ pick VS ;; ⌈ v :: vs ⌉ 
  end.

(* pick for lists: 
   NB: this is the list instance of 'sequence' from Haskell's 
   Traversable class *)
Fixpoint pickl {A} (xs : list (list A)) : list (list A) := 
  match xs with 
  | nil => [ [] ]
  | V :: VS =>  v <- V ;; vs <- pickl VS ;; [ v :: vs ] 
  end.


Definition pure_sl {A} : A -> P (list A) := fun x =>  ⌈ [ x ] ⌉ .

Definition map_sl { A B} : (A -> B) -> P (list A) -> P (list B) := 
  fun op E => s ⭅ E ;; ⌈ List.map op s ⌉.

Definition liftA2 {A B C} : (A -> B -> C) -> P (list A) -> P (list B) -> P (list C) := 
  fun op E1 E2 => 
    s1 ⭅ E1 ;;
    let t : list (P (list C)) := 
      Δ <- s1 ;;
      [ map_sl (op Δ) E2 ] in
    let r : P (list (list C)) := pick t in 
    ss ⭅ r ;;
    ⌈ List.concat ss ⌉.

Definition bind_sl {A B} : P (list A) -> (A -> P (list B)) -> P (list B) := 
  fun S K => 
    s ⭅ S ;;
    ss ⭅ pick (List.map K s) ;;
    ⌈ List.concat ss ⌉.

Lemma bind_ret_l {A B} (x : A) (k : A -> P (list B))  : 
  bind_sl (pure_sl x) k = k x.
Proof.
  unfold pure_sl, bind_sl.
  set_simpl.
  cbn.
  replace (v0 ⭅ k x;; vs ⭅ ⌈ [] ⌉;; ⌈ v0 :: vs ⌉) with (v0 ⭅ k x;; ⌈ v0 :: [] ⌉).
  set_simpl.
  cbn.
  replace (fun x0 => x0 ++ []) with (fun (x0 : list B) => x0).
  set_simpl.
  done.
  extensionality y. rewrite List.app_nil_r. done.
  f_equal.
  extensionality v0.
  set_simpl.
  done.
Qed.

Lemma pick_pure {A} (s : list A) : 
  pick (pure_sl <$> s) =  ⌈ List.map (fun x => [x]) s ⌉.
Proof.
  induction s.
  - cbn. done.
  - cbn. set_simpl. rewrite IHs.
    unfold pure_sl.
    set_simpl.
    done.
Qed.

Lemma bind_ret_r {A} (m : P (list A))  : 
  bind_sl m pure_sl = m.
Proof.
  unfold bind_sl.
  replace (s ⭅ m;; ss ⭅ pick (pure_sl <$> s) ;;  ⌈ concat ss ⌉) with 
          (s ⭅ m;; ss ⭅ ⌈ List.map (fun x => [x]) s ⌉ ;;  ⌈ concat ss ⌉).
  replace (s ⭅ m;; ss ⭅ ⌈ List.map (fun x => [x]) s ⌉ ;;  ⌈ concat ss ⌉) with
          (s ⭅ m;; ⌈ concat (List.map (fun x => [x]) s) ⌉).
  set_simpl.
  replace (fun x0 : list A => concat ((fun x1 : A => [x1]) <$> x0)) with (fun (x0 : list A) => x0).
  set_simpl.
  done.
  - extensionality x0. rewrite <- flat_map_concat_map. list_simpl. done.
  - f_equal. extensionality s. set_simpl. done.
  - f_equal. extensionality s. set_simpl.
    rewrite pick_pure.
    set_simpl.
    done.
Qed.


Lemma Concat_Map {A B} (f : A -> B) (l : P (list (list A))) :
  map_sl f (Concat l) = Concat (map_sl (map f) l).
Proof.
  unfold map_sl, Concat.
  set_simpl.
  f_equal. extensionality x0.
  set_simpl.
  rewrite concat_map. 
  done.
Qed.


Lemma Append_Nil_l {A} (XS : P (list A)) : Append Nil XS = XS.
unfold Nil, Append. set_simpl. done. Qed.
Lemma Append_Nil_r {A} (XS : P (list A)) : Append XS Nil = XS.
unfold Nil, Append. 
rewrite <- (@Sets.bind_singleton_r _ XS) at 2.
f_equal. extensionality y1.
set_simpl.
eapply List.app_nil_r.
Qed.
Lemma Append_Append {A} (XS YS ZS : P (list A)) : 
  Append (Append XS YS) ZS = Append XS (Append YS ZS).
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

Lemma Concat_Cons {A} (x : P (list A)) (xs : P (list (list A))):
  Concat (Cons x xs) = Append x (Concat xs).
Proof.
  unfold Concat, Cons, Append.
  set_simpl.
  f_equal. extensionality x0.
  set_simpl.
  rewrite <- Sets.bind_singleton_map.
  f_equal. extensionality x1.
  set_simpl.
  rewrite concat_cons.
  done.
Qed.

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
  - cbn. set_simpl. done.
  - cbn. set_simpl.
    rewrite IHx1.
    f_equal. extensionality z1.
    set_simpl.
    f_equal. extensionality z2.
    set_simpl.
    rewrite <- bind_singleton_map.
    f_equal. extensionality z3.
    set_simpl.
    cbn.
    done.
Qed.

Lemma pick_def {A} (xs : list (P A)) :
  pick xs = List.fold_right Cons Nil xs.
induction xs; try done. rewrite pick_cons. cbn. f_equal. done. Qed.

Lemma fold_right_map {A B C} (f : A -> C) g (b : B) (z : list A) :
  List.fold_right g b (List.map f z) = List.fold_right (fun x => g (f x)) b z.
Proof. induction z. cbn. done. cbn. rewrite IHz. f_equal. Qed.


Lemma bind_cons {A B} (f : A -> list B) (x : A) xs : 
  List.flat_map f (x :: xs) = (List.flat_map f [x]) ++ (List.flat_map f xs).
Proof.
  rewrite <- flat_map_app. cbn. done.
Qed.

Lemma Bind_Cons {A B} (f : A -> P (list B)) (x : P A) (l2 : P (list A)) : 
  bind_sl (Cons x l2) f = Append (bind_sl (Cons x Nil) f) (bind_sl l2 f).
Proof.
unfold bind_sl, Cons, Nil.
set_simpl. 
unfold Append. set_simpl.
f_equal. extensionality x0.
set_simpl.
replace (x1 ⭅ l2;; s ⭅ ⌈ x0 :: x1 ⌉;; ss ⭅ pick (f <$> s);; ⌈ concat ss ⌉) with 
        (x1 ⭅ l2;; ss ⭅ pick (f x0 :: f <$> x1);; ⌈ concat ss ⌉).
2: { f_equal. extensionality x1. set_simpl. f_equal. }
replace (x1 ⭅ l2;; ss ⭅ pick (f x0 :: f <$> x1);; ⌈ concat ss ⌉) with 
        (x1 ⭅ l2;; ss ⭅ Cons (f x0) (pick (f <$> x1));; ⌈ concat ss ⌉).
2: { reflexivity. } 
replace (x1 ⭅ l2;; ss ⭅ Cons (f x0) (pick (f <$> x1));; ⌈ concat ss ⌉) with 
  (x1 ⭅ l2;; Concat (Cons (f x0) (pick (f <$> x1)))).
2: { reflexivity. } 
replace (x1 ⭅ l2;; Concat (Cons (f x0) (pick (f <$> x1)))) with 
        (x1 ⭅ l2;; Append (f x0) (Concat (pick (f <$> x1)))).
2: { f_equal. extensionality x1. rewrite Concat_Cons. done. } 
rewrite pick_cons. rewrite pick_nil.
replace (x1 ⭅ Cons (f x0) Nil;; y1 ⭅ ⌈ concat x1 ⌉;; y2 ⭅ (s ⭅ l2;; ss ⭅ pick (f <$> s);; ⌈ concat ss ⌉);; ⌈ y1 ++ y2 ⌉) with 
(Append (Concat (Cons (f x0) Nil)) (s ⭅ l2 ;; Concat (pick (f <$> s)))).
2: { unfold Append. unfold Concat. set_simpl. done.  } 
rewrite Concat_Cons. rewrite Append_Append. 
replace (Concat Nil) with (Nil : P (list B)). 2: { unfold Concat, Nil. set_simpl. cbn. done. } 
replace (Append Nil (s ⭅ l2;; Concat (pick (f <$> s)))) with (s ⭅ l2;; Concat (pick (f <$> s))).
2: { unfold Append, Nil. set_simpl. done. } 
rewrite (Set_Bind_Append l2 (f x0) (fun x1 => Concat (pick (f <$> x1)))).
done.
Qed.



Lemma bind_bind: forall {A B C : Type} {ma : P (list A)} {f : A -> P (list B)} {g : B -> P (list C)}, 
   bind_sl (bind_sl ma f) g = bind_sl ma (fun x => bind_sl (f x) g).
Proof.
  intros.
  unfold bind_sl.
  set_simpl.
  f_equal.
  extensionality z.
  set_simpl.
  replace (x0 ⭅ pick (f <$> z);; s ⭅ ⌈ concat x0 ⌉;; ss ⭅ pick (g <$> s);; ⌈ concat ss ⌉ ) with
          (x0 ⭅ pick (f <$> z);; ss ⭅ pick (g <$> concat x0);; ⌈ concat ss ⌉).
  2: { f_equal. extensionality x0. set_simpl. done. } 
  rewrite <- Sets.bind_singleton_map.
  rewrite <- Sets.bind_bind.
  f_equal.
(*
   x0 ⭅ pick (f <$> z);; pick (g <$> concat x0) = 
   pick ((fun x0 => s ⭅ f x0;; ss ⭅ pick (g <$> s);; ⌈ concat ss ⌉) <$> z)
*)
  replace (fun x0 => s ⭅ f x0;; ss ⭅ pick (g <$> s);; ⌈ concat ss ⌉) with
          (fun (x0 : A) => s ⭅ f x0;; Sets.map (@concat C) (pick (g <$> s))).
  2: { f_equal. extensionality x0. f_equal. extensionality s. 
       set_simpl. done. } 
Admitted.
