Require Import Imports.
From Stdlib Require Import Lists.List.

Require Import Laws.

Import ListNotations.

(*
Lemma flat_map_app {A B} (f : A -> list B) (a1 a2: list A) : 
  flat_map f (a1 ++ a2) = flat_map f a1 ++ flat_map f a2.
*)

Lemma app_nil_inv {A} (a b : list A) :
  a ++ b = [] -> a = [] /\ b = [].
Proof. induction a. cbn. intros ->. done.
cbn. intros h. inversion h. Qed.

Lemma app_singleton_inv {A} (a b : list A) c : 
   a ++ b = [c] -> ((a = [c] /\ b = []) \/ (a = [] /\ b = [c])).
Proof. 
  move: b.
  induction a. move=> b eq. cbn in eq. inversion eq. 
  right. split; auto.
  move=> b eq. cbn in eq. inversion eq. subst.
  apply app_nil_inv in H1. destruct H1. subst.
  left. split. done. done.
Qed.

Lemma flat_map_map {A B} (f : A -> B) (ma : list A) :
   flat_map (fun x : A => [f x]) ma = map f ma.
induction ma. done.
cbn. f_equal. Qed.

(* 
Lemma in_flat_map {A B} (b : B) (k : A -> list B) (ma : list A) :
  In b (flat_map k ma) -> exists a : A, In a ma /\ In b (k a).
*)

(* Monad definitions *)

Definition ap_List {A B} 
  (fs : list (A -> B)) (xs : list A) : list B :=
  (* [f x | f <- fs, x <- xs] *)
  flat_map (fun f => 
              flat_map (fun x => 
                          (f x :: nil)) xs) fs.
  

#[export] Instance Functor_list : Functor list :=
{ fmap := map }.

#[export] Instance Monad_list : Monad list :=
{ ret  := fun _ x => x :: nil; 
  bind := fun _ _ x f =>  flat_map f x
}.

#[export] Instance Applicative_list : Applicative list :=
{ pure := fun _ x => x :: nil;
  ap   := @ap_List
}.

#[export] Instance Alternative_list : Alternative list :=
  { empty := @nil ;
    choose := @app
  }.

#[export] Instance Elem_list : Elem list := 
  { elem := fun {A} ls x => @List.In A x ls }.

#[export] Instance BindRetL_list : BindRetL (m:=list).
intros A B f a. cbn. rewrite app_nil_r. done. Qed.

#[export] Instance BindRetR_list : BindRetR (m:=list).
intros A ma. cbn. 
induction ma; cbn. done.
f_equal; auto. Qed.

#[export] Instance BindBind_list : BindBind (m:=list).
intros A B C ma f g.
cbn.
induction ma. cbn. done.
cbn. rewrite flat_map_app. f_equal. rewrite
  IHma. done.
Qed.

#[export] Instance RetInv_list : RetInv (m:=list).
intros A a1 a2 h. cbn in h. inversion h. done.
Qed.

#[export] Instance BindRetInv_list : BindRetInv (m:=list).
Abort.

Lemma fmap_inv_ret :
      forall (A B:Type) (ma :list A) (f f' :A -> B),
        (fmap f ma) = (fmap f' ma) ->
        forall a : A, List.In a ma -> ret (f a) = ret (f' a).
intros A B ma f f' h.
cbn. move: h.
induction ma.
cbn. done.
cbn. intro h. inversion h. clear h.
intros a0 [h1|h1].
+ subst. f_equal. done.
+ repeat rewrite flat_map_map in H1. 
  specialize (IHma H1). eauto.
Qed.

(* List Library definitions (bool). *)

(* 
forallb_forall: forall [A : Type] (f : A -> bool) (l : list A), forallb f l = true <-> (forall x : A, In x l -> f x = true)
*)

Fixpoint forallb2 {A B} (f : A -> B -> bool) (xs : list A) (ys : list B) : bool := 
  match xs , ys with 
  | nil , nil => true
  | x :: xs1 , y :: ys1 => f x y && forallb2 f xs1 ys1 
  | _ , _ => false
  end.

Lemma forallb2_forall : forall {A B} (f : A -> B -> bool) (xs : list A) (ys : list B), 
    forallb2 f xs ys = true <-> forall x y, In x xs -> In y ys -> f x y = true.
Proof.
  intros A B f.
  split.
  + move: ys. induction xs. 
    ++ intros. inversion H0.
    ++ intros ys. destruct ys as [|y ys].
       intros. inversion H1.
       simpl. intros h0 x y0 [-> |h1]; intros [->|h2]; simpl in h0.
Admitted.


(* List Library definitions (Prop). *)

Inductive ExistsT {A : Type} (P : A -> Type) : list A -> Type :=
  | ExistsT_cons1 : forall (x : A) (l : list A), 
      P x  -> ExistsT P (x :: l)
  | ExistsT_cons2 : forall (x : A) (l : list A), 
      ExistsT P l -> ExistsT P (x :: l).

Inductive Exists2 {A B : Type} (P : A -> B -> Prop) : 
  list A -> list B -> Prop :=
    Exists2_cons1 : forall x y l l',
      P x y -> Exists2 P (x :: l) (y :: l')
  | Exists2_cons2 : forall x y l l', 
      Exists2 P l l' -> Exists2 P (x :: l) (y :: l').

Inductive ForallT {A : Type} (P : A -> Type) : list A -> Type :=
    ForallT_nil : ForallT P nil
  | ForallT_cons : forall (x : A) (l : list A), P x -> ForallT P l -> ForallT P (x :: l).

#[export] Hint Constructors ExistsT Exists2 : core.
#[export] Hint Constructors ForallT : core.

(* Properties of Forall2 *)

Lemma Forall_Forall2 {A} {P : A -> A -> Prop} l : 
  Forall (fun x : A => P x x) l <->
  Forall2 P l l.
Proof.
  induction l; split; intro h; inversion h; subst; eauto.
  econstructor; eauto. rewrite <- IHl. auto.
  econstructor; eauto. rewrite -> IHl. auto.
Qed.

#[export] Instance Reflexive_Forall2 {A}{P: A -> A -> Prop}`{Reflexive _ P} : Reflexive (Forall2 P).
Proof.
  intros.
  intros l. induction l; eauto.
Qed.

#[export] Instance Symmetric_Forall2 {A}{P: A -> A -> Prop}`{Symmetric _ P} : Symmetric (Forall2 P).
Proof.
  intros x y F.
  induction F; eauto.
Qed.

#[export] Instance Transitive_Forall2 {A}{P: A -> A -> Prop}`{Transitive _ P} : Transitive (Forall2 P).
Proof.
  intros x y.
  generalize x. clear x. 
  induction y; intros x z F1 F2;
  inversion F1; inversion F2; subst; eauto.
Qed.


Lemma Forall2_length {A} {P : A -> A -> Prop} l1 l2 : 
  Forall2 P l1 l2 -> 
  length l1 = length l2.
Proof.
  generalize l2. clear l2.
  induction l1; intros l2 h;
  inversion h; subst; simpl in *; eauto.
Qed.  

Lemma Exists_Exists2 : forall {A} (P : A -> A -> Prop) (l:list A), 
    Exists (fun x => P x x) l <-> Exists2 P l l.
Proof.
  split.
  intro h. induction h; eauto.
  intro h. dependent induction h; eauto.
Qed.

Definition Forall2_any {A B:Type} : 
  forall (P : A -> B -> Prop), list A -> list B -> Prop :=
  fun P XS YS =>
      forall x y, List.In x XS -> List.In y YS -> P x y.

Definition Exists2_any {A B:Type} : 
  forall (P : A -> B -> Prop), list A -> list B -> Prop :=
  fun P XS YS =>
      exists x y, List.In x XS /\ List.In y YS /\ P x y.



(* Create the list [j , j + 1 , ... , j+(k-1) ]  *)
Fixpoint enumFrom j k := 
  match k with 
  | 0 => nil
  | S m => j :: enumFrom (S j) m 
  end.

(* Truncate a list to contain at most one element *)
Definition take1 {A} (xs : list A) : list A := 
  match xs with 
  | h :: _ => [ h ] 
  | [] => [] 
  end.
