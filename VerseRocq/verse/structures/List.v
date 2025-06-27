Require Import Imports.
From Stdlib Require Import Lists.List.

Import ListNotations.

(* Monad definitions *)

Definition ap_List {A B} (fs : list (A -> B)) (xs : list A) : list B :=
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
