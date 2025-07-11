From Stdlib Require Import List.
From Stdlib Require Import Classes.EquivDec Classes.DecidableClass.
From Stdlib Require Import Sorting.Sorted.

Require Import structures.Imports.
Require Import structures.List.

Import ListNotations.

Definition pfun A B := list (A * B)%type.

Module PFun.

Definition dom {A B} : pfun A B -> list A := map fst.

Section PFun.

Variable A B : Type.
Variable A_eqb : A -> A -> bool.
Definition A_eq : A -> A -> Prop := 
  fun x y => A_eqb x y = true.

Context `{EQA : Equivalence A A_eq}.

Fixpoint apply_opt (f : pfun A B) (x : A) : option B := 
  match f with 
    | [] => None
    | (y,b) :: xs => 
        if A_eqb x y then Some b else apply_opt xs x
  end.

Lemma total_on_dom : forall {f x}, In x (dom f) -> 
                              { b & apply_opt f x = Some b }.
Proof. induction f; intros.
       inversion H.
       destruct a as (y,b).
       destruct (A_eqb x y) eqn:EQ.
       + exists b. simpl. rewrite EQ. reflexivity.
       + destruct (IHf x) as [b' h]. simpl in H.
         destruct H. subst. 
         simpl in EQ.
         move: (@Equivalence_Reflexive _ _ EQA) => h.
         unfold Reflexive, A_eq in h.
         rewrite h in EQ. discriminate.
         exact H.
         exists b'. simpl. rewrite EQ. exact h.
Qed.

Definition apply {f x} (P: In x (dom f)): B := projT1 (total_on_dom P).

Section Eqb.

Variable B_eqb : B -> B -> bool.

Definition eqb : pfun A B -> pfun A B -> bool := 
  List.forallb2 (fun '(x1,y1) '(x2, y2) => (A_eqb x1 x2 && B_eqb y1 y2)%bool).

End Eqb.

Section Compare.

Variable A_compare : A -> A -> comparison.
Variable B_compare : B -> B -> comparison.

Definition compare   
  (xs: pfun A B) (ys: pfun A B) : comparison := 
  list_compare (fun '(x1,y1) '(x2, y2) => 
                  match A_compare x1 x2 with 
                  | Lt => Lt
                  | Eq => B_compare y1 y2
                  | Gt => Gt
                  end) xs ys.

Definition A_lt : A -> A -> Prop := 
  fun x y => A_compare x y = Lt.
Definition A_ltb x y := 
  match A_compare x y with | Lt => true | _ => false end.


(* intersection of two partial maps *)
Fixpoint intersect (l1 l2 : pfun A B) := 
  let fix aux l2 :=
  match l1, l2 with
  | [], _ => []
  | _, [] => []
  | (a1,b1)::l1', (a2,b2)::l2' =>
      match A_compare a1 a2 with 
      | Lt =>  intersect l1' l2
      | Eq =>  (a2,b2) :: intersect l1' l2'
      | Gt =>  aux l2'
      end
  end
  in aux l2.

(* right-biased union of two partial maps. *)
Fixpoint merge (l1 l2 : pfun A B) :=
  let fix merge_aux l2 :=
  match l1, l2 with
  | [], _ => l2
  | _, [] => l1
  | (a1,b1)::l1', (a2,b2)::l2' =>
      match A_compare a1 a2 with 
      | Lt =>  (a1,b1) :: merge l1' l2
      | Eq =>  (a2,b2) :: merge l1' l2'
      | Gt =>  (a2,b2) :: merge_aux l2'
      end
  end
  in merge_aux l2.


Section Valid.

Context `{SOA : StrictOrder A A_lt}.

Definition valid : pfun A B -> Prop := fun f => LocallySorted A_lt (dom f).


Lemma intersect_valid {f g} : valid f -> valid g -> valid (intersect f g).
Proof. 
Admitted.
  
Lemma merge_valid : forall f g, valid f -> valid g -> valid (merge f g).
Proof. induction f.
       - intros. unfold merge. destruct g. auto. auto.
       - intros. destruct g. destruct a. simpl. auto.
         destruct a. destruct p.
         simpl.
         destruct A_compare eqn:COMP.
         -- unfold valid in *.
Admitted. 

End Valid.
End Compare.
End PFun.     
End PFun.


Declare Scope pfun_scope.
Delimit Scope pfun_scope with pfun.

Module PFunNotation.
Notation "∅" := nil : pfun_scope.
Infix "∩" := PFun.intersect (at level 90) : pfun_scope.
Infix "∪" := PFun.merge (at level 90) : pfun_scope.
End PFunNotation.
