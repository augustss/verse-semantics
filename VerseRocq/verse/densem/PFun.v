From Stdlib Require Import List.
From Stdlib Require Import Classes.EquivDec Classes.DecidableClass.
From Stdlib Require Import Sorting.Sorted.

Require Import structures.Imports.
Require Import structures.List.

Import ListNotations.

Definition PFun A B := list (A * B)%type.

Definition dom {A B} : PFun A B -> list A := map fst.

Section PFun.

Parameter A B : Type.
Parameter A_eqb : A -> A -> bool.
Definition A_eq : A -> A -> Prop := 
  fun x y => A_eqb x y = true.

Context `{EQA : Equivalence A A_eq}.

Fixpoint apply_opt (f : PFun A B) (x : A) : option B := 
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

Parameter B_eqb : B -> B -> bool.

Definition eqb : PFun A B -> PFun A B -> bool := 
  List.forallb2 (fun '(x1,y1) '(x2, y2) => (A_eqb x1 x2 && B_eqb y1 y2)%bool).

End Eqb.

Section Compare.

Parameter A_compare : A -> A -> comparison.
Parameter B_compare : B -> B -> comparison.

Definition compare   
  (xs: PFun A B) (ys: PFun A B) : comparison := 
  list_compare (fun '(x1,y1) '(x2, y2) => 
                  match A_compare x1 x2 with 
                  | Lt => Lt
                  | Eq => B_compare y1 y2
                  | Gt => Gt
                  end) xs ys.

End Compare.

Section Valid.

Definition A_lt : A -> A -> Prop := 
  fun x y => A_compare x y = Lt.

Context `{SOA : StrictOrder A A_lt}.

Definition valid : PFun A B -> Prop := fun f => LocallySorted A_lt (dom f).

End Valid.

End PFun.     


