(* Partial functions, represented as tables. 
   (Cannot represent as functions as that is not inductive.)
 *)

Require Import structures.Imports.
Require densem.PFun.
Require Import structures.List.

From Stdlib Require Import List.
From Stdlib Require Import Sorting.Sorted.
 
Import ListNotations.

Definition PFun A B := list (A * B)%type.

Definition dom {A B} : PFun A B -> list A := List.map fst.

Module PFun.
Section PFun.

Variable A B : Type.
Variable A_eqb : A -> A -> bool.
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

Variable B_eqb : B -> B -> bool.

Definition eqb : PFun A B -> PFun A B -> bool := 
  List.forallb2 (fun '(x1,y1) '(x2, y2) => (A_eqb x1 x2 && B_eqb y1 y2)%bool).

End Eqb.

Section Compare.

Variable A_compare : A -> A -> comparison.
Variable B_compare : B -> B -> comparison.

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

Variable A_compare : A -> A -> comparison.
Definition A_lt : A -> A -> Prop := 
  fun x y => A_compare x y = Lt.
Definition A_ltb x y := 
  match A_compare x y with | Lt => true | _ => false end.

Context `{SOA : StrictOrder A A_lt}.

Definition valid : PFun A B -> Prop := fun f => LocallySorted A_lt (dom f).

(* right-biased union of two partial maps. *)
Fixpoint merge (l1 l2 : PFun A B) :=
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

End PFun. (* section *)
End PFun.


(* ------------------------------------------------------ *)

Inductive Value : Type := 
  | Int : nat -> Value
  | Fun : list (PFun.PFun Value Value) -> Value. 
   
(* Function values can have ordered and unordered domains. *)
(* If the domain is ordered, you can iterate over it. 
   For example, a tuple (a,b,c) is represented as:
      Fun [ (0 |-> a) ; (1 |-> b) ; (2 |-> c) ]
 *)
 


Module Value.

Fixpoint eqb (v1 : Value) (v2 : Value) : bool := 
  match v1 , v2 with 
  | Int i , Int j => Nat.eqb i j
  | Fun fs1 , Fun fs2 => 
      forallb2 (PFun.eqb _ _ eqb eqb) fs1 fs2
  | _ , _ => false
  end.
                        
Fixpoint compare (v1 : Value) (v2 : Value) : comparison := 
  match v1 , v2 with 
  | Int i , Int j => Nat.compare i j
  | Int _ , Fun _ => Lt
  | Fun _ , Int _ => Gt
  | Fun fs1 , Fun fs2 => 
      list_compare (PFun.compare _ _ compare compare) fs1 fs2 
  end.

(* n-ary tuples are enumerated partial functions i -> v *)
Definition mkTup (vs : list Value) : Value :=
  let fix loop ws k : list (PFun.PFun Value Value) := 
    match ws with 
    | nil => nil
    | cons v vs => cons (cons (Int k , v) nil) (loop vs (S k))
    end
  in Fun (loop vs 0).

(* partial function with empty domain *)
Definition emptyFcn : Value := Fun [].

Parameter joinFcn : forall {a b} (f1 : PFun a b) (f2: PFun a b), PFun a b.
(* 
-- (?\/) :: Ord a => (a :->? b) -> (a :->? b) -> (a :->? b)
-- f1 ?\/ f2 = PFun (dom f1 `union` dom f2)
                 -- (\x -> if x `elem` dom f1 then apply f1 x else apply f2 x) *)
  
End Value.

Definition V_eq (v1 : Value) (v2 : Value) := Value.eqb v1 v2 = true.
Definition V_lt (v1 : Value) (v2 : Value) := Value.compare v1 v2 = Lt.

#[export] Instance V_Equivalence : Equivalence V_eq.
unfold V_eq.
split.
Admitted.

#[export] Instance V_StrictOrder : StrictOrder V_lt.
Admitted.

(* -------------------- example primitives ------------- *)

(* The denotation of an add1 function. 
   Must be a singleton list (domain is unordered).
   Each partial function must only include 
   mappings x |-> x + 1. 
 *)
Definition add1 : Value -> Prop := 
  fun v => 
      match v with 
      | Fun (cons h nil ) => 
             h = [(Int 0,Int 1); (Int 1, Int 2); (Int 2,Int 3)]
      | _ => False
      end.

(* identity function on any argument. union of 
   all partial functions only map x |-> x for 
   any value. 
*)
Definition idFun : Value -> Prop :=  
  fun v => 
    match v with 
    | Fun (cons h nil) => 
        forall x y, List.In (x,y) h -> x = y
    | _ => False
    end.


(* identity function, but only on ints *)
Definition isInt : Value -> Prop := 
  fun v => 
    match v with 
    | Fun (cons h nil) => 
        forall x y, List.In (x,y) h -> 
               exists k, x = Int k /\ y = Int k
    | _ => False
    end.

(* identity function, but only on functions *)
Definition isFun : Value -> Prop := 
    fun v => 
    match v with 
    | Fun (cons h nil) => 
        forall x y, List.In (x,y) h -> 
               exists hs, x = Fun hs /\ y = Fun hs
    | _ => False
    end.

(* identity function, but only on functions with 
   enumerated domains [(0,v0) .. (n,vn)] *)
Definition isArr : Value -> Prop :=
    fun v => 
      match v with 
      | Fun (cons h nil) => 
          forall x y, List.In (x,y) h -> 
                 x = y /\ exists vs, x = Value.mkTup vs
      | _ => False
    end.

(* Array length *)
Definition arrayLen : Value -> Prop := 
  fun v =>
    match v with 
    | Fun (cons h nil) => 
        forall x y, List.In (x,y) h -> 
               exists vs, x = Value.mkTup vs /\ y = Int (length vs)
    | _ => False
    end.
