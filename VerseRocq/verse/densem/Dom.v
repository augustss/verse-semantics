(* The domain of values for the denotational semantics *)

From Stdlib Require Import List.
From Stdlib Require Import Sorting.Sorted.
From Stdlib Require Import Classes.EquivDec.
From Stdlib Require Import Psatz.

Require Import structures.Imports.
Require Import structures.List.
Require Import densem.PFun.

Import ListNotations.
Import ListMonadNotation.
Open Scope list_scope.


(* There are a finite number of values in this semantics, 
   but a lot of them. As a result, we can represent 
   all functions as finite tables. 
*)


(** * Numbers 

   The size of the value domain is determined by the definition 
   of the largestNumber and the rank of the largest function. 
   We keep these definitions abstract.
*)

Definition largestNum := 1.
Definition limitNum := S largestNum.
Definition largestRank := 1.


(*
Axiom ge10 : 10 <= largestNum.
Definition ge0 : 0 <= largestNum. lia. Qed.
Definition ge1 : 1 <= largestNum. move: ge10; lia. Qed.
Definition ge2 : 2 <= largestNum. move: ge10; lia. Qed.
Definition ge3 : 3 <= largestNum. move: ge10; lia. Qed.
*)

(* because there is a bound on the numbers, we can list them all *)
Definition allNums := enumFrom 0 limitNum.
Definition mostNums := enumFrom 0 largestNum.
Lemma allNums_mostNums : allNums = mostNums ++ [largestNum].
Admitted.

Lemma all_in_allNums : forall n, n <= largestNum -> List.In n allNums.
Proof. Admitted.

(* ------------------------------------------------------ *)

(** * Values *)

(* Invariant: domains of partial 
   functions in the list are disjoint   
   but --- advantage for nondisjoint -- functinos 
   can return choices*)
Inductive value : Type := 
  | Int : nat -> value
  | Fun : list (pfun value value) -> value. 

Definition v0 := Int 0.
Definition v1 := Int 1.
Definition v2 := Int 2.
Definition v3 := Int 3.
   
(* n-ary tuples are enumerated partial functions i -> v *)
Definition mkTup (vs : list value) : value :=
  let fix loop ws k : list (pfun value value) := 
    match ws with 
    | nil => nil
    | cons v vs => cons (cons (Int k , v) nil) (loop vs (S k))
    end
  in Fun (loop vs 0).

(* extend a tuple value with a new component at the end *)
Definition snoc (tup : value) (v: value) : value := 
  match tup with 
  | Fun hs =>
      Fun (hs ++ [[(Int (List.length hs),v)]])
  | _ => tup
  end.

Lemma snoc_mktup xs v : 
  snoc (mkTup xs) v = mkTup (xs ++ [v]).
Admitted.


(* partial function with empty domain *)
Definition emptyFun : value := Fun nil.

(* Function values can have ordered and unordered domains. *)
(* If the domain is ordered, you can iterate over it. 
   For example, a tuple (a,b,c) is represented as:
      Fun [ (0 |-> a) ; (1 |-> b) ; (2 |-> c) ]
 *)
 
Module Value.

Fixpoint eqb (v1 : value) (v2 : value) : bool := 
  match v1 , v2 with 
  | Int i , Int j => Nat.eqb i j
  | Fun fs1 , Fun fs2 => 
      forallb2 (PFun.eqb _ _ eqb eqb) fs1 fs2
  | _ , _ => false
  end.
                        
Fixpoint compare (v1 : value) (v2 : value) : comparison := 
  match v1 , v2 with 
  | Int i , Int j => Nat.compare i j
  | Int _ , Fun _ => Lt
  | Fun _ , Int _ => Gt
  | Fun fs1 , Fun fs2 => 
      list_compare (PFun.compare _ _ compare compare) fs1 fs2 
  end.



Definition V_eq (v1 : value) (v2 : value) := 
  Value.eqb v1 v2 = true.
Definition V_lt (v1 : value) (v2 : value) := 
  Value.compare v1 v2 = Lt.


Lemma eqb_eq v1 v2 : Value.eqb v1 v2 = true <-> v1 = v2. Admitted.
Lemma eqb_neq v1 v2 : Value.eqb v1 v2 = false <-> v1 <> v2. Admitted.

Definition leb (v1 : value) (v2 : value) := 
  match (Value.compare v1 v2) with 
  | Lt => true
  | Eq => true
  | Gt  => false
  end.

Definition ltb (v1 : value) (v2 : value) := 
  match (Value.compare v1 v2) with 
  | Lt => true
  | Eq => false
  | Gt  => false
  end.

Definition gtb (v1 : value) (v2 : value) := 
  match (Value.compare v1 v2) with 
  | Lt => false
  | Eq => false
  | Gt  => true
  end.

Lemma value_dec ( v1 v2 : Dom.value) : 
  {v1 = v2} + { not (v1 = v2) }.
Admitted.


Inductive valid : value -> Prop := 
 | valid_Int k : (k <= largestNum) -> valid (Int k)
 | valid_Fun fs : 
   Forall (fun f => PFun.valid _ _ compare f /\ forall v1 v2, 
               List.In (v1, v2) f -> valid v1 /\ valid v2) fs ->
   valid (Fun fs).

(* all valid values, in order. *)
Parameter universe : list value.
Axiom universe_valid : forall v, List.In v universe -> valid v.  
Axiom valid_universe : forall v, valid v -> List.In v universe.

Lemma emptyFun_valid : valid emptyFun. 
  eapply valid_Fun. econstructor. Qed.
Lemma mkTuple_valid : forall vs, Forall valid vs -> 
                            valid (mkTup vs).
Admitted.

End Value.

#[export] Instance V_Equivalence : Equivalence Value.V_eq.
unfold Value.V_eq.
split.
Admitted.

#[export] Instance V_StrictOrder : StrictOrder Value.V_lt.
Admitted.

#[export] Instance V_EquivDec : DecidableEquivalence V_Equivalence.
Admitted.

#[export] Instance EqDec_value : EqDec Dom.value Logic.eq.
exact Value.value_dec. Defined.



(* -------------------- example primitives ------------- *)

Module Prim.


Definition add1 : value := 
  let h := x <- mostNums ;;
           [ (Int x, Int (x + 1)) ]
  in
    Fun [ h ].

Lemma add1_spec : 
  exists h, add1 = Fun [ h ] /\
  forall x, x < largestNum -> List.In (Int x, Int (x+1)) h.
Admitted.

Definition any : value := 
  let h := v <- Value.universe ;; [ (v, v) ] 
  in Fun [ h ].

Lemma any_spec :
  exists h, any = Fun [ h ] /\ 
         forall v, Value.valid v -> List.In (v,v) h.
Admitted.    

Definition isInt : value := 
  let h := k <- allNums ;; [ (Int k, Int k) ] 
  in Fun [ h ].
Lemma isInt_spec :
  exists h, isInt = Fun [ h ] /\
         forall k, k <= largestNum -> List.In (Int k,Int k) h.
Admitted.


Parameter isArr : value.

(* identity function, but only on functions with 
   enumerated domains [(0,v0) .. (n,vn)] *)
Lemma isArr_spec :
  exists h, isArr = Fun [ h ] /\
       forall v vs, Value.valid v -> 
               v = mkTup vs -> 
               List.In (v,v) h.
Admitted.

Parameter isFun : value.
Lemma isFun_spec : 
  exists h, isFun = Fun [h] /\
     forall v hs, Value.valid v -> 
             v = Fun hs ->
             List.In (v,v) h.
Admitted.

Parameter arrayLen : value.
Lemma arrayLen_spec :
  exists h, arrayLen = Fun [h] /\
         forall v vs, v = mkTup vs -> 
                 List.In (v, Int (length vs)) h.
Admitted.

End Prim.


Declare Scope value_scope.
Delimit Scope value_scope with value.
Open Scope value_scope. 

Module ValueNotation.

Notation "0" := v0 : value_scope.
Notation "1" := v1 : value_scope.
Notation "2" := v2 : value_scope.
Notation "3" := v3 : value_scope.

End ValueNotation.
