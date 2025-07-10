(* Partial functions, represented as tables. 
   (Cannot represent as functions as that is not inductive.)
 *)

Require Import structures.Imports.
Require densem.PFun.
Require Import structures.List.

From Stdlib Require Import List.
From Stdlib Require Import Sorting.Sorted.
From Stdlib Require Import Classes.EquivDec.

(* Create the list [j , j + 1 , ... , j+(k-1) ]  *)
Fixpoint enumFrom j k := 
  match k with 
  | 0 => nil
  | S m => j :: enumFrom (S j) m 
  end.

 
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
Definition emptyFcn : Value := Fun nil.

Parameter joinFcn : forall {a b} (f1 : PFun.PFun a b) (f2: PFun.PFun a b), PFun.PFun a b.
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

#[export] Instance V_EquivDec : DecidableEquivalence V_Equivalence.
Admitted.

Lemma Value_dec ( v1 v2 : Dom.Value) : {v1 = v2} + { not (v1 = v2) }.
Admitted.

#[export] Instance EqDec_Value : EqDec Dom.Value Logic.eq.
exact Value_dec. Defined.

(* -------------------- example primitives ------------- *)

Import ListNotations.
Import MonadNotation.

Open Scope monad_scope.

Definition most_ints := [0;1;2;3;4;5].
Definition all_ints := most_ints ++ [6].

(* The denotation of an add1 function. 
   Must be a singleton list (domain is unordered).
   The partial function must only include 
   mappings x |-> x + 1. 
 *)
Definition add1 : Value -> Prop := 
  fun v => 
      match v with 
      | Fun (cons h nil ) => 
          forall x, List.In x most_ints ->
               List.In (Int x, Int (x+1)) h
      | _ => False
      end.

(* identity function, but only on ints *)
Definition isInt : Value -> Prop := 
  fun v => 
    match v with 
    | Fun (cons h nil) => 
        forall x, List.In x all_ints ->
             List.In (Int x, Int x) h
    | _ => False
    end.

(* identity function, but only on functions with 
   enumerated domains [(0,v0) .. (n,vn)] *)
Definition isArr : Value -> Prop :=
    fun v => 
      match v with 
      | Fun (cons h nil) => 
          forall sz, List.In sz all_ints ->
                let vs := enumFrom 0 sz in
                let t  := Value.mkTup (List.map Int vs) in 
                exists v, List.In (t,v) h
      | _ => False
    end.

(* NONE of these definitions are correct. They should be singleton sets, not 
   the union of all finite approximations of the function. *)

(* identity function on any argument. *)
Definition any : Value -> Prop :=  
  fun v => 
    match v with 
    | Fun (cons h nil) => 
        forall x y, List.In (x,y) h -> x = y
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


(* Array length *)
Definition arrayLen : Value -> Prop := 
  fun v =>
    match v with 
    | Fun (cons h nil) => 
        forall x y, List.In (x,y) h -> 
               exists vs, x = Value.mkTup vs /\ y = Int (length vs)
    | _ => False
    end.
