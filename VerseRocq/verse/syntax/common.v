From Stdlib Require MSets.MSetList.

(* Identifiers are just nats *)
Definition Ident : Type := nat.

Definition srcUnderscore := 0.

(* A scope is a set of identitfiers *)
Module Scope := MSetList.Make(PeanoNat.Nat).

Definition Scope_unions :=
      List.fold_right Scope.union Scope.empty.

Definition Scope_concatMap {A}
  (f : A -> Scope.t) : list A -> Scope.t :=
  fun x => List.fold_right Scope.union Scope.empty 
                  (List.map f x).

Inductive PrimOp : Type :=
| Add : PrimOp
| ArrayLen : PrimOp
| Lt  : PrimOp
| IsInt : PrimOp
| IsStr : PrimOp
| IsArr : PrimOp
| IsFun : PrimOp
.

Inductive LitType : Type := 
| Int : nat -> LitType
.

Inductive IterType : Type := 
| IterIf : IterType
| IterOne : IterType
| IterAll : IterType
| IterFor : IterType
| IterCounce : IterType
.

Inductive Effect : Type := 
| Fails : Effect    (* no results *)
| Succeeds : Effect (* one result *)
| Decides : Effect  (* two results *)
| Iterates : Effect (* 0,1,2,3,... *)
.

Inductive Aperture : Type := 
| Open : Aperture
| Closed : Aperture
.
