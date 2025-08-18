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
| TimesTwo : PrimOp
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

Inductive Simple : Type := 
| Var : Ident -> Simple
| Lit : LitType -> Simple
| EPrim : PrimOp -> Simple
| SArray : list Simple -> Simple
.

Fixpoint fvs (a : Simple) : Scope.t := 
  match a with 
  | Var x => Scope.singleton x 
  | Lit _ => Scope.empty 
  | EPrim _ => Scope.empty
  | SArray s => Scope_unions (List.map fvs s)
  end.

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

Module CommonNotation.
Coercion Int   : nat >-> LitType.
Coercion Lit   : LitType >-> Simple.
Coercion Var   : Ident >-> Simple.
Coercion EPrim : PrimOp >-> Simple.
End CommonNotation.


Module ConcreteVars.
Definition r : Ident := 0.
Definition x : Ident := 1.
Definition y : Ident := 2.
Definition t : Ident := 3.
Definition i : Ident := 4.
Definition u : Ident := 5.
Definition v : Ident := 6.
End ConcreteVars.
