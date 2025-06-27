Require Import syntax.mini.
Require Import autosubst.fintype.
Require Import Nat.

Definition table n := Ident -> fin n.

Definition 
  push (x : Ident) {n:nat} (s:table n) : table (S n) := 
  fun y => if Nat.eqb x y then var_zero else shift (s x).
Module TableNotations.
Infix ".:" := push (at level 70).  
End TableNotations.
Import TableNotations.

Require Import syntax.core.

Module CoreNotations.
Infix ":=:" := core.Core.Unify (at level 70).
Infix ":>:" := core.Core.Seq   (at level 70).
Infix ":|:" := core.Core.Or    (at level 70).
Infix ":@:" := core.Core.App   (at level 71).
End CoreNotations.

Import CoreNotations.

Definition isVar {n} (e:Expr n) : bool := 
  match e with 
  | var_Expr x => true
  | _ => false
  end.

Fixpoint isHNF {n} (e : Expr n) {struct e} : bool :=
  let isVal e := orb (isVar e) (isHNF e) 
  in 
  match e with 
  | Lit _ => true
  | Op _ => true
  | Tup es => List.forallb isVal es
  | Tru e => isVal e
  | Lam _ => true 
  | _ => false
  end.
Definition isVal {n}(e : Expr n) := orb (isVar e) (isHNF e).

Definition mkIf {n}(e1 : Expr n) (e2 : Expr n) := 
  core.Core.Iter IterIf e1 e2.
Definition mkOne {n}(e: Expr n) := 
  core.Core.Iter IterOne e core.Core.Fail.
Definition mkAll {n}(e: Expr n) := 
  core.Core.Iter IterAll e (core.Core.Tup nil).
Definition mkThunk {n}(e:Expr (S n)) : Expr n := 
  core.Core.Lam e.

Fixpoint mkSeq {n} (e1 : Expr n ) (e2 : Expr n) := 
  match e1 with 
  | a :>: b => mkSeq a (b :>: e2)
  | _ => if isVal e1 then e2 else e1 :>: e2
  end.

Variant mode := | execute | verify | check.

Fixpoint expr (m : mode) {n : nat} (s : table n) (v : mini.Expr)  {struct v}: 
  core.Core.Expr n := 
  match v with 
  | mini.Var x => var_Expr (s x)
  | mini.DefineV x => var_Expr (s x)

  (* basic cases *)
  | mini.Lit x => core.Core.Lit x
  | mini.EPrim op => core.Core.Op op
  | mini.Array es => core.Core.Tup (List.map (expr m s) es)
  | mini.ApplyD e1 e2 => expr m s e1 :@: expr m s e2
      
  (* binding/table *)
  (* | mini.Exists x e => core.Core.Exi (expr (push x s) e) *)
  (* | mini.Lam x e => core.Core.Lam (expr (push x s) e) *)

  (* combinators *)
  | mini.Seq e1 e2 => expr m s e1 :>: expr m s e2
  | mini.Unify e1 e2 => expr m s e1 :=: expr m s e2
  | mini.Choice e1 e2 => expr m s e1 :|: expr m s e2
  | mini.Fail => core.Core.Fail

  (* iter constructs *)
  | mini.One e => mkOne (expr m s e)
  | mini.All e => mkAll (expr m s e)
  | mini.If3 e1 e2 e3 => mkIf                           
     (mkSeq (expr m s e1) (mkThunk (expr m (push srcUnderscore s) e2)))
     (expr m s e3)

  (* functions *)
  | mini.Fun q eff i e1 None e2 => 
      match m with 
      | execute => 
      (*  𝜆x.Vx[[e1]] (𝜋+i); Vx [[e2]] (𝜋+i) *)
          core.Core.Lam (mkSeq (expr m (push i s) e1) (expr m (push i s) e2))
      | _ => core.Core.Fail
      end

  | mini.Fun q eff i e1 (Some (y,h,x)) e2 => 
      match m with 
      | execute => 
      (*  𝜆x.Vx[[e1]] (𝜋+i); Vx [[e2]] (𝜋+i) *)
          let y' : fin (S n) := Some (s y) in
          let h' : fin (S n) := Some (s h) in
          let x' : fin (S n) := Some (s x) in
          core.Core.Lam ((expr m (push i s) e1) :>: 
                           (var_Expr y') :=: (var_Expr h' :@: 
                                              var_Expr x') :>:
                           (expr m (push i s) e2))
      | _ => core.Core.Fail
      end




  | _ => core.Core.Fail
  end.
