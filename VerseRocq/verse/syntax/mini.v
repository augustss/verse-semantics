(* This is MiniVerse. It uses the 'Ident' type for variables. *)


Require Import autosubst.lib autosubst.fintype.
Require Export syntax.common.
Require Import Setoid Morphisms Relation_Definitions.

Definition Wrapping := (Ident * Ident * Ident)%type.

Inductive Expr : Type :=


  | DefineV : Ident -> Expr 
  | ES : Simple -> Expr

  (* basic cases *)
  | Array : list Expr -> Expr
  | Truth : Expr -> Expr
  | ApplyD : Simple -> Simple -> Expr

  (* create a new scope *)
  | Block : Expr -> Expr 

  | Fun : forall (q: Aperture)
            (omega: Effect)
            (i  : Ident)
            (e1 : Expr) 
            (hw : Wrapping) 
            (e2 : Expr), 
          Expr

  (* combinators *)
  | Seq : Expr -> Expr -> Expr
  | Unify : Expr -> Expr -> Expr
  | Choice : Expr -> Expr -> Expr
  | Fail : Expr

  (* verification *)
  | Verify : list Ident -> Expr -> Expr
  | Check : Effect -> Expr -> Expr
  | ESome : Expr -> Expr
  | Guard : Expr -> Expr -> Expr 
  | Range : Expr -> Expr

  (* iter *)
  | One : Expr -> Expr
  | All : Expr -> Expr
  | If3 : Expr -> Expr -> Expr -> Expr 
  | For2 : Expr -> Expr -> Expr 
.



Fixpoint isValue (e : mini.Expr) : bool := 
  match e with 
  | ES _ => true
  | Fun _ _ _ _ _ _ => true
  | Array es => List.forallb isValue es
  | Truth e => isValue e
  | _ => false
  end.  

(* smart constructor for sequences. Right associate and drop irrelevant parts. *)
Fixpoint mkSeq (e1 : Expr) (e2: Expr) : Expr := 
  match e1 with 
  | Seq a b => mkSeq a (mkSeq b e2)
  | _ => if isValue e1 then e2 else Seq e1 e2
  end.


Definition eUnit : Expr := Array nil.

Definition eSeq (e : list Expr) := List.fold_right mkSeq eUnit e.


(* Calculate outer ∃-bound variables in expression e *)
(* NOTE: for now, input scope is ignored *)
(* like getVisibleBinders in FrontEnv / Expr.hs *)
Fixpoint I (e : Expr) : Scope.t := 
  match e with 
  | Block e => Scope.empty
  | ES _ => Scope.empty

  | DefineV i => Scope.singleton i
  | Array es => Scope_concatMap I es
  | Truth e => I e
  | ApplyD e1 e2 => Scope.empty
  | Unify e1 e2 => Scope.union (I e1) (I e2)
  | Seq e1 e2 => Scope.union (I e1) (I e2)
  | Guard e1 _ => I e1
  (* | Range e => I e   TODO: disagrees with fvs below *)

                     
  (* either doesn't bind any variables, or starts a new scope *)
  | _ => Scope.empty (* Lit / EPrim / Var / Fail / Fun  
                       If3 / Choice / One / All /
                       Verify / Check / ESome  
                     *)
  end.

Declare Scope mini_expr_scope.

Module MiniNotation. 

Export common.CommonNotation.

Infix ":>:" := mini.Seq (at level 70, right associativity) : mini_expr_scope.
Infix ":=:" := mini.Unify (at level 65, left associativity) : mini_expr_scope.
Infix ":|:" := mini.Choice (at level 71, left associativity) : mini_expr_scope.
Infix ":@:" := mini.ApplyD (at level 63, left associativity) : mini_expr_scope.
Notation "e |>< eff >" := (mini.Check eff e) : mini_expr_scope.
Notation "{ e }" := (mini.Block e) : mini_expr_scope.
Notation "∃ x"   := (mini.DefineV x) (at level 25, only printing) : mini_expr_scope.

Coercion ES : Simple >-> Expr.

End MiniNotation.

(* These tests are from densem.versetest *)
Module Test.

Import common.ConcreteVars.
Import List.ListNotations.
Import MiniNotation.
Open Scope list_scope.
Open Scope mini_expr_scope.

Definition t1 : Expr := 2.

(* SRC: {x:=1; x}
   MINI: {exists x; x = 1; x}
   DEN: {1} 
*) 
Definition t2 := { DefineV x :>: (Var x :=: 1) :>: Var x }.

Definition t3 : Expr := Array [ ES 1 ; ES 2 ; ES 3 ].
Definition t4 := Array [].

(* NOTE: missing wrapping 
SRC: (x:int => x+1)[2]
MINI:
exists int;
int = (\y. isInt$[y]; y);
exists operator'+';
operator'+' =
  (\p. exists x y. (x, y) = p; isInt$[x]; isInt$[y]; intAdd$[x, y]);
(\<succeeds>$i1.(exists x; x = int[$i1]) (){operator'+'[x, 1]})[2]
DEN: 3
*)
Definition t5 :=
  { DefineV t :>:  t :=: IsInt :>:
    DefineV u :>:  u :=: Fun Closed Succeeds i (DefineV x :>: x) (x, t, i) (Add :@: x) :>: 
    u :@: 2 }.

(* missing type test *)
Definition t6 := 
  (DefineV x :>: (x :=: Check Succeeds (1))).

(* x:=1; y:= if(x=2) then 0 else 3; y *)
(* exists x; x = 1; exists y; y = (if (x = 2) then 0 else 3); y *)
(* {3} *)
Definition t7 := 
  DefineV x :>: (x :=: 1) :>: DefineV y :>: (y :=: If3 (x :=: 2) 0 3).

(* y:= if(x=2) then 0 else 3; x:=1; y *)
(*  exists y; y = (if (x = 2) then 0 else 3); exists x; x = 1; y *)
(* {3} *)
Definition t8 := 
  DefineV y :>: y :=: If3 (x :=: 2) (0) (3)
            :>: DefineV x :>: (x :=: 1) :>: y.
  
(*   exists $x1; exists i; i = $x1; exists x; x = ($x1 = 2); (i, x) *)
(* (2,2) *)
Definition t9 := 
  DefineV y :>: DefineV i :>: i :=: y :>:
    DefineV x :>: y :=: 2 :>: Array [ ES (Var i) ; ES (Var x) ].

(* 
if (x = 1) then 1 | 2 else 0;
exists x;
x = 1 | 2;
if (x = 1) then 0 else 1 | 2
*)


Definition t10 := 
  (If3 (x :=: 1) (1 :|: 2) 0) :>: 
    DefineV x :>: 
    (x :=: 1 :|: 2) :>: 
    (If3 (x :=: 1) 0 (1 :|: 2)).


End Test.

(*
t1  
testeq(pass) {2}                                   {2}
t2
testeq(pass) {x:=1; x}                             {1}
  exists x; x = 1; x
t3
testeq(pass) {array{1;2;3}}                        {1,2,3}
t4
testeq(pass) {array{}}                             {()}
t5
testeq(pass) {(x:int => x+1)[2]}                   {3}
  exists int;
  int = (\y. isInt$[y]; y);
  exists operator'+';
  operator'+' =
     (\p. exists x y. (x, y) = p; isInt$[x]; isInt$[y]; intAdd$[x, y]);
  (\<succeeds>$i1.(exists x; x = int[$i1]) (){operator'+'[x, 1]})[2]
t6: 
testeq(pass) {x:int:=1}                            {1}
    exists int;
    int = (\y. isInt$[y]; y);
    exists x;
    x = (1 |><succeeds> int)
t7
testeq(pass) {x:=1; y:= if(x=2) then 0 else 3; y}  {3}
t8
testeq(pass) {y:= if(x=2) then 0 else 3; x:=1; y}  {3}
testeq(pass) {for{:false}}                         {()}
testeq(pass) {for{1}}	                          {array{1}}
testeq(pass) {for{1|2}}	                          {(1,2)}
testeq(pass) {succ(x:int):=x+1; y:succ:=1; y}      {2}
testeq(pass) {i->x := 2; (i, x)}                   {2,2}
# SLOW testeq(pass) {f(x:int where y:=2*x):=y; f[3]}      {6}
testeq(pass) {(x:int) = 2}                         {2}
testeq(pass) {all{:(1,2,3)}}                            {1,2,3}
testeq(pass) {all{x:(1,2,3); x}}                        {1,2,3}
testeq(pass) {all{x:= :(1,2,3); x}}                     {1,2,3}
testeq(pass) {all{x:(1,2,3)}}                           {1,2,3}
testeq(pass) {one{t:=type{0|1}; f(x:t)<invariant>:=x+1; f[0]}}     {1}
testeq(pass) {one{t:=type{0|1}; f(x:t)<invariant>:=x+1; f[2]}}     {:false}

testeq(pass) { x := ((:any) => 1)[x+1] }           {1}
*)
