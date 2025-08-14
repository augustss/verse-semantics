(* This is Essential Verse. *)


Require Import autosubst.lib autosubst.fintype.
Require Export syntax.common.
Require Import Setoid Morphisms Relation_Definitions.

Definition Wrapping := (Ident * Ident * Ident)%type.

Inductive Expr : Type :=
  | ES : Simple -> Expr
  | Underscore : Expr
  | ApplyD : Simple -> Simple -> Expr
  | Seq : Expr -> Expr -> Expr
  | Where : Expr -> Expr -> Expr  (* reverse sequencing *)
  | Unify : Expr -> Expr -> Expr  (* t1 = t2 *)
  | Define : Ident -> Expr -> Expr (* x := t *)
  | Capture : Ident -> Expr -> Expr (* x -> t  Capture input *)
  | Choice : Expr -> Expr -> Expr
  | Fail : Expr
  | If3 : Expr -> Expr -> Expr -> Expr 
  | For2 : Expr -> Expr -> Expr    (* finite loop *)
  | Array : list Expr -> Expr  
  | Truth : Expr -> Expr

  | Fun : forall (q: Aperture)
            (omega: Effect)
            (i  : Ident)
            (e1 : Expr) 
            (hw : Wrapping) 
            (e2 : Expr), 
          Expr
  | Range : Expr -> Expr             (* :t *)
  | Check : Effect -> Expr -> Expr    (* t1 |>omega t2  -- opacity *)

  (* derived *)
  | Iter : Simple -> Simple -> Expr       (* t1..tn  *)
  | One : Expr -> Expr
  | All : Expr -> Expr

  (* create a new scope *)
  | Block : Expr -> Expr 
.


Fixpoint isValue (e : Expr) : bool := 
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

Definition eSeq (e : list Expr) : Expr := 
  List.fold_right mkSeq eUnit e.


(* Calculate outer ∃-bound variables in expression e *)
(* NOTE: for now, input scope is ignored *)
(* like getVisibleBinders in FrontEnv / Expr.hs *)
Fixpoint I (e : Expr) : Scope.t := 
  match e with 
  | Block e => Scope.empty

  | Define i e => Scope.union (Scope.singleton i) (I e)
  | Array es => Scope_concatMap I es
  | Truth e => I e
  | ApplyD e1 e2 => Scope.empty
  | Unify e1 e2 => Scope.union (I e1) (I e2)
  | Seq e1 e2 => Scope.union (I e1) (I e2)
  (* | Range e => I e   TODO: disagrees with fvs below *)

                     
  (* either doesn't bind any variables, or starts a new scope *)
  | _ => Scope.empty (* Lit / EPrim / Var / Fail / Fun  
                       If3 / Choice / One / All /
                       Verify / Check / ESome  
                     *)
  end.

Fixpoint fvs (e : Expr) : Scope.t := 
  let fvs_blk e := Scope.diff (fvs e) (I e) in
  let fvs_wrp hw (s:Scope.t) : Scope.t := 
    match hw with 
    | (h, x, y) => Scope.add h (Scope.remove y s)
    end in
  let fvs e := 
    match e with 
    | Block e => fvs_blk e
    | ES a => common.fvs a
    | Array es => Scope_unions (List.map fvs es)
    | Truth e => fvs e
    | ApplyD e1 e2 => Scope.union (common.fvs e1) (common.fvs e2)
    | Unify e1 e2  => Scope.union (fvs e1) (fvs e2)
    | Choice e1 e2 => Scope.union (fvs_blk e1) (fvs_blk e2)
    | Seq e1 e2    => Scope.union (fvs e1)(fvs e2)
    | One e => fvs_blk e 
    | All e => fvs_blk e 
    | If3 e1 e2 e3 => 
        (* binders of e1 scope over e2 *)
        Scope.union
          (Scope.diff (Scope.union (fvs e1) (fvs_blk e2)) (I e1))
          (fvs e3)
    | Fun q eff i e1 hw e2 => 
        (* binders of e1 scope over the body e2 *)
        let binders := (Scope.add i (I e1)) in
        Scope.union (fvs e1) (Scope.diff (fvs_wrp hw (fvs_blk e2)) binders)
    | Range e => fvs_blk e 
    | Check _ e => fvs_blk e 
    | _ => Scope.empty
    end in (fvs e).

Definition fresh (t : Expr) : Ident := 
  match Scope.max_elt (fvs t) with 
  | Some i => 1 + i
  | None => 0
  end.

Definition fresh_two (t : Expr) : Ident * Ident := 
  match Scope.max_elt (fvs t) with 
  | Some i => (1 + i, 2 + i)
  | None => (0, 1)
  end.



Declare Scope essential_expr_scope.

Module EssentialNotation. 

Export CommonNotation.

Infix ":>:" := Seq (at level 70, right associativity) : essential_expr_scope.
Infix ":=:" := Unify (at level 65, left associativity) : essential_expr_scope.
Infix ":|:" := Choice (at level 71, left associativity) : essential_expr_scope.
Infix ":@:" := ApplyD (at level 63, left associativity) : essential_expr_scope.
Notation "e |>< eff >" := (Check eff e) : essential_expr_scope.
Notation "{ e }" := (Block e) : essential_expr_scope.
Notation "x := e"   := (Define x e) (at level 25, only printing) : essential_expr_scope.

Coercion ES  : Simple >-> Expr.

End EssentialNotation.
