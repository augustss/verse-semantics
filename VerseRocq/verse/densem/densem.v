Require densem.Dom.
Require Import structures.Sets.
Require Import syntax.common.
Require syntax.mini.
From Stdlib Require Lists.List.
Import structures.List.
Import structures.Monad.

Import mini.MiniNotation.

Import MonadNotation.
Import SetNotations.
Import List.ListNotations.
Import ssreflect.

Open Scope monad_scope.
Open Scope list_scope.
Open Scope mini_expr_scope.

Arguments Inhabited {_}.

Definition VAL := P Dom.Value.

(* gives a value for in-scope identitfiers *)
Definition env := Ident -> option Dom.Value.

Module Env. 

Definition empty : env := fun x => None.

Definition extend : Ident -> Dom.Value -> env -> env := 
  fun x v rho => 
    fun y => if Nat.eqb x y then Some v else rho y.

End Env.

Definition ENV := P env.

Definition evalPrim (p : PrimOp) : Dom.Value -> Prop := 
  match p with 
  | Add  => Dom.add1
  | ArrayLen => Dom.arrayLen
  | IsInt => Dom.isInt
  | IsArr => Dom.isArr
  | IsFun => Dom.isFun
  | _ => fun v => False    (* TODO: Lt, etc. *)
  end.

(* Evaluation of atomic expressions. This is 
   partial because variables may be unbound. *)
Inductive evalA (rho : env) : mini.Expr -> Dom.Value -> Prop := 
  | evalA_Var : forall x v,
    rho x = Some v ->
    evalA rho (mini.Var x) v
  | evalA_Lit : forall x, 
    evalA rho (mini.Lit (Int x)) (Dom.Int x)
  | evalA_Prim p v :
    evalPrim p v ->
    evalA rho (mini.EPrim p) v
.

Definition tups (VS : list VAL) : VAL := 
  fun v => 
    exists vs, v = Dom.Value.mkTup vs 
      /\ List.Forall2 Sets.In vs VS.

Definition retP (vo : option Dom.Value) : VAL := 
  match vo with 
  | Some v => ⌈ v ⌉ 
  | None => ∅
  end.


(* inject the result of evaluation of an atomic 
   expression into the computational result. *)
Definition ret_ (vo : option Dom.Value) : list VAL := 
  match vo with 
  | Some v => [⌈ v ⌉]
  | None => []
  end.

(* apply f to x. 
   All elts in the list will either be singleton
   or emptysets. 
   NOTE: Tail is not squashed in this definition.
 *)
Definition apply (f : Dom.Value) 
                 (v : Dom.Value) : list VAL := 
  match f with 
  | Dom.Fun hs => 
      h <- hs ;;
      match (Dom.PFun.apply_opt _ _ 
              Dom.Value.eqb h v) with 
      | Some w => [ ⌈ w ⌉ ]
      | None => [ ∅ ] 
      end
  | _ => []
  end.


(*
Fixpoint Squashed (VS : list VAL) : P (list VAL) := 
  match VS with 
  | nil => fun VS' => VS' = nil
  | cons x xs => fun VS' => 
                  (x <> ∅ -> exists y ys, VS' = cons y ys /\ Squashed xs ys) /\
                  (x = ∅ -> Squashed xs VS')
  end.
*)

Inductive Squashed : list VAL -> list VAL -> Prop := 
  | sq_nil : Squashed nil nil 
  | sq_consIn : forall x xs ys,
    x <> ∅ ->
    Squashed xs ys -> 
    Squashed (cons x xs) (cons x ys)
  | sq_consOut : forall x xs ys,
    x = ∅ ->
    Squashed xs ys -> 
    Squashed (cons x xs) ys.

Definition NonEmptyTail : list VAL -> Prop := 
  fun VS => 
    (* NB: if VS is nil, last returns d, which 
       is trivially inhabited. *)
    let d := ⌈ Dom.Int 0 ⌉ in
    (List.last VS d) <> ∅.

Definition TailSquashed : list VAL -> list VAL -> Prop := 
  fun VS1 VS2 => 
    (exists n, VS1 = VS2 ++ List.repeat ∅ n) /\ NonEmptyTail VS2.


Lemma noEmptyInSquashed : forall xs ys, Squashed xs ys -> 
                                   not (List.In ∅ ys).
Proof.
  induction 1; unfold not; intros. inversion H.
  inversion H1. contradiction. contradiction. contradiction.
Qed.

Definition take1 {A} (xs : list A) : list A := 
  match xs with 
  | h :: _ => [ h ] 
  | [] => [] 
  end.


(* The set of all environments that extend rho
   with definitions for the variables defined 
   by e. *)
Definition X (e : mini.Expr) (rho : env) : ENV :=
  fun rho' =>
    forall x, 
      (* must agree with rho on its domain *)
      (forall v, rho x = Some v -> rho' x = Some v) /\
      (* must be defined on binding variables of e *)
      (Scope.In x (mini.I e) -> exists v, rho' x = Some v).
         
         
(* elementwise union of sequences *)
Fixpoint unions (VS : list VAL) (WS : list VAL) : 
  list VAL := 
  match VS , WS with 
  | [] , _ => WS
  | _  , [] => VS 
  | V :: VS', W :: WS' => (V ∪ W) :: unions VS' WS' 
  end.

Definition Unions : list (list VAL) -> list VAL :=
  List.fold_right unions nil.
  
(* every position in VS is the union of corresponding 
   elements in VVS *)
Definition UNIONS : P (list VAL) -> list VAL -> Prop := 
  fun (VVS : list VAL -> Prop) (VS : list VAL) => 
    forall V i, 
      List.nth_error VS i = Some V -> 
      V = fun (v : Dom.Value) => 
          exists (WS : list VAL) W, 
            (WS ∈ VVS) /\
            List.nth_error WS i = Some W /\
            (v ∈ W).
(* Type of denotation function *)
Definition D := forall (rho : env) (e : mini.Expr), P (list VAL).

Definition check (eval:D) (rho:env) (q:Aperture) (eff:Effect) i e1 y h (x:Ident) e2 : Prop := 
    (exists vh, rho h = Some vh /\   (* h is in scope *)
      forall v, forall rho', 
        rho' ∈ X e1 (Env.extend i v rho) ->
        exists V1, eval rho' e1 V1 /\
            (Squashed V1 []) \/
            (* evaluating e1 produces a value vx *)
            (exists vx, Squashed V1 [ ⌈ vx ⌉ ] /\
             exists V2, apply vh vx = V2   /\
                 (Squashed V2 []) \/
                 (* apply h[x] produces a value vy *)
                 exists vy, Squashed V2 [ ⌈ vy ⌉ ] /\
                   (* evaluating e2 doesn't fail *)
                   exists w, eval (Env.extend y vy rho') e2 [⌈ w ⌉])).

(*
Fixpoint eval (n : nat) (rho : env) (e : mini.Expr) : list VAL := 
  ...
*)

Inductive eval (rho : env) : mini.Expr -> list VAL -> Prop :=

  | eval_Var : forall x v VS,
    rho x = Some v ->
    [ ⌈v⌉ ] = VS ->
    eval rho (mini.Var x) VS

  | eval_Lit : forall x VS,
    [ ⌈Dom.Int x⌉ ] = VS ->
    eval rho (mini.Lit (Int x)) VS

  | eval_Prim p v VS:
    evalPrim p v ->
    [ ⌈v⌉ ] = VS ->
    eval rho (mini.EPrim p) VS

  | eval_DefineV x VS : 
    VS = [ ⌈ Dom.Value.mkTup nil ⌉ ]  ->
    eval rho (mini.DefineV x) VS

  | eval_Array es vs VS : 
    List.Forall2 (evalA rho) es vs ->
    VS =  [ ⌈ Dom.Value.mkTup vs ⌉ ] ->
    eval rho (mini.Array es) VS

  | eval_ApplyD a1 a2 h v VS:
    evalA rho a1 h -> 
    evalA rho a2 v ->
    TailSquashed (apply h v) VS ->
    eval rho (mini.ApplyD a1 a2) VS

  | eval_Seq e1 e2 VS1 VS1' VS2 VS :
    eval rho e1 VS1 -> 
    Squashed VS1 VS1' ->
    eval rho e2 VS2 -> 
    not (List.In ∅ VS1) ->
    TailSquashed ( s1 <- VS1' ;; s2 <- VS2 ;; [ s2 ] ) VS ->
    eval rho (mini.Seq e1 e2) VS
         

  | eval_Unify e1 e2 VS1 VS2 VS3 : 
    eval rho e1 VS1 -> 
    eval rho e2 VS2 ->
    TailSquashed (s1 <- VS1 ;; s2 <- VS2 ;; [ s1 ∩ s2 ] ) VS3 ->
    eval rho (mini.Unify e1 e2) VS3
             
  | eval_Choice e1 e2 VS1 VS2 VS : 
    eval rho e1 VS1 -> 
    eval rho e2 VS2 ->
    (VS1 ++ VS2) = VS ->
    eval rho (mini.Choice e1 e2) VS

  | eval_Fail VS :
    [] = VS ->
    eval rho mini.Fail VS


  | eval_One e VS1 VS2 VS:
    eval rho e VS1 -> 
    Squashed VS1 VS2 ->
    take1 VS2 = VS ->
    eval rho (mini.One e) VS

  | eval_All e VS1 VS2 :
    eval rho e VS1 ->
    Squashed VS1 VS2 -> 
    eval rho (mini.All e) [ tups VS2 ]

  | eval_If3_false e1 e2 e3  VS :
    (* if e1 fails on all extensions of rho *)
    (forall rho', rho' ∈ X e1 rho ->
        eval rho' e1 nil) ->
    eval rho e3 VS ->
    eval rho (mini.If3 e1 e2 e3) VS

(*
  | eval_If3_true e1 e2 e3 rho' V1 V2:
    (* there is an extension of rho where e1 
       doesn't fail. *)
    rho' ∈ X e1 s rho ->
    eval (mini.I e1) rho' e1 V1 ->
    V1 <> nil ->
    eval rho' e2 V2 ->
    eval rho (mini.If3 e1 e2 e3) V2
*)
  
  | eval_If3_true e1 e2 e3 (VV : P (list VAL)) VS :
    (* union together the result of e2 
       for all extensions of rho where e1 doesn't 
       fail *)
    (forall rho', 
       rho' ∈ X e1 rho ->
       exists V1 V2, 
         eval rho' e1 V1 /\
         eval rho' e2 V2 /\
         V1 <> nil /\ (V2 ∈ VV)) ->
    UNIONS VV VS ->
    eval rho (mini.If3 e1 e2 e3) VS

  | eval_Fun q eff i e1 y h x e2 F FS:
    (* see if check succeeds, function is defined *)
    check eval rho q eff i e1 y h x e2 ->
    (* the function has a denotation when, for any argument, 
       the guarded body of that function can be evaluated    
       with that argument. 
       
       NOTE: Every f ∈ F must have a mapping for 
       every v in the domain of the function.
     *)
    (forall f, f ∈ F ->
       forall v W, apply f v = W ->
       eval (Env.extend i v rho)
       ((mini.Var x :=: e1)
         :>: mini.Var y :=: 
           (mini.One (mini.Var h :@: mini.Var x))
         :>: e2) W) ->
    Squashed [ F ] FS ->  (* if F is emptyset, get rid of it *)
    eval rho (mini.Fun q eff i e1 (Some (y,h,x)) e2) FS


  | eval_FunFail q eff i e1 y h x e2 VS :
    (* check fails, but this is not strictly positive *)
    (* not (check eval rho q eff i e1 y h x e2) -> *)
    [] = VS ->
    eval rho (mini.Fun q eff i e1 (Some (y,h,x)) e2) VS
.

Definition eval_top t d := eval Env.empty t d.

Create HintDb sets.

Lemma singleton_not_empty {A}{v:A} : ⌈ v ⌉ <> ∅. Admitted.
Lemma Intersection_same {A}{v:P A} : (v ∩ v) = v.  Admitted.
Lemma Intersection_diff {A}{v1 v2:A} : 
  v1 <> v2 ->
  (⌈v1⌉ ∩ ⌈v2⌉) = ∅. 
  Admitted.
Lemma Intersection_comm {A}{v1 v2:P A} : (v1 ∩ v2) = (v2 ∩ v1).
 Admitted.


Lemma NonEmptyTail_nil : NonEmptyTail [].
cbv. eapply singleton_not_empty. Qed.
Lemma NonEmptyTail_singleton {v}: NonEmptyTail [ ⌈v⌉ ].
cbv. eapply singleton_not_empty. Qed.
Lemma notIn_singleton {A}{v : A} : ~ List.In ∅ [⌈ v ⌉]. 
intro h. inversion h.  apply singleton_not_empty in H. done. inversion H.
Qed.

Lemma TailSquashed_singleton v VS : 
  VS = [⌈ v ⌉] ->
  TailSquashed [⌈ v ⌉] VS.
Proof. intros ->. unfold TailSquashed. split. exists 0. cbn. auto.
eapply NonEmptyTail_singleton; eauto. Qed.

Lemma TailSquashed_nil (VS : list VAL) : 
  VS = nil ->
  TailSquashed [] VS.
Proof. intros ->. unfold TailSquashed. split. exists 0. cbn. auto.
eapply NonEmptyTail_nil; eauto. Qed.

Lemma TailSquashed_empty (VS : list VAL) : 
  VS = nil ->
  TailSquashed [∅] VS.
Proof. intros ->. unfold TailSquashed. split. exists 1. cbn. auto.
eapply NonEmptyTail_nil; eauto. Qed.


Lemma Squashed_singleton v VS : 
  VS = [⌈ v ⌉] ->
  Squashed [⌈ v ⌉] VS.
Proof. intros ->. eapply sq_consIn; eauto using singleton_not_empty.
       eapply sq_nil. Qed.

Lemma Squashed_nil (VS : list VAL) : 
  VS = nil ->
  Squashed [] VS.
Proof. intros ->. eapply sq_nil. Qed.

Hint Resolve 
  singleton_not_empty 
  NonEmptyTail_nil 
  NonEmptyTail_singleton 
  notIn_singleton
  TailSquashed_singleton
  TailSquashed_nil
  TailSquashed_empty
  Squashed_singleton
 :sets.

Lemma Squashed_singleton_invert v VS : 
  Squashed [⌈ v ⌉] VS -> VS = [⌈ v ⌉].
Proof. intro h. inversion h. subst. inversion H3. auto.
subst. apply singleton_not_empty in H1. done. Qed.
Lemma Squashed_nil_invert VS : 
  Squashed [] VS -> VS = [].
Proof. intro h. inversion h. auto. Qed.
Lemma Squashed_empty_invert VS : 
  Squashed [∅] VS -> VS = [].
Proof. intro h. inversion h. done. inversion H3. done. Qed.


Ltac ego := eauto with sets ; 
            cbn ; 
            try rewrite Intersection_same ;
            try (rewrite Intersection_diff ; 
                 [ intros ? ; discriminate| eauto with sets]) ;
            eauto with sets.

Ltac eval1 := match goal with 
              | [ |- eval ?env ?e ?VS ] =>
                  econstructor ; eauto end.


Ltac eeval := match goal with 
              | [ |- eval ?env (mini.Seq _ _) ?VS ] =>
                  econstructor ; 
                  [ eeval | eauto with sets | eeval 
                  | eauto with sets 
                  | cbn ; eauto with sets ]
              | [ |- eval ?env (mini.Unify _ _) ?VS ] =>
                  econstructor ; [ eeval | eeval | ego ]
              | [ |- eval ?env (mini.Var _) ?VS ] =>
                  econstructor ; cbn ; eauto 
              | [ |- eval ?env ?e ?VS ] =>
                  econstructor ; eauto end.


Module Test.



Lemma t1 : eval_top mini.Test.t1 [ ⌈ Dom.Int 2 ⌉ ].
Proof. unfold mini.Test.t1, eval_top. repeat eeval. Qed.



(* x:=1; x  *)
Lemma t2 : 
  exists v, eval (Env.extend mini.Test.x v Env.empty) mini.Test.t2 [ ⌈ Dom.Int 1 ⌉ ].
Proof. exists (Dom.Int 1).
       unfold mini.Test.t2.
       eeval.
Qed.

(* version with no automation *)
(*
Lemma t2' : 
  exists v, eval (Env.extend mini.Test.x v Env.empty) mini.Test.t2 [ ⌈ Dom.Int 1 ⌉ ].
Proof. exists (Dom.Int 1).
       unfold mini.Test.t2.
       eapply eval_Seq.
       - eapply eval_DefineV; eauto.
       - eapply Squashed_singleton; eauto.
       - eapply eval_Seq; eauto.
         + eapply eval_Unify; eauto.
           -- eapply eval_Var; cbn; eauto.
           -- eapply eval_Lit; eauto.
           -- cbn. rewrite Intersection_same. 
              eapply TailSquashed_singleton. eauto.
         + eapply Squashed_singleton. eauto.
         + eapply eval_Var; cbn; eauto.
         + eauto with sets.
         + cbn. eapply TailSquashed_singleton. eauto.
       - eauto with sets.
       - cbn. eapply TailSquashed_singleton. eauto.
Qed. *)


(* x:=1; y:= if(x=2) then 0 else 3; y *)

Lemma t7 : 
  exists vx, exists vy, eval (Env.extend mini.Test.x vx (Env.extend mini.Test.y vy Env.empty)) mini.Test.t7 [ ⌈ Dom.Int 3 ⌉ ].
Proof.  exists (Dom.Int 1). exists (Dom.Int 3).
        unfold mini.Test.t7.
        eapply eval_Seq.
        - eeval.
        - ego.
        - eapply eval_Seq.
          + eeval.
          + ego.
          + eapply eval_Seq.
            -- eeval.
            -- ego.
            -- eapply eval_Unify.
               ++ eeval.
               ++ eapply eval_If3_false.
                  --- intros rho' rho'X.
                      unfold X, Ensembles.In in rho'X. 
                      move: (rho'X mini.Test.x) => [h _].
                      cbn in h.
                      specialize (h (Dom.Int 1) ltac:(auto)).                       
                      eeval.
                  --- eeval.
               ++ ego.
            -- ego.
            -- ego.
          + ego.
          + ego.
        - ego.
        - ego.
Qed.

End Test.


Module Theory.

Lemma NonEmptyTail_UNIONS : 
  forall VS VV V, 
    NonEmptyTail VS -> VS ∈ VV -> UNIONS VV V -> 
    NonEmptyTail V.
Proof.
  intros.
  unfold UNIONS in H1.
  unfold NonEmptyTail.
Admitted.

Lemma NonEmptyTail_app VS1 VS2 : 
  NonEmptyTail VS1 -> NonEmptyTail VS2 ->
  NonEmptyTail (VS1 ++ VS2).
Admitted.

Lemma evalNonEmptyTail : 
  forall e rho VS, eval rho e VS -> NonEmptyTail VS.
Proof.
  induction 1.
  all: subst.
  all: auto.
  all: try eapply NonEmptyTail_singleton.

  (* true because we tail squashed *)
  all: try solve [match goal with 
    | [ H : TailSquashed _ ?VS |- _ ] => move: H => [ _ h1 ] 
    end; done].

  - (* choice *)
    eapply NonEmptyTail_app; eauto.
  - (* one *)
    admit.
  - (* all *)
    admit.
Admitted.

End Theory.


Module Rewrites.

Ltac invert_eval := 
  match goal with 
  | [ H : eval ?rho (_ _) ?S |- _ ] => inversion H; subst; clear H 
  | [ H : evalA ?rho (_ _) ?S |- _ ] => inversion H; subst; clear H 
  | [ H : List.Forall2 (evalA _) (cons _ _) _ |- _ ] => 
      inversion H; subst; clear H 
  | [ H : List.Forall2 (evalA _) nil _ |- _ ] => 
      inversion H; subst; clear H 
  end.

Lemma VarSwap : forall rho (x y : Ident) e, 
    eval rho ((mini.Var x :=: mini.Var y) :>: e) ⊆
    eval rho ((mini.Var y :=: mini.Var x) :>: e).
Proof. 
  intros.
  intros VS vIn. unfold Ensembles.In in *.
  invert_eval.
  invert_eval.
  invert_eval.
  invert_eval.
  cbn in H9.
  eapply eval_Seq.
  - eeval. rewrite Intersection_comm. eauto.
  - ego.
  - ego.
  - ego.
  - ego.
Qed.



Lemma AppTup x rho v v0 v1: 
  eval rho 
  (mini.DefineV x :>: (mini.Var x :=: mini.Array [v0 ; v1])  :>: (mini.Var x :@: mini.Lit (Int v))) ⊆
  eval rho (((mini.Lit (Int v) :=: mini.Lit (Int 0)) :>: v0) :|: ((mini.Lit (Int v) :=: mini.Lit (Int 1)) :>: v1)).
Proof.
  intros VS vIn. unfold Ensembles.In in *.
  repeat invert_eval.
  clear H4.
  apply Squashed_singleton_invert in H2. subst.
  eval1.
  - eval1.
    + eval1.
      -- eval1.
Admitted.

Lemma AppLam rho (x y :Ident) v e : 
  eval rho 
    (mini.DefineV y :>:
       (mini.Var y :=: (mini.Fun Closed Succeeds x (mini.Var x) None e)) :>: 
    (mini.ApplyD (mini.Var y) v)) ⊆
  eval rho ((mini.DefineV x) :>: mini.Var x :=: v :>: e).
Proof.
  intros VS vIn. unfold Ensembles.In in *.
  invert_eval.
  invert_eval.
  invert_eval.
  invert_eval.
  inversion H3. subst. clear H3.
Admitted.

End Rewrites.
