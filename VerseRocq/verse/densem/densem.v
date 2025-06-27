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

(* --------------- environments ----------- *)

(* gives a value for in-scope identitfiers *)
Definition env := Ident -> option Dom.Value.

Module Env. 

Definition empty : env := fun x => None.

Definition extend : Ident -> Dom.Value -> env -> env := 
  fun x v rho => 
    fun y => if Nat.eqb x y then Some v else rho y.

End Env.

Definition ENV := P env.

(* -------------- primitives ------------ *)

Definition evalPrim (p : PrimOp) : Dom.Value -> Prop := 
  match p with 
  | Add  => Dom.add1
  | ArrayLen => Dom.arrayLen
  | IsInt => Dom.isInt
  | IsArr => Dom.isArr
  | IsFun => Dom.isFun
  | _ => fun v => False    (* TODO: Lt, etc. *)
  end.

(* -------------- atomic expressions ------------ *)

(*
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
.*)

Definition tups (VS : list VAL) : VAL := 
  fun v => 
    exists vs, v = Dom.Value.mkTup vs /\ List.Forall2 Sets.In vs VS.

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
      match (PFun.apply_opt _ _ Dom.Value.eqb h v) with 
      | Some w => [ ⌈ w ⌉ ]
      | None => [ ∅ ] 
      end
  | _ => []
  end.

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

Lemma UNIONS_mem (ls : list (list VAL)) : (UNIONS (mem ls)) = ⌈ Unions ls ⌉.
Proof.
Admitted.

                     

(* Type of denotation relation *)
Definition D := forall (rho : env) (e : mini.Expr), (list VAL) -> Prop.


(* check that h does not fail on the input *)
Definition check (eval:D) (rho:env) (q:Aperture) 
  (eff:Effect) i e1 y h (x:Ident) e2 : Prop := 
    (exists vh, rho h = Some vh /\   (* h is in scope *)
      forall v, exists w, (* for every input, there is some result *)
      forall rho', 
        rho' ∈ X e1 (Env.extend i v rho) ->
        exists V1, eval rho' e1 V1 /\
            (Squashed V1 [] /\ apply vh v = []) (* !! /\ or \/ ?? *)
              \/
            (* evaluating e1 produces a value vx *)
            (exists vx, Squashed V1 [ ⌈ vx ⌉ ] /\
             exists V2, apply vh vx = V2 /\
                 (* apply h[x] fails, eff must be decides, and e2 must fail *)
                 (Squashed V2 [] /\ eff = Decides
                                 /\ forall vy, eval (Env.extend y vy rho') e2 []) 
                 \/
                 (* apply h[x] produces a value vy *)
                 exists vy k, TailSquashed V2 (List.repeat ∅ k ++ [ ⌈ vy ⌉ ]) /\
                   (* evaluating e2 doesn't fail *)
                    eval (Env.extend y vy rho') e2 (List.repeat ∅ k ++ [⌈ w ⌉]))).


Inductive eval (rho : env) : mini.Expr -> list VAL -> Prop :=

  | eval_Block e VV VS :
    (forall rho', rho' ∈ X e rho -> exists VS', eval rho' e VS' /\ (VS' ∈ VV)) ->
    UNIONS VV VS ->
    eval rho (mini.Block e) VS

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

  | eval_If3 e1 e2 e3  VS :
    eval_if rho (mini.If3 e1 e2 e3) VS ->
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
            (mini.Block (mini.DefineV x :>: (x :=: e1)
                         :>: y :=: (mini.One (h :@: x))
                         :>: e2)) W) ->
    Squashed [ F ] FS ->  (* if F is emptyset, get rid of it *)
    eval rho (mini.Fun q eff i e1 (y,h,x) e2) FS


  | eval_FunFail q eff i e1 y h x e2 VS :
    (* check fails, but this is not strictly positive *)
    (* not (check eval rho q eff i e1 y h x e2) -> *)
    [] = VS ->
    eval rho (mini.Fun q eff i e1 (y,h,x) e2) VS

with evalA (rho : env) : mini.Expr -> Dom.Value -> Prop := 
 | evalA_one e v : 
   eval rho e [ ⌈ v ⌉ ] -> 
   evalA rho e v

with eval_if (rho : env) : mini.Expr -> list VAL -> Prop := 
 | eval_If3_false e1 e2 e3  VS :
    (* if e1 fails on all extensions of rho *)
    (forall rho', rho' ∈ X e1 rho ->
        eval rho' e1 nil) ->
    eval rho e3 VS ->
    eval_if rho (mini.If3 e1 e2 e3) VS
 | eval_if3_true e1 e2 e3 (VV : P (list VAL)) VS :
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
    eval_if rho (mini.If3 e1 e2 e3) VS
.

Definition eval_top t d := eval Env.empty t d.

Create HintDb sets.


Lemma singleton_not_empty {A}{v:A} : ⌈ v ⌉ <> ∅. Admitted.
Lemma Intersection_same {A}{v:P A} : (v ∩ v) = v.  Admitted.
Lemma Intersection_diff {A}{v1 v2:A} : v1 <> v2 -> (⌈v1⌉ ∩ ⌈v2⌉) = ∅. Admitted.
Lemma Intersection_commutes {A}{v1 v2:P A} : (v1 ∩ v2) = (v2 ∩ v1). Admitted.
Lemma notIn_singleton {A}{v : A} : ~ List.In ∅ [⌈ v ⌉]. 
Proof.
  intro h. inversion h.  apply singleton_not_empty in H. done. inversion H.
Qed.



Lemma NonEmptyTail_nil : NonEmptyTail [].
cbv. eapply singleton_not_empty. Qed.
Lemma NonEmptyTail_singleton {v}: NonEmptyTail [ ⌈v⌉ ].
cbv. eapply singleton_not_empty. Qed.

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

Lemma noEmptyInSquashed : forall xs ys, Squashed xs ys -> 
                                   not (List.In ∅ ys).
Proof.
  induction 1; unfold not; intros. inversion H.
  inversion H1. contradiction. contradiction. contradiction.
Qed.


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
  Squashed_nil
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

Ltac eeval1 := match goal with 
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



(* ----------------------- examples --------------- *)

Module Test.

Lemma t1 : eval_top mini.Test.t1 [ ⌈ Dom.Int 2 ⌉ ].
Proof. unfold mini.Test.t1, eval_top. repeat eeval. Qed.

Lemma Value_dec ( v1 v2 : Dom.Value) : {v1 = v2} + { not (v1 = v2) }.
Admitted.

(* { x:=1; x }  *)
Lemma t2 : 
  eval Env.empty mini.Test.t2 [ ⌈ Dom.Int 1 ⌉ ].
Proof. unfold mini.Test.t2.
       (* set up the block. *)
       eeval1.
       intros rho' inX.
       cbv in inX.
       specialize (inX mini.Test.x) as [_ h2].
       specialize (h2 ltac:(eauto)) as [v EQv].
       (* need to do this case analysis here, before we instantiate 
          the individual result. *)
       destruct (Value_dec v (Dom.Int 1)).
       - exists ([ ⌈ Dom.Int 1 ⌉ ]).
         split; subst.
         eeval.
         ego.
       - exists ([ ]).
         split. 
         eeval1.
         + eeval.
         + ego.
         + eeval1.
           ++ eeval1.
              eeval.
              eeval.
              cbn.
              rewrite Intersection_diff; auto.
              ego.
           ++ eapply Squashed_nil. eauto.
           ++ eeval.
           ++ ego.
           ++ cbn. ego.
         + ego.
         + ego.
         + eauto with sets.
       - instantiate (1 := nil).
         rewrite UNIONS_mem.
         eapply in_singleton.
Qed.



(* No block here. *)
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
               ++ eeval1.
                  +++ eapply eval_If3_false.
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

Lemma empty_is_empty {A} : forall (S : P A), S = ∅ -> forall x, not (x ∈ S).
Admitted.


Lemma NonEmptyTail_UNIONS : 
  forall V VV, 
    UNIONS VV V -> 
    (forall VS, VS ∈ VV -> NonEmptyTail VS) -> 
    NonEmptyTail V.
Proof.
  intro V. induction V.
  - intros.
    unfold UNIONS in *.
    unfold NonEmptyTail.
    cbn. eauto with sets.
  - intros VV VH U.
    unfold UNIONS in *.
Admitted.

Lemma NonEmptyTail_app VS1 VS2 : 
  NonEmptyTail VS1 -> NonEmptyTail VS2 ->
  NonEmptyTail (VS1 ++ VS2).
Admitted.

Fixpoint evalNonEmptyTail {e}{rho}{VS} (ev : eval rho e VS) : NonEmptyTail VS.
  destruct ev; subst; eauto.
  all: try eapply NonEmptyTail_singleton.
  all: try solve [match goal with 
    | [ H : TailSquashed _ ?VS |- _ ] => move: H => [ _ h1 ] 
    end; done].
  - (* block *)
    eapply NonEmptyTail_UNIONS; eauto.
    intros V1 in1.
Admitted.

(*
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
  - (* block *)

  - (* choice *)
    eapply NonEmptyTail_app; eauto.
  - (* one *)
    admit.
  - (* all *)
    admit.
Admitted.
*)

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
Admitted.



Lemma AppTup x rho v v0 v1: 
  eval rho 
  (mini.DefineV x :>: (mini.Var x :=: mini.Array [v0 ; v1])  :>: (mini.Var x :@: mini.Lit (Int v))) ⊆
  eval rho (((mini.Lit (Int v) :=: mini.Lit (Int 0)) :>: v0) :|: ((mini.Lit (Int v) :=: mini.Lit (Int 1)) :>: v1)).
Proof.
  intros VS vIn. unfold Ensembles.In in *.
  repeat invert_eval.
  clear H4.
  apply Squashed_singleton_invert in H2. subst.
Admitted.

(*
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
Admitted. *)

End Rewrites.
