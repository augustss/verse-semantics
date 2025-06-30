Require densem.Dom.
Require Import structures.Sets.
Require Import syntax.common.
Require syntax.mini.
From Stdlib Require Lists.List.
From Stdlib Require Import Classes.EquivDec.

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

Lemma Value_dec ( v1 v2 : Dom.Value) : {v1 = v2} + { not (v1 = v2) }.
Admitted.

#[export] Instance EqDec_Value : EqDec Dom.Value Logic.eq.
exact Value_dec. Defined.

Locate EqDec.

(* Truncate a list to contain at most one element *)
Definition take1 {A} (xs : list A) : list A := 
  match xs with 
  | h :: _ => [ h ] 
  | [] => [] 
  end.


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

Declare Scope env_scope.
Delimit Scope env_scope with env.
Bind Scope env_scope with env.

Module EnvNotation.
Notation "[ x |-> v ]" := (Env.extend x v Env.empty) : env_scope.
End EnvNotation.

Open Scope env_scope.
Import EnvNotation.


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

(* -------------- auxiliary definitions ------------ *)

Definition tups (VS : list VAL) : VAL := 
  fun v => 
    exists (vs : list Dom.Value), v = Dom.Value.mkTup vs 
                             /\ List.Forall2 Sets.In vs VS.

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

(* -------------- Squash and TailSquash ------------ *)

(* Squashed VS WS holds when WS = filter (fun x => x <> ∅) VS

   (We cannot define this as a function because results
   in Type cannot depend on Prop.)
*)
Inductive Squashed {A} : list (P A) -> list (P A) -> Prop := 
  | sq_nil : Squashed nil nil 
  | sq_consIn : forall x xs ys,
    x <> ∅ ->
    Squashed xs ys -> 
    Squashed (cons x xs) (cons x ys)
  | sq_consOut : forall x xs ys,
    x = ∅ ->
    Squashed xs ys -> 
    Squashed (cons x xs) ys.

Search list.

(* The list is either empty, or the last element is inhabited. *)
Definition NonEmptyTail {A} : list (P A) -> Prop := 
  fun VS => 
    match VS with 
    | [] => True 
    | _  => exists WS d, VS = WS ++ [d] /\ d <> ∅
    end.

Definition TailSquashed {A} : list (P A) -> list (P A) -> Prop := 
  fun VS WS => 
    (exists n, VS = WS ++ List.repeat ∅ n) /\ NonEmptyTail WS.

(* ---------- Extended environments for blocks ------------ *)

(* The set of all environments that extend rho with arbitrary 
   definitions for the variables declared in e. 

*)

Search Scope.t bool.
Definition X (e : mini.Expr) (rho : env) : ENV :=
  fun rho' =>
    let sc := mini.I e in 
    forall x, 
      if (Scope.mem x sc) then exists v, rho' x = Scope v
      else rho' x = rho x.
    forall x, 
      (* rho' must agree with rho on variables already in scope *)
      (forall v, rho x = Some v -> rho' x = Some v) /\
      (* rho' must be defined on binding variables of e *)
      (Scope.In x sc -> exists v, rho' x = Some v) /\
      (* rho' shouldn't bind anything else *) 
      (rho x = None /\ not (Scope.In x (mini.I e)) -> rho' x = None).
         
(* --------- (dodgy) unions ------------ *)
         
(* elementwise union of sequences, missing elements
   are ∅s.  *)
Fixpoint unions (VS : list VAL) (WS : list VAL) : 
  list VAL := 
  match VS , WS with 
  | [] , _ => WS
  | _  , [] => VS 
  | V :: VS', W :: WS' => (V ∪ W) :: unions VS' WS' 
  end.

Definition Unions : list (list VAL) -> list VAL :=
  List.fold_right unions nil.
  
Definition UNION (VS : P VAL) : VAL := 
  fun v => exists V, (V ∈ VS) /\ (v ∈ V).

Definition nth (i : nat) (VS : P (list VAL)) : P VAL := 
  fun V => exists (vs : list VAL), (vs ∈ VS) /\ List.nth_error vs i = Some V.


(* every position in VS contains corresponding 
   elements from VVS *)
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


(* --------- (dodgy) unions ------------ *)
                     

(* Type of denotation relation *)
Definition D := forall (rho : env) (e : mini.Expr), (list VAL) -> Prop.


(* check that h does not fail on the input *)
Definition check (eval:D) (rho:env) (q:Aperture) 
  (eff:Effect) i e1 y h (x:Ident) e2 : Prop := 
    (exists vh, rho h = Some vh /\   (* h is in scope *)
      forall v, exists w, (* for every input, there is some result *)
      forall rho', rho' ∈ X e1 (Env.extend i v rho) ->  (* for every extension of rho' *)

        (* evaluating e1 should be defined *)
        exists V1, eval rho' e1 V1 /\

             (* evaluating e1 fails and applying h also fails *) 
             (Squashed V1 [] /\ exists V2, apply vh v = V2 /\ Squashed V2 []) 
              \/

             (* evaluating e1 produces a singleton value vx *)
             (forall vx, Squashed V1 [ ⌈ vx ⌉ ] /\
              exists V2, apply vh vx = V2 /\
                 (* apply h[x] fails, eff must be decides, and e2 must fail *)
                 (Squashed V2 [] /\ (eff = Decides
                                    /\ forall vy, eval (Env.extend y vy rho') e2 [])) 
                 \/
                 (* apply h[x] produces a value vy after k failures *)
                 exists vy k, TailSquashed V2 (List.repeat ∅ k ++ [ ⌈ vy ⌉ ]) /\
                   (* evaluating e2 produces w after k failures *)
                    eval (Env.extend y vy rho') e2 (List.repeat ∅ k ++ [⌈ w ⌉]))).

Inductive eval (rho : env) : mini.Expr -> list VAL -> Prop :=

  | eval_Block e VV VS :
    (* every result has to be decomposable into a union of 
       evaluations of e with an appropriate rho' *)
    (forall WS, WS ∈ VV -> 
            exists rho', (rho' ∈ X e rho) /\ eval rho' e WS) ->
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

Lemma empty_is_empty {A} : forall (S : P A), S = ∅ -> forall x, not (x ∈ S).
Admitted.
Lemma singleton_not_empty {A}{v:A} : ⌈ v ⌉ <> ∅. Admitted.
Lemma Intersection_same {A}{v:P A} : (v ∩ v) = v.  Admitted.
Lemma Intersection_diff {A}{v1 v2:A} : v1 <> v2 -> (⌈v1⌉ ∩ ⌈v2⌉) = ∅. Admitted.
Lemma Intersection_commutes {A}{v1 v2:P A} : (v1 ∩ v2) = (v2 ∩ v1). Admitted.
Lemma notIn_singleton {A}{v : A} : ~ List.In ∅ [⌈ v ⌉]. 
Proof.
  intro h. inversion h.  apply singleton_not_empty in H. done. inversion H.
Qed.


Lemma NonEmptyTail_nil {A} : @NonEmptyTail A [].
done. Qed.
Lemma NonEmptyTail_singleton {A}{v :A}: NonEmptyTail [ ⌈v⌉ ].
cbn. exists nil. exists ⌈v⌉. cbv. split; auto; eapply singleton_not_empty. Qed.

Lemma TailSquashed_singleton {A} (v:A) VS : 
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

Lemma noEmptyInSquashed : forall {A} (xs ys : list (P A)), Squashed xs ys -> 
                                   not (List.In ∅ ys).
Proof.
  induction 1; unfold not; intros. inversion H.
  inversion H1. contradiction. contradiction. contradiction.
Qed.


Lemma Squashed_singleton {A} (v:A) VS : 
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

Lemma Squashed_singleton_invert {A} (v:A) VS : 
  Squashed [⌈ v ⌉] VS -> VS = [⌈ v ⌉].
Proof. intro h. inversion h. subst. inversion H3. auto.
subst. apply singleton_not_empty in H1. done. Qed.
Lemma Squashed_nil_invert {A} (VS : list (P A)) : 
  Squashed [] VS -> VS = [].
Proof. intro h. inversion h. auto. Qed.
Lemma Squashed_empty_invert {A} (VS : list (P A)) : 
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


(* { x:=1; x }  *)
Lemma t2 : 
  eval Env.empty mini.Test.t2 [ ⌈ Dom.Int 1 ⌉ ].
Proof. unfold mini.Test.t2.
       (* set up the block. *)
       eeval1.
       instantiate (1 := mem [ [ ⌈ Dom.Int 1 ⌉] ]).
       intros WS WSIn.
       2: { rewrite UNIONS_mem. cbn. eapply in_singleton. } 
       apply mem_In in WSIn. cbn in WSIn.
       destruct WSIn. 
       - subst.
         exists [ mini.Test.x |-> Dom.Int 1 ].
         split; auto.
         ++ intros y.
            split. intros sv h1. done.
            split. intros h1. cbv in h1. inversion h1. subst.
            eexists. cbv. eauto.
            subst. inversion H0.
            intros [h1 h2]. cbv in h2. 
            cbv. destruct y eqn:Ey. done.
            destruct i eqn: Ei. assert False. eapply h2. eauto. done. done.
         ++ eeval.
       - done.
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

Lemma NonEmptyTail_app {A} ( VS1 VS2 : list (P A)) : 
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
