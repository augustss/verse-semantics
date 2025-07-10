Require Import Imports.

From Stdlib Require Lists.List.
From Stdlib Require Import Classes.EquivDec.
Import ssreflect.

Require Import syntax.common.
Require syntax.mini.
Require densem.Dom.
Require Import structures.Sets.
Import structures.List.
Import structures.Monad.

Import mini.MiniNotation.
Import MonadNotation.
Import SetNotations.
Import List.ListNotations.

Open Scope monad_scope.
Open Scope list_scope.
Open Scope mini_expr_scope.

(* Truncate a list to contain at most one element *)
Definition take1 {A} (xs : list A) : list A := 
  match xs with 
  | h :: _ => [ h ] 
  | [] => [] 
  end.

(* --------------------------------------------------- *)


Definition VAL := P Dom.Value.

(* --------------- (total) environments ----------- *)

Definition tenv := Ident -> Dom.Value.

Module TEnv.

Definition empty : tenv := fun x => Dom.Int 0.

Definition extend : Ident -> Dom.Value -> tenv -> tenv := 
  fun x v rho => 
    fun y => if Nat.eqb x y then v else rho y.

End TEnv.

Declare Scope tenv_scope.
Delimit Scope tenv_scope with tenv.
Bind Scope tenv_scope with tenv.

Module TEnvNotation.
Notation " x |-> v " := (TEnv.extend x v TEnv.empty) (at level 80) : tenv_scope.
Notation " x |-> v , e " := (TEnv.extend x v e) (at level 80, right associativity): tenv_scope. 
End TEnvNotation.

(* --------------- (partial) environments ----------- *)

(* gives a value for *in-scope* identitfiers *)
(* The denotation of out of scope identifiers is NOT defined. *)
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
Notation " x |-> v " := (Env.extend x v Env.empty) (at level 80) : env_scope.
Notation " x |-> v , e " := (Env.extend x v e) (at level 80, right associativity): env_scope. 

Section NotationExamples.
Open Scope env_scope.
Variable x y : Ident.
Variable v w : Dom.Value.
Check  x |-> v.
Check  x |-> v, y |-> w.
End NotationExamples.

End EnvNotation.



(* -------------- primitives and atomic expressions ------ *)

Definition evalPrim (p : PrimOp) : Dom.Value -> Prop := 
  match p with 
  | common.Add  => Dom.add1
  | ArrayLen => Dom.arrayLen
  | IsInt => Dom.isInt
  | IsArr => Dom.isArr
  | IsFun => Dom.isFun
  | _ => fun v => False    (* TODO: Lt, etc. *)
  end.

(* Total evaluation function for atomic expressions. 
   If e is not atomic, it is interpreted as 0 *)
Definition evalA (e : mini.Expr) (ρ : tenv) : Dom.Value := 
  match e with 
  | mini.Var x => ρ x
  | mini.Lit (Int i) => Dom.Int i
  | _ => Dom.Int 0
  end.


(* -------------- auxiliary definitions ------------ *)

(* Create a tuple from a list of VALs *)
Definition tups (VS : list VAL) : VAL := 
  fun v => 
    exists (vs : list Dom.Value), v = Dom.Value.mkTup vs 
                             /\ List.Forall2 Sets.In vs VS.

(* apply f to x. 
   F is a list of partial functions, corresponding to 
   iteration on the input. Any inputs that are in the 
   domain of the function will produce output.
 *)
Definition apply (f : Dom.Value) 
                 (v : Dom.Value) : list VAL := 
  match f with 
  | Dom.Fun hs => 
      h <- hs ;;
      match (PFun.apply_opt _ _ Dom.Value.eqb h v) with 
      | Some w => [ ⌈ w ⌉ ]
      | None => [ ] 
      end
  | _ => []
  end.


(* ----------- dest passing style --------------------- *)

Module DPS.

Open Scope tenv_scope.
Import TEnvNotation.
Import Dom.

(* Operations on Sets of environments. *)

Definition ENV := P tenv.

Definition M A := P (list A).

(* total set of environments *)
Definition Env : ENV := fun ρ => True. 

(* Constrain a variable to be equal to a particular value.
   All other mappings in the environment are unconstrained. *)
Definition constrain (x : Ident) (f : tenv -> Value) : ENV := 
  fun ρ => ρ x = f ρ.

Infix "≈" := constrain (at level 60).

(* Generalize all of the xs to be anything *)
Definition hide (xs : Scope.t) (Δ : ENV) : ENV := 
  fun ρ => exists ρ', (ρ' ∈ Δ) /\ 
              forall x, ~ (Scope.In x xs) -> (ρ x = ρ' x).

Definition liftA1 {A} (f : A -> A) : P A -> P A := fmap f.
Definition liftA2 {A} (f : A -> A -> A) : P A -> P A -> P A := 
  fun D1 D2 => 
  d1 <- D1 ;; 
  d2 <- D2 ;;
  ⌈ f d1 d2 ⌉.

(* Semantic operations *)

Definition FAIL : P (list ENV) := ⌈ [] ⌉.

Definition CHOICE (d1 : list ENV) (d2: list ENV) : list ENV := 
      d1 ++ d2.


Definition SEQ (r:Ident) (d1 : list ENV) (d2: list ENV) : list ENV := 
  ρ1 <- d1 ;;
  ρ2 <- d2 ;;
  [ hide (Scope.singleton r) ρ1 ∩ ρ2 ].

Definition UNIFY (d1 : list ENV) (d2: list ENV) : list ENV := 
  ρ1 <- d1 ;;
  ρ2 <- d2 ;;
  [ ρ1 ∩ ρ2 ].


Fixpoint eval (e : mini.Expr) (r : Ident) : P (list ENV) := 
  match e with 

  | mini.Block e =>  liftA1 (map (hide (mini.I e))) (eval e r)

  | mini.Var _ => ⌈[ r ≈ evalA e ]⌉

  | mini.Lit _ => ⌈[ r ≈ evalA e ]⌉

  | mini.DefineV _ => ⌈[ Env ]⌉

  | mini.Array es => 
      ⌈[ r ≈ (fun ρ => Value.mkTup (fmap (fun e => evalA e ρ) es)) ]⌉

  | mini.Fail => FAIL

  | mini.Choice e1 e2 => liftA2 CHOICE (eval e1 r) (eval e2 r)

  | mini.Seq e1 e2 => liftA2 (SEQ r) (eval e1 r) (eval e2 r)

  | mini.Unify e1 e2 => liftA2 UNIFY ⌈[r ≈ evalA e1]⌉ (eval e2 r)

(*
  | mini.All e1 => fun rhos =>
      d1 <- eval e1 r 
      *)
  | _ => fun _ => False
  end.


(* -------------------------------------------------------- *)
(* -------------------------------------------------------- *)
(* -------------------------------------------------------- *)

(* -------------- Squash ------------ *)


(* Squashed VS WS holds when WS := filter (fun x => x <> ∅) VS

   NOTE: We cannot define this as a function because results
   in Type cannot depend on Prop. But this relation 
   is total and functional.
*)
Inductive Squash {A} : list (P A) -> list (P A) -> Prop := 
  | sq_nil : Squash nil nil 
  | sq_consIn : forall x xs ys,
    x <> ∅ ->
    Squash xs ys -> 
    Squash (cons x xs) (cons x ys)
  | sq_consOut : forall x xs ys,
    x = ∅ ->
    Squash xs ys -> 
    Squash (cons x xs) ys.

(* A success is any list that contains at least one nonempty VAL *)
Definition Succeeds (VS : P (list VAL)) : Prop :=
  forall l, l ∈ VS -> exists v, v <> ∅ /\ List.In v l.

Lemma Squash_functional {A} (VS : list (P A)) : forall WS1 WS2,
      Squash VS WS1 -> Squash VS WS2 -> WS1 = WS2.
Proof. 
  induction VS; intros WS1 WS2 h1 h2; inversion h1; inversion h2; subst.
  all: try done.
  all: eauto.
  - erewrite (IHVS ys ys0); auto.
Qed.

(* NOTE: Showing that Squash is functional requires classical reasoning. 
   We need to know that 

       forall any S, S = ∅ \/ S <> ∅

   In order to prove this lemma:
*)

Lemma Squash_total {A} (decide_equality : forall (S:P A), S = ∅ \/ S <> ∅) 
  : forall (VS : list (P A)), exists WS, Squash VS WS.
Proof.
  induction VS. exists nil. eapply sq_nil.
  move: IHVS => [WS' IHWS'].
  destruct (decide_equality a).
  + exists WS'. eapply sq_consOut; auto.
  + exists (a :: WS'). eapply sq_consIn; auto.
Qed.

(* -------------- SquashTail ------------ *)

(* The list is either empty, or the last element is inhabited. *)
Definition NonEmptyTail {A} : list (P A) -> Prop := 
  fun VS => 
    match VS with 
    | [] => True 
    | _  => exists WS d, VS = WS ++ [d] /\ d <> ∅
    end.

Definition TailSquash {A} : list (P A) -> list (P A) -> Prop := 
  fun VS WS => 
    (exists n, VS = WS ++ List.repeat ∅ n) /\ NonEmptyTail WS.

(* ---------- Extended environments for blocks ------------ *)


Open Scope env_scope.
Import EnvNotation.


Definition ENV := P env.

(* The set of all environments that extend rho with arbitrary 
   definitions for the variables declared in e. 

*)

Definition X (e : mini.Expr) (rho : env) : ENV :=
  fun rho' =>
    forall x, 
      if (Scope.mem x (mini.I e)) 
      then exists v, rho' x = Some v    (* arbitrary value for vars in new scope *)
      else rho' x = rho x.         (* same value for vars in old scope *)



(* --------- (dodgy) unions ------------ *)
         
(* elementwise union of sequences, missing elements are ∅s.  *)
Fixpoint unions {A} (VS : list (P A)) (WS : list (P A)) : 
  list (P A) := 
  match VS , WS with 
  | [] , _ => WS
  | _  , [] => VS 
  | V :: VS', W :: WS' => (V ∪ W) :: unions VS' WS' 
  end.

Definition Unions {A} : list (list (P A)) -> list (P A) :=
  List.fold_right unions nil.

(* every position in VS contains corresponding 
   elements from VVS *)
Definition UNIONS : P (list VAL) -> P (list VAL) := 
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


(* ----------------- version: fixpoint ----------- *)

Module FixpointVersion.

(* In this version, the denotation is defined as a fixpoint 
   over the syntax of miniverse expressions, taking an 
   environment as an argument. 

   The result of this function is a (potentially-infinite) 
   set of sequences of sets of values. i.e. 

     P (list VAL) 

   The set plays two roles:

    - allows us to eliminate VAL in determining the list
      (list in Rocq may not depend on Prop).

    - allow the result of evaluation to be "undefined" in 
      the case of an unbound variable, type error, etc.
   
   Generally, this set will either be an empty set or a 
   singleton set.

*)

Definition M A := P (list A).

Definition TestEmpty {A B} (e1 : P A) (e2 : P (list B)) (e3 : P (list B)) := 
  fun bs => 
    (e1 = ∅ -> e2 bs) /\ (e1 <> ∅ -> e3 bs).

Definition evalA (rho : env) (e : mini.Expr) : VAL := 
  match e with 
  | mini.Var x => fun v => rho x = Some v
  | mini.Lit (Int i) => ⌈ Dom.Int i ⌉
  | mini.EPrim p => evalPrim p
  | _ => fun v => False
  end.

(* empty if x is empty, singleton if x is singleton. *)
Definition retA (x : VAL) : P (list VAL) := 
  TestEmpty x ∅ ⌈ [ x ] ⌉.
(*
  fun xs => 
    (exists v, x = ⌈ v ⌉ /\ xs = [ ⌈ v ⌉ ]). *)

Fixpoint E_e (rho : env) (e : mini.Expr) : P (list VAL) := 
  
  (* extend the environment with all possible versions, producing 
     multiple results in the set *)
  let D' (rho : env) (e : mini.Expr)  : P (list VAL) :=
    UNION (fun VS => rho' <- X e rho ;; E_e rho' e = VS) in

  (* extend the environment with all possible versions, 
     dodgy unioning the results together into a set 
     containing at most 1 result *)
  let D (rho : env) (e : mini.Expr )  : P (list VAL) :=
    UNIONS (rho' <- X e rho ;; E_e rho' e ) in

  (* all extended environments that succeed when evaluating e *)
  let R (rho : env) (e : mini.Expr) : ENV := 
    rho' <- X e rho  ;;
    VS <- E_e rho' e ;;
    ⌈ rho' ⌉ in
  
  match e with 
  | mini.Block e => D rho e

  | mini.Var _ => retA (evalA rho e)
  | mini.Lit _ => retA (evalA rho e)
  | mini.EPrim _ => retA (evalA rho e)

  | mini.DefineV x => pure [ ⌈ Dom.Value.mkTup nil ⌉ ] 

  | mini.Array es => fun VS => 
     exists vs, (List.Forall2 (evalA rho) es vs) /\ VS = [ ⌈ Dom.Value.mkTup vs ⌉ ]

  | mini.Seq e1 e2 =>
     VS1  <- E_e rho e1 ;;    (* outer set monad *)
     VS1' <- Squash VS1 ;;
     VS2  <- E_e rho e2 ;;  
     pure (s1 <- VS1' ;; s2 <- VS2 ;; [s2] )  (* inner list monad *)

  | mini.Unify e1 e2 => 
     VS1 <- E_e rho e1 ;;
     VS2 <- E_e rho e2 ;;
     pure ( s1 <- VS1 ;; s2 <- VS2 ;; [s1 ∩ s2] )

  | mini.Choice e1 e2 =>
     VS1 <- D rho e1 ;;
     VS2 <- D rho e2 ;;
     pure (VS1 ++ VS2)

  | mini.Fail => 
      pure []

  | mini.One e => 
      VS1 <- E_e rho e ;;
      VS2 <- Squash VS1 ;;
      pure (take1 VS2)

  | mini.All e => 
      VS1 <- E_e rho e ;;
      VS2 <- Squash VS1 ;;
      pure [tups VS2]

  | mini.If3 e1 e2 e3 =>
      let Δ := R rho e1 in
      TestEmpty Δ (D rho e3)
                  (UNIONS (rho' <- Δ ;; E_e rho' e2))
  | _ => empty
  end.

End FixpointVersion.


(* ----------------- version two: Inductive relation ----------- *)


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
             (Squash V1 [] /\ exists V2, apply vh v = V2 /\ Squash V2 []) 
              \/

             (* evaluating e1 produces a singleton value vx *)
             (forall vx, Squash V1 [ ⌈ vx ⌉ ] /\
              exists V2, apply vh vx = V2 /\
                 (* apply h[x] fails, eff must be decides, and e2 must fail *)
                 (Squash V2 [] /\ (eff = Decides
                                    /\ forall vy, eval (Env.extend y vy rho') e2 [])) 
                 \/
                 (* apply h[x] produces a value vy after k failures *)
                 exists vy k, TailSquash V2 (List.repeat ∅ k ++ [ ⌈ vy ⌉ ]) /\
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
    TailSquash (apply h v) VS ->
    eval rho (mini.ApplyD a1 a2) VS

  | eval_Seq e1 e2 VS1 VS1' VS2 VS :
    eval rho e1 VS1 -> 
    Squash VS1 VS1' ->
    eval rho e2 VS2 -> 
    not (List.In ∅ VS1) ->
    TailSquash ( s1 <- VS1' ;; s2 <- VS2 ;; [ s2 ] ) VS ->
    eval rho (mini.Seq e1 e2) VS
         

  | eval_Unify e1 e2 VS1 VS2 VS3 : 
    eval rho e1 VS1 -> 
    eval rho e2 VS2 ->
    TailSquash (s1 <- VS1 ;; s2 <- VS2 ;; [ s1 ∩ s2 ] ) VS3 ->
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
    Squash VS1 VS2 ->
    take1 VS2 = VS ->
    eval rho (mini.One e) VS

  | eval_All e VS1 VS2 :
    eval rho e VS1 ->
    Squash VS1 VS2 -> 
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
    Squash [ F ] FS ->  (* if F is emptyset, get rid of it *)
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

Lemma TailSquash_singleton {A} (v:A) VS : 
  VS = [⌈ v ⌉] ->
  TailSquash [⌈ v ⌉] VS.
Proof. intros ->. unfold TailSquash. split. exists 0. cbn. auto.
eapply NonEmptyTail_singleton; eauto. Qed.

Lemma TailSquash_nil (VS : list VAL) : 
  VS = nil ->
  TailSquash [] VS.
Proof. intros ->. unfold TailSquash. split. exists 0. cbn. auto.
eapply NonEmptyTail_nil; eauto. Qed.

Lemma TailSquash_empty (VS : list VAL) : 
  VS = nil ->
  TailSquash [∅] VS.
Proof. intros ->. unfold TailSquash. split. exists 1. cbn. auto.
eapply NonEmptyTail_nil; eauto. Qed.

Lemma noEmptyInSquash : forall {A} (xs ys : list (P A)), Squash xs ys -> 
                                   not (List.In ∅ ys).
Proof.
  induction 1; unfold not; intros. inversion H.
  inversion H1. contradiction. contradiction. contradiction.
Qed.


Lemma Squash_singleton {A} (v:A) VS : 
  VS = [⌈ v ⌉] ->
  Squash [⌈ v ⌉] VS.
Proof. intros ->. eapply sq_consIn; eauto using singleton_not_empty.
       eapply sq_nil. Qed.

Lemma Squash_nil (VS : list VAL) : 
  VS = nil ->
  Squash [] VS.
Proof. intros ->. eapply sq_nil. Qed.

Hint Resolve 
  singleton_not_empty 
  NonEmptyTail_nil 
  NonEmptyTail_singleton 
  notIn_singleton
  TailSquash_singleton
  TailSquash_nil
  TailSquash_empty
  Squash_singleton
  Squash_nil
 :sets.

Lemma Squash_singleton_invert {A} (v:A) VS : 
  Squash [⌈ v ⌉] VS -> VS = [⌈ v ⌉].
Proof. intro h. inversion h. subst. inversion H3. auto.
subst. apply singleton_not_empty in H1. done. Qed.
Lemma Squash_nil_invert {A} (VS : list (P A)) : 
  Squash [] VS -> VS = [].
Proof. intro h. inversion h. auto. Qed.
Lemma Squash_empty_invert {A} (VS : list (P A)) : 
  Squash [∅] VS -> VS = [].
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

Coercion Dom.Int : nat >-> Dom.Value.

(*   {y:=if(x=1){0|1}else{2|3}; x:=0|1; y}  =?= 2|0|3|1 *)

Definition y := mini.Test.y.
Definition x := mini.Test.x.

Definition lennart := 
  { y :=: mini.If3 (mini.Unify x 1) (0 :|: 1) (2 :|: 3) :>: 
    x :=: (0 :|: 1)  :>:
    y }.

Lemma dodgy : eval Env.empty lennart 
                ([ ⌈ Dom.Int 2 ⌉ ; ⌈ Dom.Int 0 ⌉ ; ⌈ Dom.Int 3 ⌉ ; ⌈ Dom.Int 1 ⌉ ]).
Proof.
  unfold lennart.
  eeval.
  instantiate (1 := 
    (mem [ [ ⌈ Dom.Int 2 ⌉ ; ⌈ Dom.Int 0 ⌉ ; ⌈ Dom.Int 3 ⌉ ; ⌈ Dom.Int 1 ⌉ ] ;
           [ ⌈ Dom.Int 2 ⌉ ; ⌈ Dom.Int 0 ⌉ ; ⌈ Dom.Int 3 ⌉ ; ⌈ Dom.Int 1 ⌉ ] ])).
  intros WS WSIn.
  apply mem_In in WSIn.
  destruct WSIn.
  - (* first option *)
    exists ( (Env.extend y (Dom.Int 2) (Env.extend x (Dom.Int 1) Env.empty))).
Admitted.


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
         exists (mini.Test.x |-> Dom.Int 1 ).
         split; auto.
         ++ intros y.
            destruct Scope.mem eqn:IN.
            -- (* in new scope *) 
              unfold mini.I in IN.
              cbn in IN. fold Nat.compare in IN.
              destruct Nat.compare eqn:CMP; try done.
              apply PeanoNat.Nat.compare_eq in CMP. subst.
              eexists. cbn. eauto. 
            -- (* in old scope *)
              unfold mini.I in IN.
              cbn in IN. fold Nat.compare in IN.
              destruct Nat.compare eqn:CMP; try done.
              apply PeanoNat.Nat.compare_lt_iff in CMP.
              admit.
              admit.
         ++ eeval.
       - done.
Admitted.



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
Admitted.
(*
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
*)
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
    | [ H : TailSquash _ ?VS |- _ ] => move: H => [ _ h1 ] 
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
    | [ H : TailSquash _ ?VS |- _ ] => move: H => [ _ h1 ] 
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
  apply Squash_singleton_invert in H2. subst.
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
