Require Import Imports.

From Stdlib Require Lists.List.
From Stdlib Require Import Classes.EquivDec.
Import ssreflect.

From Stdlib Require Import Logic.PropExtensionality.
From Stdlib Require Import Logic.FunctionalExtensionality.
From Stdlib Require Import Sets.Classical_sets.

Require Import syntax.common.
Require syntax.mini.
Require Import PFun.
Require Import structures.Sets.
Import structures.List.

Require Export densem.Dom.
Require Export densem.tenv.  (* environments are total *)
Require Export densem.envSet. (* def of ENV , hide, constraints *)
Require Export densem.squash.

Import mini.MiniNotation.
Import SetNotations.
Import SetMonadNotation.
Import List.ListNotations.
Import ListMonadNotation.
Import EnvNotation.
Import envSetNotation.

Open Scope list_scope.
Open Scope mini_expr_scope.
Open Scope env_scope.
Open Scope set_scope.


(* ---------------------------------------------- *)



Ltac set_crunch :=
    crunch ; repeat match goal with 
    | [ H : ?ρ ∈ (Sets.bind ?ma ?k) |- _ ] =>
        let ρ1 := fresh ρ in
        move: H => [ρ1 H]; crunch
    | [ H : ?ρ ∈ (Sets.seq ?s1 ?s2) |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ (Sets.map ?f ?s) |- _ ] =>
        let ρ1 := fresh ρ in
        move: H => [ρ1 H]; crunch
    | [ H : ?ρ ∈ ⌈?v ⌉ |- _ ] =>
        inv H; crunch
    | [ H : ⌈?v ⌉ ?ρ |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ (?s1 ∩ ?s2) |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ (when ?x ?k) |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ ∅ |- _] => 
        inv H
    | [ H : ?ρ ∈ (fun x => _ ) |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ (?x ≈ ?k) |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ (?x ≉ ?k) |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ (hide ?s ?S) |- _ ] =>
        inv H; crunch
    | [ H : ?ρ ∈ ∅ |- _ ] =>
        inv H
      end.

(* --------------------------------------------------- *)

Notation VAL := (P value).

Import mini.MiniNotation.
Notation "⟅ r ⟆" := (Scope.singleton r).
(* distinguished result variable (0) *)
Definition r : Ident := mini.Test.r.

(* -------------- atomic expressions  ---------------- *)

Definition evalPrim (p : PrimOp) : value  := 
  match p with 
  | common.Add  => Prim.add1
  | common.ArrayLen => Prim.arrayLen
  | common.IsInt => Prim.isInt
  | common.IsArr => Prim.isArr
  | common.IsFun => Prim.isFun
  | _ => Dom.Int 0  (* others primitives *)
  end.

(* Evaluation function for simple values. 
   If e is not of the right form, it is interpreted as 0 *)
Fixpoint evalA (e : mini.Expr) (ρ : env) : value := 
  match e with 
  | mini.Var x => ρ x
  | mini.Lit (common.Int i) => Dom.Int i
  | mini.EPrim p => evalPrim p
  | mini.Array es => mkTup (List.map (fun e => evalA e ρ) es)
  | _ => Dom.Int 0
  end.

(* ------- operations on lists of sets and sets of lists ------ *)

Definition SUCCEED {A} : list (P A) := [Total_set]. 

Definition FAIL {A} : list (P A) := [].

Definition CHOICE {A} (d1 : list A) (d2: list A) : list A := 
  d1 ++ d2.

Definition UNIFY {A} (d1 : list (P A)) (d2: list (P A)) : list (P A) := 
  ρ1 <- d1 ;;
  ρ2 <- d2 ;;
  [ρ1 ∩ ρ2].

Definition MINUS {A} (d1 : list (P A)) (d2: list (P A)) : 
  list (P A) := 
  ρ1 <- d1 ;;
  ρ2 <- d2 ;;
  [ρ1 - ρ2].
 
Definition IF2 {A B} (d1 : list (P A)) (d2: list (P B)) : list (P B) := 
  ρ1 <- d1 ;;
  ρ2 <- d2 ;;
  [If2 ρ1 ρ2].

Infix "*" := UNIFY.

Definition UNIONLIST {A} : list (P A) -> (P A) := 
  List.fold_right Union ∅.


(* --------- pick/sequence ----------- *)

(* This operation is liftM2 snoc *)
Definition Snoc {A} := (fun (VS : P (list A)) (V : P A) => 
    vs ⭅ VS ;;  v  ⭅ V  ;; ⌈ vs ++ [v]⌉).
   
(* The set of all first elements from a list. *)
(*   {{ v | (v :: _) <- V }}   *)
Definition Head {A} (V : P (list A)) : P A := 
  vs ⭅ V ;;
  match vs with 
  | v :: _ => ⌈ v ⌉
  | _ => ∅
  end. 

Fixpoint pick {A} (xs : list (P A)) : P (list A) := 
  match xs with 
  | nil => ⌈ [] ⌉
  | V :: VS => v  ⭅ V ;; vs ⭅ pick VS ;; ⌈ v :: vs⌉ 
  end.

(* pick / 
   this is the list instance of 'sequence' from Haskell's Traversable class *)
Fixpoint pickl {A} (xs : list (list A)) : list (list A) := 
  match xs with 
  | nil => [ [] ]
  | V :: VS =>  v <- V ;; vs <- pickl VS ;; [ v :: vs ] 
  end.

(* --------- dodgy union ----------- *)
(* elementwise union of sequences, missing elements are ∅s.  *)
Fixpoint pointwise_union {A} (VS : list (P A)) (WS : list (P A)) : 
  list (P A) := 
  match VS , WS with 
  | [] , _ => WS
  | _  , [] => VS 
  | V :: VS', W :: WS' => (V ∪ W) :: pointwise_union VS' WS' 
  end.

Definition Pointwise_Unions {A} : list (list (P A)) -> list (P A) :=
  List.fold_right pointwise_union nil.         

(* another version of the dodgy big union *)
Definition DODGY_UNIONS {A} (VVS : P (list (P A))) : list (P A) := 
  i <- allNums ;;
  let VS : P (P A) := Sets.map (fun VS => List.nth i VS ∅) VVS in
  [ ⨃ VS ].

Infix "⩅" := pointwise_union (at level 70).

(* -------------- tuples and application ------ *)

(* Create a tuple from a list of VALs *)
Definition tups (VS : list VAL) : VAL := 
  fun v => 
    exists (vs : list value), 
      v = mkTup vs /\ List.Forall2 (fun x y => x ∈ y) vs VS.

(* apply domain function value f to x 
   f is a list of partial functions, corresponding to 
   iteration on the input. Any inputs that are in the 
   domain of the function will produce output.
 *)
Definition apply (f : value) (v : value) : list VAL := 
  match f with 
  | Dom.Fun hs => 
      h <- hs ;;
      match (PFun.apply_opt _ _ Value.eqb h v) with 
      | Some w => [⌈w⌉]
      | None => []
      end
  | _ => []
  end.

(* Two different versions of application. Arguments must be atomic, 
   i.e. have a single value
   The set of all environments such that the ith result of 
   apply e1 to e2 is ρ r. Not sure yet which one of these is easier to 
   reason about.
*)
Definition APPi (e1 : mini.Expr) (e2 : mini.Expr) (i : nat) : ENV := 
  fun ρ => 
    List.nth_error (apply (evalA e1 ρ) (evalA e2 ρ)) i = Some (⌈ ρ r ⌉).

Definition APPi' (e1 : mini.Expr) (e2 : mini.Expr) (i : nat) : ENV := 
  fun ρ => 
    exists hs h, evalA e1 ρ = Fun hs 
            /\ List.nth_error hs i = Some h 
            /\ List.In (evalA e2 ρ , ρ r ) h.

(* This is a bit dodgy by using the iteration over all numbers. 
   We need to iterate only over each partial function *)
Definition APP (e1 : mini.Expr) (e2 : mini.Expr) : list ENV := 
  i <- allNums ;;
  [APPi e1 e2 i].

(* --- semantics of ALL / ONE for dest passing --------- *)

(* find all environments in Δ such that ρ(r) = v, then hide r *)

(* {{  (v, Δ') | ρ ∈ Δ , v = ρ.r, Δ' = (Δ ∩ {{ r = v }}) \ r  }} *)
Definition extract r (Δ : ENV) : P (value * ENV) := 
  ρ ⭅ Δ ;;
  ⌈ (ρ r , (Δ ∩ r ≈ ⟨ρ r⟩) \ ⟅r⟆) ⌉.

(* The set of results in Δ that could be produced by environments 
   consistent with ρ *)
(* {{ v | (v , Δ') ∈ extract Δ , ρ ∈ Δ ' }} *)
Definition consistent_results (ρ : env) (Δ : ENV) : VAL := 
  '(v, Δ') ⭅ extract r Δ ;; when (ρ ∈ Δ') ⌈v⌉.

Definition ALL (Δs : list ENV) : ENV := 
  fun ρ => exists vs,
      (vs ∈ squash_pick (List.map (consistent_results ρ) Δs)) /\
      (ρ r = mkTup vs).
       
Definition ONE (Δs : list ENV) : ENV := 
  fun ρ => 
      (ρ r ∈ Head (squash_pick (List.map (consistent_results ρ) Δs))).

(* --- other definitions --------- *)

Definition SEQ (d1 : list ENV) (d2: list ENV) : list ENV := 
  (d1 [\] ⟅ r ⟆) * d2.

(* This character C-X 8 ret U+2A3E,  or \fcmp  *)
(* Infix "⨾" := seq (at level 70, left associativity) : list_scope. *)


(* The semantics of 0..x 
   [ {{r=1,x>=1}}, {{r=2,x>=2}}, ... ]
   [ {{r=i,x>=i}} | i <- [0 ..] ]

 *)
Definition enumTo x : list ENV := 
  i <- allNums ;;
  [ (fun ρ => (ρ r = Int i) /\ exists n, (ρ x = Int n) /\ n >= i) ].

Fixpoint combine (xs : list ENV) : P (list value * ENV) := 
    match xs with 
    | [] => ⌈ ([], Total_set) ⌉
    | (Δi :: rest) => 
        let failΔi '(vs, Δ) := (vs, (Total_set - Δi) ∩ Δ) in 
        (Sets.map failΔi (combine rest)) ∪

        ('(vi, Δi') ⭅ extract r Δi ;;
         let succΔi '(vs, Δ) := (vi :: vs, Δi' ∩ Δ) in
          Sets.map succΔi (combine rest))
    end.


(* ----- semantics of IF ------------ *)

(* Tim's version of IF *)

(* Given a list of ENVs,  E{a}[0] ... E{a}[n]

   If any of these succeed, we want to take the first one.
   So, we will produce a list of environments, where the first one 
   is the first one, but the second needs to know that the 
   first one failed. So we use set difference to subtract the first 
   set from the second.

   NB: not sure what is going on with hiding here.

 ([E{a}[0]                                      ]             + 
  [E{a}[1] \{xs} E{a}[0]                        ]             + … +
  [E{a}[n] \{xs} E{a}[0] \{xs} … \{xs} E{a}[n-1]])
*)


(* Tims version in Euv semantics 

Euv{if(a){b}else{c}} := 
   [A[0], 
    A[1]\A[0], ..., 
    A[n]\A[0]\A[1]\...\ A[n-1]] * B + 
   [P(Env)\A[0]\A[1]\...\A[n]] * C
where A:=Epq{a}-a -{p,q}
where B:=Euv{b}-a-b-{p,q}
where C:=Euv{c}-a-c-{p,q}
where p&q fresh, n:=Length(Epq{a})

Questions:
1. should it be
   where C:=Euv{c}-c
   i.e. variables bound in a don't scope over c
        p,q are fresh for c, and not input to Euv
2. Aren't we subtracting a variables from A and B too early?
   They need to communicate. We should only subtract
   after the * has been calculated.
2'. But, the a variables aren't bound in C, so we can 
   subtract them before we do the unification
3. Let's replace A[i]\A[0]...\A[i-i] with 
     A[i]\(A[0] ∪ ... ∪ A[i-1])

Proposed update:

Euv{if(a){b}else{c}} := 
   ([A[0], 
     A[1]\A[0], 
     ..., 
     A[n]\(A[0] ∪ A[1] ... ∪ A[n-1])] * B)-a + 
   [P(Env)\(A[0] ∪ A[1] ... ∪ A[n])-a] * C
where A:=Epq{a}-{p,q}
where B:=Euv{b}-b
where C:=Euv{c}-c
where p&q fresh, n:=Length(Epq{a})

*)


Fixpoint try (A : list ENV) : list ENV * ENV := 
  let step := fun '(envs, avoid) Ai => 
                (envs ++ [Ai - avoid], avoid ∪ Ai) in
  List.fold_left step A ([],∅).

Definition IF_TIM1 a (A B C : list ENV) : list ENV := 
    let (success, avoid) := try (A [\] ⟅r⟆ [\] a) in 
     (success * (B [\] a) ) ++
     ([(Total_set - avoid)] * C).

Definition IF_TIM2 a (A B C : list ENV) : list ENV := 
    let (success, avoid) := try (A [\] ⟅r⟆) in 
     ((success * B) [\] a) ++
     ([(Total_set - avoid) \ a] * C).

(* Koen's encoding of if *)

(*
if e1 e2 e3 =
  exists y; 
  y = one{ (e1; z=⟨⟩) | z=0 }@z;
  (y=⟨⟩; e2) | (y=0; e3)
*)

(* Translated to DPS style, with distinguished result r *)
Definition koen_if y e1 e2 e3 : mini.Expr := 
  mini.DefineV y :>:
  y :=: mini.One ( (e1 :>: (r :=: mini.Array [])) :|: r :=: 0 ) :>:
  (y :=: mini.Array [] :>: e2) :|: (y :=: 0 :>: e3).

Definition IF_KOEN y (xs:Scope.t) (S1 S2 S3 : list ENV) : list ENV :=
  let D1 := ONE( SEQ S1 [r ≈ ⟨mkTup []⟩] ++ [ r ≈ ⟨Int 0⟩]) in
  SEQ [ (y ≈ fun ρ => ρ r) ∩ D1 ]
      (SEQ [y≈⟨mkTup[]⟩] S2) ++ (SEQ [y ≈⟨Int 0⟩] S3).

(* Simon's version of If *)

Definition IF_SPJ (xs:Scope.t) (S1 S2 S3 : list ENV) : list ENV :=
    let GOOD := UNIONLIST (S1 [\] ⟅ r ⟆) in
    ((List.map (fun D => D ∩ GOOD) S2) [\] xs) ++ 
    (List.map (fun D => D - GOOD) S3).

(* ----------- non-dodgy dest passing style -------------- *)

Module DLS. 

Fixpoint E (e : mini.Expr) : list ENV := 

  let B (e : mini.Expr) :list ENV := 
    Δ <- E e ;; [Δ \ mini.I e] 
  in
  let V (e : mini.Expr) : list (P (value * ENV)) := 
    Δ <- B e ;; [extract r Δ] 
  in

  match e with 
  | mini.DefineV _ => [ Total_set ]

  | mini.Var _ => [ r ≈ evalA e ]
  | mini.Lit _ => [ r ≈ evalA e ]
  | mini.EPrim _ => [ r ≈ evalA e ]
  | mini.Array es =>  [ r ≈ evalA e ]

  | mini.Fail => []  

  | mini.Choice e1 e2 => B e1 ++ B e2

  | mini.Unify e1 e2 => 
      E e1 * E e2

  | mini.Seq e1 e2 => 
      (E e1 [\] Scope.singleton r) * (E e2) 

  | mini.ApplyD e1 e2 => APP e1 e2

  | mini.If3 a b c =>  

    let xs := mini.I a in 
    IF_TIM1 xs (E a) (B b) (B c)


  (* TODO: should this be E or B *)
  | mini.All a => 
      [ ALL (E a) \ mini.I a ] 

  | mini.One a => 
      [ ONE (E a) \ mini.I a ]

  (* TODO: functions *)

  | _ => [ ] 
  end.

Definition  B (e : mini.Expr) :list ENV := 
    Δ <- E e ;; [Δ \ mini.I e]. 

Lemma E_Var (i:Ident) : E i = [ fun ρ => ρ r = ρ i ]. reflexivity. Qed.
Lemma E_One e : E (mini.One e) = [ ONE (E e) \ mini.I e ]. reflexivity. Qed.
Lemma E_Choice e1 e2 : E (e1 :|: e2) = (B e1) ++ (B e2). reflexivity. Qed.
Lemma E_Seq e1 e2 : E (e1 :>: e2) = (E e1 [\] ⟅r⟆) * E e2. reflexivity. Qed.
Lemma E_Unify e1 e2 : E (e1 :=: e2) = E e1 * E e2. reflexivity. Qed.

Create HintDb E.
Hint Rewrite E_Var E_One E_Choice E_Seq E_Unify : E.

End DLS.


(* ------------------------------------------------------ *)
(* ------  Fig 17 D-LS (using dodgy union) -------------- *)

Module DLS_Dodgy.


Fixpoint E (e : mini.Expr) : list ENV := 

  let B (e : mini.Expr) :list ENV := 
    Δ <- E e ;;
    [ Δ \ mini.I e ] in

  match e with 
  | mini.DefineV _ => [ Total_set ]

  | mini.Var _ => [ r ≈ evalA e ]
  | mini.Lit _ => [ r ≈ evalA e ]
  | mini.EPrim _ => [ r ≈ evalA e ]
  | mini.Array es =>  [ r ≈ evalA e ]

  | mini.Fail => [ ]

  | mini.Choice e1 e2 => E e1 ++ E e2

  | mini.Seq e1 e2 => (E e1 [\] Scope.singleton r) * (E e2)

  | mini.Unify e1 e2 => E e1 * E e2  (* missing from fig *)

  | mini.ApplyD e1 e2 => APP e1 e2

  | mini.If3 e1 e2 e3 => 
      IF_SPJ (mini.I e1) (E e1) (E e2) (E e3)

  | mini.All a => 
       [ ALL (B a)  ] 

  | mini.One a =>  (* not quite right *)
       [ first (E a) \ mini.I a ]

  (* TODO: functions *)

  | _ => [ ∅ ] 
  end.


End DLS_Dodgy.

(* ----------- dest passing style - sets of lists of ENV -------------- *)
(* TODO: complete this definition *)
Module DSLS.

Definition map2 {A B} : (A -> B) -> (P (list A)) -> (P (list B)) := 
  fun f => Sets.map (List.map f).

Fixpoint E (e : mini.Expr) : P (list ENV) := 

  let B (e : mini.Expr) : P (list ENV)  :=
    Sets.map (hide_list (mini.I e)) (E e) 
  in

  match e with 

  | mini.Block e =>  B e

  | mini.Var _ => ⌈[ r ≈ evalA e ]⌉
  | mini.Lit _ => ⌈[ r ≈ evalA e ]⌉
  | mini.EPrim _ => ⌈[ r ≈ evalA e ]⌉
  | mini.Array es =>  ⌈[ r ≈ evalA e ]⌉

  | mini.DefineV _ => ⌈ SUCCEED ⌉

  | mini.ApplyD e1 e2 => ⌈ APP e1 e2 ⌉

  | mini.Fail => ⌈ FAIL ⌉

  | mini.Choice e1 e2 => 
      D1 ⭅ E e1 ;;
      D2 ⭅ E e2 ;;
      ⌈ CHOICE D1 D2 ⌉

  | mini.Seq e1 e2 => 
      D1 ⭅ E e1 ;;
      D2 ⭅ E e2 ;;
      ⌈ SEQ D1 D2 ⌉


  | mini.Unify e1 e2 => 
      D1 ⭅ E e1 ;;
      D2 ⭅ E e2 ;;
      ⌈ D1 * D2 ⌉
(* 
  | mini.All e1 => (fun x => [ ALL x ]) <$> (B e1)
*)
  | mini.If3 e1 e2 e3 => fun _ => False
      
  | mini.Fun q eff i e1 (y,h,x) e2 => fun _ => False

  | _ => fun _ => False

  end.

End DSLS.

(* ------------------------------------------------------ *)
(* ------  Fig 15 E-LV (uses dodgy union) --------------- *)

Module ELV.

Definition Snoc (VS : VAL) (V : VAL) : VAL := 
  vs ⭅ VS ;; v ⭅ V ;; ⌈ Dom.snoc vs v ⌉.

Definition ALL (VS : list VAL) : VAL := 
  squash_fold_left Snoc VS ⌈ mkTup [] ⌉ .

Fixpoint E (e :mini.Expr) (ρ:env) : list VAL := 
  
  let B (e : mini.Expr) (ρ : env) : list VAL  := 
    DODGY_UNIONS
      (ρ' ⭅ X e ρ ;; ⌈ E e ρ' ⌉) 
    in

  let R (e : mini.Expr) ρ : ENV := 
    ρ' ⭅ X e ρ ;; 
    when (E e ρ' <> []) 
    ⌈ ρ' ⌉
  in

  match e with 

  | mini.Block e =>  B e ρ

  | mini.Var _ => [ ⌈evalA e ρ⌉ ]
  | mini.Lit _ => [ ⌈evalA e ρ⌉ ] 
  | mini.EPrim _ => [ ⌈evalA e ρ⌉ ] 
  | mini.Array es =>  [ ⌈evalA e ρ⌉ ] 

  | mini.DefineV _ => [ ⌈ mkTup[] ⌉ ]

  | mini.ApplyD e1 e2 => apply (evalA e1 ρ) (evalA e2 ρ)

  | mini.Fail => [ ∅ ]

  | mini.Choice e1 e2 => CHOICE (B e1 ρ) (B e2 ρ)

  | mini.Seq e1 e2 => IF2 (E e1 ρ) (E e2 ρ)

  | mini.Unify e1 e2 => UNIFY (E e1 ρ) (E e2 ρ)

  | mini.All e1 =>  [ ALL (B e1 ρ) ]

  | mini.If3 e1 e2 e3 => 
      let Δ  : ENV := R e1 ρ in
      if is_Empty_set Δ then
        B e2 ρ
      else
        DODGY_UNIONS ( ρ' ⭅ Δ ;; ⌈ B e2 ρ' ⌉ )

  (* TODO: functions, one *)      

  | mini.Fun q eff i e1 (y,h,x) e2 =>  []

  | _ => []

  end.

End ELV.


(* ------------------------------------------------------ *)
(* ------  Sets of lists of values ---------------------- *)

Module ESL.

Definition ALL (VS : list value) : value := 
  List.fold_left snoc VS (mkTup []).

Definition apply (f : value) (v : value) : list value := 
  match f with 
  | Dom.Fun hs => 
      h <- hs ;;
      match (PFun.apply_opt _ _ Value.eqb h v) with 
      | Some w => [w]
      | None => []
      end
  | _ => []
  end.


Fixpoint E (e :mini.Expr) (ρ:env) : P (list value) := 
  
  let B (e : mini.Expr) (ρ : env) : P (list value)  := 
      (ρ' ⭅ X e ρ ;; E e ρ' ) 
    in

  match e with 

  | mini.Block e =>  B e ρ

  | mini.Var _ => ⌈[ evalA e ρ ]⌉
  | mini.Lit _ => ⌈[ evalA e ρ ]⌉
  | mini.EPrim _ => ⌈[ evalA e ρ ]⌉
  | mini.Array es =>  ⌈[ evalA e ρ ]⌉

  | mini.DefineV _ => ⌈[  mkTup[]  ]⌉

  | mini.ApplyD e1 e2 => ⌈apply (evalA e1 ρ) (evalA e2 ρ) ⌉

  | mini.Fail =>  ⌈[]⌉

  | mini.Choice e1 e2 => ∅

  | mini.Unify e1 e2 => ∅

  | mini.Seq e1 e2 =>  ∅
  | mini.All e1 =>  
      vs ⭅ B e1 ρ ;;
      ⌈ [ ALL vs ] ⌉

  (* TODO: functions, one *)      
  | mini.If3 e1 e2 e3 => ∅
  | mini.Fun q eff i e1 (y,h,x) e2 =>  ∅
  | _ => ∅

  end.

End ESL.

(* -------------------------------------------------------- *)

Lemma Equiv1 (e : mini.Expr) (ρ : env) : 
  ELV.E e ρ = 
    Δ <- DLS.E e ;;
    [ '(v, Δ') ⭅ extract r Δ ;;
         when (ρ ∈ Δ') ⌈v⌉ ].
Admitted.

Lemma Equiv2 (e : mini.Expr) : 
  DLS.E e = 
    i <- allNums ;;
    [ ρ ⭅ Total_set ;;
      let VS := ELV.E e ρ in
      when (ρ r ∈ List.nth i VS ∅) 
      ⌈ ρ ⌉ ].
Admitted.  



(* ----------------- ELV defined with a fixpoint ----------- *)

Module ESLV_Fixpoint.

(* In this version, the denotation is defined as a fixpoint 
   over the syntax of miniverse expressions, taking an 
   environment as an argument. 

   The result of this function is a (potentially-infinite) 
   set of sequences of sets of values. i.e. 

     P (list VAL) 

   The set plays two roles:

    - allows us to eliminate VAL in determining the list
      (as lists in Rocq may not depend on Prop).

    - allow the result of evaluation to be "undefined" 
      (for type errors, etc.)
   
   Generally, this set will either be an empty set or a 
   singleton set.

*)

Definition TestEmpty {A B} (e1 : P A) 
  (e2 : P (list B)) (e3 : P (list B)) := 
  fun bs => 
    (e1 = ∅ -> e2 bs) /\ (e1 <> ∅ -> e3 bs).

(* empty if x is empty, singleton if x is singleton. *)
Definition retA {A} (x : P A) : P (list (P A)) := 
  TestEmpty x ∅ ⌈ [ x ] ⌉.

Fixpoint E_e (rho : env) (e : mini.Expr) : P (list VAL) := 
  
  (* extend the environment with all possible versions, 
     dodgy unioning the results together into a set 
     containing at most 1 result *)
  let D (rho : env) (e : mini.Expr )  : P (list VAL) :=
    (rho' ⭅ X e rho ;; E_e rho' e ) in

  (* all extended environments that succeed when evaluating e *)
  let R (rho : env) (e : mini.Expr) : ENV := 
    rho' ⭅ X e rho  ;;
    VS ⭅ E_e rho' e ;;
    ⌈ rho' ⌉ in
  
  match e with 
  | mini.Block e => D rho e

  | mini.Var _ =>   ⌈[ ⌈evalA e rho⌉ ]⌉
  | mini.Lit _ =>   ⌈[ ⌈evalA e rho⌉ ]⌉
  | mini.EPrim _ => ⌈[ ⌈evalA e rho⌉ ]⌉                     
  | mini.Array _ => ⌈[ ⌈evalA e rho⌉ ]⌉

  | mini.DefineV x => ⌈[ ⌈mkTup nil⌉ ]⌉

  | mini.Seq e1 e2 =>
     VS1  ⭅ E_e rho e1 ;;    (* outer set monad *)
     VS1' ⭅ Squash VS1 ;;
     VS2  ⭅ E_e rho e2 ;;  
     ⌈ (s1 <- VS1' ;; s2 <- VS2 ;; [s2]) ⌉  (* inner list monad *)

  | mini.Unify e1 e2 => 
     VS1 ⭅ E_e rho e1 ;;
     VS2 ⭅ E_e rho e2 ;;
     ⌈ ( s1 <- VS1 ;; s2 <- VS2 ;; [s1 ∩ s2]) ⌉

  | mini.Choice e1 e2 =>
     VS1 ⭅ D rho e1 ;;
     VS2 ⭅ D rho e2 ;;
     ⌈ (VS1 ++ VS2) ⌉

  | mini.Fail => 
      ⌈ [] ⌉

  | mini.One e => 
      VS1 ⭅ E_e rho e ;;
      VS2 ⭅ Squash VS1 ;;
      ⌈ take1 VS2 ⌉

  | mini.All e => 
      VS1 ⭅ D rho e ;;
      VS2 ⭅ Squash VS1 ;;
      ⌈ [tups VS2] ⌉

  | mini.If3 e1 e2 e3 =>
      let Δ := R rho e1 in
      TestEmpty Δ (D rho e3)
        (rho' ⭅ Δ ;; E_e rho' e2)
  | _ => ∅
  end.

End ESLV_Fixpoint.


(* ----------------- version two: Inductive relation ----------- *)

(*
Module ELV_Inductive.

(* Type of denotation relation *)
Definition D := forall (rho : env) (e : mini.Expr), (list VAL) -> Prop.


(* check that h does not fail on the input *)
Definition check (eval:D) (rho:env) (q:Aperture) 
  (eff:Effect) i e1 y h (x:Ident) e2 : Prop := 
      forall v, exists w, (* for every input, there is some result *)
      forall rho', rho' ∈ X e1 (Env.extend i v rho) ->  (* for every extension of rho' *)

        (* evaluating e1 should be defined *)
        exists V1, eval rho' e1 V1 /\

             (* evaluating e1 fails and applying h also fails *) 
             (Squash V1 [] /\ exists (V2 : VAL), ESL.apply (rho h) v = V2 /\ Squash V2 []) 
              \/

             (* evaluating e1 produces a singleton value vx *)
             (forall vx, Squash V1 [ ⌈ vx ⌉ ] /\
              exists V2, apply (rho h) vx = V2 /\
                 (* apply h[x] fails, eff must be decides, and e2 must fail *)
                 (Squash V2 [] /\ (eff = Decides
                                    /\ forall vy, eval (Env.extend y vy rho') e2 [])) 
                 \/
                 (* apply h[x] produces a value vy after k failures *)
                 exists vy k, TailSquash V2 (List.repeat ∅ k ++ [ ⌈ vy ⌉ ]) /\
                   (* evaluating e2 produces w after k failures *)
                    eval (Env.extend y vy rho') e2 (List.repeat ∅ k ++ [⌈ w ⌉])).

Inductive eval (rho : env) : mini.Expr -> list VAL -> Prop :=

  | eval_Block e VV VS :
    (* every result has to be decomposable into a union of 
       evaluations of e with an appropriate rho' *)
    (forall WS, WS ∈ VV -> 
            exists rho', (rho' ∈ X e rho) /\ eval rho' e WS) ->
    UNIONS VV VS ->
    eval rho (mini.Block e) VS

  | eval_Var : forall x VS,
    [ ⌈evalA (mini.Var x) rho⌉ ] = VS ->
    eval rho (mini.Var x) VS

  | eval_Lit : forall x VS,
    [ ⌈Dom.Int x⌉ ] = VS ->
    eval rho (mini.Lit (common.Int x)) VS

  | eval_Prim p VS:
    [ ⌈evalPrim p⌉ ] = VS ->
    eval rho (mini.EPrim p) VS

  | eval_DefineV x VS : 
    VS = [ ⌈ mkTup nil ⌉ ]  ->
    eval rho (mini.DefineV x) VS

  | eval_Array es  VS : 
    [ ⌈evalA (mini.Array es) rho⌉ ] = VS ->
    eval rho (mini.Array es) VS

  | eval_ApplyD a1 a2  VS:
    TailSquash (apply (evalA a1 rho) (evalA a2 rho)) VS ->
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

End ELVInductive.
*)


