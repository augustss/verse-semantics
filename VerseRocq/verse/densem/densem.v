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
Import structures.Monad.
Require Import structures.Laws.

Require Import densem.Dom.
Require Import densem.tenv.  (* environments are total *)
Require Import densem.envSet. (* ENV , hide, constraints *)

Import mini.MiniNotation.
Import SetNotations.
Import SetMonadNotation.
Import List.ListNotations.
Import ListMonadNotation.
Import EnvNotation.
Import ENVNotation.


Open Scope monad_scope.
Open Scope list_scope.
Open Scope mini_expr_scope.
Open Scope env_scope.
Open Scope set_scope.

Notation "r ≈ x ≈ ⟨ k ⟩" := ((r ≈ ⟪ x ⟫) ∩ (x ≈ ⟨ k ⟩)).

(* ---------------------------------------------- *)

(* This axiom is BAD. But convenient for now.... *)
Axiom empty_dec : forall {A} (s : P A), 
    { s ≃ ∅ } + { ~ (s ≃ ∅) }.

Definition is_Empty_set {A} (s : P A) : bool := 
  match empty_dec s with 
  | left _ => true | right _ => false 
  end.


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

Definition VAL := P value.

Import mini.MiniNotation.

(* distinguished result variable (0) *)
Definition r : Ident := mini.Test.r.
Definition rs : Scope.t := Scope.singleton r.
Notation "⟅ r ⟆" := (Scope.singleton r).
(* another variable (1) *)
Definition x := mini.Test.x.
Definition y := mini.Test.y.


(* This is a bit of a hack. For examples, we special case the variables
   x,y, and y when simplifying environment lookups *)

Ltac rewrite_env := 
  repeat match goal with 
    | [ |- context[ (?x |-> ?v , ?rho) ?x ] ] => 
        rewrite extend_lookup_same
    | [ |- context[ (x |-> ?v , ?rho) r ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
    | [ |- context[ (y |-> ?v , ?rho) r ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
    | [ |- context[ (r |-> ?v , ?rho) x ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
    | [ |- context[ (r |-> ?v , ?rho) y ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
    | [ |- context[ (y |-> ?v , ?rho) x ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
    | [ |- context[ (x |-> ?v , ?rho) y ] ] => 
        rewrite extend_lookup_diff;
        [rewrite PeanoNat.Nat.eqb_neq;easy|]
  end.



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
 
Definition IF2 {A B} : list (P A) -> list (P B) -> list (P B) := liftM2 If2.

Infix "*" := UNIFY.

(* --------- pick/sequence ----------- *)

Definition Cons {A} := (fun (V : P A) (VS : P (list A)) => 
    v  ⭅ V  ;;  vs ⭅ VS ;; ⌈ v :: vs⌉).

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

(* ------------- squash -------------- *)

(* squash using axiom *)
Definition squash {A} (xs : list(P A)) : list (P A) := 
  x <- xs ;; if is_Empty_set x then [x] else [].

(* left to right squash, doesn't require axiom, but cannot 
   produce a list.
   fold_left allows us to use snoc in the definition of ALL
*)
Definition squash_fold_left {A B} (f : P A -> P B -> P A) : 
  list (P B) -> P A -> P A := 
  List.fold_left (fun bs x => If3 x (f bs x) bs).

(* Find the first nonempty set, or an empty set if none exist. *)
Definition first {A} (VS : list (P A)) : P A := 
  squash_fold_left (fun xs x => x) VS ∅.

(* squashing and picking at the same time *)
(* doesn't require axiom *)
Fixpoint squash_pick {A} (xs : list (P A)) : P (list A) := 
  match xs with 
  | nil => ⌈ [] ⌉
  | V :: VS => If3 V  
               (Cons V (squash_pick VS))
               (squash_pick VS)
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
  let VS : P (P A) := fmap (fun VS => List.nth i VS ∅) VVS in
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
    List.nth_error (apply (evalA e1 ρ) (evalA e2 ρ)) i = Some (ret (ρ r)).

Definition APPi' (e1 : mini.Expr) (e2 : mini.Expr) (i : nat) : ENV := 
  fun ρ => 
    exists hs h, evalA e1 ρ = Fun hs 
            /\ List.nth_error hs i = Some h 
            /\ List.In (evalA e2 ρ , ρ r ) h.

(* This is a bit dodgy by using the iteration over all numbers. 
   We need to iterate only over each partial function *)
Definition APP (e1 : mini.Expr) (e2 : mini.Expr) : list ENV := 
  i <- allNums ;;
  ret (APPi e1 e2 i).

(* --- semantics of ALL / ONE for dest passing --------- *)

(* find all environments in Δ such that ρ(r) = v, then hide r *)

(* {{  (v, Δ') | ρ ∈ Δ , v = ρ.r, Δ' = (Δ ∩ {{ r = v }}) \ r  }} *)
Definition extract r (Δ : ENV) : P (value * ENV) := 
  ρ ⭅ Δ ;;
  ret ( ρ r , (Δ ∩ r ≈ ⟨ρ r⟩) \ ⟅r⟆ ).

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
Fixpoint try (xs : Scope.t) (Δs : list ENV) : list ENV * ENV := 
  let step := fun '(envs, avoid) Δi => 
                (envs ++ [Δi - avoid], avoid ∪ (Δi \ xs)) in
  List.fold_left step Δs ([],∅).

Definition IF_TIM xs S1 S2 S3 := 
    let (success, avoid) := try xs S1 in 
     (SEQ success (S2 [\] xs)) ++
     (SEQ [Total_set - avoid] S3).


(* Koen's encoding of if *)

(*
if e1 e2 e3 =
  exists y; 
  y = one{ (e1; z=⟨⟩) | z=0 }@z;
  (y=⟨⟩; e2) | (y=0; e3)
*)

(* Translated to DPS style, with distinguished result r *)
Definition koen_if e1 e2 e3 : mini.Expr := 
  mini.DefineV y :>:
  y :=: mini.One ( (e1 :>: (r :=: mini.Array [])) :|: r :=: 0 ) :>:
  (y :=: mini.Array [] :>: e2) :|: (y :=: 0 :>: e3).

Definition IF_KOEN (xs:Scope.t) (S1 S2 S3 : list ENV) : list ENV :=
  SEQ [ONE( SEQ S1 [r ≈ ⟨mkTup []⟩] ++ [r≈⟨Int 0⟩])]
      (SEQ [y≈⟨mkTup[]⟩] S2) ++ (SEQ [y ≈⟨Int 0⟩] S3).

(* Simon's version of If *)

Definition UNIONLIST : list ENV -> ENV := 
  List.fold_right Union ∅.

Definition IF_SPJ (xs:Scope.t) (S1 S2 S3 : list ENV) : list ENV :=
    let GOOD := UNIONLIST (S1 [\] ⟅ r ⟆) in
    (List.map (fun D => D ∩ GOOD) S2) ++ 
    (List.map (fun D => D - GOOD) S3).

(* E [if (x=1|x=2) { x } { 0 }] = [ {r=1,x=1} | {r=2,x=2} | {r=0,x<>1,2} *)


Definition if_iter := mini.If3 (x :=: 1 :|: x :=: 2) x 0.

(* ----------- non-dodgy dest passing style -------------- *)

Module D_LS. 

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

  | mini.Choice e1 e2 => E e1 ++ E e2

  | mini.Unify e1 e2 => 
      E e1 * E e2

  | mini.Seq e1 e2 => 
      (E e1 [\] Scope.singleton r) * (E e2) 

  | mini.ApplyD e1 e2 => APP e1 e2

  | mini.If3 a b c =>  

    let xs := mini.I a in 
    let (success, avoid) := try xs (E a) in 

     (SEQ success (E b [\] xs)) ++
     (SEQ [Total_set - avoid] (E c))


  | mini.All a => 
      [ ALL (E a) \ mini.I a ] 

  | mini.One a => 
      [ ONE (E a) \ mini.I a ]

  (* TODO: functions *)

  | _ => [ ] 
  end.

End D_LS.

Module D_LS_Theory.

Import D_LS.

Lemma E_Var (i:Ident) : E i = [ fun ρ => ρ r = ρ i ]. reflexivity. Qed.
Lemma E_One e : E (mini.One e) = [ ONE (E e) \ mini.I e ]. reflexivity. Qed.
Lemma E_Choice e1 e2 : E (e1 :|: e2) = (E e1) ++ (E e2). reflexivity. Qed.
Lemma E_Seq e1 e2 : E (e1 :>: e2) = (E e1 [\] ⟅r⟆) * E e2. reflexivity. Qed.
Lemma E_Unify e1 e2 : E (e1 :=: e2) = E e1 * E e2. reflexivity. Qed.

Create HintDb E.
Hint Rewrite E_Var E_One E_Choice E_Seq E_Unify : E.

(* if(a){b}else{c} <=> if(a){b}else{:false} | if(a){:false}else{c} *)
Lemma opinionated_if a b c: 
  E(mini.If3 a b c) =  E( mini.If3 a b mini.Fail :|: mini.If3 a mini.Fail c).
Proof. 
  unfold E. fold E.
  destruct (try (mini.I a) (E a)) as [success avoid].
  replace (SEQ [Total_set - avoid] []) with ([] : list ENV).
  2 : { cbn. auto. } 
  rewrite -> List.app_nil_r.
  replace (SEQ success ([] [\] mini.I a)) with ([] : list ENV).
  2: { cbn. unfold SEQ, UNIFY. cbn.
       admit. } 
  rewrite -> List.app_nil_l. auto.
Admitted.


(* 

if e1 e2 e3 =
  exists y; 
  y = one{ (e1; z=⟨⟩) | z=0 }@z;
  (y=⟨⟩; e2) | (y=0; e3)

koen_if =
  fun e1 e2 e3 : mini.Expr =>
  ∃ y :>: 
  y :=: mini.One (e1 :>: r :=: mini.Array [] :|: r :=: 0) :>: 
  y :=: mini.Array [] :>: e2 :|: y :=: 0 :>: e3
*)

(*
= [ D ⋂ GOOD | D ∈ Ed[e2] ] ++
  [ D \ GOOD | D ∈ Ed[e3] ]
where
  GOOD :: ENV = UNIONLIST (map hide(r) Ed[e1])

*)

End D_LS_Theory.


(* ------------------------------------------------------ *)
(* ------  Fig 17 D-LS (using dodgy union) -------------- *)

Module DLS_OLD.


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

    let Y  := mini.I e1 in 
    let Δ1 := first (E e1) \ Scope.singleton r in

     (Δ2 <- E e2 ;; [(Δ1 ∪ Δ2) \ Y])  ⩅
     (Δ3 <- E e3 ;; [Δ3 - Δ1 \ Y])

  | mini.All a => 
       [ ALL (B a)  ] 

  | mini.One a =>  (* not quite right *)
       [ first (E a) \ mini.I a ]

  (* TODO: functions *)

  | _ => [ ∅ ] 
  end.


End DLS_OLD.




(* ---- examples / theory about extract / squash_pick ----- *)

Lemma squash_pick_singleton {A} (v : A) : 
  squash_pick [ ⌈ v ⌉ ] = ⌈ [v] ⌉.  
  cbn.
  set_simpl.
  unfold Cons. set_simpl.
  done.
Qed.

Example squash_pickExample :
  squash_pick [  ⌈ (Int 0) ⌉ ; ⌈ (Int 1) ⌉ ] = 
    (⌈ [ Int 0 ; Int 1 ] ⌉).
cbn. set_simpl.
unfold Cons. set_simpl.
done.
Qed.

Lemma squash_pick_two_example :
  squash_pick [ ⌈ (Int 0) ⌉ ∪ ⌈ (Int 1) ⌉ ] = 
               (⌈ [ Int 0 ] ⌉ ∪ ⌈ [ Int 1 ] ⌉).
Proof.
  cbn. set_simpl.
  unfold squash_fold_left.
  unfold List.fold_left.
Admitted.


Lemma extract_one {v} : 
  extract r (r ≈ ⟨ v ⟩) = ⌈ (v, Total_set) ⌉.
Proof. 
eapply set_extensionality. intro ρ.
unfold extract. 
set_simpl.
split.
- move=>h.
  set_crunch.
  set_simpl.
  f_equal.
- destruct ρ as [w Δ].
  intro h. inv h.
  exists (r |-> v).
  set_simpl.
  done.
Qed. 

Lemma extract_example {k} :
  extract r (x ≈ ⟨ k ⟩ ∩ r ≈ ⟨ k ⟩) = 
    ⌈ (k, x ≈ ⟨ k ⟩) ⌉.
Proof.
  unfold extract.
  eapply set_extensionality. intros [v Δ].
  set_simpl.
  split.
  + intros h.
    inv h.
    set_crunch.
    inv H.
    repeat rewrite H2.  clear H2.
    f_equal.
    set_simpl.
    rewrite constrain_eq_hide_two. done. done.
  + intro h. inv h.
    exists (x |-> k, r |-> k).
    set_simpl.
    rewrite_env.
    rewrite constrain_eq_hide_two. done. done.
Qed. 

Lemma extract_two :
  extract r (r ≈ ⟨ Int 0 ⟩ ∪ (r ≈ ⟨ Int 1 ⟩)) = 
    (ret (Int 0, Total_set) ∪ ret (Int 1, Total_set)).
Admitted.


Lemma bcp_example : 
  ALL [x ≈ ⟨ Int 0 ⟩ ∩ r ≈ ⟨ Int 0 ⟩] = 
      ((x ≈ ⟨ Int 0 ⟩ ∩ r ≈ ⟨mkTup [Int 0]⟩) ∪
       (x ≉ ⟨ Int 0 ⟩ ∩ r ≈ ⟨mkTup []⟩)).
Proof.
  unfold ALL.
  apply set_extensionality. intros ρ.
  unfoldIn.
  unfold consistent_results.
  list_simpl.
  rewrite extract_example.
  set_simpl.
  unfold squash_pick.
  remember (when (ρ ∈ x ≈ ⟨ Int 0 ⟩) ⌈ Int 0 ⌉) as ϕ.
  split.
  + intro h. 
    crunch.
    unfold If3 in H.
    inv H. 
    ++ Sets.set_crunch.
       unfold Cons in H1.
       Sets.set_crunch.
       inv H2.
       rewrite H3.
       left. split; eauto.
    ++ unfold when, guard in H1.
       Sets.set_crunch.       
       inv H.
       have F: ⟨ ρ ∈ x ≈ ⟨ Int 0 ⟩ ⟩ = ∅. admit.
       right. split; auto.
       intro h.
       admit.       
  + intro h. inv h.
    ++ set_crunch. 
       exists [Int 0].
       split; auto.
       cbn. unfold constrain_eq. 
Admitted.
(*
rewrite H1.
       left.
       have hh: (Int 0 = Int 0) = True. { apply propositional_extensionality. tauto. }
       rewrite hh.
       set_simpl.
       reflexivity.
    ++ exists nil.
       cbn. 
       set_crunch.
       unfold constrain_ne,In in H0.
       have hh: ((x ≈ ⟨ Int 0 ⟩) ρ) = False.
       { unfold constrain_eq. 
         eapply propositional_extensionality. tauto. }
       rewrite hh. 
       set_simpl.
       eapply in_singleton.
       auto.
Qed. *)

(* one { 0 | 1 } = [ {{ r = 0 }} ]     *)
(* one { x = 0 } = [ {{ r = x = 0 }} ] *)
(* one { x = 0 | x = 1 } = [ {{ r=x=0 }} union {{r=x=1}} ] *)

Definition D0 : ENV := r ≈ ⟨ Int 0 ⟩.
Definition D1 : ENV := r ≈ ⟨ Int 1 ⟩.
Definition Dx v : ENV := x ≈ ⟨ v ⟩ ∩ r ≈ ⟨ v ⟩.


Lemma extract_D0 : 
  extract D0 = ⌈ ( Int 0, Total_set ) ⌉. 
Admitted.
Lemma extract_D1 :
  extract D1 = ⌈ (Int 1 , Total_set) ⌉.
Admitted.

Lemma ONE_example1 : ONE [ D0 ; D1 ] = D0.
Proof.
  unfold ONE.
  apply set_extensionality. intros ρ.
  unfold consistent_results, In.
  unfold fmap, Functor_list,ListDef.map.
  rewrite extract_D0. rewrite extract_D1.
  set_simpl.
  unfold squash_pick.
  unfold Head.
  split.
  + intros h.
    inv h.
    inv H.
Admitted.
(*    inv H0. 
    unfold D0, constrain_eq. 
    autorewrite with set_simpl in H1. inv H1.
  + unfold D0. 
    intro h. inv h.
    exists [ Int 0 ; Int 1].
    rewrite H0.
    split.
    cbn.
    set_simpl.
    eapply in_singleton. cbn.
    eapply in_singleton.
Qed.
*)

Lemma ONE_example2 : 
  ONE [ Dx (Int 0) ] = Dx (Int 0).
Admitted.

Lemma bind_when {A B} ϕ (s : P A) (k : A -> P B) : 
  (bind (when ϕ s) k) = (⟨ϕ⟩ ∩ (bind s k)).
Proof.
Admitted.

Lemma If3_when_ret {A B} (k : A) ϕ (s2 s3 : P B): 
  If3 (when ϕ (ret k)) s2 s3 = ((⟨ϕ⟩ ∩ s2) ∪ (⟨not ϕ⟩ ∩ s3)).
Admitted.

Lemma ONE_example3 : 
  ONE [ Dx (Int 0) ; Dx (Int 1) ] = (Dx (Int 0) ∪ Dx (Int 1)).
Proof.
  unfold ONE, Dx, consistent_results.
  apply set_extensionality. intros ρ.
  unfold In.
  unfold fmap, Functor_list, List.map.
  repeat rewrite extract_example.
  set_simpl.
  split.
  + intros h. set_crunch.
    unfold Head, squash_pick in h.
    cbn in h.
Admitted.    


Lemma ALL_two_example :
  ALL (ret (r ≈ ⟨Int 0⟩ ∪ (r ≈ ⟨Int 1⟩))) = 
      (r ≈ ⟨mkTup [Int 0]⟩ ∪ (r ≈ ⟨mkTup [Int 1]⟩)).
  apply set_extensionality. intros ρ.
  set_simpl.
  unfold ALL.
  unfold In.
  list_simpl.
Admitted.
(*
  rewrite extract_two.
  rewrite bind_union.
  repeat rewrite bind_ret_l.
  replace (Total_set ρ) with True; try easy.
  set_simpl.
  rewrite squash_pick_two_example.
  split.
  + intro h. crunch.
    inv H; inv H1. left; auto. right; auto.
  + intro h. inv h.
    exists [Int 0]. split; auto. left. eauto with sets.
    exists [Int 1]. split; auto. right; eauto with sets.
Qed.




Lemma conflict_example : 
  ALL2 [ x ≈ ⟨ Int 0 ⟩ ∩ r ≈ ⟨ Int 0 ⟩ ; 
         x ≈ ⟨ Int 1 ⟩ ∩ r ≈ ⟨ Int 1 ⟩ ]
   = (((x ≈ ⟨ Int 0 ⟩) ∩ (r ≈ ⟨ mkTup [Int 0] ⟩))
   ∪ ((x ≈ ⟨ Int 1 ⟩) ∩ (r ≈ ⟨ mkTup [Int 1] ⟩))
   ∪ ((x ≉ ⟨ Int 0 ⟩) ∩ x ≉ ⟨ Int 1 ⟩ ∩ (r ≈ ⟨ mkTup []⟩))).
Admitted.  


Lemma squash_irr_ALL (s : list ENV) : 
  ALL2 s = ALL2 (squash s).
Proof.
  unfold ALL2.
  eapply set_extensionality. intros ρ.
  unfold In.
  split.
  + intro h. crunch.
    exists x0. split; auto.
Abort.
    



(* Ed[ e1; r=<> ] = [ hide(r) D1 ∩ {{ r=<> }} | D1 <- E[e1] ] *)
Lemma part1 e1 : E (e1 :>: r :=: mini.Array []) = 
        List.map (fun D1 => hide_r D1 ∩ (r ≈ ⟨mkTup []⟩)) (E e1).
Proof.
  cbn.
  unfold hide_r. unfold hide_list. unfold UNIFY.
  list_simpl.
  set_simpl.
Admitted.

(*
Ed[ (e1; r=<>) | r=0 ] = [ hide_r D1 \cap {{r=<>}} | D1 <- E[e1] ] ++ [ {{r=0}} ] 
*)
Lemma part2 e1 : E ( (e1 :>: r :=: mini.Array []) :|: r :=: 0) = 
                fmap (fun D1 => hide_r D1 ∩ (r ≈ ⟨mkTup []⟩)) (E e1) ++ [ r ≈ ⟨Int 0⟩ ].
rewrite E_Choice. 
rewrite part1.
cbn. set_simpl. done.
Qed.

(* ---- y = one{ (e1; z=<>) | z=0}@z -------------
  Ed[ y = one{(e1; z=<>) | z=0}@z ]

= [ { rho | v =<>,  
      if rho ∈ UNIONLIST (map hide(z) E[e1])   # UNIONLIST :: [ENV] -> ENV, union them all
               = 0,   otherwise
     , rho y = v } ]
*)


Definition UNIONLIST : list ENV -> ENV := 
  List.fold_right Union ∅.

(* SCW this is NOT true. These are only true up to squashing. *)
Lemma disjoint_append (S1 S2 S3 : ENV) : 
 [(S1 ∩ S2) ∪ (Total_set - S1 ∩ S3)] = [S1 ∩ S2] ++ [Total_set - S1 ∩ S3].
Proof.
  rewrite distrib_union_l.
Abort.

Definition if2 (ϕ1 : Prop) (ϕ3 : Prop) := 
  (~ ϕ1 /\ ϕ3).

Definition if3 (ϕ1 : Prop) (ϕ2 : Prop) (ϕ3 : Prop) := 
  (ϕ1 /\ ϕ2) \/ (~ ϕ1 /\ ϕ3).

Lemma if3_rhoIn (S1 S2 S3 : ENV) :
  [ (fun ρ => if3 (ρ ∈ S1) (ρ ∈ S2) (ρ ∈ S3)) ] = 
  [(S1 ∩ S2) ∪ (Total_set - S1 ∩ S3)].
Proof.
  unfold if3.
  replace (fun ρ : env => ρ ∈ S1 /\ ρ ∈ S2 \/ ~ ρ ∈ S1 /\ ρ ∈ S3) with 
          ((fun ρ => ρ ∈ S1 /\ ρ ∈ S2) ∪ (fun ρ => ~ ρ ∈ S1 /\ ρ ∈ S3)).
  2: { set_ext ρ. rewrite in_union. intuition. }
  replace (fun ρ => ρ ∈ S1 /\ ρ ∈ S2) with (S1 ∩ S2).
  2: { set_ext ρ. rewrite in_intersection. intuition. inv H. auto. inv H. auto. } 
  replace (fun ρ => ~ ρ ∈ S1 /\ ρ ∈ S3) with (Total_set - S1 ∩ S3).
  2: { set_ext ρ. rewrite in_intersection. unfold In.
       intuition. inv H0. done. unfold Total_set, Setminus. intuition. }
  done.
Qed.

Lemma squash_pick_app : 
  forall xs ys, squash_pick (xs ++ ys) = 
             xs' ⭅ (squash_pick xs) ;; 
             ys' ⭅ (squash_pick ys) ;; 
             ⌈ xs' ++ ys' ⌉ .
Proof. 
  induction xs.
  + intros ys. cbn. 
    set_simpl.
    reflexivity.
  + intros ys.
    cbn.
    have h: ((Inhabited a) \/ (a = ∅)). admit.
    inv h.
    ++ repeat rewrite If3_nonempty; auto.
       rewrite IHxs.
       unfold Cons.
       set_simpl.
       f_equal.
       apply functional_extensionality. intro v.
       set_simpl.
       f_equal.
       apply functional_extensionality. intro vs.
       rewrite Sets.bind_bind.
       rewrite Sets.bind_singleton_l.
       f_equal.
       apply functional_extensionality. intro ws.
       set_simpl.
       done.
    ++ repeat rewrite If3_empty; auto.
Admitted.




Lemma list_map_singleton {A B} (f:A -> B)(v:A) : 
  List.map f [v] = [f v].
Proof. auto. Qed.



(* = 
   Ed[ y = one{(e1; z=<>) | z=0}@z ]
   = [ { rho | v  = tup[],  if rho ∈ UNIONLIST (map hide(z) E[e1]) 
                  = 0,      otherwise
             , rho y = v } ]
*)

Lemma if3_union: forall (S1 S1' : Prop) (S2 S3 : Prop),  
                if3 (S1 \/ S1') S2 S3 = if3 S1 S2 (if3 S1' S2 S3).
Proof.
 intros. unfold if3. 
 apply propositional_extensionality. tauto. Qed.

Lemma part3 e1 : 
    E (y :=: mini.One ((e1 :>: r :=: mini.Array []) :|: r :=: 0)) = 
      let GOOD := UNIONLIST (List.map hide_r (E e1)) in
      [ (GOOD ∩ (r ≈ y ≈ ⟨mkTup []⟩)) 
      ∪ ((Total_set - GOOD) ∩ (r ≈ y ≈ ⟨Int 0⟩)) ].
Proof.
  rewrite E_Unify.
  rewrite E_One.
  rewrite part2.
  rewrite hide_nothing.
  rewrite E_Var.
  unfold UNIFY.
  list_simpl.

  remember (fmap (fun D2 : ENV => hide_r D2 ∩ r ≈ ⟨ mkTup [] ⟩) (E e1) ++ [r ≈ ⟨ Int 0 ⟩]) as Δs.

  rewrite <- if3_rhoIn.
  remember (UNIONLIST (List.map hide_r (E e1))) as GOOD.

  unfold ONE.

  f_equal.
  set_ext ρ. rewrite in_intersection. 

  unfold In. 
  remember  (List.map (consistent_results ρ) Δs) as S.
  rewrite HeqΔs in HeqS.
  unfold fmap, Functor_list in HeqS.
  rewrite List.map_app in HeqS.
  rewrite List.map_map in HeqS.
  rewrite list_map_singleton in HeqS.
  unfold consistent_results in HeqS.
  rewrite extract_one in HeqS.
  set_simpl in HeqS.
  rewrite when_is_true in HeqS; [unfold Total_set;done|].

  replace (fun x : ENV => x0 ⭅ extract (hide_r x ∩ r ≈ ⟨ mkTup [] ⟩);; 
                       (let (v, Δ') := x0 in when (ρ ∈ Δ') (⌈v⌉)))
     with (fun x : ENV => when (ρ ∈ hide_r x) (⌈mkTup []⌉)) 
  in HeqS.
  2: { 
    apply functional_extensionality. intro Δ1.
    unfold extract.
    admit.
  } 
  subst.
  rewrite squash_pick_app.

  cbn.
  rewrite If3_nonempty. econstructor;econstructor; eauto.
  unfold Cons. set_simpl.
  
  split.
  + intros h. crunch. 
    rewrite H in H0. 
    unfold Head in H0. 
    set_simpl in H0.
    inv H0; crunch.
    inv H1; crunch.
    set_simpl in H1.
    destruct x0. 
    ++ (* squash_pick fails *) 
      inv H1.
      cbn in H2. inv H2.

       right. split; auto. 
       2: { unfold constrain_eq. split. unfoldIn. done.
            unfoldIn. done. }
       intro h.

       remember (E e1) as VS.
       move: H0 h.
       clear.
       (* need to prove that it fails *)
       move: ρ.
       induction VS; intro ρ.
       - cbn. done.
       - intros. 
         replace (a :: VS) with ([a] ++ VS) in H0. 2: { auto. } 
         rewrite List.map_app in H0.
         rewrite squash_pick_app in H0.
         inv H0; crunch.
         rewrite List.map_singleton in H.
         cbn in H.
         inv H0; crunch.
         destruct x0. 2: { inv H1. } 
         destruct x1. 2: { inv H1. } 
         cbn in h. inv h.
         -- rewrite when_is_true in H; auto.
            rewrite If3_nonempty in H. econstructor; econstructor; eauto.
            unfold Cons in H.
            autorewrite with set_simpl in H.
            inv H.
         -- apply IHVS in H0. done. auto.
    ++ (* squash pick succeeds *)
      inv H1. clear H3.
      remember (E e1) as VS.
      clear HeqVS.
      cbn in H2. inv H2.
      induction VS as [|W WS].
      - inv H0.
      - cbn.
        cbn in H0.
        have CL: (ρ ∈ hide_r W) \/ ~(ρ ∈ hide_r W). admit.
        destruct CL.
        -- (* first one works *) 
          clear IHWS.
          rewrite when_is_true in H0; auto.
          rewrite If3_nonempty in H0. econstructor; econstructor; eauto.
          unfold Cons in H0.
          set_simpl in H0.
          inv H0; crunch.
          inv H0; crunch.
          clear H2.

          replace ((hide_r W ∪ UNIONLIST (ListDef.map hide_r WS)) ρ) with
            ((ρ ∈ hide_r W) \/ (ρ ∈ UNIONLIST (ListDef.map hide_r WS))). 2: { unfold In. admit. } 
                                                                      
          rewrite if3_union.
          unfold if3. left. split. auto. split; eauto. 
          rewrite H4. unfold constrain_eq. unfold In. done.
      -- (* first one doesn't *)
        rewrite when_is_false in H0; auto.
        autorewrite with set_simpl in H0.
        move: (IHWS H0) => ih. clear IHWS H0.
        replace ((hide_r W ∪ UNIONLIST (ListDef.map hide_r WS)) ρ) with
            ((ρ ∈ hide_r W) \/ (ρ ∈ UNIONLIST (ListDef.map hide_r WS))). 2: { admit. } 
        rewrite if3_union.
        unfold if3.
        right.
        split. auto.
        unfold if3 in ih.
        inv ih; crunch. left. split. auto. auto.
        right. split. auto. auto.
 + intro h.
   unfold if3 in h.
   destruct h.
   ++ (* ρ in UNIONLIST (E e1 \ r) *)
     crunch.
     inv H0. inv H1. done.
     inv H0. inv H1. inv H2.
     unfold Head.
     set_simpl.
Admitted.

(* y = one{ (e1; z=<>) | z=0}@z; (y=⟨⟩; e2) | (y=0; e3)  
  [ GOOD ⋂ {{y=<>}} ⋂ D | D ∈ Ed[e2] ] ++
  [ BAD ⋂ {{y=0}}   ⋂ D  | D ∈ Ed[e3] ]
*)

Lemma UNIONLIST_hide VS s :
  UNIONLIST (VS [\] s) = ((UNIONLIST VS) \ s).
induction VS.
cbn. set_simpl. done.
cbn. set_simpl. f_equal.
auto.
Qed.

Lemma part4 e1 e2 e3 : 
  E (y :=: mini.One (e1 :>: r :=: mini.Array [] :|: r :=: 0) 
       :>: 
    ((y :=: mini.Array [] :>: e2) :|: (y :=: 0 :>: e3))) = 
  let GOOD := UNIONLIST (List.map hide_r (E e1)) in
  let BAD  := Total_set - GOOD in 
  (List.map (fun D => GOOD ∩ (y ≈ ⟨mkTup []⟩) ∩ D) (E e2)) ++
  (List.map (fun D => BAD  ∩ (y ≈ ⟨Int 0⟩) ∩ D) (E e3)).
Proof.
  rewrite E_Seq.
  rewrite part3.
  rewrite E_Choice.
  repeat rewrite E_Seq.
  repeat rewrite E_Unify.
  repeat rewrite E_Var.
  cbn.
  remember (UNIONLIST (ListDef.map hide_r (E e1))) as GOOD.
  remember (Total_set - GOOD) as BAD.
  repeat rewrite flat_map_map.
  repeat rewrite List.app_nil_r.
  repeat rewrite List.map_app.
  repeat rewrite List.map_map.
  f_equal.
  - f_equal.
    apply functional_extensionality. intro D.
    set_simpl.
    set_ext ρ.
    split.
    ++ set_simpl.
       intros [h1 h2].
       inv h2. inv H. inv H1. inv H. inv H3. inv H1.
       have NI: ~ Scope.In y ⟅ r ⟆. { admit. }
       move: (H2 y ltac:(eauto))=> NY.
       repeat split; try congruence.
       inv h1.
       -- unfold hide in H. inv H. 
          inv H1.
          inv H.
          unfold constrain_eq in H6. inv H6. inv H. inv H7.
          rewrite UNIONLIST_hide in H1. inv H1. crunch.
          rewrite UNIONLIST_hide. unfold hide.
          exists x2. split; auto. intros z zIn. rewrite H5; auto.
       -- unfold hide in H. inv H. crunch.
          inv H. unfold constrain_eq in H6.
          inv H6. inv H. inv H7.
          rewrite <- H1 in H6; auto.
          rewrite H2 in H6; auto.
          rewrite <- H3 in H6.
          rewrite H4 in H6.
          inv H6.
    ++ set_simpl. 
       intro h. inv h. inv H0.
       split.
       left.
       rewrite  UNIONLIST_hide.
       unfold hide. unfoldIn.
       rewrite UNIONLIST_hide in H.
       unfold hide in H. destruct H as [ρ' H].
       inv H.
       exists (r |-> mkTup [], ρ). repeat split.
Admitted.

End DLS.

(*
Module D2LS.

Fixpoint E (e : mini.Expr) : list ENV := 

  let B (e : mini.Expr) :list ENV := 
    Δ <- E e ;; [Δ \ mini.I e] 
  in
  let V (e : mini.Expr) : list (P (value * ENV)) := 
    Δ <- B e ;; [extract Δ] 
  in

  match e with 
  | mini.DefineV _ => [ Total_set ]

  | mini.Unify (mini.Var z) (mini.Var w) => 
      [ z ≈ evalA (mini.Var w) ]

  | mini.Unify (mini.Var z) (mini.Lit w) => [ r ≈ evalA (mini.Lit w) ]
(*   | mini.EPrim _ => [ r ≈ evalA e ]
  | mini.Array es =>  [ r ≈ evalA e ] *)

  | mini.Fail => []  

  | mini.Choice e1 e2 => E e1 ++ E e2

  | mini.Seq e1 e2 => 
      (E e1 * E e2) 


  | mini.Unify (mini.Var z) (mini.ApplyD a1 a2) => 
      APP e1 e2

  | mini.If3 a b c =>  []

  | mini.All a => 
      [ ALL (E a) \ mini.I a ] 

  | mini.One a => 
      [ ONE (E a) \ mini.I a ]

  (* TODO: functions *)

  | _ => [ ] 
  end.


End D2LS.
*)

(* ----------- dest passing style Fig 20 ----------------- *)

Module DSLS.

Definition map2 {A B} : (A -> B) -> (P (list A)) -> (P (list B)) := 
  fun f => fmap (fmap f).

Fixpoint E (e : mini.Expr) : P (list ENV) := 

  let B (e : mini.Expr) : P (list ENV)  :=
    List.map (fmap (hide (mini.I e))) (E e)
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

  | mini.Choice e1 e2 => List.map CHOICE (B e1) <*> B e2

  | mini.Seq e1 e2 => SEQ <$> E e1 <*> E e2

  | mini.Unify e1 e2 => UNIFY <$> E e1 <*> E e2
(* 
  | mini.All e1 => (fun x => [ ALL x ]) <$> (B e1)
*)
  | mini.If3 e1 e2 e3 => fun _ => False
      
  | mini.Fun q eff i e1 (y,h,x) e2 => fun _ => False

  | _ => fun _ => False

  end.

Module DSLS.

(* ---------------------------------------------------- *)


Definition x : Ident := mini.Test.x.


(* { x:=1; x }  *)
Lemma t2 : 
  [ ⌈r |-> Int 1⌉ ] ∈ E mini.Test.t2.
Proof.
  unfold mini.Test.t2.
  exists [ ⌈r |-> Int 1, mini.Test.x |-> Int 1⌉ ].
Admitted.
(*
  split.
  - admit.
  - cbn.
    eexists.
    split.
    eexists.
    split; eapply in_singleton.
    eexists.
    repeat split.
    2: {  rewrite SEQ_SUCCEED_X. eapply in_singleton. }    
    eexists.
    split.
    eexists.
    eexists.
    eexists.
    repeat split.
    eexists. split.
    eapply in_singleton.
    eapply in_singleton.
    eexists.
    split.
    eapply in_singleton.
    eapply in_singleton.
    eapply in_singleton.
    eexists.
    split.
    eapply in_singleton.
    unfold SEQ, UNIFY, hide.
    cbn.
    eapply in_singleton'.
    f_equal.
    eapply Extensionality_Ensembles.
    split.
    + intros x xIn. inversion xIn. 
      split.
      ++ unfold hide. cbv. 
         exists x. subst x.
         split. split. cbv. auto.
         cbv. auto.
         intros x nIn.
         destruct x eqn:EX. cbn. auto.
         destruct n eqn:EN. cbn. auto.
         reflexivity.
         
      ++ unfold constrain.
         cbv. auto.
    + intros x xIn. inversion xIn. clear xIn H1 x0.
      destruct H as [y [h1 h2]].
      unfold constrain in *.
      inversion H0. clear H0.
      inversion h1. inversion H. inversion H0. clear h1 H H0. subst.
      have NIN: ~ Scope.In mini.Test.x (Scope.singleton r). admit.
      move: (h2 mini.Test.x NIN) => h3.
      eapply in_singleton'.
      symmetry.
      eapply extend_equal. congruence.
      intros z yNE. 
      cbv.
      destruct z eqn:HZ. cbv in yNE. done.
      destruct n eqn:HN. subst.
      unfold mini.Test.x in *.
      unfold r,mini.Test.r in *.
      rewrite h3. auto.
Admitted.      
*)      

End DSLS.


(* -------------------------------------------------------- *)
(* -------------------------------------------------------- *)
(* -------------------------------------------------------- *)

(* ------------------------------------------------------ *)
(* ------  Fig 15 E-LV (uses dodgy union) --------------- *)

Module ELV.

Definition ALL (VS : list VAL) : VAL := 
  squash_fold_left (liftM2 snoc) VS (ret (mkTup empty)).

Fixpoint E (e :mini.Expr) (ρ:env) : list VAL := 
  
  let B (e : mini.Expr) (ρ : env) : list VAL  := 
    DODGY_UNIONS
      (ρ' <- X e ρ ;; ret (E e ρ')) 
    in

  let R (e : mini.Expr) ρ : ENV := 
    ρ' <- X e ρ ;; 
    when (E e ρ' <> empty) 
    (ret ρ')
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
        DODGY_UNIONS ( ρ' <- Δ ;; ⌈ B e2 ρ' ⌉ )

  (* TODO: functions, one *)      

  | mini.Fun q eff i e1 (y,h,x) e2 =>  []

  | _ => []

  end.

End ELV.


(* ------------------------------------------------------ *)
(* ------  E-LV (doesn't use dodgy union) --------------- *)

Module NonDodgyELV.

Definition ALL (VS : list VAL) : VAL := 
  List.fold_left (liftM2 snoc) VS ⌈ mkTup [] ⌉.

Fixpoint E (e :mini.Expr) (ρ:env) : P (list VAL) := 
  
  let B (e : mini.Expr) (ρ : env) : P (list VAL)  := 
      (ρ' <- X e ρ ;; E e ρ' ) 
    in

(*
  let R (e : mini.Expr) ρ : ENV := 
    ρ' <- X e ρ ;; 
    when (E e ρ' <> []) 
    ⌈ ρ' ⌉
  in
*)

  match e with 

  | mini.Block e =>  B e ρ

  | mini.Var _ => ⌈ [ ⌈evalA e ρ⌉ ] ⌉
  | mini.Lit _ => ⌈[ ⌈evalA e ρ⌉ ] ⌉
  | mini.EPrim _ => ⌈[ ⌈evalA e ρ⌉ ] ⌉
  | mini.Array es =>  ⌈ [ ⌈evalA e ρ⌉ ] ⌉

  | mini.DefineV _ => ⌈ [ ⌈ mkTup[] ⌉ ] ⌉

  | mini.ApplyD e1 e2 => ⌈apply (evalA e1 ρ) (evalA e2 ρ) ⌉

  | mini.Fail =>  ⌈[]⌉

  | mini.Choice e1 e2 => CHOICE <$> (B e1 ρ) <*> (B e2 ρ)

  | mini.Unify e1 e2 => UNIFY <$> (E e1 ρ) <*> (E e2 ρ)

  | mini.Seq e1 e2 =>  IF2 <$> (E e1 ρ) <*> (E e2 ρ)

  | mini.All e1 =>  
      vs <- B e1 ρ ;;
      ⌈ [ ALL vs ] ⌉

(*
  | mini.If3 e1 e2 e3 => 
      let Δ  : ENV := R e1 ρ in
      if is_Empty_set Δ then
        B e2 ρ
      else
        DODGY_UNIONS ( ρ' <- Δ ;; ⌈ B e2 ρ' ⌉ )
*)

  (* TODO: functions, one *)      

  | mini.Fun q eff i e1 (y,h,x) e2 =>  ∅
  | _ => ∅

  end.


Definition t := (x :=: common.Int 0).

Example example1 :
  E t (x |-> Int 0) ≃ ⌈ [ ⌈Int 0⌉ ] ⌉.
Admitted.

Example example2 :
  E t (x |-> Int 1) ≃ ⌈ [ ] ⌉.
cbn.
split.
+ intros x xIn.
Admitted.

End NonDodgyELV.


(* ------------------------------------------------------ *)
(* ------  Sets of lists of values (nondodgy)------------ *)

Module ESL.

Definition ALL (VS : list value) : value := 
  List.fold_left snoc VS (mkTup []).

Definition apply (f : value) (v : value) : list value := 
  match f with 
  | Dom.Fun hs => 
      h <- hs ;;
      match (PFun.apply_opt _ _ Value.eqb h v) with 
      | Some w => ret w
      | None => empty
      end
  | _ => empty
  end.


Fixpoint E (e :mini.Expr) (ρ:env) : P (list value) := 
  
  let B (e : mini.Expr) (ρ : env) : P (list value)  := 
      (ρ' <- X e ρ ;; E e ρ' ) 
    in

(*
  let R (e : mini.Expr) ρ : ENV := 
    ρ' <- X e ρ ;; 
    when (E e ρ' <> []) 
    ⌈ ρ' ⌉
  in
*)

  match e with 

  | mini.Block e =>  B e ρ

  | mini.Var _ => ⌈[ evalA e ρ ]⌉
  | mini.Lit _ => ⌈[ evalA e ρ ]⌉
  | mini.EPrim _ => ⌈[ evalA e ρ ]⌉
  | mini.Array es =>  ⌈[ evalA e ρ ]⌉

  | mini.DefineV _ => ⌈[  mkTup[]  ]⌉

  | mini.ApplyD e1 e2 => ⌈apply (evalA e1 ρ) (evalA e2 ρ) ⌉

  | mini.Fail =>  ⌈[]⌉

  | mini.Choice e1 e2 => CHOICE <$> (B e1 ρ) <*> (B e2 ρ)

  | mini.Unify e1 e2 => 
      vs1 <- (E e1 ρ) ;;  (* list in first set *)
      vs2 <- (E e2 ρ) ;;  (* list in second set *)
      let unify (vs1 : list value) (vs2 : list value) : list value :=
        v1 <- vs1 ;;
        v2 <- vs2 ;; 
        if (Value.eqb v1 v2) then [v1] else []
      in
      (* need to return a set of lists *)
      ⌈ unify vs1 vs2  ⌉

  | mini.Seq e1 e2 =>  
      vs1 <- (E e1 ρ) ;;  (* list in first set *)
      vs2 <- (E e2 ρ) ;;  (* list in second set *)
      let seq (vs1 : list value) (vs2 : list value) : list value :=
        v1 <- vs1 ;; (* make sure vs1 is inhabited *)
        vs2 
      in
      (* need to return a set of lists *)
      ⌈ seq vs1 vs2  ⌉

  | mini.All e1 =>  
      vs <- B e1 ρ ;;
      ⌈ [ ALL vs ] ⌉

  (* TODO: functions, one *)      
  | mini.If3 e1 e2 e3 => ∅
  | mini.Fun q eff i e1 (y,h,x) e2 =>  ∅
  | _ => ∅

  end.


(* -------------------------------------------------------- *)

Lemma Equiv1 (e : mini.Expr) (ρ : env) : 
  ELV.E e ρ = 
    Δ <- DLS.E e ;;
    [ '(v, Δ') <- extract Δ ;;
         when (ρ ∈ Δ') ⌈v⌉ ].
Admitted.

Lemma Equiv2 (e : mini.Expr) : 
  DLS.E e = 
    i <- allNums ;;
    [ ρ <- Total_set ;;
      let VS := ELV.E e ρ in
      when (ρ r ∈ List.nth i VS ∅) 
      ⌈ ρ ⌉ ].
Admitted.  



(* if((y=3) | (y=5)){y} else :false *)

Definition example_If3 := mini.If3 ((mini.Test.y :=: 3 ) :|: (mini.Test.y :=: 5)) mini.Test.y mini.Fail.

Lemma hide_none ρ : hide Scope.empty ρ = ρ. Admitted.

Example example : E example_If3 = [ r ≈ ⟨ 3 ⟩ ].
Proof.
  cbn.
  repeat rewrite hide_none.
  rewrite empty_Union.
  rewrite hide_constraint.

End NonDodgy.

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


Lemma UNIONS_mem (ls : list (list VAL)) : (UNIONS (mem ls)) = ⌈ Pointwise_Unions ls ⌉.
Proof.
Admitted.



Module NonDodgyELV.

Fixpoint E (e : mini.Expr) (rho : env) : list VAL := 
  
  (* We can find the set of lists of VALs that result 
     for any extension of rho.

     But what do we do with this set? We don't want to 
     pointwiseunion it.

     We can fail if it is nonsingleton.
   *)
  let D (rho : env) (e : mini.Expr )  : list VAL :=
    let output : P (list VAL) :=  
         (rho' <- X e rho ;; E_e rho' e)  in

  (* all extended environments that succeed when evaluating e *)
  let R (rho : env) (e : mini.Expr) : ENV := 
    rho' <- X e rho  ;;
    VS <- E_e rho' e ;;
    ⌈ rho' ⌉ in
  
  match e with 
  | mini.Block e => D rho e

  | mini.Var _ =>   ⌈[ ⌈evalA e rho⌉ ]⌉
  | mini.Lit _ =>   ⌈[ ⌈evalA e rho⌉ ]⌉
  | mini.EPrim _ => ⌈[ ⌈evalA e rho⌉ ]⌉                     
  | mini.Array _ => ⌈[ ⌈evalA e rho⌉ ]⌉

  | mini.DefineV x => ⌈[ ⌈mkTup nil⌉ ]⌉

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
      VS1 <- D rho e ;;
      VS2 <- Squash VS1 ;;
      pure [tups VS2]

  | mini.If3 e1 e2 e3 =>
      let Δ := R rho e1 in
      TestEmpty Δ (D rho e3)
        (UNIONS (rho' <- Δ ;; E_e rho' e2))
  | _ => empty
  end.

End FixpointVersion.





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
    UNIONS (rho' <- X e rho ;; E_e rho' e ) in

  (* all extended environments that succeed when evaluating e *)
  let R (rho : env) (e : mini.Expr) : ENV := 
    rho' <- X e rho  ;;
    VS <- E_e rho' e ;;
    ⌈ rho' ⌉ in
  
  match e with 
  | mini.Block e => D rho e

  | mini.Var _ =>   ⌈[ ⌈evalA e rho⌉ ]⌉
  | mini.Lit _ =>   ⌈[ ⌈evalA e rho⌉ ]⌉
  | mini.EPrim _ => ⌈[ ⌈evalA e rho⌉ ]⌉                     
  | mini.Array _ => ⌈[ ⌈evalA e rho⌉ ]⌉

  | mini.DefineV x => ⌈[ ⌈mkTup nil⌉ ]⌉

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
      VS1 <- D rho e ;;
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
      forall v, exists w, (* for every input, there is some result *)
      forall rho', rho' ∈ X e1 (Env.extend i v rho) ->  (* for every extension of rho' *)

        (* evaluating e1 should be defined *)
        exists V1, eval rho' e1 V1 /\

             (* evaluating e1 fails and applying h also fails *) 
             (Squash V1 [] /\ exists V2, apply (rho h) v = V2 /\ Squash V2 []) 
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

Definition eval_top t d := eval Env.empty t d.

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


Lemma not_r : forall (x r : Ident), (SetoidList.InA eq x [r] -> False) -> Nat.eqb r x = false. Admitted.



Lemma hide_constraint {i v} : (i ≈ fun _ => v) \ (Scope.singleton i) ≃ Total_set.
split. 
intros x xIn. cbv. auto.
intros ρ ρIn. cbv.
eexists.
instantiate (1 := i |-> v, ρ).
split. rewrite extend_lookup_same. auto.
intros x NI. 
apply not_r in NI. rewrite extend_lookup_diff; auto.
Qed.

Lemma hide_all s : (hide s Total_set) = Total_set.
  eapply Extensionality_Ensembles.
  split.
  intros x xIn. cbv. auto.
  intros x xIn. unfold hide.
  exists x. split. auto.
  intros y IH. auto.
Qed.



Lemma SEQ_SUCCEED_X X : SEQ SUCCEED X = X.
  unfold SEQ, UNIFY, hide_list, SUCCEED.
  cbn.
  rewrite hide_all.
  rewrite List.app_nil_r.
  induction X.
  - cbn. auto.
  - cbn. f_equal.
    eapply all_intersect.
    auto.
Qed.

Example ex_ALL : 
  (r |-> mkTup [Int 3;Int 4]) ∈ (ALL [ r ≈ ⟨3⟩;  r ≈ ⟨4⟩] ).
cbn. exists ([Int 3; Int 4], Total_set).
split; [exists (Int 3, Total_set);split|idtac]. 
- cbv. extensionality ρ.
  eapply propositional_extensionality.
  split; try done. intro h.
  exists (Env.extend r (Int 3) ρ). 
  split. split. cbv. auto. cbv. auto.
  intros x xIn. apply not_r in xIn. 
  cbv. destruct x. done. done.
- cbv.
Admitted.


Example first_example_none {A} : @first A  [ ∅ ] = ∅ .
  eapply Extensionality_Ensembles. split.
  - move => y yIn. 
    unfold first in yIn. cbv in yIn.
    inversion yIn; inversion H; subst; auto.
  - move => y yIn. done.
Qed.

Example first_example {A} (x:A) : first  [ ∅ ; ∅ ; ⌈ x ⌉ ] = ⌈ x ⌉.
Proof.
  eapply Extensionality_Ensembles. split.
  - move => y yIn. 
    inversion yIn; inversion H; subst.
    eapply not_Singleton_empty; eauto.
    auto.
  - move => y yIn.
    right.
    split. intro h. eapply not_Singleton_empty; eauto. auto.
Qed.

Example apply_tup0 x y : apply (mkTup [x;y]) (Int 0) = [ ⌈ x ⌉ ; ∅ ]. cbn. auto. Qed.
Example apply_tup1 x y : apply (mkTup [x;y]) (Int 1) = [ ∅ ; ⌈ y ⌉ ]. cbn. auto. Qed.
Example apply_add1 : apply Prim.add1 (Int 0) = [ ⌈ Int 1 ⌉ ]. cbn. auto. Qed.
Example apply_isInt : apply Prim.isInt (Int 1) = [ ⌈ Int 1 ⌉ ]. cbn. auto. Qed.

Example DODGY_UNIONS_ex : 
  DODGY_UNIONS (⌈ [ ∅ ; ⌈ 3 ⌉ ] ⌉ ∪ ⌈ [ ⌈ 4 ⌉ ] ⌉) = 
    [  ⌈ 4 ⌉ ; ⌈ 3 ⌉ ].
Proof.
  unfold DODGY_UNIONS, allNums. 
  replace limitNum with 2.
  cbn.
  repeat rewrite map_union. repeat rewrite map_singleton.
  cbn.
  repeat rewrite -> UNION_union. 
Admitted.  


Module Test.

Coercion Dom.Int : nat >-> value.

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
(*
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
       - done. *)
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
  (mini.DefineV x :>: (mini.Var x :=: mini.Array [v0 ; v1])  :>: (mini.Var x :@: mini.Lit (common.Int v))) ⊆
  eval rho (((mini.Lit (common.Int v) :=: mini.Lit (common.Int 0)) :>: v0) :|: ((mini.Lit (common.Int v) :=: mini.Lit (common.Int 1)) :>: v1)).
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
