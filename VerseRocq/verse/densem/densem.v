Require Import Imports.

Import ssreflect.

Require Import syntax.common.
Require syntax.mini.
Require syntax.essential.
Require Import PFun.
Require Import structures.Sets.
Import structures.List.

Require Export densem.Dom.    (* values are finite *)
Require Export densem.tenv.   (* environments are total *)
Require Export densem.envSet. (* def of ENV , hide, constraints *)
Require Export densem.squash. (* definitions related to squashing *)

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

Lemma bind_Total_set {A B} (k : A  -> P B) (S : P B) : 
  inhabited A ->
  (forall ρ, k ρ = S) ->
  (ρ ⭅ Total_set ;; k ρ) = S.
Proof.
  intros iA h.
  set_ext ρ'.
  rewrite in_bind.
  split. 
  + intro h1. set_crunch. rewrite h in H0. done.
  + intros h1. inv iA. exists X. split; cbv; auto. rewrite h. done.
Qed.


(* --------------------------------------------------- *)

Definition envs : ENV := Total_set.

Notation VAL := (P value).

(* For destination-passing style, distinguished result variable (0) *)
Definition r : Ident := common.ConcreteVars.r.

Notation "⟅ r ⟆" := (Scope.singleton r).

Import ConcreteVars.

(** -------------- atomic/simple expressions  ---------------- *)

Definition evalPrim (p : PrimOp) : value  := 
  match p with 
  | common.Add  => Prim.add1
  | common.TimesTwo => Prim.times2
  | common.ArrayLen => Prim.arrayLen
  | common.IsInt => Prim.isInt
  | common.IsArr => Prim.isArr
  | common.IsFun => Prim.isFun
  | _ => Dom.Int 0  (* other primitives *)
  end.

(* Evaluation function for simple values. 
   If e is not of the right form, it is interpreted as 0 *)
Fixpoint evalA (a : Simple) (ρ : env) : value := 
  match a with 
  | common.Var x => ρ x
  | common.Lit (common.Int i) => Dom.Int i
  | common.EPrim p => evalPrim p
  | common.SArray es => mkTup (List.map (fun e => evalA e ρ) es)
  end.

(** ------- operations on lists of sets and sets of lists ------ *)

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


Definition UNIONLIST {A} : list (P A) -> (P A) := 
  List.fold_right Union ∅.

(* Intersect a single set with a list. This is equivalent to 
   UNIFY [D1] Ds. 
 *)
Definition MAP_INTERSECT {A} :=
  fun (Δ1:P A)(Δs:list (P A)) => List.map (fun Δ2 => Δ1 ∩ Δ2) Δs.

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


(* --------- Notation ----------- *)

(* Sometimes written as *∩* *)
Infix "*" := UNIFY : list_scope.

Infix "∩*" := MAP_INTERSECT (at level 70) : list_scope.

(* hide identitfiers in a list of sets of environments *)
Notation "Δ \* xs" := (hide_list xs Δ) (at level 70) : list_scope.

(* dodgy union *)
Infix "⩅" := pointwise_union (at level 70) : list_scope.

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

(* another version of the dodgy big union *)
Definition DODGY_UNIONS {A} (VVS : P (list (P A))) : list (P A) := 
  i <- allNums ;;
  let VS : P (P A) := Sets.map (fun VS => List.nth i VS ∅) VVS in
  [ ⨃ VS ].

(* -------------- tuples and application ------ *)

(* Create a tuple from a list of VALs *)
Definition tups (VS : list VAL) : VAL := 
  fun v => 
    exists (vs : list value), 
      v = mkTup vs /\ List.Forall2 (fun x y => x ∈ y) vs VS.

Definition APPi r (e1 : common.Simple) (e2 : common.Simple) (i : nat) : ENV := 
  fun ρ => 
    exists hs h, evalA e1 ρ = Fun hs 
            /\ List.nth_error hs i = Some h 
            /\ List.In (evalA e2 ρ , ρ r ) h.

(* This is a bit dodgy by using the iteration over all numbers. 
   We need it to iterate only over each partial function. *)
Definition APP (r : Ident) (e1 : common.Simple) (e2 : common.Simple) : list ENV := 
  i <- allNums ;;
  [APPi r e1 e2 i].

(* NB: Tims semantics, allows terms, not just simple expressions 

    ε⟦t0[t1]⟧ix := 
        ε⟦t0⟧hf * ε⟦t1⟧jy * 
         [{ρ | ρ:envs, ρ.f=Fun(fss), fs=fss[n], (ρ.j,ρ.x)∈fs, ρ.i=ρ.x} | n:[0,1,...]]-{h,f,j,y}

*)

(* Tim's semantics in DLS style. Requires fresh variables f and x. *)
Definition APP_Tim (r f x : Ident) (A : list ENV) (B : list ENV) : list ENV := 
   ((((f ≈ evalA r) ∩* A) [\] ⟅ r ⟆)
  * (((x ≈ evalA r) ∩* B) [\] ⟅ r ⟆)
  * (i <- allNums ;;
    [ APPi r f x i ]) ) [\] ⟅ f ⟆ [\] ⟅ x ⟆ .
 
        
(* --- semantics of ALL / ONE ------------ *)

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

(* Interpret atomic/simple values *)
Definition SIMPLE (a : env -> value) : list ENV := 
   [r ≈ a].

Definition SEQ (d1 : list ENV) (d2: list ENV) : list ENV := 
  (d1 [\] ⟅ r ⟆) * d2.

(* This character C-X 8 ret U+2A3E,  or \fcmp  *)
(* Infix "⨾" := SEQ (at level 70, left associativity) : list_scope. *)

(* Iteration: a1 .. a2 *)
(* This definition requires infinite lists. Here we approximate 
   because we assume that there are only a finite number of values. *)
Definition ITER (a1 : env -> value) (a2 : env -> value) : list ENV := 
  i <- allNums ;;
  [ (fun ρ => (ρ r = Int i) /\ 
             exists n1 n2, (a1 ρ = Int n1) /\ n1 <= i /\ (a2 ρ = Int n2) /\ n2 >= i) ].

(* ----- semantics of IF ------------ *)

(* Given the semantics of the condition of an IF expression, 
   produce a list of environments corresponding to successful
   completion of each choice in the If. 
   Also produce an environment that corresponds to all choices
   failing. *)
Definition try (a : Scope.t) (A : list ENV) : list ENV * ENV := 
  let step := fun '(success, avoid) Ai => 
                (success ++ [Ai - avoid], avoid ∪ (Ai \ a)) in
  List.fold_left step A ([],∅).

(* NOTE:  A should be raw, B/C should be blocks with bound variables
   already hidden. *)
Definition IF (r : Ident) a (A B C : list ENV) : list ENV := 
    let (success, avoid) := try a (A [\] ⟅r⟆) in 
     ((success * B) [\] a) ++
     ([(Total_set - avoid)] * C).  

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

(* ----------- semantics of FOR ----------- *)


(* Destination passing version, with fixed r.

Each choice in A either produces a singleton tuple ⟨vn⟩ or empty tuple ⟨⟩ stored 
in the environment in variables r0...rm. Then we concatenate all of these 
tuples together to get the final result.

ε⟦for(t0){t1}⟧r := [{ρ | ρ ← ρs, ρ.r=ρ.r0+ρ.r1+ …}    # NOTE: + is tuple concatentation
                    | ρs ← C0 * C1 * … * Cm ] – {r0, r1,… rm}

       where m  := length(A)
             Cn  := ([ A[n] - r    ] * [ {ρ | ρ ∈ Δ, ρ.rn=tuple{ρ.r}} - r
                                       | Δ ← ε⟦t1⟧r ]) – BVS(t0,t1) +
                    [ envs\A' [n] ] * [{ρ | ρ ∈ envs, ρ.rn=tuple{   }} ]
                    where 
				A := ε⟦t0⟧r 
				A' = A - BVS(t0) - r

         r s rn ∉ BVS(t0,t1) # should these be completely fresh?

*)

(* This helper function constructs Cn by iterating over A.

   To be compositional, this function takes the denotations of t0 and t1 
   as arguments.

NOTE:  
   r  is the result variable
   rn is a fresh identifier (and all of its successors are fresh)
   a  is BVS t0
   b  is BVS t1
   A  ε⟦t0⟧r
   B  ε⟦t1⟧r

*)
Fixpoint make_FOR_Choices rn a b (A B : list ENV) : list (list ENV) := 
    match A with 
    | An :: rest => 
        (* ([ A[n] - r ] * [ {ρ | ρ ∈ Δ, ρ.rn=tuple{ρ.r}} - r
                           | Δ ← ε⟦t1⟧r ]) – BVS(t0,t1) *)
        [(([ An \ ⟅r⟆ ] * (((rn ≈ fun ρ => mkTup [ρ r]) ∩* B) [\] ⟅ r ⟆))
                          [\] a [\] b) ++

        (* [ envs\A' [n] ] * [{ρ | ρ ∈ envs, ρ.rn=tuple{   }} ] *)
        ([ Total_set - (An \ a \ ⟅r⟆) ] * [ rn ≈ ⟨ mkTup [] ⟩ ])]
        ++ (make_FOR_Choices (1+rn) a b rest B)
    | [] => []
    end.


(* Semantics of FOR *)
(* z must be fresh for A and B
   a  is BVS t0
   b  is BVS t1
   A  ε⟦t0⟧r
   B  ε⟦t1⟧r
 *)

Definition FOR (z:Ident) (a b : Scope.t) (A : list ENV) (B : list ENV) : list ENV := 

  let Cs := make_FOR_Choices z a b A B in

  (* produces the constraint on r and the tensor product of all Ci in Cs
     rs = {r0, r1, .. rm}
     c  = fun ρ => ρ.r0+ρ.r1+ …   # NOTE: + is tuple concatentation
     CC = C0 * C1 * … * Cm
  *)
  let step arg Ci : Ident * Scope.t * (env -> value) * list ENV := 
      match arg with 
        | (zk, zs, f, acc) =>
          (1+zk, Scope.add zk zs, fun ρ => append (f ρ) (ρ zk), acc * Ci) end in
  let '(_,rs,c,CC) :=  
    List.fold_left step Cs (z, Scope.empty, fun ρ => mkTup [], [Total_set]) in

  (*  [ {ρ | ρ ← ρs, ρ.r=c ρ } | ρs ← CC ] – rs *)
  ( ρ <- CC  ;; [ (r ≈ c) ∩ ρ ]) \* rs.


(* ----------- D-LS semantics -------------- *)
(* destination passing style semantics for miniverse, with 
   a list of sets of env denotation. *)

Module DLS. 

Fixpoint E (e : mini.Expr) : list ENV := 

  let B (e : mini.Expr) :list ENV := 
    Δ <- E e ;; [Δ \ mini.I e] 
    (* [ Δ \ mini.I e | Δ <- E e ] *)
  in
  let V (e : mini.Expr) : list (P (value * ENV)) := 
    Δ <- B e ;; [extract r Δ] 
  in

  match e with 
  | mini.DefineV _ => [ Total_set ]

  | mini.ES a => SIMPLE (evalA a)

  | mini.Fail => []  

  | mini.Choice e1 e2 => B e1 ++ B e2

  | mini.Unify e1 e2 =>  E e1 * E e2

  | mini.Seq e1 e2 =>  (E e1 [\] ⟅r⟆) * (E e2) 

  | mini.ApplyD a1 a2 => 
      APP r a1 a2

  | mini.If3 a b c =>  
    let xs := mini.I a in 
    IF r xs (E a) (B b) (B c)

  | mini.For2 t0 t1 => 
    let xs := mini.I t0     in  (* BVS t0 *)
    let ys := mini.I t1     in  (* BVS t1 *)
    let x  := mini.fresh t0 in
    let y  := mini.fresh t1 in 
    let z  := 1 + List.list_max ([x; y] ++ Scope.elements xs ++ Scope.elements ys)   in 
    (* z is fresh for everything *)

    FOR z xs ys (E t0) (E t1)


  | mini.Iter a1 a2 => ITER (evalA a1) (evalA a2)

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
Lemma E_Iter a1 a2 : E (mini.Iter a1 a2) = ITER (evalA a1) (evalA a2). reflexivity. Qed.
Create HintDb E.
Hint Rewrite E_Var E_One E_Choice E_Seq E_Unify E_Iter : E.

End DLS.

(* ----------- S-LS semantics Fig. 16 -------------- *)
(* This is the essential verse semantics, where the definition is 
   parameterized by an input and output variables (u and v). 
   This semantics is incomplete.
*)

Module SLS.

(* ε⟦for(t0){t1}⟧ix := [{ρ | ρ ← ρs, ρ.i=ρ.i0+ρ.i1+ …, ρ.x=ρ.x0+ρ.x1+ …} 
                    | ρs ← C0 * C1 * … * Cm ] – {i0, x0, i1, x1,… xm}

       where m  := length(A)
             Cn  := [      A  [n] ] * [{ρ | ρ ∈ ρs,   ρ.in=tuple{ρ.k}, ρ.xn=tuple{ρ.z}} | ρs ← ε⟦t1⟧kz ] – BVS(t0,t1)⋃{j,k,y,z} +
                    [ envs\A' [n] ] * [{ρ | ρ ∈ envs, ρ.in=tuple{   }, ρ.xn=tuple{   }}                ]
                    where 
				A := ε⟦t0⟧jy 
				A' = A — (BVS(t0)⋃{j,y})
in j k xn y z ∉ BVS(t0,t1)
*)



Fixpoint S (u : Ident) (t : essential.Expr) (v : Ident) : list ENV := 

  let B u (t : essential.Expr) v :list ENV := 
    Δ <- S u t v ;; [Δ \ essential.I t] 
  in

  let C t : list ENV := 
    let p := essential.fresh t in
    let q := 1 + p in
    (S p t q) [\] ⟅ p ⟆ [\] ⟅ q ⟆
  in

  match t with 

  | essential.Underscore => 
      [ u ≈ evalA v ]

  | essential.ES a => 
      [ (u ≈ evalA a) ∩ (v ≈ evalA a) ]

  | essential.Array ts => [] (* TODO *)

  | essential.Define x t => 
      (x ≈ evalA v) ∩* (S u t v)

  | essential.Fail => []  

  | essential.Choice t1 t2 => 
      B u t1 v ++ B u t2 v

  | essential.Seq t1 t2 => 
      C t1 * (S u t2 v) 

  | essential.Iter a1 a2 => 
      i <- allNums ;;
      [ (u ≈ ⟨Int i⟩) ∩ (v ≈ ⟨Int i⟩) 
        ∩ (fun ρ => Value.leb (evalA a1 ρ) (Int i) = true) 
        ∩ (fun ρ => Value.leb (Int i) (evalA a2 ρ) = true) ]

  | essential.Unify t1 t2 => 
      S u t1 v * S u t2 v

  | essential.ApplyD t1 t2 => 
      (u ≈ evalA v) ∩* (APP v t1 t2) 

  | essential.If3 t0 t1 t2 =>  

    let j0  := essential.fresh t0 in 
    let j1  := essential.fresh t1 in
    let j   := 1 + max j0 j1      in 
    let y   := 1 + j              in
    let xs  := essential.I t0     in 
    let ys  := Scope.add j (Scope.add y xs) in

    IF y xs (S j t0 y) (B u t1 v) (B u t2 v)

(* TODO:

  | essential.For2 t0 t1 => []

  | essential.All a => 
      [ ALL (E a) \ essential.I a ] 

  | essential.One a => 
      [ ONE (E a) \ essential.I a ]
*)

  (* TODO: functions *)

  | _ => [ ] 
  end.

End SLS.

(** ----------- D-SLS semantics ------------------ *)
(* This is a destination passing style verse semantics, using sets of lists of
sets.

The advantages of this semantics:
- infinite lists are not required. All infinite quantification is at the set
  level.
- "if" does not need to be ordered. We can use the outer set for unordered
  choices.
*)


Module DSLS.

(* To avoid the need of infinite lists, we must update the definitions of 
   ITER and APP to not include a list comprehension over all integers. Instead 
   we include a set comprehension over all environments. *)

Definition ITER (v1 : env -> value) (v2 : env -> value) : P (list ENV) :=
  ρ ⭅ envs ;;
  match v1 ρ , v2 ρ with 
     | Int k1 , Int k2 => 
         let ks := enumFrom k1 k2 in
         ⌈  List.map (fun k => (r ≈ ⟨Int k⟩) ∩ 
                            (* make sure each env agrees with ρ on v1 / v2 *)
                            (fun ρ' => v1 ρ' = Int k1 /\ v2 ρ' = Int k2)) ks ⌉
     | _ , _ => ∅
     end.

Definition APP (v1 : env -> value) (v2 : env -> value) : P (list ENV) :=
  ρ1 ⭅ envs ;;
  ⌈ match (v1 ρ1) with 
  | Fun hs => 
    h <- hs ;;
    [ ρ2 ⭅ envs ;;
      match (PFun.apply_opt _ _ Value.eqb h (v2 ρ2)) with 
        | Some w => (r ≈ ⟨ w ⟩) ∩ (fun ρ' => v1 ρ' = v1 ρ1 /\ v2 ρ' = v2 ρ2)
        | None => ∅
      end ]
  | _ => []
  end ⌉.


(* This is NOT the same as IF above because it uses ∪ instead of ++ 
   to join the two branches together. *)
Definition IF xs TS0 TS1 TS2 : P (list ENV) :=
  T0 ⭅ TS0 ;;
  T1 ⭅ TS1 ;;
  T2 ⭅ TS2 ;;
  let (successes, avoid) := try xs (T0 [\] ⟅ r ⟆) in
  ⌈(successes * T1) [\] xs⌉ ∪ 
  ⌈(Total_set - avoid) ∩* T2⌉.


Fixpoint E (e : mini.Expr) : P (list ENV) := 

  let B (e : mini.Expr) : P (list ENV) := 
    Δs ⭅ E e ;; ⌈ Δs [\] mini.I e ⌉
    (* written with set comprehension:
       { Δs \ I e | Δs ∈ E e } 
     *)
  in

  match e with 

  | mini.DefineV _ => 
      ⌈ [ Total_set ] ⌉

  | mini.ES a => 
      ⌈ SIMPLE (evalA a) ⌉

  | mini.Fail => ⌈ [] ⌉

  | mini.Choice e1 e2 => 
      D1 ⭅ B e1 ;;
      D2 ⭅ B e2 ;;
      ⌈ D1 ++ D2 ⌉

  | mini.Unify e1 e2 => 
      D1 ⭅ E e1 ;;
      D2 ⭅ E e2 ;;
      ⌈ D1 * D2 ⌉

  | mini.Seq e1 e2 => 
      D1 ⭅ E e1 ;;  
      D2 ⭅ E e2 ;;
      ⌈ (D1 [\] ⟅r⟆) * D2 ⌉

  | mini.Iter a1 a2 =>
     ITER (evalA a1) (evalA a2)

  | mini.ApplyD a1 a2 => 
     APP (evalA a1) (evalA a2)

  | mini.If3 t0 t1 t2 =>  
     IF (mini.I t0) (E t0) (B t1) (B t2)

  | mini.All t0 => 
     D ⭅ B t0 ;;  
     ⌈ [ ALL D ] ⌉

  | mini.One a => 
     D ⭅ B a ;;  
     ⌈ [ ONE D ] ⌉

  | mini.For2 t0 t1 => 
    let xs := mini.I t0     in  (* BVS t0 *)
    let ys := mini.I t1     in  (* BVS t1 *)
    let x  := mini.fresh t0 in
    let y  := mini.fresh t1 in 
    let z  := 1 + List.list_max ([x; y] 
                                   ++ Scope.elements xs ++ Scope.elements ys)  
       in  (* z is fresh for everything *)

    T0 ⭅ E t0 ;;
    T1 ⭅ E t1 ;;
    ⌈ FOR z xs ys T0 T1 ⌉

  (* TODO: functions *)

  | _ => ∅
  end.

Definition  B (e : mini.Expr) : P (list ENV) := 
    Δs ⭅ E e ;; ⌈ Δs [\] mini.I e ⌉.

Lemma E_Choice e1 e2 : E (e1 :|: e2) = 
    D1 ⭅ B e1 ;;
    D2 ⭅ B e2 ;;
    ⌈ D1 ++ D2 ⌉. reflexivity. Qed.

Lemma E_Seq e1 e2 : E (e1 :>: e2) =
   D1 ⭅ E e1 ;;
   D2 ⭅ E e2 ;;
   ⌈ (D1 [\] ⟅r⟆) * D2 ⌉.
reflexivity. Qed.

Lemma E_Unify e1 e2 : E (e1 :=: e2) = 
  D1 ⭅ E e1 ;;
  D2 ⭅ E e2 ;;
  ⌈ D1 * D2 ⌉.
 reflexivity. Qed.


(* Examples of the semantics. *)


(* (2,3)[x] *)
Example pair_app := mini.ApplyD (common.SArray [common.Lit 2; common.Lit 3]) 
                       (common.Var x).
Lemma example_pair_app :
  APP (fun _ => mkTup [Int 2;Int 3]) (fun rho => rho x) = 
    ⌈ [ (x ≈ ⟨Int 0⟩ ∩ r ≈ ⟨Int 2⟩) ; (x ≈ ⟨Int 1⟩ ∩ r ≈ ⟨Int 3⟩) ] ⌉.
Proof.
  unfold APP.
  eapply bind_Total_set. constructor. exact Env.empty.
  intros _.
  cbn.
  f_equal.
  f_equal.
  + 
  set_ext ρ2.
  rewrite in_bind.
  split.
  - intro h. set_crunch. rename x0 into ρ.
    destruct (Value.eqb (ρ x) (Int 0)) eqn:h0.
    inv H0. inv H2. inv H1.
    econstructor.
    unfold constrain_eq. unfold Ensembles.In.
    rewrite Value.eqb_eq in h0. rewrite H3. done.
    unfold Ensembles.In. unfold constrain_eq. done.
    inv H0.
  - intro h. set_crunch. inv H. inv H0.
    exists ρ2. split. cbv; auto.
    have R: Value.eqb (ρ2 x) (ρ2 x)= true. {
      rewrite Value.eqb_eq. done. } 
    rewrite R. 
    split. 
    unfold Ensembles.In, constrain_eq. done.
    unfold Ensembles.In. split; auto.
 + 
  f_equal.
  set_ext ρ2.
  rewrite in_bind.
  split.
  - intro h. set_crunch. rename x0 into ρ.
    destruct (Value.eqb (ρ x) (Int 1)) eqn:h0.
    rewrite Value.eqb_eq in h0.
    inv H0. inv H2. inv H1.
    econstructor.
    unfold constrain_eq, Ensembles.In.
    congruence.
    unfold constrain_eq, Ensembles.In.    
    done.
    inv H0.
  - intro h. set_crunch. inv H. inv H0.
    exists ρ2. split. cbv; auto.
    have R: Value.eqb (ρ2 x) (ρ2 x)= true. {
      rewrite Value.eqb_eq. done. } 
    rewrite R. 
    split. 
    unfold constrain_eq, Ensembles.In.
    congruence.
    unfold constrain_eq, Ensembles.In. done.
Qed. 

Lemma iter_choice : 
  E (mini.Iter 1 2) = E (1 :|: 2).
Proof.
  cbn. unfold ITER, SIMPLE.
  eapply bind_Total_set. constructor. exact Env.empty. intros _.
  cbn.
  set_simpl. cbn. 
  replace (⟨ Int 1 = Int 1 /\ Int 2 = Int 2 ⟩) with envs.
  set_simpl.
  done.
  { set_ext ρ. unfold Ensembles.In, envs, Total_set.  tauto. } 
Qed.

Lemma if_example :
  E (mini.If3 (x:=: 0)(1:|:2)(3:|:4)) = 
  ⌈ [ (x ≈ ⟨Int 0⟩ ∩ r ≈ ⟨Int 1⟩) ; (x ≈ ⟨ Int 0 ⟩ ∩ r ≈ ⟨ Int 2 ⟩) ] ⌉ ∪
  ⌈ [ (x ≉ ⟨Int 0⟩ ∩ r ≈ ⟨Int 3⟩) ; (x ≉ ⟨ Int 0 ⟩ ∩ r ≈ ⟨ Int 4 ⟩) ] ⌉.
Proof.
  cbn.
  unfold IF, SIMPLE.
  set_simpl.
  replace (Scope.union Scope.empty Scope.empty) with Scope.empty.
  cbn.
  f_equal.
  - f_equal.
    set_simpl.
    f_equal.
    set_ext ρ.
    split.
    unfold constrain_eq.
    + intro h. set_crunch.
      inv H. set_crunch.
      split. unfold Ensembles.In.
      rewrite H0. rewrite Scope.singleton_spec. done.
      rewrite H3. done.
      unfold Ensembles.In. done.
    + intro h. set_crunch.
      inv H. inv H0.
      econstructor.
      unfold Ensembles.In. 
      unfold constrain_eq, hide.
      exists (r |-> Int 0, ρ).
      split. split; unfold Ensembles.In.
      done. cbn. congruence.
      intros x xI. rewrite Scope.singleton_spec in xI. 
      admit.
      unfold Ensembles.In. 
      unfold constrain_eq, hide. done.
    + 
Admitted.


Lemma if_example_2 :
  E ((mini.If3 (x:=: 0)(1:|:2)(3:|:4)) :|: 5) = 
     ⌈ [ (x ≈ ⟨Int 0⟩ ∩ r ≈ ⟨Int 1⟩) ; 
         (x ≈ ⟨Int 0⟩ ∩ r ≈ ⟨Int 2⟩) ; 
         (r ≈ ⟨Int 5⟩) ] ⌉ ∪
     ⌈ [ (x ≉ ⟨Int 0⟩ ∩ r ≈ ⟨Int 3⟩) ; 
         (x ≉ ⟨Int 0⟩ ∩ r ≈ ⟨Int 4⟩) ; 
         (r ≈ ⟨Int 5⟩)] ⌉.
Proof.
  rewrite E_Choice.
  unfold B.
  rewrite if_example.
  set_simpl. unfold mini.I.
  set_simpl.
  unfold E, SIMPLE.
  set_simpl. unfold evalA. unfold id.
  cbn.
  f_equal.
Qed.

(*
   (if x = 1 then 11| 22 else 33); 
    x = (1 | 2); 
    (if x = 1 then 44 else 55 | 66)
*)

Lemma if_example_3A :
  E (mini.If3 (x :=: 1) (11 :|: 22) 33) = 
   ⌈ [ (x ≈ ⟨Int 1⟩ ∩ r ≈ ⟨Int 11⟩) ; 
       (x ≈ ⟨Int 1⟩ ∩ r ≈ ⟨Int 22⟩) ] ⌉ ∪
   ⌈ [ (x ≉ ⟨Int 1⟩ ∩ r ≈ ⟨Int 33⟩) ] ⌉.
Admitted.
Lemma if_example_3B :
  E (x :=: (1 :|: 2)) = 
   ⌈ [ (x ≈ ⟨Int 1⟩ ∩ r ≈ ⟨Int 1⟩) ; 
       (x ≈ ⟨Int 2⟩ ∩ r ≈ ⟨Int 2⟩) ] ⌉. 
Admitted.
Lemma if_example_3C :
  E (mini.If3 (x :=: 1) 44 (55 :|: 66)) = 
     ⌈ [ (x ≈ ⟨Int 1⟩ ∩ r ≈ ⟨Int 44⟩) ] ⌉ ∪ 
     ⌈ [ (x ≉ ⟨Int 1⟩ ∩ r ≈ ⟨Int 55⟩) ;
         (x ≉ ⟨Int 1⟩ ∩ r ≈ ⟨Int 66⟩) ] ⌉.
Admitted.

Definition if_example_3 := 
  (mini.If3 (x :=: 1) (11 :|: 22) 33) :>:
  (x :=: (1 :|: 2)) :>:
  (mini.If3 (x :=: 1) 44 (55 :|: 66)).

Lemma if_example_3_meaning_AB : 
  E ((mini.If3 (x :=: 1) (11 :|: 22) 33) :>:
    (x :=: (1 :|: 2))) = 
  (⌈ [(x ≈ ⟨ Int 1 ⟩) ∩ r ≈ ⟨ Int 1 ⟩;  ∅ ; 
      (x ≈ ⟨ Int 1 ⟩) ∩ r ≈ ⟨ Int 1 ⟩;  ∅ ] ⌉
   ∪
   ⌈ [∅ ; (x ≈ ⟨ Int 2 ⟩) ∩ r ≈ ⟨ Int 2 ⟩] ⌉ ).
Proof.
  rewrite E_Seq.
  rewrite if_example_3A.
  rewrite E_Unify.
  unfold E, SIMPLE, evalA.
  set_simpl.
  unfold hide_list.
  list_simpl.
  rewrite constrain_eq_hide_two. done.
  rewrite constrain_eq_hide_two. done.
  cbn.
Admitted.

Lemma if_example_3_meaning : 
  E (((mini.If3 (x :=: 1) (11 :|: 22) 33) :>:
    (x :=: (1 :|: 2))) :>:
    (mini.If3 (x :=: 1) 44 (55 :|: 66))) = 
  (⌈ [(x ≈ ⟨ Int 1 ⟩) ∩ r ≈ ⟨ Int 44 ⟩;  ∅ ; 
      (x ≈ ⟨ Int 1 ⟩) ∩ r ≈ ⟨ Int 44 ⟩;  ∅ ] ⌉
   ∪ ⌈ [∅; ∅; ∅; ∅; ∅; ∅; ∅; ∅] ⌉
   ∪ ⌈ [∅; ∅] ⌉ 
   ∪ ⌈ [∅ ; ∅ ; (x ≈ ⟨ Int 2 ⟩) ∩ r ≈ ⟨ Int 55 ⟩ ; 
                (x ≈ ⟨ Int 2 ⟩) ∩ r ≈ ⟨ Int 66 ⟩ ] ⌉ ).
Proof.
  rewrite E_Seq.
  rewrite if_example_3_meaning_AB.
  rewrite if_example_3C.
  set_simpl.
  unfold hide_list. list_simpl.
  set_simpl.
  rewrite constrain_eq_hide_two. done.
  rewrite constrain_eq_hide_two. done.
  cbn.
  set_simpl.
  f_equal.
  f_equal.
  f_equal.
  rewrite <- intersection_assoc.
  rewrite constrain_eq_same. done.
  f_equal.
  rewrite <- intersection_assoc.
  rewrite constrain_eq_same. done.
  rewrite <- intersection_assoc.
  replace ((x ≈ ⟨ Int 1 ⟩) ∩ (x ≉ ⟨ Int 1 ⟩)) with (∅ : ENV). set_simpl.
  rewrite <- intersection_assoc.
  replace ((x ≈ ⟨ Int 1 ⟩) ∩ (x ≉ ⟨ Int 1 ⟩)) with (∅ : ENV). set_simpl.
  rewrite <- intersection_assoc.
  replace ((x ≈ ⟨ Int 2 ⟩) ∩ (x ≈ ⟨ Int 1 ⟩)) with (∅ : ENV). set_simpl.
  rewrite <- intersection_assoc.
  replace ((x ≈ ⟨ Int 2 ⟩) ∩ (x ≉ ⟨ Int 1 ⟩)) with (x ≈ ⟨ Int 2 ⟩). 
  rewrite <- intersection_assoc.
  replace ((x ≈ ⟨ Int 2 ⟩) ∩ (x ≉ ⟨ Int 1 ⟩)) with (x ≈ ⟨ Int 2 ⟩). 
  done.
  admit.
  admit.
  admit.
Admitted.


(* ------------------------------------------------------- *)

(* NB: This version of Iter DOESN't work. It includes partial runs that 
   don't correspond to any evaluation *)
(*
  k ⭅ (Total_set : P nat) ;;
  ⌈ let ks := enumFrom 0 k in 
    List.map (fun k => (r ≈ ⟨Int k⟩)
                      ∩ (fun ρ => Value.leb (v1 ρ) (Int k) = true) 
                      ∩ (fun ρ => Value.leb (Int k) (v2 ρ) = true)) ks ⌉.
*)

(* Slightly thicker application. This is NOT what we want.
   For (2,3)[i] gives us  
   { [r=2] | rho } ∪ { [(rho,i=1,r=3)] | rho } 

  { [{{r=2}}] } ∪ { [{{i=1,r=3}}] } 

 *)

(* apply domain function value f to x 
   f is a list of partial functions, corresponding to 
   iteration on the input. Any inputs that are in the 
   domain of the function will produce output.
 *)
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


Definition APP' (v1 : env -> value) (v2 : env -> value) : P (list ENV) :=
   ρ ⭅ envs ;;
   let vs := apply (v1 ρ) (v2 ρ) in
   ⌈ List.map (fun v => r ≈ ⟨v⟩) vs ⌉.

(*  (2,3)[x] == 2|3 *)
Lemma example_pair_app' :
  APP' (fun _ => mkTup [Int 2;Int 3]) (fun rho => rho x) = 
    ρ ⭅ envs ;;
    if (Value.eqb (ρ x) (Int 0)) then
    ⌈ [ (r ≈ ⟨Int 2⟩)] ⌉ 
    else if (Value.eqb (ρ x) (Int 1)) then
    ⌈ [ (r ≈ ⟨Int 3⟩) ] ⌉ else ⌈[]⌉.
Proof.
  unfold APP'.
  f_equal.  extensionality ρ. 
  cbn.
  destruct (Value.eqb (ρ x) (Int 0)) eqn:h0.
  + cbn.
    have h1: (Value.eqb (ρ x) (Int 1) = false). admit.
    rewrite h1. cbn. done.
  + destruct (Value.eqb (ρ x) (Int 1)) eqn:h1.
    cbn. done.
  cbn. done.
Admitted. 

End DSLS.


(** Old: Thin DSLS semantics  --------- *)

Module Thin_DSLS.
(*

In this "Thin" version, below, the interpretation of ITER / SIMPLE /
APP restricts the individual sets of environments to be as small as possible.

This has the disadvantage that 1..2 does not have the same semantics as 1|2.
The former is { [ {(r=1,rho)} ; {(r=2,rho)} ] | rho in envs } -- many lists
the latter is { [ (r≈1) ; (r≈2) ] } -- only a single list

See below for a "thicker" version that gives both terms the latter semantics
while still avoiding infinite lists.  *)


Definition ITER (v1 : env -> value) (v2 : env -> value) : P (list ENV) :=
  ρ ⭅ Total_set ;;
  match v1 ρ , v2 ρ with 
     | Int k1 , Int k2 => 
         let ks := enumFrom k1 k2 in
         ⌈  List.map (fun k => ⌈(r |-> Int k, ρ)⌉) ks ⌉
     | _ , _ => ∅
     end.

(*
 Thin  application. I don't think this is what we want.
   For (2,3)[i] gives us  
   { [(rho,i=0,r=2)] | rho } ∪ { [(rho,i=1,r=3)] | rho } 
*)

(* apply domain function value f to x 
   f is a list of partial functions, corresponding to 
   iteration on the input. Any inputs that are in the 
   domain of the function will produce output.
 *)
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

Definition APP (v1 : env -> value) (v2 : env -> value) : P (list ENV) :=
   ρ ⭅ envs ;;
   let vs := apply (v1 ρ) (v2 ρ) in
   ⌈ List.map (fun v => ⌈ (r |-> v, ρ) ⌉) vs ⌉.

Definition SIMPLE (a : env -> value) := 
   ρ ⭅ envs ;;
   ⌈ [ ⌈ (r |-> a ρ, ρ) ⌉ ] ⌉.



Import ConcreteVars.
Example pair_app := mini.ApplyD (common.SArray [common.Lit 2; common.Lit 3]) 
                       (common.Var x).
Lemma example_pair_app :
  APP (fun _ => mkTup [Int 2;Int 3]) (fun rho => rho x) = 
    (ρ ⭅ envs ;; 
     if Value.eqb (ρ x) (Int 0) then 
     ⌈ [ ⌈ (r |-> Int 2, ρ) ⌉ ] ⌉
     else if Value.eqb (ρ x) (Int 1) then
     ⌈ [ ⌈ (r |-> Int 3, ρ) ⌉ ] ⌉
     else ⌈ [] ⌉).
Proof.    
  unfold APP.
  f_equal. extensionality ρ.
  cbn.
  destruct (Value.eqb (ρ x) (Int 0)) eqn:h0.
  + have h1: (Value.eqb (ρ x) (Int 1) = false). admit.
    rewrite h1.
    cbn.
    done.
  + destruct (Value.eqb (ρ x) (Int 1)) eqn:h1.
    cbn. done.
    cbn. done.
Admitted.

(* This is NOT the same as IF above because it uses ∪ instead of ++ 
   to join the two branches together. *)
Definition IF xs TS0 TS1 TS2 : P (list ENV) :=
  T0 ⭅ TS0 ;;
  T1 ⭅ TS1 ;;
  T2 ⭅ TS2 ;;
  let (successes, avoid) := try xs (T0 [\] ⟅ r ⟆) in
  ⌈(successes * T1) [\] xs⌉ ∪ 
  ⌈(Total_set - avoid) ∩* T2⌉.

Fixpoint E (e : mini.Expr) : P (list ENV) := 

  let B (e : mini.Expr) : P (list ENV) := 
    Δs ⭅ E e ;; ⌈ Δs [\] mini.I e ⌉
    (* with set comprehension:
       { Δs \ I e | Δs ∈ E e } 
     *)
  in

  match e with 

  | mini.DefineV _ => 
      ⌈ [ Total_set ] ⌉

  | mini.ES a => 
      SIMPLE (evalA a)

  | mini.Fail => ⌈ [] ⌉

  | mini.Choice e1 e2 => 
      D1 ⭅ B e1 ;;
      D2 ⭅ B e2 ;;
      ⌈ D1 ++ D2 ⌉

  | mini.Unify e1 e2 => 
      D1 ⭅ B e1 ;;
      D2 ⭅ B e2 ;;
      ⌈ D1 * D2 ⌉

  | mini.Seq e1 e2 => 
      D1 ⭅ B e1 ;;
      D2 ⭅ B e2 ;;
      ⌈ (D1 [\] ⟅r⟆) * D2 ⌉

  | mini.Iter a1 a2 =>
     ITER (evalA a1) (evalA a2)

  | mini.ApplyD a1 a2 => 
     APP (evalA a1) (evalA a2)

  | mini.If3 t0 t1 t2 =>  
     IF (mini.I t0) (E t0) (B t1) (B t2)

  | mini.All t0 => 
     D ⭅ B t0 ;;  (* or E ?? *)
     ⌈ [ ALL D ] ⌉

  | mini.One a => 
     D ⭅ B a ;;  (* or E ?? *)
     ⌈ [ ONE D ] ⌉

  | mini.For2 t0 t1 => 
    let xs := mini.I t0     in  (* BVS t0 *)
    let ys := mini.I t1     in  (* BVS t1 *)
    let x  := mini.fresh t0 in
    let y  := mini.fresh t1 in 
    let z  := 1 + List.list_max ([x; y] ++ Scope.elements xs ++ Scope.elements ys)  
       in  (* z is fresh for everything *)

    T0 ⭅ E t0 ;;
    T1 ⭅ E t1 ;;
    ⌈ FOR z xs ys T0 T1 ⌉

  (* TODO: functions *)

  | _ => ∅
  end.


End Thin_DSLS.



(* ------------------------------------------------------ *)
(* ------  Sets of lists of values ---------------------- *)

Module ESL.

(* Why doesn't a set of list of values or set of list of VAL 
   work? 
*)

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

Definition intersect (xs : list value) (ys : list value) : list value := 
  x <- xs ;;
  y <- ys ;;
  if Value.eqb x y then [x] else [].

Definition seq (xs : list value) (ys : list value) : list value := 
  match xs with 
  | [] => [] 
  | _ => ys
  end.

Definition ALL (VS : list value) : value := 
  List.fold_left snoc VS (mkTup []).

Fixpoint E (e :mini.Expr) (ρ:env) : P (list value) := 
  
  let X (e : mini.Expr) (ρ : env) : ENV := 
    hide_env (mini.I e) ρ in

  let B (e : mini.Expr) (ρ : env) : P (list value)  := 
      (ρ' ⭅ X e ρ ;; E e ρ' ) 
    in

  match e with 

  | mini.Block e =>  B e ρ

  | mini.ES a => ⌈[ evalA a ρ ]⌉

  | mini.DefineV _ => ⌈[  mkTup[]   ]⌉

  | mini.ApplyD e1 e2 =>
      ⌈ apply (evalA e1 ρ) (evalA e2 ρ) ⌉

  | mini.Fail =>  ⌈[]⌉

  | mini.Choice e1 e2 =>
      vs1 ⭅ E e1 ρ ;;
      vs2 ⭅ E e2 ρ ;;
      ⌈ vs1 ++ vs2 ⌉

  | mini.Unify e1 e2 =>
      vs1 ⭅ E e1 ρ ;;
      vs2 ⭅ E e2 ρ ;;
      ⌈ intersect vs1 vs2 ⌉

  | mini.Seq e1 e2 => 
      vs1 ⭅ E e1 ρ ;;
      vs2 ⭅ E e2 ρ ;;
      ⌈ seq vs1 vs2 ⌉

  | mini.Iter a1 a2 => 
      match evalA a1 ρ, evalA a2 ρ with 
      | Int i, Int j => 
          let ks := enumFrom i j in
          ⌈ List.map Int ks ⌉
      | _,_ => ⌈[]⌉
      end

  | mini.All e => 
      vs ⭅ B e ρ ;;
      ⌈ [ ALL vs ]  ⌉

  (* with nontermination, this is probably not correct *)
  | mini.One e => 
      vs ⭅ B e ρ ;;
      ⌈ match vs with 
        | [] => [] 
        | v :: _ => [v] 
        end ⌉


  (* TODO: functions, one *)      

  | mini.If3 t0 t1 t2 => ∅

  | mini.Fun q eff i e1 (y,h,x) e2 =>  ∅
  | _ => ∅

  end.

End ESL.



(* ------------------------------------------------------ *)
(* ------  Fig 17 D-LS (using dodgy union) -------------- *)

Module DLS_Dodgy.


Fixpoint E (e : mini.Expr) : list ENV := 

  let B (e : mini.Expr) :list ENV := 
    Δ <- E e ;;
    [ Δ \ mini.I e ] in

  match e with 
  | mini.DefineV _ => [ Total_set ]

  | mini.ES a => [ r ≈ evalA a ]

  | mini.Fail => [ ]

  | mini.Choice e1 e2 => E e1 ++ E e2

  | mini.Seq e1 e2 => (E e1 [\] Scope.singleton r) * (E e2)

  | mini.Unify e1 e2 => E e1 * E e2  (* missing from fig *)

(*   | mini.ApplyD e1 e2 => APP e1 e2 TODO *) 

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


Definition X (e : mini.Expr) (ρ : env) : ENV := 
    hide_env (mini.I e) ρ.


(* ------------------------------------------------------ *)
(* ------  Fig 15 E-LV (uses dodgy union) --------------- *)

Module ELV.

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

  | mini.ES a => [ ⌈evalA a ρ⌉ ]

  | mini.DefineV _ => [ ⌈ mkTup[] ⌉ ]

  | mini.ApplyD e1 e2 => 
      List.map (fun x => ⌈x⌉)
        (apply (evalA e1 ρ) (evalA e2 ρ))

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

  | mini.ES a =>   ⌈[ ⌈evalA a rho⌉ ]⌉

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


