Require Import Imports.

From Stdlib Require Lists.List.
From Stdlib Require Import Classes.EquivDec.
Import ssreflect.

From Stdlib Require Import Logic.PropExtensionality.
From Stdlib Require Import Logic.FunctionalExtensionality.

Require Import syntax.common.
Require syntax.mini.
Require Import PFun.
Require Import structures.Sets.
Import structures.List.
Import structures.Monad.

Require Import densem.Dom.
Require Import densem.tenv.  (* environments are total *)

Import mini.MiniNotation.
Import FunctorNotation.
Import ApplicativeNotation.
Import MonadNotation.
Import SetNotations.
Import List.ListNotations.
Import EnvNotation.

Open Scope monad_scope.
Open Scope list_scope.
Open Scope mini_expr_scope.
Open Scope env_scope.
Open Scope set_scope.

Lemma not_Singleton_empty : forall A B (x:B), ⌈ x ⌉ ≃ ∅ -> A.
Admitted.


(* --------------------------------------------------- *)

Definition VAL := P value.
Definition ENV := P env.

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

(* ------- operations on sequences of sets ---------- *)


Definition SUCCEED {A} : list (P A) := [ Total_set ]. 

Definition FAIL {A} : list (P A) := [ ∅ ] .

Definition CHOICE {A} (d1 : list A) (d2: list A) : list A := 
  d1 ++ d2.

Definition UNIFY {A} (d1 : list (P A)) (d2: list (P A)) : list (P A) := 
  ρ1 <- d1 ;;
  ρ2 <- d2 ;;
  [ ρ1 ∩ ρ2 ].

Definition MINUS {A} (d1 : list (P A)) (d2: list (P A)) : 
  list (P A) := 
  ρ1 <- d1 ;;
  ρ2 <- d2 ;;
  [ @Setminus A ρ1 ρ2 ].

Definition if2 (ϕ1 : Prop) (ϕ3 : Prop) := 
  (~ ϕ1 /\ ϕ3).

Definition if3 (ϕ1 : Prop) (ϕ2 : Prop) (ϕ3 : Prop) := 
  (ϕ1 /\ ϕ2) \/ (~ ϕ1 /\ ϕ3).

Definition If2 {A B} (s1 : P A) (s3 : P B) := 
  (when (~ (s1 ≃ ∅)) s3).

Definition If3 {A B} (s1 : P A) (s2 : P B) (s3 : P B) := 
  (when (s1 ≃ ∅) s2) ∪
  (when (not (s1 ≃ ∅)) s3).

Definition IF2 {A B} : list (P A) -> list (P B) -> list (P B) := liftM2 If2.

(* left to right squash *)
Definition squash_fold_left {A B} (f : P A -> P B -> P A) : list (P B) -> P A -> P A := 
  List.fold_left (fun bs x => If3 x bs (f bs x)).

(*
fun b => ((x ≃ ∅) /\ (b ∈ bs)) \/ (not (x ≃ ∅) /\ (b ∈ (f bs x)))).
*)

(* right to left squash *)
Fixpoint squash {A} (xs : list(P A)) : list (P A) -> Prop := 
  match xs with 
  | nil => ⌈ nil ⌉
  | cons x xs' => 
      fun ys => ((x ≃ ∅) /\ squash xs' ys) 
             \/ (not (x ≃ ∅)) /\  
                 match ys with 
                 | nil => False 
                 | cons y ys' => x = y /\ squash xs' ys'
                 end
  end.

Lemma squash_unique {A} : forall (xs ys zs: list (P A)), 
    squash xs ys -> squash xs zs -> ys = zs.
Proof.
  induction xs; simpl; intros ys' zs' h1 h2. 
  - inversion h1; inversion h2; auto. 
  - destruct h1 as [ [E1 S1] | [NE1 M1] ]; destruct h2 as [[E2 S2] | [NE2 M2]].
    + eapply IHxs; eauto.
    + done.
    + done.
    + destruct ys'; destruct zs'; try done.
      move: M1 => [<- M1].
      move: M2 => [<- M2].
      f_equal; eauto.
Qed.

(* If it is decidable whether sets are empty, we can always squash a list. *)
Axiom dec_empty : forall {A} (s : P A), (s ≃ ∅) \/ not (s ≃ ∅).
Lemma squash_total {A} : forall (xs : list (P A)), exists ys, squash xs ys.
Proof.
  induction xs.
  exists nil. reflexivity.
  move: IHxs => [ys' h].
  destruct (dec_empty a).
  - exists ys'. left. split; auto.
  - exists (a :: ys'). right. split; auto.
Qed.


(* dodgy union *)
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
      V = fun (v : value) => 
          exists (WS : list VAL) W, 
            (WS ∈ VVS) /\
            List.nth_error WS i = Some W /\
            (v ∈ W).

Definition ONE {A} (VS : list (P A)) : P A := 
  let start := ∅ in
  let step := fun xs x => x in
  squash_fold_left step VS start.

Example ONE_example_none {A} : @ONE A  [ ∅ ] = ∅ .
  eapply Extensionality_Ensembles. split.
  - move => y yIn. 
    unfold ONE in yIn. cbv in yIn.
    inversion yIn; inversion H; subst; auto.
  - move => y yIn. done.
Qed.

Example ONE_example {A} (x:A) : ONE  [ ∅ ; ⌈ x ⌉ ] = ⌈ x ⌉.
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




(* -------------- auxiliary definitions for VAL ------ *)

(* Create a tuple from a list of VALs *)
Definition tups (VS : list VAL) : VAL := 
  fun v => 
    exists (vs : list value), 
      v = mkTup vs /\ List.Forall2 Sets.In vs VS.

(* apply f to x 

   f is a list of partial functions, corresponding to 
   iteration on the input. Any inputs that are in the 
   domain of the function will produce output.
 *)
Definition apply (f : value) (v : value) : list VAL := 
  match f with 
  | Dom.Fun hs => 
      h <- hs ;;
      match (PFun.apply_opt _ _ Value.eqb h v) with 
      | Some w => [ ⌈ w ⌉ ]
      | None => [ ∅ ] 
      end
  | _ => []
  end.

Example apply_tup0 x y : apply (mkTup [x;y]) (Int 0) = [ ⌈ x ⌉ ; ∅ ]. cbn. auto. Qed.
Example apply_tup1 x y : apply (mkTup [x;y]) (Int 1) = [ ∅ ; ⌈ y ⌉ ]. cbn. auto. Qed.
Example apply_add1 : apply Prim.add1 (Int 0) = [ ⌈ Int 1 ⌉ ]. cbn. auto. Qed.
Example apply_isInt : apply Prim.isInt (Int 1) = [ ⌈ Int 1 ⌉ ]. cbn. auto. Qed.
  
(* ---------- auxiliary definitions for ENV and list ENV ------ *)

(* destinguished result variable (0) *)
Definition r : Ident := mini.Test.r.

(* Constrain a variable to be equal to a particular value.
   All other mappings in the environment are unconstrained. *)
Definition constrain (x : Ident) (f : env -> value) : ENV := 
  fun ρ => ρ x = f ρ.

(* Generalize all of the xs to be anything *)
(* "Envs Drop Variables" *)
Definition hide (xs : Scope.t) (Δ : ENV) : ENV := 
  fun ρ => exists ρ', (ρ' ∈ Δ) /\ forall x, ~ (Scope.In x xs) -> (ρ x = ρ' x).

(* just generalize r *)
Definition hide_r : ENV -> ENV := hide (Scope.singleton r).

(* Generalize all of the xs to be anything *)
(* Envss Drop Variables *)
Definition hide_list (xs : Scope.t) (Δs : list ENV) : list ENV := 
  List.map (hide xs) Δs.

(* Envs Difference       es \{xs} fs := {e | e∈es, for all f∈fs. not e∈{f}\xs} *)
Definition envs_difference (es : ENV) (xs : Scope.t) (fs : ENV) : ENV := 
  e <- es ;;
  f <- fs ;;
  when (not (e ∈ (hide xs ⌈f⌉))) ⌈ e ⌉.


(* Two different versions of application. Arguments must be atomic, 
   i.e. have a single value
   The set of all environments such that the ith result of 
   apply e1 to e2 is ρ r. Not sure yet which one of these is easier to 
   reason about.
*)
Definition APPi (e1 : mini.Expr) (e2 : mini.Expr) (i : nat) : ENV := 
  fun ρ => 
    List.nth_error (apply (evalA e1 ρ) (evalA e2 ρ)) i = Some ⌈ ρ r ⌉.

Definition APPi' (e1 : mini.Expr) (e2 : mini.Expr) (i : nat) : ENV := 
  fun ρ => 
    exists hs h, evalA e1 ρ = Fun hs 
            /\ List.nth_error hs i = Some h 
            /\ List.In (evalA e2 ρ , ρ r ) h.

Definition APP (e1 : mini.Expr) (e2 : mini.Expr) : list ENV := 
  i <- allNums ;;
  [ APPi e1 e2 i ].


(* find all environments in Δ such that ρ(r) = v and then hide r *)
Definition extract (Δ : ENV) : P (value * ENV) := 
  fun '(v , Δ'') => 
    Δ'' = hide_r (Δ ∩ (constrain r (fun _ => v))).

Definition combine : list ENV -> P (list value * ENV) := 
  List.fold_right 
    (fun Δ VSS => 
       '(vi, Δi) <- extract Δ ;;
       fun '(vs, Δs) => (vi :: vs, Δi ∩ Δs) ∈ VSS)
    (fun _ => False).
       
Definition ALL (s : list ENV) : ENV := 
  '( vs, Δ ) <- combine s ;;
     (Δ ∩ (constrain r (fun _ => mkTup vs))).


(* Find the first nonempty set in the sequence, if there is
   one. *)
(*
Fixpoint ONE (xs: list ENV) : ENV := 
  match xs with 
  | nil => ∅
  | x :: xs' => fun ρ => 
        (x = ∅ /\ (ρ ∈ ONE xs')) \/
        (x <> ∅ /\ (ρ ∈ x ))
  end.

Example ONE_s s : ONE [ ∅ ; s ; ∅ ] = s.
cbv. eapply Extensionality_Ensembles.
split.    
- intros ρ [[h1 [[h3 h5]| h4]]| h2].
  + destruct h5. destruct H. subst. auto. destruct H.
    done.
  + destruct h4. done.
  + destruct h2. done.
- intros ρ h. 
  cbv. left. split; auto.
  right. split. intro h1. subst. done. done.
Qed.
*)




(* The set of all environments that extend rho with arbitrary 
   definitions for the variables declared in e. 
   Is this the same as "hide (mini.I e) ⌈ ρ ⌉"

*)

Definition X (e : mini.Expr) (ρ : env) : ENV :=
  hide (mini.I e) ⌈ ρ ⌉.
(*
  fun rho' =>
    forall x, 
      if (Scope.mem x (mini.I e)) 
      then exists v, rho' x = v    (* arbitrary value for vars in new scope *)
      else rho' x = rho x.         (* same value for vars in old scope *)
*)


Definition SEQ (d1 : list ENV) (d2: list ENV) : list ENV := 
  UNIFY (hide_list (Scope.singleton r) d1) d2.


(* ------  Notation ----------------------------------------- *)

Infix "⩅" := unions (at level 70).
Infix "≈" := constrain (at level 60).
Notation "Δ \ xs" := (hide xs Δ) (at level 70).
Notation "Δ [\] xs" := (hide_list xs Δ) (at level 70).
Notation "es \{ xs } fs" := (envs_difference es xs fs) (at level 40).
Notation "⟨ n ⟩" := (fun ρ => Int n) (at level 40).
Infix "*" := UNIFY.


(* ------  Some theory ----------------------------------------- *)

Lemma not_r : forall (x r : Ident), (SetoidList.InA eq x [r] -> False) -> Nat.eqb r x = false. Admitted.

Lemma hide_intersect x s1 s2 : hide x (s1 ∩ s2) ⊆ ((hide x s1) ∩ (hide x s2)).
Proof. 
  intros ρ [_ [[ρ' h11] h2]].
  unfold hide.
    split. 
    exists ρ'. split; auto. exists ρ'. split; auto.
Qed. 
(* NOTE: converse is not true *)


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


Lemma extract_one {x} : extract (r ≈ ⟨ x ⟩) = ⌈(Int x, Total_set)⌉.
eapply Extensionality_Ensembles.
unfold extract. unfold hide_r. 
split.
- intros [v ρ] yIn. 
  cbn in yIn.
  have: ρ ≃ Total_set.  split. intros z zIn. cbv. auto.
  intros z _. rewrite yIn.
  cbv. 
Admitted.  

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

(* ------  Fig 17 ----------------------------------------- *)

Module DLS.


Fixpoint E (e : mini.Expr) : list ENV := 

  let B (e : mini.Expr) :list ENV := 
    Δ <- E e ;;
    [ Δ \ mini.I e ] in

  let V (e : mini.Expr) : list (P (value * ENV)) := 
    Δ <- B e ;;
    [ extract Δ ] in

  match e with 
  | mini.DefineV _ => [ Total_set ]

  | mini.Var _ => [ r ≈ evalA e ]
  | mini.Lit _ => [ r ≈ evalA e ]
  | mini.EPrim _ => [ r ≈ evalA e ]
  | mini.Array es =>  [ r ≈ evalA e ]

  | mini.Fail => FAIL   (* or [] ? *)

  | mini.Choice e1 e2 => E e1 ++ E e2

  | mini.Seq e1 e2 => SEQ (E e1) (E e2)

  | mini.Unify e1 e2 => E e1 * E e2  (* missing from fig *)

  | mini.ApplyD e1 e2 => APP e1 e2

  | mini.If3 a b c =>  

    let Y := mini.I a in 
    let rY := Scope.add r Y in

     ((E a [\] rY) * (E b [\] Y)) ⩅
     (MINUS (E c) (E a [\] Y))

  | mini.All a => 
      [ ALL (E a) \ mini.I a ] 

  | mini.One a => 
      [ ONE (E a) \ mini.I a ]

  (* TODO: functions *)

  | _ => [ ∅ ] 
  end.

End DLS.

(* ----------- dest passing style Fig 20 ----------------- *)

Module DSLS.

Definition map2 {A B} : (A -> B) -> (P (list A)) -> (P (list B)) := 
  fun f => fmap (fmap f).

Fixpoint E (e : mini.Expr) : P (list ENV) := 

  let B (e : mini.Expr) : P (list ENV)  :=
    (fmap (hide (mini.I e))) <$> (E e)
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

  | mini.Choice e1 e2 => CHOICE <$> B e1 <*> B e2

  | mini.Seq e1 e2 => SEQ <$> E e1 <*> E e2

  | mini.Unify e1 e2 => UNIFY <$> E e1 <*> E e2
(* 
  | mini.All e1 => (fun x => [ ALL x ]) <$> (B e1)
*)
  | mini.If3 e1 e2 e3 => fun _ => False
      
  | mini.Fun q eff i e1 (y,h,x) e2 => fun _ => False

  | _ => fun _ => False

  end.

(* ---------------------------------------------------- *)


Definition x : Ident := mini.Test.x.


(* { x:=1; x }  *)
Lemma t2 : 
  [ ⌈r |-> Int 1⌉ ] ∈ E mini.Test.t2.
Proof.
  unfold mini.Test.t2.
  exists [ ⌈r |-> Int 1, mini.Test.x |-> Int 1⌉ ].
  split.
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
      
End DSLS.


(* -------------------------------------------------------- *)
(* -------------------------------------------------------- *)
(* -------------------------------------------------------- *)

Module ELV.

Definition ALL (VS : list VAL) : VAL := 
  let start := ⌈ mkTup [] ⌉ in
  squash_fold_left (liftM2 snoc) VS start.

Lemma snoc_mktup xs v : 
  snoc (mkTup xs) v = mkTup (xs ++ [v]).
Admitted.

Example ALL_example : 
  ALL [ ⌈ Int 2 ⌉ ; ⌈ Int 3 ⌉ ] = ⌈ mkTup [Int 2; Int 3] ⌉.
cbn. eapply Extensionality_Ensembles. split.
- 
Admitted.
(*
move => x [[h1 [[h2 h3]|h2]]|h1]. 
  eapply not_Singleton_empty; eauto.
  eapply not_Singleton_empty; eauto.
  destruct h1 as [_ [a h1]].
  destruct h1 as [[h1|h3] [v [h2 h4]]].
  destruct h1.
  eapply not_Singleton_empty; eauto.
  destruct h3 as [_ [b [h5 [c [h6 h7]]]]].
  inversion h6. inversion h2. subst. inversion h5. subst.
  inversion h7. subst.
  rewrite snoc_mktup in h4.
  rewrite snoc_mktup in h4.
  cbn in h4. eauto.
- admit.
Admitted.
*)


Lemma map_union {A B} (f : A -> B) (s1 : P A) (s2 : P A) :
  (Sets.map f (s1 ∪ s2)) = ((Sets.map f s1) ∪ (Sets.map f s2)).
Proof.
  unfold Sets.map.
  eapply Extensionality_Ensembles.
  split.
  - intros x [a [h1 h2]]. subst. inversion h2. subst.
    left. exists a. split; auto.
    right. exists a. split; auto.
  - intros b1 h1. 
    inversion h1. subst. clear h1.
    + move: H => [ y [h2 h3]]. subst.
      exists y. split; auto. left. auto.
    + move: H => [ y [h2 h3]]. subst.
      exists y. split; auto. right. auto.
Qed.

Lemma map_singleton {A B} (f : A -> B) (a : A) :
  (Sets.map f ⌈ a ⌉) = ⌈ f a ⌉.
Admitted.
Lemma Union_empty {A} (V : P A) : (∅ ∪ V) = V.
Admitted.

(* another version of the dodgy big union *)
Definition UNIONS {A} (VVS : P (list (P A))) : list (P A) := 
  i <- allNums ;;
  let VS : P (P A) := Sets.map (fun VS => List.nth i VS ∅) VVS in
  [ UNION VS ].

Lemma UNION_union {A} (V W : P A) : (⨃ (⌈V ⌉ ∪ ⌈ W ⌉)) = (V ∪ W).
eapply Extensionality_Ensembles.
split.
- intros x xIn. cbv in xIn. move: xIn => [v [h1 h2]].
  inversion h1. subst. inversion H. subst. left. auto.
  subst. inversion H. subst. right. auto.
- intros x xIn. cbv. inversion xIn.
  + subst. exists V. split. left. eapply in_singleton. auto.
  + subst. exists W. split. right. eapply in_singleton. auto.
Qed.

Lemma UNION_Singleton {A} (V : P A) W : (⨃ (⌈V ⌉ ∪ W )) = (V ∪ ⨃ W).
Proof.
eapply Extensionality_Ensembles.
split.
- intros x xIn. cbv in xIn. move: xIn => [v [h1 h2]].
  inversion h1. subst. inversion H. subst. left. auto.
  right.  exists v. split; eauto.
- intros x xIn. inversion xIn. subst.
  exists V. split; auto. left. eapply in_singleton.
  cbv in H. move: H => [w [h1 h2]].
  exists w. split. right. eauto. eauto.
Qed.

Example UNIONS_ex : 
  UNIONS (⌈ [ ∅ ; ⌈ 3 ⌉ ] ⌉ ∪ ⌈ [ ⌈ 4 ⌉ ] ⌉) = 
    [  ⌈ 4 ⌉ ; ⌈ 3 ⌉ ].
Proof.
  unfold UNIONS, allNums. 
  replace limitNum with 2.
  cbn.
  repeat rewrite map_union. repeat rewrite map_singleton.
  cbn.
  repeat rewrite -> UNION_union. rewrite Union_empty.
Admitted.  

Fixpoint E (e :mini.Expr) (ρ:env) : list VAL := 
  
  let B (e : mini.Expr) (ρ : env) : list VAL  := 
    UNIONS
      (ρ' <- X e ρ ;; ⌈ E e ρ' ⌉) 
    in

  let R (e : mini.Expr) ρ : ENV := 
    ρ' <- X e ρ ;; 
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

  | mini.Fail => [∅]

  | mini.Choice e1 e2 => CHOICE (B e1 ρ) (B e2 ρ)

  | mini.Seq e1 e2 => IF2 (E e1 ρ) (E e2 ρ)

  | mini.Unify e1 e2 => UNIFY (E e1 ρ) (E e2 ρ)

  | mini.All e1 =>  [ ALL (B e1 ρ) ]

  | mini.If3 e1 e2 e3 => 
      let Δ  : ENV := R e1 ρ in
      let TS := UNIONS ( ρ' <- Δ ;; ⌈ B e2 ρ' ⌉ )in
      let FS := B e2 ρ in
      (* NEED SETMINUS HERE *)
      IF2 TS FS
      
  | mini.Fun q eff i e1 (y,h,x) e2 =>  []

  | _ => []

  end.

End ELV.

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
