Require Import Imports.

From Stdlib Require Import Classes.EquivDec.
Require Import structures.Sets.
Require Import structures.List.

Import SetNotations.
Import SetMonadNotation.
Import List.ListNotations.
Import ListMonadNotation.

Open Scope list_scope.
Open Scope set_scope.


(* This axiom is BAD. But convenient for now.... *)
Axiom empty_dec : forall {A} (s : P A), 
    { s ≃ ∅ } + { ~ (s ≃ ∅) }.

Definition is_Empty_set {A} (s : P A) : bool := 
  match empty_dec s with 
  | left _ => true | right _ => false 
  end.

Lemma singleton_not_empty {A} {v : A} : ⌈ v ⌉ <> ∅. Admitted.

Lemma notIn_singleton {A}{v : A} : ~ List.In ∅ [⌈ v ⌉]. 
Proof.
  intro h. inversion h.  apply singleton_not_empty in H. done. inversion H.
Qed.


(* ------------- functional squash -------------- *)

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

Definition Cons {A} := (fun (V : P A) (VS : P (list A)) => 
    v  ⭅ V  ;;  vs ⭅ VS ;; ⌈ v :: vs⌉).

(* doesn't require axiom *)
Fixpoint squash_pick {A} (xs : list (P A)) : P (list A) := 
  match xs with 
  | nil => ⌈ [] ⌉
  | V :: VS => If3 V  
               (Cons V (squash_pick VS))
               (squash_pick VS)
  end.

(* -------------- Propositional Squash ------------ *)

(* Squashed VS WS holds when WS := filter (fun x => x <> ∅) VS

   NOTE: We cannot define this as a function (without using an axiom)
   because results in Type cannot depend on Prop. But this relation 
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
Definition Succeeds {A} (VS : P (list (P A))) : Prop :=
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


Lemma NonEmptyTail_nil {A} : @NonEmptyTail A [].
done. Qed.
Lemma NonEmptyTail_singleton {A}{v :A}: NonEmptyTail [ ⌈v⌉ ].
cbn. exists nil. exists ⌈v⌉. cbv. split; auto; eapply singleton_not_empty. Qed.

Lemma TailSquash_singleton {A} (v:A) VS : 
  VS = [⌈ v ⌉] ->
  TailSquash [⌈ v ⌉] VS.
Proof. intros ->. unfold TailSquash. split. exists 0. cbn. auto.
eapply NonEmptyTail_singleton; eauto. Qed.

Lemma TailSquash_nil {A}  (VS : list (P A)) : 
  VS = nil ->
  TailSquash [] VS.
Proof. intros ->. unfold TailSquash. split. exists 0. cbn. auto.
eapply NonEmptyTail_nil; eauto. Qed.

Lemma TailSquash_empty {A} (VS : list (P A)) : 
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

Lemma Squash_nil {A} (VS : list (P A)) : 
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
 : sets.

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
