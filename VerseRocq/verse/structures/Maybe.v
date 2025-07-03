Require Import structures.Monad.
Set Implicit Arguments.

Import MonadNotation.
Import ApplicativeNotation.
Open Scope monad_scope.

(* This is a version of the option type, in Prop instead of Type 
   so that we can eliminate into it. *)
Variant Maybe (A : Type) : Prop := 
  | Nothing : Maybe A
  | Just : A -> Maybe A.

Arguments Just {_}.
Arguments Nothing {_}.


Definition Maybe_map {A B} (f: A -> B) (m : Maybe A) : Maybe B := 
  match m with 
  | Just s => Just (f s)
  | Nothing => Nothing 
  end.

Definition Maybe_bind {A B} (m : Maybe A) (k : A -> Maybe B) := 
  match m with 
  | Just s => k s 
  | Nothing => Nothing 
  end.

Definition Maybe_ap {A B} (m : Maybe (A -> B)) (a : Maybe A) : Maybe B :=
  match m , a with 
  | Just f , Just x => Just (f x)
  | _ , _ => Nothing
   end.

Definition Maybe_choose {A} : Maybe A -> Maybe A -> Maybe A :=
  fun l r => 
    match l with 
    | Nothing => r
    | y => y
    end.


#[export] Instance Functor_Maybe : Functor Maybe := { 
    fmap :=  @Maybe_map
}.

#[export] Instance Applicative_Maybe : Applicative Maybe := {
    pure := @Just ;
    ap :=  @Maybe_ap 
}.

#[export] Instance Monad_Maybe : Monad Maybe := {
    ret  := @Just ;
    bind := @Maybe_bind 
}.

#[export] Instance Alternative_Maybe : Alternative Maybe := { 
    empty := @Nothing ;
    choose := @Maybe_choose 
}.



(* Functor law *)

Lemma Maybe_fmap_fmap {A B C} (f : B -> C) (g : A -> B) (y : Maybe A) :
  fmap f (fmap g y) = fmap (fun x => f (g x)) y.
Proof.
  destruct y; auto.
Qed.

(* Applicative laws *)

(*
pure id <*> v = v                            -- Identity
pure f <*> pure x = pure (f x)               -- Homomorphism
u <*> pure y = pure ($ y) <*> u              -- Interchange
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
*)

Lemma Maybe_Applicative_identity {A} (v : Maybe A) : 
  pure (fun x => x)  <*> v = v.
Proof. 
  unfold pure, ap, Applicative_Maybe.
  destruct v.
  cbn. auto.
  cbn. f_equal.  
Qed.

Lemma Maybe_Applicative_homomorphism {A B} (f : A -> B) (x : A): 
  pure f <*> pure x = pure (f x).
Proof.
  cbn. auto.
Qed.

Lemma Maybe_Applicative_interchange {A B} (u : Maybe (A -> B)) (y : A) :
  u <*> pure y = pure (fun f => f y) <*> u.
Proof.
  destruct u; cbn; auto.
Qed.

Lemma Maybe_Applicative_composition {A B C} (u : Maybe (B -> C)) (v : Maybe (A -> B))  (w : Maybe A) : 
  pure (fun f g x => f (g x)) <*> u <*> v <*> w = u <*> (v <*> w).
Proof.
  destruct u; auto.
  destruct v; auto.
  destruct w; auto.
Qed.

(* Monad laws *)

Lemma Maybe_Left_identity {A B} (x : A) (h : A -> Maybe B) : ret x >>= h = h x.
Proof.
  cbn. auto.
Qed.

Lemma Maybe_Right_identity {A} (m : Maybe A) : m >>= ret = m.
  destruct m; auto.
Qed.

Lemma Maybe_associativity {A B C} (m : Maybe A) (g : A -> Maybe B) (h : B -> Maybe C) : 
 (m >>= g) >>= h = m >>= (fun x => g x >>= h).
Proof.
  destruct m; auto.
Qed.
