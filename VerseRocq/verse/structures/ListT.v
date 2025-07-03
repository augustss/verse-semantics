(* List Monad Transformer "done right" *)
(* https://github.com/Gabriella439/list-transformer/blob/main/src/List/Transformer.hs *)

Require Import structures.Monad.
Require Import structures.Maybe.

Import MonadNotation.
Import ApplicativeNotation.

Open Scope monad_scope.

Set Implicit Arguments.

(* Specialize ListT with "Maybe" to work around 
   non-strict positivity in definition of the transformer. *)

Inductive M a := 
  Result : Maybe (List a) -> M a
with List a := 
  Cons : a -> (M a) -> List a | Nil : List a.

Arguments Result {_}.
Arguments Cons {_}.
Arguments Nil {_}.

Scheme M_List_ind := Induction for M Sort Prop
  with List_M_ind := Induction for List Sort Prop.

Definition next {A} : M A -> Maybe (List A) := 
  fun m => match m with Result x => x end.

Definition M_empty {A} : M A := 
  Result (Just Nil).

Fixpoint M_choose {A} (m : M A) (l : M A) : M A := 
  match m with 
  | Result Nothing => Result Nothing
  | Result (Just s) => Result
      (match s with 
      | Nil => next l
      | Cons x l' => Just (Cons x (M_choose l' l))
      end)
  end.

Fixpoint M_fmap {A}{B} (k : A -> B) (m : M A) : M B := 
  match m with 
  | Result Nothing => Result Nothing
  | Result (Just s) => Result (Just (List_fmap k s))
  end with
List_fmap {A}{B} (k : A -> B) (m : List A) : List B := 
  match m with 
  | Nil => Nil
  | Cons x ma => Cons (k x) (M_fmap k ma)
  end.

Definition M_pure {A} (x : A) : M A := 
  Result (Just (Cons x M_empty)).

Fixpoint M_ap {A B} (m : M (A -> B)) (l : M A) : M B := 
  match m with 
  | Result Nothing => Result Nothing
  | Result (Just s) => Result (match s with 
                      | Nil => Just Nil
                      | Cons f l' => next (M_choose (M_fmap f l) (M_ap l' l))
                      end)
  end.

Fixpoint M_bind {A B} (n : M A) (k : A -> M B) : M B := 
  match n with 
  | Result Nothing => Result Nothing
  | Result (Just s) => Result (match s with 
                              | Nil => Just Nil
                              | Cons x l' => next (M_choose (k x) (M_bind l' k))
                              end)
  end.


#[export] Instance Functor_M : Functor M := { 
    fmap :=  @M_fmap
}.

#[export] Instance Applicative_M : Applicative M := {
    pure := @M_pure ;
    ap :=  @M_ap 
}.

#[export] Instance Monad_M : Monad M := {
    ret  := @M_pure ;
    bind := @M_bind 
}.

#[export] Instance Alternative_M : Alternative M := { 
    empty := @M_empty ;
    choose := @M_choose 
}.

Import AlternativeNotation.

Lemma Result_next_eta {A} (x:M A) :
  Result (next x) = x.
Proof. destruct x; auto. Qed.

(* -------------- Laws ------------ *)

Lemma M_choose_empty_l {A} (m : M A) : (empty <|> m) = m.
destruct m; cbn. auto.
Qed.

Fixpoint M_choose_empty_r {A} (m : M A) : (m <|> empty) = m.
Proof.
  destruct m; cbn.
  destruct m; cbn; auto.
  f_equal.
  destruct l; cbn; auto.
  f_equal.
  f_equal.
  eapply M_choose_empty_r.
Qed.

Fixpoint M_choose_assoc {A} (m n p : M A) : 
  m <|> (n <|> p) = (m <|> n) <|> p.
Proof.
  destruct m; cbn.
  destruct m; cbn; auto.
  destruct l; cbn; auto.
  - f_equal.
    f_equal.
    f_equal.
    eapply M_choose_assoc.
  - rewrite Result_next_eta.
    destruct n; cbn; auto.
Qed.

Fixpoint M_fmap_id {A} (x : M A) :
  M_fmap (fun x => x) x = x.
Proof. 
  destruct x; cbn.
  destruct m; cbn; auto.
  f_equal.
  destruct l; cbn; auto.
  rewrite M_fmap_id. auto.
Qed.

Fixpoint M_fmap_fmap {A B C} (f : B -> C) (g : A -> B) (y : M A) :
  M_fmap f (M_fmap g y) = M_fmap (fun x => f (g x)) y
with List_fmap_fmap {A B C} (f : B -> C) (g : A -> B) (y : List A) :
  List_fmap f (List_fmap g y) = List_fmap (fun x => f (g x)) y.
Proof.
  - destruct y; cbn.
  destruct m; auto.
  cbn.
  rewrite List_fmap_fmap. auto.
  - destruct y; cbn; auto.
    f_equal.
    eapply M_fmap_fmap.
Qed.

Fixpoint M_Applicative_identity {A} (v : M A) : 
  pure (fun x => x)  <*> v = v.
Proof.
  destruct v.
  cbn.
  rewrite Result_next_eta.
  destruct m; cbn; auto.
  f_equal.
  destruct l; cbn; auto.
  f_equal.
  fold (@M_empty A).
  rewrite M_choose_empty_r. 
  rewrite M_fmap_id.
  auto.
Qed.

Lemma M_Applicative_homomorphism {A B} (f : A -> B) (x : A): 
  pure f <*> pure x = pure (f x).
Proof.
  cbn.
  unfold M_pure.
  f_equal.
Qed.

Fixpoint M_Applicative_interchange {A B} (u : M (A -> B)) (y : A) :
  u <*> pure y = pure (fun f => f y) <*> u.
Proof.
  destruct u; cbn; auto.
  rewrite Result_next_eta.
  destruct m; cbn; auto.
  f_equal.
  destruct l; cbn; auto.
  fold (@M_empty B).
  rewrite M_choose_empty_r.
  f_equal.
  f_equal.
  rewrite Result_next_eta.
  rewrite -> M_Applicative_interchange.
  cbn.
  rewrite Result_next_eta.
  fold (@M_empty B).
  rewrite M_choose_empty_r.
  auto.
Qed.

Fixpoint M_Applicative_composition {A B C} (u : M (B -> C)) (v : M (A -> B))  (w : M A) : 
  pure (fun f g x => f (g x)) <*> u <*> v <*> w = u <*> (v <*> w).
Proof.
  destruct u; cbn.
  destruct m; cbn; auto.
  destruct l; cbn; auto.
Abort.

(* Monad laws *)

Lemma M_left_identity {A B} (x : A) (h : A -> M B) : ret x >>= h = h x.
Proof.
  cbn. 
  fold (@M_empty B).  
  rewrite M_choose_empty_r.
  rewrite Result_next_eta.
  auto.
Qed.

Fixpoint M_right_identity {A} (m : M A) : m >>= ret = m.
  destruct m; cbn; auto.
  destruct m; cbn; auto.
  destruct l; cbn; auto.
  rewrite -> M_right_identity.
  rewrite Result_next_eta.
  auto.
Qed.

(* M_bind =
fix M_bind (A B : Type) (n : M A) (k : A -> M B) {struct n} : M B :=
  match n with
  | Result Nothing => Result Nothing
  | Result (Just s) => Result match s with
                              | Cons x l' => next (M_choose (k x) (M_bind A B l' k))
                              | Nil => Just Nil
                              end
  end *)

Fixpoint M_associativity {A B C} (n : M A) (g : A -> M B) (h : B -> M C) : 
 (n >>= g) >>= h = n >>= (fun x => g x >>= h).
Proof.
Admitted.
