Set Implicit Arguments.
Set Strict Implicit.
Set Universe Polymorphism.

(* Taken from 
https://github.com/vellvm/monad/blob/Transformers/theories/EqmR/EqmR.v
but specialized to only Logic.eq as the 
equivalence relation for the monad *)

Require Import Monad.

Open Scope monad_scope.

Import FunctorNotation.
Import ApplicativeNotation.
Import MonadNotation.
Import MonadElemNotation.

Section BasicLaws.
  Context (m : Type -> Type).
  Context {Mm : Monad m}.

(* Functor Laws *)

(* Monad laws *)
Class BindRetL : Prop :=
  bind_ret_l : forall {A B : Type}  (f : A -> m B) (a : A),
      bind (ret a) f = f a.

Class BindRetR : Prop :=
  bind_ret_r : forall {A : Type} (ma : m A),
      bind ma ret = ma.

Class BindBind : Prop :=
  bind_bind : forall {A B C : Type} (ma : m A) 
                (f : A -> m B) (g : B -> m C),
      bind (bind ma f) g = bind ma (fun x => bind (f x) g).

(* Inversion Laws *)
  Class FmapInv :=
    fmap_inv :
      forall (A B:Type) 
        (f1 : A -> B) (f2 :A -> B) (ma1 : m A) (ma2:m A),
        (f1 <$> ma1) = (f2 <$> ma2) ->
        ma1 = ma2.

  Class RetInv : Prop :=
    ret_inv :
      forall {A: Type} (a1 a2:A),
        (ret a1 : m A) = ret a2 -> a1 = a2.

  Class BindInv : Prop :=
    bind_inv : forall {A B: Type} 
      (ma1 : m A) (ma2 : m A) (k1 : A -> m B) (k2 : A -> m B),
      (x <- ma1;; k1 x) = (x <- ma2;; k2 x) ->
      ma1 = ma2.


  Class BindRetInv :=
    bind_ret_inv:
      forall {A B : Type} (ma : m A) (kb : A -> m B) (b : B),
        bind ma kb = ret b -> 
        exists a : A, ma = ret a /\ kb a = ret b.

End BasicLaws.

(*
Section MayRetLaws.
  Context (m : Type -> Type).
  Context {Mm : Monad m}.
  Context {Rm : Elem m}.

  Class MayRetBindInv : Prop :=
    mayRet_bind_inv :
      forall (A B:Type) (ma:m A) (k:A -> m B),
      forall b, b ∈ bind ma k -> exists a, a ∈ ma /\ b ∈ k a.

  Class FmapInvRet : Prop :=
    fmap_inv_ret :
      forall (A B:Type) (ma :m A) (f f' :A -> B),
        (f <$> ma) = (f' <$> ma) ->
        forall a : A, a ∈ ma -> ret (f a) = ret (f' a).


End MayRetLaws.
*)

Arguments bind_ret_l {m _ _ _}.
Arguments bind_ret_r {m _ _ _}.
Arguments bind_bind {m _ _ _}.
Arguments ret_inv {m _ _ _ _ _}.
Arguments bind_inv {m _ _ _ _ _ _}.
Arguments fmap_inv {m _ _ _}.
