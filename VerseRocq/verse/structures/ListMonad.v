Require Import Imports.
From Stdlib Require Import Lists.List.

Require Import structures.List.
Require Import structures.Monad.
Require Import structures.Laws.

Import ListNotations.

(* Monad definitions *)
  

#[export] Instance Functor_list : Functor list :=
{ fmap := map }.

#[export] Instance Monad_list : Monad list :=
{ ret  := fun _ x => x :: nil; 
  bind := fun _ _ x f =>  flat_map f x
}.

#[export] Instance Applicative_list : Applicative list :=
{ pure := fun _ x => x :: nil;
  ap   := @List.ap
}.

#[export] Instance Alternative_list : Alternative list :=
  { empty := @nil ;
    choose := @app
  }.

#[export] Instance Elem_list : Elem list := 
  { elem := fun {A} ls x => @List.In A x ls }.


(* Monad Laws *)

#[export] Instance BindRetL_list : BindRetL (m:=list).
intros A B f a. cbn. rewrite app_nil_r. done. Qed.

#[export] Instance BindRetR_list : BindRetR (m:=list).
intros A v. cbn. list_simpl. done. Qed.

#[export] Instance BindBind_list : BindBind (m:=list).
intros A B C ma f g. cbn. list_simpl. done. Qed.

#[export] Instance RetInv_list : RetInv (m:=list).
intros A a1 a2 h. cbn in h. inversion h. done.
Qed.

#[export] Instance BindRetInv_list : BindRetInv (m:=list).
Abort.
