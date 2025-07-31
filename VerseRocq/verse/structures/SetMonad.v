Require Import Imports.

Require Import structures.Sets.
Require Import structures.Monad.
Require Import structures.Laws.

Import SetNotations.

Require Import Laws.

(* ------------------------------------------------------------- *)

(* P is a monad *)

#[export] Instance Monad_P : Monad P :=
  { ret  := @Singleton;
    bind := @Sets.bind
   }.

#[export] Instance Functor_P : Functor P :=
  { fmap := @map
  }.

Definition ap := fun {A B} (m1 :  P (A -> B)) (m2 : P A) => 
                   bind m1 (fun x1 => 
                              bind m2 (fun x2 => 
                                         ret (x1 x2))).

#[export] Instance Applicative_P : Applicative P :=
  { pure := @Singleton;
     ap  := @ap 
  }.

#[export] Instance Alternative_P : Alternative P :=
  { empty := @Empty_set ;
    choose := @Union
   }.


(* -------------- some monad laws -------------- *)


#[export] Instance BindRetL_P : BindRetL (m:=P).
intros A B f a. set_simpl. done. 
Qed.

#[export] Instance BindRetR_P : BindRetR (m:=P).
intros A ma. set_simpl. done. 
Qed.

#[export] Instance BindBind_P : BindBind (m:=P).
intros A B C ma f g. set_simpl. done.
Qed.

#[export] Instance RetInv_P : RetInv (m:=P).
intros A a1 a2 h. cbn in h.
eapply Singleton_inv. auto.
Qed.

