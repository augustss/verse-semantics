From Stdlib Require Import Logic.FunctionalExtensionality.
From Stdlib Require Import Logic.PropExtensionality.

Require Import Imports.
From Stdlib Require Lists.List.
Require Import structures.Monad.
Require Import structures.Sets.
Require Import structures.List.
Set Implicit Arguments.

Import List.ListNotations.
Import SetNotations.
Import MonadNotation.
Import ApplicativeNotation.
Open Scope monad_scope.
Open Scope list_scope.

Definition M A := P (list A).

Definition M_map {A B} (f : A -> B) (x : P (list A)) :  P (list B) := 
  fmap (fmap f) x.

Definition M_ret {A} : A -> P (list A) :=
  fun x => ⌈ [ x ] ⌉.

Definition M_bind {A B} (m : P (list A)) (k : A -> P (list B)) : P (list B) := 
    l <- m ;;
    let RS := (List.map k l : list (P (list B))) in
    (List.fold_right Union ∅ RS).


Lemma fmap_map {A B C} (f:B -> A) (g : C -> B) x : 
  M_map f (M_map g x) = M_map (fun x => f (g x)) x.
Proof.
  unfold M_map. unfold fmap, Functor_P, Functor_list.
  cbn.
  extensionality b.
  eapply propositional_extensionality.
  split.
  - intros [BS [[CS [h1 h2]] h3]]. 
    inversion h2. subst. clear h2.
    inversion h3. subst. clear h3.
    exists CS. split; auto.
    rewrite List.map_map. 
    eapply in_singleton.
  - intros [CS [h1 h2]]. 
    inversion h2. subst. clear h2.
    exists (List.map g CS). split.
    exists CS. split; eauto. eapply in_singleton.
    rewrite List.map_map. eapply in_singleton.
Qed.

Lemma M_bind_ret {A B} (x : A) (k : A -> M B) : 
  M_bind (M_ret x) k = k x.
Proof.
  unfold M_bind, M_ret. 
  eapply Extensionality_Ensembles.
  split.
  - intros y yIn.
    cbn in yIn.
    move: yIn => [a [h1 h2]]. 
    inversion h1. subst. clear h1. 
    cbn in h2. inversion h2; try done. 
  - intros y yIn.
    cbn.
    exists [ x ].
    split.  eapply in_singleton; auto.
    cbn. left. auto.
Qed.  

Lemma M_ret_bind {A} (m : M A): M_bind m M_ret = m.
Proof. 
  unfold M_bind, M_ret. 
  eapply Extensionality_Ensembles.
  split.
  - intros y yIn.
    cbn in yIn.
    destruct yIn as [AS [h1 h2]].
    have LEMMA: (forall l, List.fold_right Union ∅ (List.map ret l) = mem l).
    { clear. intros T l. induction l. cbv. admit.
      cbn. 
      unfold mem. cbn.
      eapply Extensionality_Ensembles.
      split.
      - intros x xIn. destruct xIn.
        + inversion H. subst. left. auto.
        + rewrite IHl in H. unfold mem in H. right. auto.
      - intros x xIn. rewrite IHl.
        destruct xIn.
        + subst. left. auto.
        + right. auto.
    }

    specialize (LEMMA _ (List.map (fun x => [x]) AS)).
    rewrite List.map_map in LEMMA.
    unfold ret, Monad_P in LEMMA.
    rewrite LEMMA in h2.
Abort.
