Require Import autosubst.lib autosubst.fintype.
Require Export syntax.common.
Require Import Setoid Morphisms Relation_Definitions.

Module Core.

Inductive Expr (n_Expr : nat) : Type :=
  | var_Expr : fin n_Expr -> Expr n_Expr
  | Lit : LitType -> Expr n_Expr
  | Tup : list (Expr n_Expr) -> Expr n_Expr
  | Tru : Expr n_Expr -> Expr n_Expr
  | Lam : Expr (S n_Expr) -> Expr n_Expr
  | Op : PrimOp -> Expr n_Expr
  | Unify : Expr n_Expr -> Expr n_Expr -> Expr n_Expr
  | Seq : Expr n_Expr -> Expr n_Expr -> Expr n_Expr
  | Or : Expr n_Expr -> Expr n_Expr -> Expr n_Expr
  | App : Expr n_Expr -> Expr n_Expr -> Expr n_Expr
  | Exi : Expr (S n_Expr) -> Expr n_Expr
  | Fail : Expr n_Expr
  | Iter : IterType -> Expr n_Expr -> Expr n_Expr -> Expr n_Expr.

Lemma congr_Lit {m_Expr : nat} {s0 : LitType} {t0 : LitType} (H0 : s0 = t0) :
  Lit m_Expr s0 = Lit m_Expr t0.
Proof.
exact (eq_trans eq_refl (ap (fun x => Lit m_Expr x) H0)).
Qed.

Lemma congr_Tup {m_Expr : nat} {s0 : list (Expr m_Expr)}
  {t0 : list (Expr m_Expr)} (H0 : s0 = t0) : Tup m_Expr s0 = Tup m_Expr t0.
Proof.
exact (eq_trans eq_refl (ap (fun x => Tup m_Expr x) H0)).
Qed.

Lemma congr_Tru {m_Expr : nat} {s0 : Expr m_Expr} {t0 : Expr m_Expr}
  (H0 : s0 = t0) : Tru m_Expr s0 = Tru m_Expr t0.
Proof.
exact (eq_trans eq_refl (ap (fun x => Tru m_Expr x) H0)).
Qed.

Lemma congr_Lam {m_Expr : nat} {s0 : Expr (S m_Expr)} {t0 : Expr (S m_Expr)}
  (H0 : s0 = t0) : Lam m_Expr s0 = Lam m_Expr t0.
Proof.
exact (eq_trans eq_refl (ap (fun x => Lam m_Expr x) H0)).
Qed.

Lemma congr_Op {m_Expr : nat} {s0 : PrimOp} {t0 : PrimOp} (H0 : s0 = t0) :
  Op m_Expr s0 = Op m_Expr t0.
Proof.
exact (eq_trans eq_refl (ap (fun x => Op m_Expr x) H0)).
Qed.

Lemma congr_Unify {m_Expr : nat} {s0 : Expr m_Expr} {s1 : Expr m_Expr}
  {t0 : Expr m_Expr} {t1 : Expr m_Expr} (H0 : s0 = t0) (H1 : s1 = t1) :
  Unify m_Expr s0 s1 = Unify m_Expr t0 t1.
Proof.
exact (eq_trans (eq_trans eq_refl (ap (fun x => Unify m_Expr x s1) H0))
         (ap (fun x => Unify m_Expr t0 x) H1)).
Qed.

Lemma congr_Seq {m_Expr : nat} {s0 : Expr m_Expr} {s1 : Expr m_Expr}
  {t0 : Expr m_Expr} {t1 : Expr m_Expr} (H0 : s0 = t0) (H1 : s1 = t1) :
  Seq m_Expr s0 s1 = Seq m_Expr t0 t1.
Proof.
exact (eq_trans (eq_trans eq_refl (ap (fun x => Seq m_Expr x s1) H0))
         (ap (fun x => Seq m_Expr t0 x) H1)).
Qed.

Lemma congr_Or {m_Expr : nat} {s0 : Expr m_Expr} {s1 : Expr m_Expr}
  {t0 : Expr m_Expr} {t1 : Expr m_Expr} (H0 : s0 = t0) (H1 : s1 = t1) :
  Or m_Expr s0 s1 = Or m_Expr t0 t1.
Proof.
exact (eq_trans (eq_trans eq_refl (ap (fun x => Or m_Expr x s1) H0))
         (ap (fun x => Or m_Expr t0 x) H1)).
Qed.

Lemma congr_App {m_Expr : nat} {s0 : Expr m_Expr} {s1 : Expr m_Expr}
  {t0 : Expr m_Expr} {t1 : Expr m_Expr} (H0 : s0 = t0) (H1 : s1 = t1) :
  App m_Expr s0 s1 = App m_Expr t0 t1.
Proof.
exact (eq_trans (eq_trans eq_refl (ap (fun x => App m_Expr x s1) H0))
         (ap (fun x => App m_Expr t0 x) H1)).
Qed.

Lemma congr_Exi {m_Expr : nat} {s0 : Expr (S m_Expr)} {t0 : Expr (S m_Expr)}
  (H0 : s0 = t0) : Exi m_Expr s0 = Exi m_Expr t0.
Proof.
exact (eq_trans eq_refl (ap (fun x => Exi m_Expr x) H0)).
Qed.

Lemma congr_Fail {m_Expr : nat} : Fail m_Expr = Fail m_Expr.
Proof.
exact (eq_refl).
Qed.

Lemma congr_Iter {m_Expr : nat} {s0 : IterType} {s1 : Expr m_Expr}
  {s2 : Expr m_Expr} {t0 : IterType} {t1 : Expr m_Expr} {t2 : Expr m_Expr}
  (H0 : s0 = t0) (H1 : s1 = t1) (H2 : s2 = t2) :
  Iter m_Expr s0 s1 s2 = Iter m_Expr t0 t1 t2.
Proof.
exact (eq_trans
         (eq_trans (eq_trans eq_refl (ap (fun x => Iter m_Expr x s1 s2) H0))
            (ap (fun x => Iter m_Expr t0 x s2) H1))
         (ap (fun x => Iter m_Expr t0 t1 x) H2)).
Qed.

Lemma upRen_Expr_Expr {m : nat} {n : nat} (xi : fin m -> fin n) :
  fin (S m) -> fin (S n).
Proof.
exact (up_ren xi).
Defined.

Lemma upRen_list_Expr_Expr (p : nat) {m : nat} {n : nat}
  (xi : fin m -> fin n) : fin (plus p m) -> fin (plus p n).
Proof.
exact (upRen_p p xi).
Defined.

Fixpoint ren_Expr {m_Expr : nat} {n_Expr : nat}
(xi_Expr : fin m_Expr -> fin n_Expr) (s : Expr m_Expr) {struct s} :
Expr n_Expr :=
  match s with
  | var_Expr _ s0 => var_Expr n_Expr (xi_Expr s0)
  | Lit _ s0 => Lit n_Expr s0
  | Tup _ s0 => Tup n_Expr (list_map (ren_Expr xi_Expr) s0)
  | Tru _ s0 => Tru n_Expr (ren_Expr xi_Expr s0)
  | Lam _ s0 => Lam n_Expr (ren_Expr (upRen_Expr_Expr xi_Expr) s0)
  | Op _ s0 => Op n_Expr s0
  | Unify _ s0 s1 => Unify n_Expr (ren_Expr xi_Expr s0) (ren_Expr xi_Expr s1)
  | Seq _ s0 s1 => Seq n_Expr (ren_Expr xi_Expr s0) (ren_Expr xi_Expr s1)
  | Or _ s0 s1 => Or n_Expr (ren_Expr xi_Expr s0) (ren_Expr xi_Expr s1)
  | App _ s0 s1 => App n_Expr (ren_Expr xi_Expr s0) (ren_Expr xi_Expr s1)
  | Exi _ s0 => Exi n_Expr (ren_Expr (upRen_Expr_Expr xi_Expr) s0)
  | Fail _ => Fail n_Expr
  | Iter _ s0 s1 s2 =>
      Iter n_Expr s0 (ren_Expr xi_Expr s1) (ren_Expr xi_Expr s2)
  end.

Lemma up_Expr_Expr {m : nat} {n_Expr : nat} (sigma : fin m -> Expr n_Expr) :
  fin (S m) -> Expr (S n_Expr).
Proof.
exact (scons (var_Expr (S n_Expr) var_zero) (funcomp (ren_Expr shift) sigma)).
Defined.

Lemma up_list_Expr_Expr (p : nat) {m : nat} {n_Expr : nat}
  (sigma : fin m -> Expr n_Expr) : fin (plus p m) -> Expr (plus p n_Expr).
Proof.
exact (scons_p p (funcomp (var_Expr (plus p n_Expr)) (zero_p p))
         (funcomp (ren_Expr (shift_p p)) sigma)).
Defined.

Fixpoint subst_Expr {m_Expr : nat} {n_Expr : nat}
(sigma_Expr : fin m_Expr -> Expr n_Expr) (s : Expr m_Expr) {struct s} :
Expr n_Expr :=
  match s with
  | var_Expr _ s0 => sigma_Expr s0
  | Lit _ s0 => Lit n_Expr s0
  | Tup _ s0 => Tup n_Expr (list_map (subst_Expr sigma_Expr) s0)
  | Tru _ s0 => Tru n_Expr (subst_Expr sigma_Expr s0)
  | Lam _ s0 => Lam n_Expr (subst_Expr (up_Expr_Expr sigma_Expr) s0)
  | Op _ s0 => Op n_Expr s0
  | Unify _ s0 s1 =>
      Unify n_Expr (subst_Expr sigma_Expr s0) (subst_Expr sigma_Expr s1)
  | Seq _ s0 s1 =>
      Seq n_Expr (subst_Expr sigma_Expr s0) (subst_Expr sigma_Expr s1)
  | Or _ s0 s1 =>
      Or n_Expr (subst_Expr sigma_Expr s0) (subst_Expr sigma_Expr s1)
  | App _ s0 s1 =>
      App n_Expr (subst_Expr sigma_Expr s0) (subst_Expr sigma_Expr s1)
  | Exi _ s0 => Exi n_Expr (subst_Expr (up_Expr_Expr sigma_Expr) s0)
  | Fail _ => Fail n_Expr
  | Iter _ s0 s1 s2 =>
      Iter n_Expr s0 (subst_Expr sigma_Expr s1) (subst_Expr sigma_Expr s2)
  end.

Lemma upId_Expr_Expr {m_Expr : nat} (sigma : fin m_Expr -> Expr m_Expr)
  (Eq : forall x, sigma x = var_Expr m_Expr x) :
  forall x, up_Expr_Expr sigma x = var_Expr (S m_Expr) x.
Proof.
exact (fun n =>
       match n with
       | Some fin_n => ap (ren_Expr shift) (Eq fin_n)
       | None => eq_refl
       end).
Qed.

Lemma upId_list_Expr_Expr {p : nat} {m_Expr : nat}
  (sigma : fin m_Expr -> Expr m_Expr)
  (Eq : forall x, sigma x = var_Expr m_Expr x) :
  forall x, up_list_Expr_Expr p sigma x = var_Expr (plus p m_Expr) x.
Proof.
exact (fun n =>
       scons_p_eta (var_Expr (plus p m_Expr))
         (fun n => ap (ren_Expr (shift_p p)) (Eq n)) (fun n => eq_refl)).
Qed.

Fixpoint idSubst_Expr {m_Expr : nat} (sigma_Expr : fin m_Expr -> Expr m_Expr)
(Eq_Expr : forall x, sigma_Expr x = var_Expr m_Expr x) (s : Expr m_Expr)
{struct s} : subst_Expr sigma_Expr s = s :=
  match s with
  | var_Expr _ s0 => Eq_Expr s0
  | Lit _ s0 => congr_Lit (eq_refl s0)
  | Tup _ s0 => congr_Tup (list_id (idSubst_Expr sigma_Expr Eq_Expr) s0)
  | Tru _ s0 => congr_Tru (idSubst_Expr sigma_Expr Eq_Expr s0)
  | Lam _ s0 =>
      congr_Lam
        (idSubst_Expr (up_Expr_Expr sigma_Expr) (upId_Expr_Expr _ Eq_Expr) s0)
  | Op _ s0 => congr_Op (eq_refl s0)
  | Unify _ s0 s1 =>
      congr_Unify (idSubst_Expr sigma_Expr Eq_Expr s0)
        (idSubst_Expr sigma_Expr Eq_Expr s1)
  | Seq _ s0 s1 =>
      congr_Seq (idSubst_Expr sigma_Expr Eq_Expr s0)
        (idSubst_Expr sigma_Expr Eq_Expr s1)
  | Or _ s0 s1 =>
      congr_Or (idSubst_Expr sigma_Expr Eq_Expr s0)
        (idSubst_Expr sigma_Expr Eq_Expr s1)
  | App _ s0 s1 =>
      congr_App (idSubst_Expr sigma_Expr Eq_Expr s0)
        (idSubst_Expr sigma_Expr Eq_Expr s1)
  | Exi _ s0 =>
      congr_Exi
        (idSubst_Expr (up_Expr_Expr sigma_Expr) (upId_Expr_Expr _ Eq_Expr) s0)
  | Fail _ => congr_Fail
  | Iter _ s0 s1 s2 =>
      congr_Iter (eq_refl s0) (idSubst_Expr sigma_Expr Eq_Expr s1)
        (idSubst_Expr sigma_Expr Eq_Expr s2)
  end.

Lemma upExtRen_Expr_Expr {m : nat} {n : nat} (xi : fin m -> fin n)
  (zeta : fin m -> fin n) (Eq : forall x, xi x = zeta x) :
  forall x, upRen_Expr_Expr xi x = upRen_Expr_Expr zeta x.
Proof.
exact (fun n =>
       match n with
       | Some fin_n => ap shift (Eq fin_n)
       | None => eq_refl
       end).
Qed.

Lemma upExtRen_list_Expr_Expr {p : nat} {m : nat} {n : nat}
  (xi : fin m -> fin n) (zeta : fin m -> fin n)
  (Eq : forall x, xi x = zeta x) :
  forall x, upRen_list_Expr_Expr p xi x = upRen_list_Expr_Expr p zeta x.
Proof.
exact (fun n =>
       scons_p_congr (fun n => eq_refl) (fun n => ap (shift_p p) (Eq n))).
Qed.

Fixpoint extRen_Expr {m_Expr : nat} {n_Expr : nat}
(xi_Expr : fin m_Expr -> fin n_Expr) (zeta_Expr : fin m_Expr -> fin n_Expr)
(Eq_Expr : forall x, xi_Expr x = zeta_Expr x) (s : Expr m_Expr) {struct s} :
ren_Expr xi_Expr s = ren_Expr zeta_Expr s :=
  match s with
  | var_Expr _ s0 => ap (var_Expr n_Expr) (Eq_Expr s0)
  | Lit _ s0 => congr_Lit (eq_refl s0)
  | Tup _ s0 =>
      congr_Tup (list_ext (extRen_Expr xi_Expr zeta_Expr Eq_Expr) s0)
  | Tru _ s0 => congr_Tru (extRen_Expr xi_Expr zeta_Expr Eq_Expr s0)
  | Lam _ s0 =>
      congr_Lam
        (extRen_Expr (upRen_Expr_Expr xi_Expr) (upRen_Expr_Expr zeta_Expr)
           (upExtRen_Expr_Expr _ _ Eq_Expr) s0)
  | Op _ s0 => congr_Op (eq_refl s0)
  | Unify _ s0 s1 =>
      congr_Unify (extRen_Expr xi_Expr zeta_Expr Eq_Expr s0)
        (extRen_Expr xi_Expr zeta_Expr Eq_Expr s1)
  | Seq _ s0 s1 =>
      congr_Seq (extRen_Expr xi_Expr zeta_Expr Eq_Expr s0)
        (extRen_Expr xi_Expr zeta_Expr Eq_Expr s1)
  | Or _ s0 s1 =>
      congr_Or (extRen_Expr xi_Expr zeta_Expr Eq_Expr s0)
        (extRen_Expr xi_Expr zeta_Expr Eq_Expr s1)
  | App _ s0 s1 =>
      congr_App (extRen_Expr xi_Expr zeta_Expr Eq_Expr s0)
        (extRen_Expr xi_Expr zeta_Expr Eq_Expr s1)
  | Exi _ s0 =>
      congr_Exi
        (extRen_Expr (upRen_Expr_Expr xi_Expr) (upRen_Expr_Expr zeta_Expr)
           (upExtRen_Expr_Expr _ _ Eq_Expr) s0)
  | Fail _ => congr_Fail
  | Iter _ s0 s1 s2 =>
      congr_Iter (eq_refl s0) (extRen_Expr xi_Expr zeta_Expr Eq_Expr s1)
        (extRen_Expr xi_Expr zeta_Expr Eq_Expr s2)
  end.

Lemma upExt_Expr_Expr {m : nat} {n_Expr : nat} (sigma : fin m -> Expr n_Expr)
  (tau : fin m -> Expr n_Expr) (Eq : forall x, sigma x = tau x) :
  forall x, up_Expr_Expr sigma x = up_Expr_Expr tau x.
Proof.
exact (fun n =>
       match n with
       | Some fin_n => ap (ren_Expr shift) (Eq fin_n)
       | None => eq_refl
       end).
Qed.

Lemma upExt_list_Expr_Expr {p : nat} {m : nat} {n_Expr : nat}
  (sigma : fin m -> Expr n_Expr) (tau : fin m -> Expr n_Expr)
  (Eq : forall x, sigma x = tau x) :
  forall x, up_list_Expr_Expr p sigma x = up_list_Expr_Expr p tau x.
Proof.
exact (fun n =>
       scons_p_congr (fun n => eq_refl)
         (fun n => ap (ren_Expr (shift_p p)) (Eq n))).
Qed.

Fixpoint ext_Expr {m_Expr : nat} {n_Expr : nat}
(sigma_Expr : fin m_Expr -> Expr n_Expr)
(tau_Expr : fin m_Expr -> Expr n_Expr)
(Eq_Expr : forall x, sigma_Expr x = tau_Expr x) (s : Expr m_Expr) {struct s}
   :
subst_Expr sigma_Expr s = subst_Expr tau_Expr s :=
  match s with
  | var_Expr _ s0 => Eq_Expr s0
  | Lit _ s0 => congr_Lit (eq_refl s0)
  | Tup _ s0 =>
      congr_Tup (list_ext (ext_Expr sigma_Expr tau_Expr Eq_Expr) s0)
  | Tru _ s0 => congr_Tru (ext_Expr sigma_Expr tau_Expr Eq_Expr s0)
  | Lam _ s0 =>
      congr_Lam
        (ext_Expr (up_Expr_Expr sigma_Expr) (up_Expr_Expr tau_Expr)
           (upExt_Expr_Expr _ _ Eq_Expr) s0)
  | Op _ s0 => congr_Op (eq_refl s0)
  | Unify _ s0 s1 =>
      congr_Unify (ext_Expr sigma_Expr tau_Expr Eq_Expr s0)
        (ext_Expr sigma_Expr tau_Expr Eq_Expr s1)
  | Seq _ s0 s1 =>
      congr_Seq (ext_Expr sigma_Expr tau_Expr Eq_Expr s0)
        (ext_Expr sigma_Expr tau_Expr Eq_Expr s1)
  | Or _ s0 s1 =>
      congr_Or (ext_Expr sigma_Expr tau_Expr Eq_Expr s0)
        (ext_Expr sigma_Expr tau_Expr Eq_Expr s1)
  | App _ s0 s1 =>
      congr_App (ext_Expr sigma_Expr tau_Expr Eq_Expr s0)
        (ext_Expr sigma_Expr tau_Expr Eq_Expr s1)
  | Exi _ s0 =>
      congr_Exi
        (ext_Expr (up_Expr_Expr sigma_Expr) (up_Expr_Expr tau_Expr)
           (upExt_Expr_Expr _ _ Eq_Expr) s0)
  | Fail _ => congr_Fail
  | Iter _ s0 s1 s2 =>
      congr_Iter (eq_refl s0) (ext_Expr sigma_Expr tau_Expr Eq_Expr s1)
        (ext_Expr sigma_Expr tau_Expr Eq_Expr s2)
  end.

Lemma up_ren_ren_Expr_Expr {k : nat} {l : nat} {m : nat}
  (xi : fin k -> fin l) (zeta : fin l -> fin m) (rho : fin k -> fin m)
  (Eq : forall x, funcomp zeta xi x = rho x) :
  forall x,
  funcomp (upRen_Expr_Expr zeta) (upRen_Expr_Expr xi) x =
  upRen_Expr_Expr rho x.
Proof.
exact (up_ren_ren xi zeta rho Eq).
Qed.

Lemma up_ren_ren_list_Expr_Expr {p : nat} {k : nat} {l : nat} {m : nat}
  (xi : fin k -> fin l) (zeta : fin l -> fin m) (rho : fin k -> fin m)
  (Eq : forall x, funcomp zeta xi x = rho x) :
  forall x,
  funcomp (upRen_list_Expr_Expr p zeta) (upRen_list_Expr_Expr p xi) x =
  upRen_list_Expr_Expr p rho x.
Proof.
exact (up_ren_ren_p Eq).
Qed.

Fixpoint compRenRen_Expr {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
(xi_Expr : fin m_Expr -> fin k_Expr) (zeta_Expr : fin k_Expr -> fin l_Expr)
(rho_Expr : fin m_Expr -> fin l_Expr)
(Eq_Expr : forall x, funcomp zeta_Expr xi_Expr x = rho_Expr x)
(s : Expr m_Expr) {struct s} :
ren_Expr zeta_Expr (ren_Expr xi_Expr s) = ren_Expr rho_Expr s :=
  match s with
  | var_Expr _ s0 => ap (var_Expr l_Expr) (Eq_Expr s0)
  | Lit _ s0 => congr_Lit (eq_refl s0)
  | Tup _ s0 =>
      congr_Tup
        (list_comp (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr) s0)
  | Tru _ s0 =>
      congr_Tru (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr s0)
  | Lam _ s0 =>
      congr_Lam
        (compRenRen_Expr (upRen_Expr_Expr xi_Expr)
           (upRen_Expr_Expr zeta_Expr) (upRen_Expr_Expr rho_Expr)
           (up_ren_ren _ _ _ Eq_Expr) s0)
  | Op _ s0 => congr_Op (eq_refl s0)
  | Unify _ s0 s1 =>
      congr_Unify (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr s0)
        (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr s1)
  | Seq _ s0 s1 =>
      congr_Seq (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr s0)
        (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr s1)
  | Or _ s0 s1 =>
      congr_Or (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr s0)
        (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr s1)
  | App _ s0 s1 =>
      congr_App (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr s0)
        (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr s1)
  | Exi _ s0 =>
      congr_Exi
        (compRenRen_Expr (upRen_Expr_Expr xi_Expr)
           (upRen_Expr_Expr zeta_Expr) (upRen_Expr_Expr rho_Expr)
           (up_ren_ren _ _ _ Eq_Expr) s0)
  | Fail _ => congr_Fail
  | Iter _ s0 s1 s2 =>
      congr_Iter (eq_refl s0)
        (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr s1)
        (compRenRen_Expr xi_Expr zeta_Expr rho_Expr Eq_Expr s2)
  end.

Lemma up_ren_subst_Expr_Expr {k : nat} {l : nat} {m_Expr : nat}
  (xi : fin k -> fin l) (tau : fin l -> Expr m_Expr)
  (theta : fin k -> Expr m_Expr) (Eq : forall x, funcomp tau xi x = theta x)
  :
  forall x,
  funcomp (up_Expr_Expr tau) (upRen_Expr_Expr xi) x = up_Expr_Expr theta x.
Proof.
exact (fun n =>
       match n with
       | Some fin_n => ap (ren_Expr shift) (Eq fin_n)
       | None => eq_refl
       end).
Qed.

Lemma up_ren_subst_list_Expr_Expr {p : nat} {k : nat} {l : nat}
  {m_Expr : nat} (xi : fin k -> fin l) (tau : fin l -> Expr m_Expr)
  (theta : fin k -> Expr m_Expr) (Eq : forall x, funcomp tau xi x = theta x)
  :
  forall x,
  funcomp (up_list_Expr_Expr p tau) (upRen_list_Expr_Expr p xi) x =
  up_list_Expr_Expr p theta x.
Proof.
exact (fun n =>
       eq_trans (scons_p_comp' _ _ _ n)
         (scons_p_congr (fun z => scons_p_head' _ _ z)
            (fun z =>
             eq_trans (scons_p_tail' _ _ (xi z))
               (ap (ren_Expr (shift_p p)) (Eq z))))).
Qed.

Fixpoint compRenSubst_Expr {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
(xi_Expr : fin m_Expr -> fin k_Expr) (tau_Expr : fin k_Expr -> Expr l_Expr)
(theta_Expr : fin m_Expr -> Expr l_Expr)
(Eq_Expr : forall x, funcomp tau_Expr xi_Expr x = theta_Expr x)
(s : Expr m_Expr) {struct s} :
subst_Expr tau_Expr (ren_Expr xi_Expr s) = subst_Expr theta_Expr s :=
  match s with
  | var_Expr _ s0 => Eq_Expr s0
  | Lit _ s0 => congr_Lit (eq_refl s0)
  | Tup _ s0 =>
      congr_Tup
        (list_comp (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr) s0)
  | Tru _ s0 =>
      congr_Tru (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr s0)
  | Lam _ s0 =>
      congr_Lam
        (compRenSubst_Expr (upRen_Expr_Expr xi_Expr) (up_Expr_Expr tau_Expr)
           (up_Expr_Expr theta_Expr) (up_ren_subst_Expr_Expr _ _ _ Eq_Expr)
           s0)
  | Op _ s0 => congr_Op (eq_refl s0)
  | Unify _ s0 s1 =>
      congr_Unify (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr s0)
        (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr s1)
  | Seq _ s0 s1 =>
      congr_Seq (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr s0)
        (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr s1)
  | Or _ s0 s1 =>
      congr_Or (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr s0)
        (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr s1)
  | App _ s0 s1 =>
      congr_App (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr s0)
        (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr s1)
  | Exi _ s0 =>
      congr_Exi
        (compRenSubst_Expr (upRen_Expr_Expr xi_Expr) (up_Expr_Expr tau_Expr)
           (up_Expr_Expr theta_Expr) (up_ren_subst_Expr_Expr _ _ _ Eq_Expr)
           s0)
  | Fail _ => congr_Fail
  | Iter _ s0 s1 s2 =>
      congr_Iter (eq_refl s0)
        (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr s1)
        (compRenSubst_Expr xi_Expr tau_Expr theta_Expr Eq_Expr s2)
  end.

Lemma up_subst_ren_Expr_Expr {k : nat} {l_Expr : nat} {m_Expr : nat}
  (sigma : fin k -> Expr l_Expr) (zeta_Expr : fin l_Expr -> fin m_Expr)
  (theta : fin k -> Expr m_Expr)
  (Eq : forall x, funcomp (ren_Expr zeta_Expr) sigma x = theta x) :
  forall x,
  funcomp (ren_Expr (upRen_Expr_Expr zeta_Expr)) (up_Expr_Expr sigma) x =
  up_Expr_Expr theta x.
Proof.
exact (fun n =>
       match n with
       | Some fin_n =>
           eq_trans
             (compRenRen_Expr shift (upRen_Expr_Expr zeta_Expr)
                (funcomp shift zeta_Expr) (fun x => eq_refl) (sigma fin_n))
             (eq_trans
                (eq_sym
                   (compRenRen_Expr zeta_Expr shift (funcomp shift zeta_Expr)
                      (fun x => eq_refl) (sigma fin_n)))
                (ap (ren_Expr shift) (Eq fin_n)))
       | None => eq_refl
       end).
Qed.

Lemma up_subst_ren_list_Expr_Expr {p : nat} {k : nat} {l_Expr : nat}
  {m_Expr : nat} (sigma : fin k -> Expr l_Expr)
  (zeta_Expr : fin l_Expr -> fin m_Expr) (theta : fin k -> Expr m_Expr)
  (Eq : forall x, funcomp (ren_Expr zeta_Expr) sigma x = theta x) :
  forall x,
  funcomp (ren_Expr (upRen_list_Expr_Expr p zeta_Expr))
    (up_list_Expr_Expr p sigma) x =
  up_list_Expr_Expr p theta x.
Proof.
exact (fun n =>
       eq_trans (scons_p_comp' _ _ _ n)
         (scons_p_congr
            (fun x => ap (var_Expr (plus p m_Expr)) (scons_p_head' _ _ x))
            (fun n =>
             eq_trans
               (compRenRen_Expr (shift_p p)
                  (upRen_list_Expr_Expr p zeta_Expr)
                  (funcomp (shift_p p) zeta_Expr)
                  (fun x => scons_p_tail' _ _ x) (sigma n))
               (eq_trans
                  (eq_sym
                     (compRenRen_Expr zeta_Expr (shift_p p)
                        (funcomp (shift_p p) zeta_Expr) (fun x => eq_refl)
                        (sigma n)))
                  (ap (ren_Expr (shift_p p)) (Eq n)))))).
Qed.

Fixpoint compSubstRen_Expr {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
(sigma_Expr : fin m_Expr -> Expr k_Expr)
(zeta_Expr : fin k_Expr -> fin l_Expr)
(theta_Expr : fin m_Expr -> Expr l_Expr)
(Eq_Expr : forall x, funcomp (ren_Expr zeta_Expr) sigma_Expr x = theta_Expr x)
(s : Expr m_Expr) {struct s} :
ren_Expr zeta_Expr (subst_Expr sigma_Expr s) = subst_Expr theta_Expr s :=
  match s with
  | var_Expr _ s0 => Eq_Expr s0
  | Lit _ s0 => congr_Lit (eq_refl s0)
  | Tup _ s0 =>
      congr_Tup
        (list_comp
           (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr) s0)
  | Tru _ s0 =>
      congr_Tru
        (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr s0)
  | Lam _ s0 =>
      congr_Lam
        (compSubstRen_Expr (up_Expr_Expr sigma_Expr)
           (upRen_Expr_Expr zeta_Expr) (up_Expr_Expr theta_Expr)
           (up_subst_ren_Expr_Expr _ _ _ Eq_Expr) s0)
  | Op _ s0 => congr_Op (eq_refl s0)
  | Unify _ s0 s1 =>
      congr_Unify
        (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr s0)
        (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr s1)
  | Seq _ s0 s1 =>
      congr_Seq
        (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr s0)
        (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr s1)
  | Or _ s0 s1 =>
      congr_Or (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr s0)
        (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr s1)
  | App _ s0 s1 =>
      congr_App
        (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr s0)
        (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr s1)
  | Exi _ s0 =>
      congr_Exi
        (compSubstRen_Expr (up_Expr_Expr sigma_Expr)
           (upRen_Expr_Expr zeta_Expr) (up_Expr_Expr theta_Expr)
           (up_subst_ren_Expr_Expr _ _ _ Eq_Expr) s0)
  | Fail _ => congr_Fail
  | Iter _ s0 s1 s2 =>
      congr_Iter (eq_refl s0)
        (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr s1)
        (compSubstRen_Expr sigma_Expr zeta_Expr theta_Expr Eq_Expr s2)
  end.

Lemma up_subst_subst_Expr_Expr {k : nat} {l_Expr : nat} {m_Expr : nat}
  (sigma : fin k -> Expr l_Expr) (tau_Expr : fin l_Expr -> Expr m_Expr)
  (theta : fin k -> Expr m_Expr)
  (Eq : forall x, funcomp (subst_Expr tau_Expr) sigma x = theta x) :
  forall x,
  funcomp (subst_Expr (up_Expr_Expr tau_Expr)) (up_Expr_Expr sigma) x =
  up_Expr_Expr theta x.
Proof.
exact (fun n =>
       match n with
       | Some fin_n =>
           eq_trans
             (compRenSubst_Expr shift (up_Expr_Expr tau_Expr)
                (funcomp (up_Expr_Expr tau_Expr) shift) (fun x => eq_refl)
                (sigma fin_n))
             (eq_trans
                (eq_sym
                   (compSubstRen_Expr tau_Expr shift
                      (funcomp (ren_Expr shift) tau_Expr) (fun x => eq_refl)
                      (sigma fin_n)))
                (ap (ren_Expr shift) (Eq fin_n)))
       | None => eq_refl
       end).
Qed.

Lemma up_subst_subst_list_Expr_Expr {p : nat} {k : nat} {l_Expr : nat}
  {m_Expr : nat} (sigma : fin k -> Expr l_Expr)
  (tau_Expr : fin l_Expr -> Expr m_Expr) (theta : fin k -> Expr m_Expr)
  (Eq : forall x, funcomp (subst_Expr tau_Expr) sigma x = theta x) :
  forall x,
  funcomp (subst_Expr (up_list_Expr_Expr p tau_Expr))
    (up_list_Expr_Expr p sigma) x =
  up_list_Expr_Expr p theta x.
Proof.
exact (fun n =>
       eq_trans
         (scons_p_comp' (funcomp (var_Expr (plus p l_Expr)) (zero_p p)) _ _ n)
         (scons_p_congr
            (fun x => scons_p_head' _ (fun z => ren_Expr (shift_p p) _) x)
            (fun n =>
             eq_trans
               (compRenSubst_Expr (shift_p p) (up_list_Expr_Expr p tau_Expr)
                  (funcomp (up_list_Expr_Expr p tau_Expr) (shift_p p))
                  (fun x => eq_refl) (sigma n))
               (eq_trans
                  (eq_sym
                     (compSubstRen_Expr tau_Expr (shift_p p) _
                        (fun x => eq_sym (scons_p_tail' _ _ x)) (sigma n)))
                  (ap (ren_Expr (shift_p p)) (Eq n)))))).
Qed.

Fixpoint compSubstSubst_Expr {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
(sigma_Expr : fin m_Expr -> Expr k_Expr)
(tau_Expr : fin k_Expr -> Expr l_Expr)
(theta_Expr : fin m_Expr -> Expr l_Expr)
(Eq_Expr : forall x,
           funcomp (subst_Expr tau_Expr) sigma_Expr x = theta_Expr x)
(s : Expr m_Expr) {struct s} :
subst_Expr tau_Expr (subst_Expr sigma_Expr s) = subst_Expr theta_Expr s :=
  match s with
  | var_Expr _ s0 => Eq_Expr s0
  | Lit _ s0 => congr_Lit (eq_refl s0)
  | Tup _ s0 =>
      congr_Tup
        (list_comp
           (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr) s0)
  | Tru _ s0 =>
      congr_Tru
        (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr s0)
  | Lam _ s0 =>
      congr_Lam
        (compSubstSubst_Expr (up_Expr_Expr sigma_Expr)
           (up_Expr_Expr tau_Expr) (up_Expr_Expr theta_Expr)
           (up_subst_subst_Expr_Expr _ _ _ Eq_Expr) s0)
  | Op _ s0 => congr_Op (eq_refl s0)
  | Unify _ s0 s1 =>
      congr_Unify
        (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr s0)
        (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr s1)
  | Seq _ s0 s1 =>
      congr_Seq
        (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr s0)
        (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr s1)
  | Or _ s0 s1 =>
      congr_Or
        (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr s0)
        (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr s1)
  | App _ s0 s1 =>
      congr_App
        (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr s0)
        (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr s1)
  | Exi _ s0 =>
      congr_Exi
        (compSubstSubst_Expr (up_Expr_Expr sigma_Expr)
           (up_Expr_Expr tau_Expr) (up_Expr_Expr theta_Expr)
           (up_subst_subst_Expr_Expr _ _ _ Eq_Expr) s0)
  | Fail _ => congr_Fail
  | Iter _ s0 s1 s2 =>
      congr_Iter (eq_refl s0)
        (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr s1)
        (compSubstSubst_Expr sigma_Expr tau_Expr theta_Expr Eq_Expr s2)
  end.

Lemma renRen_Expr {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
  (xi_Expr : fin m_Expr -> fin k_Expr) (zeta_Expr : fin k_Expr -> fin l_Expr)
  (s : Expr m_Expr) :
  ren_Expr zeta_Expr (ren_Expr xi_Expr s) =
  ren_Expr (funcomp zeta_Expr xi_Expr) s.
Proof.
exact (compRenRen_Expr xi_Expr zeta_Expr _ (fun n => eq_refl) s).
Qed.

Lemma renRen'_Expr_pointwise {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
  (xi_Expr : fin m_Expr -> fin k_Expr) (zeta_Expr : fin k_Expr -> fin l_Expr)
  :
  pointwise_relation _ eq (funcomp (ren_Expr zeta_Expr) (ren_Expr xi_Expr))
    (ren_Expr (funcomp zeta_Expr xi_Expr)).
Proof.
exact (fun s => compRenRen_Expr xi_Expr zeta_Expr _ (fun n => eq_refl) s).
Qed.

Lemma renSubst_Expr {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
  (xi_Expr : fin m_Expr -> fin k_Expr) (tau_Expr : fin k_Expr -> Expr l_Expr)
  (s : Expr m_Expr) :
  subst_Expr tau_Expr (ren_Expr xi_Expr s) =
  subst_Expr (funcomp tau_Expr xi_Expr) s.
Proof.
exact (compRenSubst_Expr xi_Expr tau_Expr _ (fun n => eq_refl) s).
Qed.

Lemma renSubst_Expr_pointwise {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
  (xi_Expr : fin m_Expr -> fin k_Expr) (tau_Expr : fin k_Expr -> Expr l_Expr)
  :
  pointwise_relation _ eq (funcomp (subst_Expr tau_Expr) (ren_Expr xi_Expr))
    (subst_Expr (funcomp tau_Expr xi_Expr)).
Proof.
exact (fun s => compRenSubst_Expr xi_Expr tau_Expr _ (fun n => eq_refl) s).
Qed.

Lemma substRen_Expr {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
  (sigma_Expr : fin m_Expr -> Expr k_Expr)
  (zeta_Expr : fin k_Expr -> fin l_Expr) (s : Expr m_Expr) :
  ren_Expr zeta_Expr (subst_Expr sigma_Expr s) =
  subst_Expr (funcomp (ren_Expr zeta_Expr) sigma_Expr) s.
Proof.
exact (compSubstRen_Expr sigma_Expr zeta_Expr _ (fun n => eq_refl) s).
Qed.

Lemma substRen_Expr_pointwise {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
  (sigma_Expr : fin m_Expr -> Expr k_Expr)
  (zeta_Expr : fin k_Expr -> fin l_Expr) :
  pointwise_relation _ eq
    (funcomp (ren_Expr zeta_Expr) (subst_Expr sigma_Expr))
    (subst_Expr (funcomp (ren_Expr zeta_Expr) sigma_Expr)).
Proof.
exact (fun s => compSubstRen_Expr sigma_Expr zeta_Expr _ (fun n => eq_refl) s).
Qed.

Lemma substSubst_Expr {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
  (sigma_Expr : fin m_Expr -> Expr k_Expr)
  (tau_Expr : fin k_Expr -> Expr l_Expr) (s : Expr m_Expr) :
  subst_Expr tau_Expr (subst_Expr sigma_Expr s) =
  subst_Expr (funcomp (subst_Expr tau_Expr) sigma_Expr) s.
Proof.
exact (compSubstSubst_Expr sigma_Expr tau_Expr _ (fun n => eq_refl) s).
Qed.

Lemma substSubst_Expr_pointwise {k_Expr : nat} {l_Expr : nat} {m_Expr : nat}
  (sigma_Expr : fin m_Expr -> Expr k_Expr)
  (tau_Expr : fin k_Expr -> Expr l_Expr) :
  pointwise_relation _ eq
    (funcomp (subst_Expr tau_Expr) (subst_Expr sigma_Expr))
    (subst_Expr (funcomp (subst_Expr tau_Expr) sigma_Expr)).
Proof.
exact (fun s =>
       compSubstSubst_Expr sigma_Expr tau_Expr _ (fun n => eq_refl) s).
Qed.

Lemma rinstInst_up_Expr_Expr {m : nat} {n_Expr : nat}
  (xi : fin m -> fin n_Expr) (sigma : fin m -> Expr n_Expr)
  (Eq : forall x, funcomp (var_Expr n_Expr) xi x = sigma x) :
  forall x,
  funcomp (var_Expr (S n_Expr)) (upRen_Expr_Expr xi) x = up_Expr_Expr sigma x.
Proof.
exact (fun n =>
       match n with
       | Some fin_n => ap (ren_Expr shift) (Eq fin_n)
       | None => eq_refl
       end).
Qed.

Lemma rinstInst_up_list_Expr_Expr {p : nat} {m : nat} {n_Expr : nat}
  (xi : fin m -> fin n_Expr) (sigma : fin m -> Expr n_Expr)
  (Eq : forall x, funcomp (var_Expr n_Expr) xi x = sigma x) :
  forall x,
  funcomp (var_Expr (plus p n_Expr)) (upRen_list_Expr_Expr p xi) x =
  up_list_Expr_Expr p sigma x.
Proof.
exact (fun n =>
       eq_trans (scons_p_comp' _ _ (var_Expr (plus p n_Expr)) n)
         (scons_p_congr (fun z => eq_refl)
            (fun n => ap (ren_Expr (shift_p p)) (Eq n)))).
Qed.

Fixpoint rinst_inst_Expr {m_Expr : nat} {n_Expr : nat}
(xi_Expr : fin m_Expr -> fin n_Expr) (sigma_Expr : fin m_Expr -> Expr n_Expr)
(Eq_Expr : forall x, funcomp (var_Expr n_Expr) xi_Expr x = sigma_Expr x)
(s : Expr m_Expr) {struct s} : ren_Expr xi_Expr s = subst_Expr sigma_Expr s
:=
  match s with
  | var_Expr _ s0 => Eq_Expr s0
  | Lit _ s0 => congr_Lit (eq_refl s0)
  | Tup _ s0 =>
      congr_Tup (list_ext (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr) s0)
  | Tru _ s0 => congr_Tru (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr s0)
  | Lam _ s0 =>
      congr_Lam
        (rinst_inst_Expr (upRen_Expr_Expr xi_Expr) (up_Expr_Expr sigma_Expr)
           (rinstInst_up_Expr_Expr _ _ Eq_Expr) s0)
  | Op _ s0 => congr_Op (eq_refl s0)
  | Unify _ s0 s1 =>
      congr_Unify (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr s0)
        (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr s1)
  | Seq _ s0 s1 =>
      congr_Seq (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr s0)
        (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr s1)
  | Or _ s0 s1 =>
      congr_Or (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr s0)
        (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr s1)
  | App _ s0 s1 =>
      congr_App (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr s0)
        (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr s1)
  | Exi _ s0 =>
      congr_Exi
        (rinst_inst_Expr (upRen_Expr_Expr xi_Expr) (up_Expr_Expr sigma_Expr)
           (rinstInst_up_Expr_Expr _ _ Eq_Expr) s0)
  | Fail _ => congr_Fail
  | Iter _ s0 s1 s2 =>
      congr_Iter (eq_refl s0) (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr s1)
        (rinst_inst_Expr xi_Expr sigma_Expr Eq_Expr s2)
  end.

Lemma rinstInst'_Expr {m_Expr : nat} {n_Expr : nat}
  (xi_Expr : fin m_Expr -> fin n_Expr) (s : Expr m_Expr) :
  ren_Expr xi_Expr s = subst_Expr (funcomp (var_Expr n_Expr) xi_Expr) s.
Proof.
exact (rinst_inst_Expr xi_Expr _ (fun n => eq_refl) s).
Qed.

Lemma rinstInst'_Expr_pointwise {m_Expr : nat} {n_Expr : nat}
  (xi_Expr : fin m_Expr -> fin n_Expr) :
  pointwise_relation _ eq (ren_Expr xi_Expr)
    (subst_Expr (funcomp (var_Expr n_Expr) xi_Expr)).
Proof.
exact (fun s => rinst_inst_Expr xi_Expr _ (fun n => eq_refl) s).
Qed.

Lemma instId'_Expr {m_Expr : nat} (s : Expr m_Expr) :
  subst_Expr (var_Expr m_Expr) s = s.
Proof.
exact (idSubst_Expr (var_Expr m_Expr) (fun n => eq_refl) s).
Qed.

Lemma instId'_Expr_pointwise {m_Expr : nat} :
  pointwise_relation _ eq (subst_Expr (var_Expr m_Expr)) id.
Proof.
exact (fun s => idSubst_Expr (var_Expr m_Expr) (fun n => eq_refl) s).
Qed.

Lemma rinstId'_Expr {m_Expr : nat} (s : Expr m_Expr) : ren_Expr id s = s.
Proof.
exact (eq_ind_r (fun t => t = s) (instId'_Expr s) (rinstInst'_Expr id s)).
Qed.

Lemma rinstId'_Expr_pointwise {m_Expr : nat} :
  pointwise_relation _ eq (@ren_Expr m_Expr m_Expr id) id.
Proof.
exact (fun s =>
       eq_ind_r (fun t => t = s) (instId'_Expr s) (rinstInst'_Expr id s)).
Qed.

Lemma varL'_Expr {m_Expr : nat} {n_Expr : nat}
  (sigma_Expr : fin m_Expr -> Expr n_Expr) (x : fin m_Expr) :
  subst_Expr sigma_Expr (var_Expr m_Expr x) = sigma_Expr x.
Proof.
exact (eq_refl).
Qed.

Lemma varL'_Expr_pointwise {m_Expr : nat} {n_Expr : nat}
  (sigma_Expr : fin m_Expr -> Expr n_Expr) :
  pointwise_relation _ eq (funcomp (subst_Expr sigma_Expr) (var_Expr m_Expr))
    sigma_Expr.
Proof.
exact (fun x => eq_refl).
Qed.

Lemma varLRen'_Expr {m_Expr : nat} {n_Expr : nat}
  (xi_Expr : fin m_Expr -> fin n_Expr) (x : fin m_Expr) :
  ren_Expr xi_Expr (var_Expr m_Expr x) = var_Expr n_Expr (xi_Expr x).
Proof.
exact (eq_refl).
Qed.

Lemma varLRen'_Expr_pointwise {m_Expr : nat} {n_Expr : nat}
  (xi_Expr : fin m_Expr -> fin n_Expr) :
  pointwise_relation _ eq (funcomp (ren_Expr xi_Expr) (var_Expr m_Expr))
    (funcomp (var_Expr n_Expr) xi_Expr).
Proof.
exact (fun x => eq_refl).
Qed.

Class Up_Expr X Y :=
    up_Expr : X -> Y.

#[global]
Instance Subst_Expr  {m_Expr n_Expr : nat}: (Subst1 _ _ _) :=
 (@subst_Expr m_Expr n_Expr).

#[global]
Instance Up_Expr_Expr  {m n_Expr : nat}: (Up_Expr _ _) :=
 (@up_Expr_Expr m n_Expr).

#[global]
Instance Ren_Expr  {m_Expr n_Expr : nat}: (Ren1 _ _ _) :=
 (@ren_Expr m_Expr n_Expr).

#[global]
Instance VarInstance_Expr  {n_Expr : nat}: (Var _ _) := (@var_Expr n_Expr).

Notation "s [ sigma_Expr ]" := (subst_Expr sigma_Expr s)
( at level 7, left associativity, only printing)  : subst_scope.

Notation "↑__Expr" := up_Expr (only printing)  : subst_scope.

Notation "↑__Expr" := up_Expr_Expr (only printing)  : subst_scope.

Notation "s ⟨ xi_Expr ⟩" := (ren_Expr xi_Expr s)
( at level 7, left associativity, only printing)  : subst_scope.

Notation "'var'" := var_Expr ( at level 1, only printing)  : subst_scope.

Notation "x '__Expr'" := (@ids _ _ VarInstance_Expr x)
( at level 5, format "x __Expr", only printing)  : subst_scope.

Notation "x '__Expr'" := (var_Expr x) ( at level 5, format "x __Expr")  :
subst_scope.

#[global]
Instance subst_Expr_morphism  {m_Expr : nat} {n_Expr : nat}:
 (Proper (respectful (pointwise_relation _ eq) (respectful eq eq))
    (@subst_Expr m_Expr n_Expr)).
Proof.
exact (fun f_Expr g_Expr Eq_Expr s t Eq_st =>
       eq_ind s (fun t' => subst_Expr f_Expr s = subst_Expr g_Expr t')
         (ext_Expr f_Expr g_Expr Eq_Expr s) t Eq_st).
Qed.

#[global]
Instance subst_Expr_morphism2  {m_Expr : nat} {n_Expr : nat}:
 (Proper (respectful (pointwise_relation _ eq) (pointwise_relation _ eq))
    (@subst_Expr m_Expr n_Expr)).
Proof.
exact (fun f_Expr g_Expr Eq_Expr s => ext_Expr f_Expr g_Expr Eq_Expr s).
Qed.

#[global]
Instance ren_Expr_morphism  {m_Expr : nat} {n_Expr : nat}:
 (Proper (respectful (pointwise_relation _ eq) (respectful eq eq))
    (@ren_Expr m_Expr n_Expr)).
Proof.
exact (fun f_Expr g_Expr Eq_Expr s t Eq_st =>
       eq_ind s (fun t' => ren_Expr f_Expr s = ren_Expr g_Expr t')
         (extRen_Expr f_Expr g_Expr Eq_Expr s) t Eq_st).
Qed.

#[global]
Instance ren_Expr_morphism2  {m_Expr : nat} {n_Expr : nat}:
 (Proper (respectful (pointwise_relation _ eq) (pointwise_relation _ eq))
    (@ren_Expr m_Expr n_Expr)).
Proof.
exact (fun f_Expr g_Expr Eq_Expr s => extRen_Expr f_Expr g_Expr Eq_Expr s).
Qed.

Ltac auto_unfold := repeat
                     unfold VarInstance_Expr, Var, ids, Ren_Expr, Ren1, ren1,
                      Up_Expr_Expr, Up_Expr, up_Expr, Subst_Expr, Subst1,
                      subst1.

Tactic Notation "auto_unfold" "in" "*" := repeat
                                           unfold VarInstance_Expr, Var, ids,
                                            Ren_Expr, Ren1, ren1,
                                            Up_Expr_Expr, Up_Expr, up_Expr,
                                            Subst_Expr, Subst1, subst1 
                                            in *.

Ltac asimpl' := repeat (first
                 [ progress setoid_rewrite substSubst_Expr_pointwise
                 | progress setoid_rewrite substSubst_Expr
                 | progress setoid_rewrite substRen_Expr_pointwise
                 | progress setoid_rewrite substRen_Expr
                 | progress setoid_rewrite renSubst_Expr_pointwise
                 | progress setoid_rewrite renSubst_Expr
                 | progress setoid_rewrite renRen'_Expr_pointwise
                 | progress setoid_rewrite renRen_Expr
                 | progress setoid_rewrite varLRen'_Expr_pointwise
                 | progress setoid_rewrite varLRen'_Expr
                 | progress setoid_rewrite varL'_Expr_pointwise
                 | progress setoid_rewrite varL'_Expr
                 | progress setoid_rewrite rinstId'_Expr_pointwise
                 | progress setoid_rewrite rinstId'_Expr
                 | progress setoid_rewrite instId'_Expr_pointwise
                 | progress setoid_rewrite instId'_Expr
                 | progress
                    unfold up_list_Expr_Expr, up_Expr_Expr,
                     upRen_list_Expr_Expr, upRen_Expr_Expr, up_ren
                 | progress cbn[subst_Expr ren_Expr]
                 | progress fsimpl ]).

Ltac asimpl := check_no_evars;
                repeat
                 unfold VarInstance_Expr, Var, ids, Ren_Expr, Ren1, ren1,
                  Up_Expr_Expr, Up_Expr, up_Expr, Subst_Expr, Subst1, subst1
                  in *;
                asimpl'; minimize.

Tactic Notation "asimpl" "in" hyp(J) := revert J; asimpl; intros J.

Tactic Notation "auto_case" := auto_case ltac:(asimpl; cbn; eauto).

Ltac substify := auto_unfold; try setoid_rewrite rinstInst'_Expr_pointwise;
                  try setoid_rewrite rinstInst'_Expr.

Ltac renamify := auto_unfold;
                  try setoid_rewrite_left rinstInst'_Expr_pointwise;
                  try setoid_rewrite_left rinstInst'_Expr.

End Core.

Module Extra.

Import
Core.

Arguments var_Expr {n_Expr}.

Arguments Iter {n_Expr}.

Arguments Fail {n_Expr}.

Arguments Exi {n_Expr}.

Arguments App {n_Expr}.

Arguments Or {n_Expr}.

Arguments Seq {n_Expr}.

Arguments Unify {n_Expr}.

Arguments Op {n_Expr}.

Arguments Lam {n_Expr}.

Arguments Tru {n_Expr}.

Arguments Tup {n_Expr}.

Arguments Lit {n_Expr}.

#[global] Hint Opaque subst_Expr: rewrite.

#[global] Hint Opaque ren_Expr: rewrite.

End Extra.

Module interface.

Export Core.

Export Extra.

End interface.

Export interface.

