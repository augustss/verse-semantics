theory CoreVerse
imports
  Main 
  Nominal2.Nominal2
  "HOL-Eisbach.Eisbach"
begin

atom_decl var

(*
nominal_datatype "exp" =
  Val val
| Seq exp exp
| Bar exp exp
| App val val
| Def x::var e::"exp" binds x in e
| One exp
| All exp
and val =
  Var var
| Const int
| Unit
| Tuple val val
| Lam x::var e::exp binds x in e
*)

nominal_datatype "exp" =
  Seq exp exp
| Bar exp exp
| App exp exp
| Def x::var e::"exp" binds x in e
| One exp
| All exp
(* Now the values *)
| Var var
| Const int
| Unit
| Tup exp exp
| Lam x::var e::exp binds x in e

subsection \<open>Substitutions\<close>

nominal_function subst
where
  "subst x s (Seq t u) = Seq (subst x s t) (subst x s u)"
| "subst x s (Bar t u) = Bar (subst x s t) (subst x s u)"
| "subst x s (One e) = One (subst x s e)"
| "subst x s (All e) = All (subst x s e)"
| "atom y \<sharp> (x, s) \<Longrightarrow> subst x s (Def y e) = Def y (subst x s e)"
| "subst x s (App t u) = App (subst x s t) (subst x s u)"
| "subst x s (Var y) = (if x = y then s else Var y)"
| "subst x s (Const k) = Const k"
| "subst x s Unit = Unit"
| "subst x s (Tup v1 v2) = Tup (subst x s v1) (subst x s v2)"
| "atom y \<sharp> (x, s) \<Longrightarrow> subst x s (Lam y t) = Lam y (subst x s t)"
proof goal_cases
  case (3 P x)
  then show ?case 
  proof (cases x)
    case (fields a b c)
    then show ?thesis using 3
      by (rule_tac exp.strong_exhaust[of c _ "(a,b)"])
         (auto simp add: fresh_star_def)
  qed
next
  case (42 y x s e y' x' s' e')
  then show ?case sorry
next
  case (69 y x s t y' x' s' t')
  then show ?case sorry
qed (auto simp add: eqvt_def subst_graph_aux_def)

nominal_termination (eqvt) by lexicographic_order


inductive is_exp :: "exp \<Rightarrow> bool" and is_val :: "exp \<Rightarrow> bool" where
  is_exp_Val: "is_val e \<Longrightarrow> is_exp e"
| is_exp_Seq: "is_exp e1 \<Longrightarrow> is_exp e2 \<Longrightarrow> is_exp (Seq e1 e2)"
| is_exp_Bar: "is_exp e1 \<Longrightarrow> is_exp e2 \<Longrightarrow> is_exp (Bar e1 e2)"
| is_exp_App: "is_val e1 \<Longrightarrow> is_val e2 \<Longrightarrow> is_exp (App e1 e2)"
| is_exp_Def: "is_exp e \<Longrightarrow> is_exp (Def x e)"
| is_exp_One: "is_exp e \<Longrightarrow> is_exp (One e)"
| is_exp_All: "is_exp e \<Longrightarrow> is_exp (All e)"
(* Now the values *)
| is_val_Var: "is_val (Var x)"
| is_val_Const: "is_val (Const k)"
| is_val_Unit: "is_val Unit"
| is_val_Tuple: "is_val v1 \<Longrightarrow> is_val v2 \<Longrightarrow> is_val (Tup v1 v2)"
| is_val_Lam: "is_exp e \<Longrightarrow> is_val (Lam x e)"

equivariance is_exp
nominal_inductive is_exp avoids is_exp_Def: x | is_val_Lam: x
  by (auto simp add: fresh_star_def)


inductive rule_Seq where
  rule_Seq: "rule_Seq (Seq v e) e"
equivariance rule_Seq



inductive cc for R where
  here: "R x y \<Longrightarrow> cc R x y"
| under_Seql: "cc R x y \<Longrightarrow> cc R (Seq x e) (Seq y e)"
| under_Seqr: "cc R x y \<Longrightarrow> cc R (Seq e x) (Seq e y)"
| under_Barl: "cc R x y \<Longrightarrow> cc R (Bar x e) (Bar y e)"
| under_Barr: "cc R x y \<Longrightarrow> cc R (Bar e x) (Bar e y)"
| under_Appl: "cc R x y \<Longrightarrow> cc R (App x e) (App y e)"
| under_Appr: "cc R x y \<Longrightarrow> cc R (App e x) (App e y)"
| under_Def: "cc R x y \<Longrightarrow> cc R (Def z x) (Def z y)"
| under_One: "cc R x y \<Longrightarrow> cc R (One x) (One y)"
| under_All: "cc R x y \<Longrightarrow> cc R (All x) (All y)"
| under_Tupl: "cc R x y \<Longrightarrow> cc R (Tup x e) (Tup y e)"
| under_Tupr: "cc R x y \<Longrightarrow> cc R (Tup e x) (Tup e y)"
| under_Lam: "cc R x y \<Longrightarrow> cc R (Lam z x) (Lam z y)"
equivariance cc

inductive_cases cc_SeqE[elim!, case_names]: "cc R (Seq e1 e2) e3"

inductive_cases is_exp_SeqE[elim!]: "is_exp (Seq e1 e2)"
inductive_cases is_val_SeqE[elim!]: "is_val (Seq e1 e2)"


definition "VR = cc (rule_Seq)"

lemma cc_local_confluence:
  assumes "\<And> e1 e2 e3.
   R e1 e2 \<Longrightarrow>
   is_exp e1 \<Longrightarrow> is_exp e2 \<Longrightarrow> is_exp e3 \<Longrightarrow>
   cc R e1 e3 \<Longrightarrow>
  ((cc R)\<^sup>*\<^sup>* OO ((cc R)\<inverse>\<inverse>)\<^sup>*\<^sup>*) e2 e3"
  shows "
   is_exp e1 \<Longrightarrow> is_exp e2 \<Longrightarrow> is_exp e3 \<Longrightarrow>
   cc R e1 e2 \<Longrightarrow> cc R e1 e3 \<Longrightarrow>
  ((cc R)\<^sup>*\<^sup>* OO ((cc R)\<inverse>\<inverse>)\<^sup>*\<^sup>*) e2 e3"
proof (induction e1 arbitrary: e2 e3 rule:exp.induct)
  case (Seq x1 x2)

  from `cc R (Seq x1 x2) e2`
  show ?case
  apply rule
  proof(goal_cases)
    case 1
    then show ?case by (rule assms) (auto intro: Seq)
  next
    case (2 x1')
    then show ?case sorry
  next
    case (3 y)
    then show ?case sorry
  qed
    case goal1
    apply rule
      apply (erule assms;fact)
     apply (rule Seq.IH(1))
    using Seq apply fast
    using Seq apply fast
    using Seq apply fast

         apply fact

    using Seq
    apply auto
    apply (subst e2)
    apply (rule Seq.IH)
    
next
  case (Bar x1 x2)
  then show ?case sorry
next
  case (App x1 x2)
  then show ?case sorry
next
  case (Def x1 x2)
  then show ?case sorry
next
  case (One x)
  then show ?case sorry
next
  case (All x)
  then show ?case sorry
next
  case (Var x)
  then show ?case sorry
next
  case (Const x)
  then show ?case sorry
next
  case Unit
  then show ?case sorry
next
  case (Tup x1 x2)
  then show ?case sorry
next
  case (Lam x1 x2)
  then show ?case sorry
qed
  

theorem local_confluence: 
  assumes "is_exp e1" "is_exp e2" "is_exp e3"
  assumes "R e1 e2" "R e1 e3"
  shows "(R\<^sup>*\<^sup>* OO (R\<inverse>\<inverse>)\<^sup>*\<^sup>*) e2 e3"
proof-
  show ?thesis
  using assms
  proof (induction e1 rule:exp.induct)
    case (Seq x1 x2)
    from `R (Seq x1 x2) e2`
    show ?case unfolding R_def
    proof
      
      assume "rule_Seq (Seq x1 x2) e2"
      thus ?case
    
    qed
      
  next
    case (Bar x1 x2)
    then show ?case sorry
  next
    case (App x1 x2)
    then show ?case sorry
  next
    case (Def x1 x2)
    then show ?case sorry
  next
    case (One x)
    then show ?case sorry
  next
    case (All x)
    then show ?case sorry
  next
    case (Var x)
    then show ?case sorry
  next
    case (Const x)
    then show ?case sorry
  next
    case Unit
    then show ?case sorry
  next
    case (Tup x1 x2)
    then show ?case sorry
  next
    case (Lam x1 x2)
    then show ?case sorry
  qed



end