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


inductive_cases is_exp_SeqE[elim!]: "is_exp (Seq e1 e2)"
inductive_cases is_val_SeqE[elim!]: "is_val (Seq e1 e2)"


(* Contexts *)

datatype C =
  CHole
| CSeql C exp
| CSeqr exp C
| CBarl C exp
| CBarr exp C
| CAppl C exp
| CAppr exp C
| CDef var C
| COne C
| CAll C
| CTupl C exp
| CTupr exp C
| CLam var C

function appC :: "C \<Rightarrow> exp \<Rightarrow> exp" where
  "appC CHole e' = e'"
| "appC (CSeql C e) e' = Seq (appC C e') e"
| "appC (CSeqr e C) e' = Seq e (appC C e')"
| "appC (CBarl C e) e' = Bar (appC C e') e"
| "appC (CBarr e C) e' = Bar e (appC C e')"
| "appC (CAppl C e) e' = App (appC C e') e"
| "appC (CAppr e C) e' = App e (appC C e')"
| "appC (CDef x C) e' = Def x (appC C e')"
| "appC (COne C) e' = One (appC C e')"
| "appC (CAll C) e' = All (appC C e')"
| "appC (CTupl C e) e' = Tup (appC C e') e"
| "appC (CTupr e C) e' = Tup e (appC C e')"
| "appC (CLam x C) e' = Lam x (appC C e')"
by pat_completeness auto+
termination by lexicographic_order
(* TODO: Not equivariant! equivariance appC *)


function compC :: "C \<Rightarrow> C \<Rightarrow> C" where
  "compC CHole e' = e'"
| "compC (CSeql C e) e' = CSeql (compC C e') e"
| "compC (CSeqr e C) e' = CSeqr e (compC C e')"
| "compC (CBarl C e) e' = CBarl (compC C e') e"
| "compC (CBarr e C) e' = CBarr e (compC C e')"
| "compC (CAppl C e) e' = CAppl (compC C e') e"
| "compC (CAppr e C) e' = CAppr e (compC C e')"
| "compC (CDef x C) e' = CDef x (compC C e')"
| "compC (COne C) e' = COne (compC C e')"
| "compC (CAll C) e' = CAll (compC C e')"
| "compC (CTupl C e) e' = CTupl (compC C e') e"
| "compC (CTupr e C) e' = CTupr e (compC C e')"
| "compC (CLam x C) e' = CLam x (compC C e')"
by pat_completeness auto+
termination by lexicographic_order

lemma appC_appC_compC:
"appC C1 (appC C2 e) = appC (compC C1 C2) e"
by (induction C1) auto

type_synonym red = "exp \<Rightarrow> exp \<Rightarrow> bool"

inductive congruent :: "red \<Rightarrow> bool"  where
  congruentI: "(\<And> x y C. R x y \<Longrightarrow> R (appC C x) (appC C y)) \<Longrightarrow> congruent R"

lemma congruentE[elim, consumes 2]:
  assumes "congruent R" and "R x y"
  shows "R (appC C x) (appC C y)"
  using assms
  by (simp add: congruent.simps)

lemma congruent_star:
  assumes "congruent R"
  shows "congruent R\<^sup>*\<^sup>*"
proof
  fix x y C
  assume "R\<^sup>*\<^sup>* x y"
  then show "R\<^sup>*\<^sup>* (appC C x) (appC C y)"
  proof (induction rule: converse_rtranclp_induct)
    case base
    then show ?case..
  next
    case (step x z)
    from `R x z`
    have "R (appC C x) (appC C z)" using  `congruent R` by auto
    with `R\<^sup>*\<^sup>* (appC C z) (appC C y)`
    show ?case by auto
  qed
qed

inductive underC :: "C \<Rightarrow> red \<Rightarrow> red" for C and R where
  underCI: "R x y \<Longrightarrow> underC C R (appC C x) (appC C y)"


inductive cc :: "red \<Rightarrow> red" for R where
  ccI: "R x y \<Longrightarrow> cc R (appC C x) (appC C y)"

lemma congruent_cc[simp]:
  "congruent (cc R)"
  by (auto intro!: congruentI elim!:cc.cases underC.cases
           simp add: appC_appC_compC intro: underC.intros cc.intros)


lemma cc_local_confluence:
  assumes "congruent S"
  assumes "symp R"
  assumes "R\<inverse>\<inverse> OO R \<le> R'"
  assumes "\<And> C. R\<inverse>\<inverse> OO underC C R \<le> R'"
  shows "cc R\<inverse>\<inverse> OO cc R \<le> S"
  sorry

inductive rule_Seq where
  rule_Seq: "rule_Seq (Seq v e) e"
equivariance rule_Seq
definition "VR = cc (rule_Seq)"




end