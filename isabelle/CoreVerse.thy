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

lemma appC_inj[simp]: "appC C x = appC C y \<longleftrightarrow> x = y"
  apply(induction C)
              apply auto
  apply (meson fresh_PairD(1) fresh_PairD(2) obtain_fresh)
  apply (meson fresh_PairD(1) fresh_PairD(2) obtain_fresh)
  done  

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

lemma comp_nest[case_names Same InLeft InRight]:
  fixes C1 C2
  obtains "C1 = C2"
  | C3 where "C3 \<noteq> CHole" "C1 = compC C2 C3"
  | C3 where "C3 \<noteq> CHole" "C2 = compC C1 C3"
  apply (induction C1 arbitrary: C2; case_tac C2)
  sorry

type_synonym red = "exp \<Rightarrow> exp \<Rightarrow> bool"

inductive congruent :: "red \<Rightarrow> bool"  where
  congruentI: "(\<And> x y C. R x y \<Longrightarrow> R (appC C x) (appC C y)) \<Longrightarrow> congruent R"

lemma congruentE[elim, consumes 2]:
  assumes "congruent R" and "R x y"
  shows "R (appC C x) (appC C y)"
  using assms
  by (simp add: congruent.simps)

lemma congruent_OO[simp]:
  assumes "congruent R" and "congruent S"
  shows "congruent (R OO S)"
  using assms
  by(auto intro!: congruentI)

lemma congruent_inv[simp]:
  assumes "congruent R"
  shows "congruent (R\<inverse>\<inverse>)"
  using assms by(auto intro!: congruentI)

lemma congruent_star[simp]:
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
  underCI: "C \<noteq> CHole \<Longrightarrow> R x y \<Longrightarrow> underC C R (appC C x) (appC C y)"


inductive cc :: "red \<Rightarrow> red" for R where
  ccI: "R x y \<Longrightarrow> cc R (appC C x) (appC C y)"

inductive cc' :: "red \<Rightarrow> red" for R where
  cc'I: "C \<noteq> CHole \<Longrightarrow> R x y \<Longrightarrow> cc' R (appC C x) (appC C y)"


lemma congruent_cc[simp]:
  "congruent (cc R)"
  by (auto intro!: congruentI elim!:cc.cases 
           simp add: appC_appC_compC intro: cc.intros)

lemma compC_eq_Hole[simp]:
  "compC C1 C2 = CHole \<longleftrightarrow> C1 = CHole \<and> C2 = CHole"
  by (cases C1; cases C2) auto

lemma compC_neq_Hole1[simp]:
  "C1 \<noteq> CHole \<Longrightarrow> compC C1 C2 \<noteq> CHole"
  by (cases C1; cases C2) auto
lemma compC_neq_Hole2[simp]:
  "C2 \<noteq> CHole \<Longrightarrow> compC C1 C2 \<noteq> CHole"
  by (cases C1; cases C2) auto


lemma congruent_cc'[simp]:
  "congruent (cc' R)"
(*
  by (auto intro!: congruentI elim!:cc'.cases 
           simp add: appC_appC_compC intro:  cc'.intros)
*)
  sorry


lemma cc_local_confluence:
  assumes "congruent S"
  assumes "symp S"
  assumes "R\<inverse>\<inverse> OO R \<le> S"
  assumes "\<And> C. R\<inverse>\<inverse> OO cc' R \<le> S"
  shows "(cc R)\<inverse>\<inverse> OO cc R \<le> S"
proof-
{
  fix a1 a2 C1 b C2 c
  assume "appC C1 a1 = appC C2 a2"
  assume "R a1 b"
  assume "R a2 c"
  have "S (appC C2 c) (appC C1 b)"
  proof(cases C1 C2 rule: comp_nest)
    case Same
    have "a1 = a2" using `C1 = C2` `appC _ _ = _` sorry
    with `R a1 b` `R a2 c` `R\<inverse>\<inverse> OO R \<le> S`
    have "S c b" by auto 
    then show ?thesis using `C1 = C2` `congruent S` by auto
  next
    case (InLeft C3)
    have "a2 = appC C3 a1" using `C1 = _` `appC _ _ = _`
      by (simp add:appC_appC_compC[symmetric])
    with `R a1 b`  `C3 \<noteq> CHole`
    have "cc' R a2 (appC C3 b)" by (auto simp add:cc'.simps)
    with `R a2 c` assms(4)
    have "S c (appC C3 b)" by auto
    then show ?thesis using `C1 = _` `congruent S`
      by (auto simp add: appC_appC_compC[symmetric])
  next
    case (InRight C3)
    have "a1 = appC C3 a2" using `C2 = _` `appC _ _ = _`
      by (simp add:appC_appC_compC[symmetric])
    with `R a2 c`  `C3 \<noteq> CHole`
    have "cc' R a1 (appC C3 c)" by (auto simp add:cc'.simps)
    with `R a1 b` assms(4)
    have "S b (appC C3 c)" by auto
    hence "S (appC C1 b) (appC C2 c)" using `C2 = _` `congruent S`
      by (auto simp add: appC_appC_compC[symmetric])
    then show ?thesis using `symp S` by (auto simp add: symp_def)
  qed
  
} thus ?thesis by (auto simp add: cc.simps)
qed

inductive rule_Seq where
  rule_Seq: "rule_Seq (Seq v e) e"
equivariance rule_Seq

definition "Rs = rule_Seq"
definition "VR = cc Rs"

definition "J = VR\<^sup>*\<^sup>* OO VR\<^sup>*\<^sup>*\<inverse>\<inverse>"

lemma refl_J[simp]: "J x x"
  unfolding J_def by auto

lemma congruent_J[simp]: "congruent J"
  unfolding J_def VR_def by simp

lemma symp_J[simp]: "symp J"
  unfolding J_def VR_def symp_def by auto

lemma joinI[case_names Peak]:
  assumes "\<And> a b c. R1 a b \<Longrightarrow> R2 a c \<Longrightarrow> S b c"
  shows "R1\<inverse>\<inverse> OO R2 \<le> S"
  using assms by auto

lemma J_VR[trans]:
  assumes "J a c"
  assumes "VR b c"
  shows "J a b"
  using assms by (auto simp add: J_def VR_def)

(* Joinable at the root *)

lemma Seq_Seq: "rule_Seq\<inverse>\<inverse> OO rule_Seq \<le> J"
  by(auto intro!: joinI elim!: rule_Seq.cases)

(* Joinable not at the root *)

lemma cc'_Seq:
  assumes "cc' R (Seq v e) c"
  obtains (left) v' where "cc R v v'" and "c = Seq v' e"
  | (right) e' where "cc R e e'" and "c = Seq v e'"
  using assms
  apply (elim cc'.cases)
  apply (case_tac C)
  apply(auto intro:underC.intros  simp add: cc.simps)
  apply blast
done

(* R just to tidy things up. Should be union of all rules. *)
lemma Seq_C: "rule_Seq\<inverse>\<inverse> OO cc' Rs \<le> J"
proof (induction rule: joinI)
  case (Peak a b c)
  then show ?case
  proof(induction)
    case (rule_Seq v e)
    from `cc' Rs (Seq v e) c`
    show ?case
    proof(induct rule: cc'_Seq)
      case (left v')
      then show ?thesis apply simp sorry
    next
      case (right e')
      have "J e e" by simp
      then show ?thesis apply simp sorry
    qed
  qed
qed


theorem local_confluence:
  "VR\<inverse>\<inverse> OO VR \<le> J"
  unfolding VR_def Rs_def
  apply (rule cc_local_confluence)
     apply simp
    apply simp
  apply (rule Seq_Seq)
  apply (rule Seq_C[unfolded Rs_def])
  done

end