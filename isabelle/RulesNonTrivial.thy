theory RulesNonTrivial
imports Rules  SubstContext
begin

section \<open>A relation is non-trivial if x \<rightarrow> x does not hold\<close>

definition rel_non_trivial :: "red \<Rightarrow> bool"  where
  "rel_non_trivial R \<longleftrightarrow> (\<forall> x. \<not> R x x)"

lemma rel_non_trivial_sup2:
  assumes "rel_non_trivial R" and "rel_non_trivial S"
  shows "rel_non_trivial (R \<squnion> S)"
  using assms by (auto simp add: rel_non_trivial_def)

section \<open>The closure context preserves non-trivialness\<close>

lemma rel_non_trivial_cc:
  assumes "rel_non_trivial R"
  shows "rel_non_trivial (cc R)"
  using assms 
by (auto simp add: rel_non_trivial_def elim!: cc.cases)

section \<open>The Verse rules are non-trivial\<close>

lemma rel_non_trivial_rule_PAdd: "rel_non_trivial rule_PAdd"
  by (auto simp add: rel_non_trivial_def elim!: rule_PAdd.cases)

lemma rel_non_trivial_rule_PGt: "rel_non_trivial rule_PGt"
  by (auto simp add: rel_non_trivial_def elim!: rule_PGt.cases)

lemma rel_non_trivial_rule_App_Beta: "rel_non_trivial rule_App_Beta"
  by (auto simp add: rel_non_trivial_def elim!: rule_App_Beta.cases simp add: occursV_liftV)

lemma rel_non_trivial_rule_App_Tup: "rel_non_trivial rule_App_Tup" 
  apply (auto simp add: rel_non_trivial_def elim!: rule_App_Tup.cases)
  apply (case_tac vs; simp)
  apply (case_tac list; simp)
  done

lemma rel_non_trivial_rule_ULit: "rel_non_trivial rule_ULit"
  by (auto simp add: rel_non_trivial_def elim!: rule_ULit.cases)

lemma rel_non_trivial_rule_UTup: "rel_non_trivial rule_UTup"
  apply (auto simp add: rel_non_trivial_def elim!: rule_UTup.cases in_set_zipE)
  apply (case_tac vs1; simp)
  apply (case_tac vs2; simp)
  apply (case_tac list; simp)
  apply (case_tac lista; simp)
  done

lemma rel_non_trivial_rule_UX: "rel_non_trivial rule_UX"
  by (auto simp add: rel_non_trivial_def elim!: rule_UX.cases)

lemma rel_non_trivial_rule_UXOccurs: "rel_non_trivial rule_UXOccurs"
  by (auto simp add: rel_non_trivial_def elim!: rule_UXOccurs.cases)

lemma map_eq:
  "xs = map f xs \<longleftrightarrow> (\<forall> x \<in> set xs. x = f x)"
  by (induction xs) auto

lemma appVCE_substVCE_eq[simp]:
  "appVCE vce e1 = appVCE (substVCE n v vce) e2 \<longleftrightarrow> vce = substVCE n v vce \<and> e1 = e2"
by (cases vce) auto

lemma appVC'_substVC_eq[simp]:
  "appVC' vc e1 = appVC' (substVC n v vc) e2 \<longleftrightarrow> vc = substVC n v vc \<and> e1 = e2"
by (induction vc arbitrary: n v e1 e2) (auto simp add: substVC_def appVC'_def)

lemma appVC_substVC_eq[simp]:
  "appVC vc e1 = appVC (substVC n v vc) e2 \<longleftrightarrow> vc = substVC n v vc \<and> e1 = e2"
by (simp add:  appVC_def)

lemma appECE_substECE_eq[simp]:
  "appECE ece e1 = appECE (substECE n v ece) e2 \<longleftrightarrow> ece = substECE n v ece \<and> e1 = e2"
by (cases ece) auto

lemma appEC_substEC_eq[simp]:
  "appEC ec e1 = appEC (substEC n v ec) e2 \<longleftrightarrow> ec = substEC n v ec \<and> e1 = e2"
  by (induction n v ec arbitrary: e1 e2 rule: substEC.induct)
     (auto simp add: appEC_def)

lemma substE_eq[simp]:
  "e = substE n v e \<longleftrightarrow> (occursE n e \<longrightarrow> v = Var n)" 
and  substV_eq[simp]:
  "v' = substV n v v' \<longleftrightarrow> (occursV n v' \<longrightarrow> v = Var n)"
by(induction n v e and n v v' rule: substE_substV.induct) (auto simp add: map_eq)

lemma substVCE_eq[simp]:
  "vce = substVCE n v vce \<longleftrightarrow> (occursVCE n vce \<longrightarrow> v = Var n)"
by (induction vce) (auto simp add: map_eq)

lemma substVC_eq[simp]:
  "vc = substVC n v vc \<longleftrightarrow> (occursVC n vc \<longrightarrow> v = Var n)"
by (auto simp add: substVC_def occursVC_def map_eq)

lemma substECE_eq[simp]:
  "ece = substECE n v ece \<longleftrightarrow> (occursECE n ece \<longrightarrow> v = Var n)"
by (cases ece) (auto simp add: occursECE_def)

lemma substEC_eq[simp]:
  "ec = substEC n v ec \<longleftrightarrow> (occursEC n ec \<longrightarrow> v = Var n)"
by (induction n v ec rule: substEC.induct) (auto simp add: )

lemma rel_non_trivial_rule_Subst: "rel_non_trivial rule_Subst"
  by (auto simp add: rel_non_trivial_def elim!: rule_Subst.cases)

lemma size_exp_substE_Var[simp]: "size_exp (substE n (Var k) e) = size_exp e"
  sorry

lemma size_exp_liftE[simp]: "size_exp (liftE n k e) = size_exp e"
  sorry


lemma rel_non_trivial_rule_SubstRec: "rel_non_trivial rule_SubstRec"
  apply (auto simp add: rel_non_trivial_def elim!: rule_SubstRec.cases)
  apply (drule arg_cong[where f =  size_exp])
  apply (auto)
  done

lemma occursEC_replicate_CDef[simp]: "\<not> occursEC k (replicate n CDef)"
  by (induction n arbitrary: k) (auto simp add: occursECE_def)

lemma rel_non_trivial_rule_DefEliml: "rel_non_trivial rule_DefEliml"
  by (auto simp add: rel_non_trivial_def elim!: rule_DefEliml.cases
        simp add: occursE_liftE)

lemma rel_non_trivial_rule_DefElimr: "rel_non_trivial rule_DefElimr"
  by (auto simp add: rel_non_trivial_def elim!: rule_DefElimr.cases
        simp add: occursE_liftE)

lemma rel_non_trivial_rule_Swap: "rel_non_trivial rule_Swap"
  by (auto simp add: rel_non_trivial_def elim!: rule_Swap.cases)

lemma rel_non_trivial_rule_DefFloat: "rel_non_trivial rule_DefFloat"
  by (auto simp add: rel_non_trivial_def elim!: rule_DefFloat.cases)

lemma rel_non_trivial_rule_Seq: "rel_non_trivial rule_Seq"
  by (auto simp add: rel_non_trivial_def elim!: rule_Seq.cases)

lemma rel_non_trivial_rule_Unify_Seql: "rel_non_trivial rule_Unify_Seql"
  by (auto simp add: rel_non_trivial_def elim!: rule_Unify_Seql.cases)

lemma rel_non_trivial_rule_Unify_Seqr: "rel_non_trivial rule_Unify_Seqr"
  by (auto simp add: rel_non_trivial_def elim!: rule_Unify_Seqr.cases)

lemma rel_non_trivial_rule_Unify_Unifyl: "rel_non_trivial rule_Unify_Unifyl"
  by (auto simp add: rel_non_trivial_def elim!: rule_Unify_Unifyl.cases simp add: occursE_liftE)

lemma rel_non_trivial_rule_Unify_Unifyr: "rel_non_trivial rule_Unify_Unifyr"
  by (auto simp add: rel_non_trivial_def elim!: rule_Unify_Unifyr.cases simp add: occursE_liftE)

lemma rel_non_trivial_rule_DefFail: "rel_non_trivial rule_DefFail"
  by (auto simp add: rel_non_trivial_def elim!: rule_DefFail.cases)

lemma rel_non_trivial_rule_Fail: "rel_non_trivial rule_Fail"
  by (auto simp add: rel_non_trivial_def elim!: rule_Fail.cases)

lemma rel_non_trivial_rule_OneFail: "rel_non_trivial rule_OneFail"
  by (auto simp add: rel_non_trivial_def elim!: rule_OneFail.cases)

lemma rel_non_trivial_rule_OneChoice: "rel_non_trivial rule_OneChoice"
  by (auto simp add: rel_non_trivial_def elim!: rule_OneChoice.cases)

lemma rel_non_trivial_rule_OneValue: "rel_non_trivial rule_OneValue"
  by (auto simp add: rel_non_trivial_def elim!: rule_OneValue.cases)

lemma rel_non_trivial_rule_All: "rel_non_trivial rule_All"
  by (auto simp add: rel_non_trivial_def elim!: rule_All.cases)

lemma rel_non_trivial_rule_FailL: "rel_non_trivial rule_FailL"
  by (auto simp add: rel_non_trivial_def elim!: rule_FailL.cases)

lemma rel_non_trivial_rule_FailR: "rel_non_trivial rule_FailR"
  by (auto simp add: rel_non_trivial_def elim!: rule_FailR.cases)

lemma rel_non_trivial_rule_AssocChoice: "rel_non_trivial rule_AssocChoice"
  by (auto simp add: rel_non_trivial_def elim!: rule_AssocChoice.cases)

lemma rel_non_trivial_rule_Choose: "rel_non_trivial rule_Choose"
  by (auto simp add: rel_non_trivial_def elim!: rule_Choose.cases)

theorem ARs_non_trivial: "rel_non_trivial ARs"
unfolding ARs_def
by (intro rel_non_trivial_sup2
   rel_non_trivial_rule_PAdd
   rel_non_trivial_rule_PGt
   rel_non_trivial_rule_App_Beta
   rel_non_trivial_rule_App_Tup
   rel_non_trivial_rule_ULit
   rel_non_trivial_rule_UTup
   rel_non_trivial_rule_UX
   rel_non_trivial_rule_UXOccurs
   rel_non_trivial_rule_Subst
   rel_non_trivial_rule_SubstRec
   rel_non_trivial_rule_DefEliml
   rel_non_trivial_rule_DefElimr
   rel_non_trivial_rule_Swap
   rel_non_trivial_rule_DefFloat
   rel_non_trivial_rule_Seq
   rel_non_trivial_rule_Unify_Seql
   rel_non_trivial_rule_Unify_Seqr
   rel_non_trivial_rule_Unify_Unifyl
   rel_non_trivial_rule_Unify_Unifyr
   rel_non_trivial_rule_DefFail
   rel_non_trivial_rule_Fail
   rel_non_trivial_rule_OneFail
   rel_non_trivial_rule_OneChoice
   rel_non_trivial_rule_OneValue
   rel_non_trivial_rule_All
   rel_non_trivial_rule_FailL
   rel_non_trivial_rule_FailR
   rel_non_trivial_rule_AssocChoice
   rel_non_trivial_rule_Choose
)



end