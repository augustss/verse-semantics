theory RulesClosed
imports Rules  SubstContext
begin

section \<open>A relation is closed if it does not invent fresh free names\<close>

inductive rel_closed :: "red \<Rightarrow> bool"  where
  rel_closedI: "(\<And> e1 e2 n. R e1 e2 \<Longrightarrow> occursE n e2 \<Longrightarrow> occursE n e1) \<Longrightarrow> rel_closed R"

lemma rel_closed_OO[simp]:
  assumes "rel_closed R" and "rel_closed S"
  shows "rel_closed (R OO S)"
  using assms by(auto simp add: rel_closed.simps)

lemma rel_closed_star[simp]:
  assumes "rel_closed R"
  shows "rel_closed R\<^sup>*\<^sup>*"
proof
  fix x y n
  assume "R\<^sup>*\<^sup>* x y" and "occursE n y"
  then show "occursE n x"
    using `rel_closed R`
    by (induction rule: converse_rtranclp_induct) (auto simp add: rel_closed.simps)
qed

lemma rel_closed_sup2:
  assumes "rel_closed R" and "rel_closed S"
  shows "rel_closed (R \<squnion> S)"
  using assms  by(auto simp add: rel_closed.simps)

section \<open>The closure context preserves closedness\<close>

lemma rel_closed_cc:
  assumes "rel_closed R"
  shows "rel_closed (cc R)"
proof
  fix x y n
  assume "cc R x y" and "occursE n y"
  from `cc R x y`
  show "occursE n x"
  proof
    fix x' y' C
    assume "x = appEC C x'" and "y = appEC C y'" and "R x' y'"
    show ?thesis using `occursE n y` unfolding `x = _` `y = _`
      using `R x' y'` `rel_closed R` by (auto simp add: rel_closed.simps)
  qed
qed

section \<open>The Verse rules are closed\<close>

lemma rel_closed_rule_PAdd: "rel_closed rule_PAdd"
  by (auto intro: rel_closed.intros elim!: rule_PAdd.cases)

lemma rel_closed_rule_PGt: "rel_closed rule_PGt"
  by (auto intro!: rel_closed.intros elim!: rule_PGt.cases)

lemma rel_closed_rule_App_Beta: "rel_closed rule_App_Beta"
  by (auto intro!: rel_closed.intros elim!: rule_App_Beta.cases simp add: occursV_liftV)

lemma rel_closed_rule_App_Tup: "rel_closed rule_App_Tup" 
  by (auto 4 4 intro!: rel_closed.intros elim!: rule_App_Tup.cases
      simp add:  nth_enumerate_eq set_conv_nth )

lemma rel_closed_rule_ULit: "rel_closed rule_ULit"
  by (auto intro!: rel_closed.intros elim!: rule_ULit.cases)

lemma rel_closed_rule_UTup: "rel_closed rule_UTup"
  by (auto 4 4 intro!: rel_closed.intros elim!: rule_UTup.cases in_set_zipE)

lemma rel_closed_rule_UX: "rel_closed rule_UX"
  by (auto intro!: rel_closed.intros elim!: rule_UX.cases)

lemma rel_closed_rule_UXOccurs: "rel_closed rule_UXOccurs"
  by (auto intro!: rel_closed.intros elim!: rule_UXOccurs.cases)

lemma rel_closed_rule_Subst: "rel_closed rule_Subst"
  by (auto intro!: rel_closed.intros elim!: rule_Subst.cases)

lemma rel_closed_rule_SubstRec: "rel_closed rule_SubstRec"
  by (auto intro!: rel_closed.intros elim!: rule_SubstRec.cases
        simp add: occursE_liftE)

lemma occursEC_replicate_CDef[simp]: "\<not> occursEC k (replicate n CDef)"
  by (induction n arbitrary: k) (auto simp add: occursECE_def)

lemma rel_closed_rule_DefEliml: "rel_closed rule_DefEliml"
  by (auto intro!: rel_closed.intros elim!: rule_DefEliml.cases
        simp add: occursE_liftE)

lemma rel_closed_rule_DefElimr: "rel_closed rule_DefElimr"
  by (auto intro!: rel_closed.intros elim!: rule_DefElimr.cases
        simp add: occursE_liftE)

lemma rel_closed_rule_Swap: "rel_closed rule_Swap"
  by (auto intro!: rel_closed.intros elim!: rule_Swap.cases)

lemma rel_closed_rule_DefFloat: "rel_closed rule_DefFloat"
  by (auto intro!: rel_closed.intros elim!: rule_DefFloat.cases)

lemma rel_closed_rule_Seq: "rel_closed rule_Seq"
  by (auto intro!: rel_closed.intros elim!: rule_Seq.cases)

lemma rel_closed_rule_Unify_Seql: "rel_closed rule_Unify_Seql"
  by (auto intro!: rel_closed.intros elim!: rule_Unify_Seql.cases)

lemma rel_closed_rule_Unify_Seqr: "rel_closed rule_Unify_Seqr"
  by (auto intro!: rel_closed.intros elim!: rule_Unify_Seqr.cases)

lemma rel_closed_rule_Unify_Unifyl: "rel_closed rule_Unify_Unifyl"
  by (auto intro!: rel_closed.intros elim!: rule_Unify_Unifyl.cases simp add: occursE_liftE)

lemma rel_closed_rule_Unify_Unifyr: "rel_closed rule_Unify_Unifyr"
  by (auto intro!: rel_closed.intros elim!: rule_Unify_Unifyr.cases simp add: occursE_liftE)

lemma rel_closed_rule_DefFail: "rel_closed rule_DefFail"
  by (auto intro!: rel_closed.intros elim!: rule_DefFail.cases)

lemma rel_closed_rule_Fail: "rel_closed rule_Fail"
  by (auto intro!: rel_closed.intros elim!: rule_Fail.cases)

lemma rel_closed_rule_OneFail: "rel_closed rule_OneFail"
  by (auto intro!: rel_closed.intros elim!: rule_OneFail.cases)

lemma rel_closed_rule_OneChoice: "rel_closed rule_OneChoice"
  by (auto intro!: rel_closed.intros elim!: rule_OneChoice.cases)

lemma rel_closed_rule_OneValue: "rel_closed rule_OneValue"
  by (auto intro!: rel_closed.intros elim!: rule_OneValue.cases)

lemma rel_closed_rule_All: "rel_closed rule_All"
  by (auto intro!: rel_closed.intros elim!: rule_All.cases)

theorem ARs_closed: "rel_closed ARs"
unfolding ARs_def
by (intro rel_closed_sup2
   rel_closed_rule_PAdd
   rel_closed_rule_PGt
   rel_closed_rule_App_Beta
   rel_closed_rule_App_Tup
   rel_closed_rule_ULit
   rel_closed_rule_UTup
   rel_closed_rule_UX
   rel_closed_rule_UXOccurs
   rel_closed_rule_Subst
   rel_closed_rule_SubstRec
   rel_closed_rule_DefEliml
   rel_closed_rule_DefElimr
   rel_closed_rule_Swap
   rel_closed_rule_DefFloat
   rel_closed_rule_Seq
   rel_closed_rule_Unify_Seql
   rel_closed_rule_Unify_Seqr
   rel_closed_rule_Unify_Unifyl
   rel_closed_rule_Unify_Unifyr
   rel_closed_rule_DefFail
   rel_closed_rule_Fail
   rel_closed_rule_OneFail
   rel_closed_rule_OneChoice
   rel_closed_rule_OneValue
   rel_closed_rule_All
)



end