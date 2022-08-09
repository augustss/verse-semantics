theory SubstContext
imports Subst Contexts
begin

fun (sequential) depthECE :: "ece \<Rightarrow> nat" where
  "depthECE (CVal _) = 1"
| "depthECE CDef = 1"
| "depthECE (CAppl _ _) = 1"
| "depthECE (CAppr _ _) = 1"
| "depthECE _ = 0"

fun depthEC :: "ec \<Rightarrow> nat" where
  "depthEC ec = sum_list (map depthECE ec)"

definition "occursVCE n vce = occursV n (appVCE vce (Const 0))"

lemma occursV_appVCE[simp]:
  "occursV n (appVCE vce v) \<longleftrightarrow> occursV n v \<or> occursVCE n vce"
  by (cases vce) (auto simp add: occursVCE_def)

definition "occursVC n vc = occursV n (appVC vc Fail)"

lemma occursE_appVC[simp]:
  "occursV n (appVC vc e) \<longleftrightarrow> occursE (1 + n) e \<or> occursVC n vc"
  by (induction vc) (auto simp add: appVC_def appVC'_def occursVC_def)

definition "occursECE n ece = occursE n (appECE ece Fail)"

lemma occursE_appECE[simp]:
  "occursE n (appECE ece e) \<longleftrightarrow> occursE (depthECE ece + n) e \<or> occursECE n ece"
  by (cases ece) (auto simp add: occursECE_def)

definition "occursEC n ec = occursE n (appEC ec Fail)"

lemma occursE_appEC[simp]:
  "occursE n (appEC ec e) \<longleftrightarrow> occursE (depthEC ec + n) e \<or> occursEC n ec "
  apply (induction ec arbitrary: n)
   apply (auto simp add: appEC_def occursEC_def simp del: occursE_appECE)
  apply (metis ab_semigroup_add_class.add_ac(1) group_cancel.add2 occursE_appECE)
  apply (simp add: ab_semigroup_add_class.add_ac(1) add.commute)
  apply simp
  done

end