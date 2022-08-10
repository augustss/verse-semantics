theory SubstContext
imports Subst Contexts
begin

fun (sequential) depthECE :: "ece \<Rightarrow> nat" where
  "depthECE (CVal _) = 1"
| "depthECE CDef = 1"
| "depthECE (CAppl _ _) = 1"
| "depthECE (CAppr _ _) = 1"
| "depthECE _ = 0"

definition depthEC :: "ec \<Rightarrow> nat" where
  "depthEC ec = sum_list (map depthECE ec)"

lemma depthECE_isXE[simp]: "isXE ece \<Longrightarrow> depthECE ece = 0"
  by (induction ece) (auto elim: isXE.cases)

lemma depthEC_isX[simp]: "isX ec \<Longrightarrow> depthEC ec = 0"
  by (induction ec) (auto simp add: isX_def depthEC_def)

fun substVCE :: "nat \<Rightarrow> val \<Rightarrow> vce \<Rightarrow> vce" where
  "substVCE n v (CTup vs1 vs2) = CTup (map (substV n v) vs1) (map (substV n v) vs2)"

definition substVC :: "nat \<Rightarrow> val \<Rightarrow> vc \<Rightarrow> vc" where
  "substVC n v vc = map (substVCE n v) vc"

fun substECE :: "nat \<Rightarrow> val \<Rightarrow> ece \<Rightarrow> ece" where
  "substECE n v (CVal vc) = CVal (substVC n v vc)"
| "substECE n v CDef = CDef"
| "substECE n v (CAppl vc v2) = CAppl (substVC n v vc) (substV n v v2)"
| "substECE n v (CAppr v1 vc) = CAppr (substV n v v1) (substVC n v vc)"
| "substECE n v (CSeql e2) = CSeql (substE n v e2)"
| "substECE n v (CSeqr e1) = CSeqr (substE n v e1)"
| "substECE n v (CBarl e2) = CBarl (substE n v e2)"
| "substECE n v (CBarr e1) = CBarr (substE n v e1)"
| "substECE n v (CUnil e2) = CUnil (substE n v e2)"
| "substECE n v (CUnir e1) = CUnir (substE n v e1)"
| "substECE n v COne = COne"
| "substECE n v CAll = CAll"

fun substEC :: "nat \<Rightarrow> val \<Rightarrow> ec \<Rightarrow> ec" where
  "substEC n v [] = []" 
| "substEC n v (ece#ec) = substECE n v ece # substEC (depthECE ece + n) (\<up>\<^sub>v (depthECE ece) 0 v) ec" 

lemma depthECE_substECE[simp]: "depthECE (substECE n v ece) = depthECE ece"
  by (induction ece) auto

lemma depthEC_substEC[simp]: "depthEC (substEC n v ec) = depthEC ec"
  by (induction ec arbitrary: n v) (auto simp add: depthEC_def)


definition "occursVCE n vce = occursV n (appVCE vce (Const 0))"

definition occursVC where "occursVC n vc \<longleftrightarrow> (\<exists> vce \<in> set vc. occursVCE n vce)"

lemma occursVC_Nil[simp]: "occursVC n [] = False"
  by (simp add: occursVC_def appVC_def appVC'_def)

lemma occursVC_Cons[simp]: "occursVC n (vce # vc) = occursVCE n vce \<or> occursVC n vc"
  by (auto simp add: occursVC_def appVC_def appVC'_def)

definition "occursECE n ece = occursE n (appECE ece Fail)"

fun occursEC where
  "occursEC n [] = False"
| "occursEC n (ece # ec) \<longleftrightarrow> occursECE n ece \<or> occursEC (depthECE ece + n) ec"

lemma occursV_appVCE[simp]:
  "occursV n (appVCE vce v) \<longleftrightarrow> occursV n v \<or> occursVCE n vce"
  by (cases vce) (auto simp add: occursVCE_def)

lemma occursE_appVC[simp]:
  "occursV n (appVC vc e) \<longleftrightarrow> occursE (1 + n) e \<or> occursVC n vc"
  by (induction vc) (auto simp add: appVC_def appVC'_def occursVC_def)

lemma occursE_appECE[simp]:
  "occursE n (appECE ece e) \<longleftrightarrow> occursE (depthECE ece + n) e \<or> occursECE n ece"
  by (cases ece) (auto simp add: occursECE_def)

lemma occursE_appEC[simp]:
  "occursE n (appEC ec e) \<longleftrightarrow> occursE (depthEC ec + n) e \<or> occursEC n ec "
  apply (induction n ec rule: occursEC.induct)
   apply (auto simp add: appEC_def depthEC_def)
  apply (metis ab_semigroup_add_class.add_ac(1) group_cancel.add2)
  apply (simp add: ab_semigroup_add_class.add_ac(1) add.commute)
  done

lemma occursVCE_substVCE[simp]:
  "occursVCE i (substVCE n v vce) \<longleftrightarrow> 
  (i \<noteq> n \<and> occursVCE i vce) \<or> (occursVCE n vce \<and> occursV i v)"
  apply (induction vce)
  apply (auto simp add: occursVCE_def )
  apply (meson Un_iff imageI occursV_substV)+
  done

lemma occursVC_substVC[simp]:
  "occursVC i (substVC n v vc) \<longleftrightarrow> 
  (i \<noteq> n \<and> occursVC i vc) \<or> (occursVC n vc \<and> occursV i v)"
  by (auto simp add: substVC_def occursVC_def)

lemma occursECE_substECE[simp]:
  "occursECE i (substECE n v ece) \<longleftrightarrow> 
  (i \<noteq> n \<and> occursECE i ece) \<or> (occursECE n ece \<and> occursV i v)"
  by (cases ece) (auto simp add: occursECE_def)


lemma occursEC_substEC[simp]:
  "occursEC i (substEC n v ec) \<longleftrightarrow> 
  (i \<noteq> n \<and> occursEC i ec) \<or> (occursEC n ec \<and> occursV i v)"
  by (induction ec arbitrary: i n v) (auto simp add: occursV_liftV)

end