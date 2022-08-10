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

lemma depthEC_replicate[simp]: "depthEC (replicate n ece) = n * depthECE ece"
  by (auto simp add: depthEC_def sum_list_replicate)


fun liftVCE :: "nat \<Rightarrow> nat \<Rightarrow> vce \<Rightarrow> vce" where
  "liftVCE n k (CTup vs1 vs2) = CTup (map (liftV n k) vs1) (map (liftV n k) vs2)"

definition liftVC :: "nat \<Rightarrow> nat \<Rightarrow> vc \<Rightarrow> vc" where
  "liftVC n k vc = map (liftVCE n k) vc"

fun liftECE :: "nat \<Rightarrow> nat \<Rightarrow> ece \<Rightarrow> ece" where
  "liftECE n k (CVal vc) = CVal (liftVC n k vc)"
| "liftECE n k CDef = CDef"
| "liftECE n k (CAppl vc v2) = CAppl (liftVC n k vc) (liftV n k v2)"
| "liftECE n k (CAppr v1 vc) = CAppr (liftV n k v1) (liftVC n k vc)"
| "liftECE n k (CSeql e2) = CSeql (liftE n k e2)"
| "liftECE n k (CSeqr e1) = CSeqr (liftE n k e1)"
| "liftECE n k (CBarl e2) = CBarl (liftE n k e2)"
| "liftECE n k (CBarr e1) = CBarr (liftE n k e1)"
| "liftECE n k (CUnil e2) = CUnil (liftE n k e2)"
| "liftECE n k (CUnir e1) = CUnir (liftE n k e1)"
| "liftECE n k COne = COne"
| "liftECE n k CAll = CAll"

fun liftEC :: "nat \<Rightarrow> nat \<Rightarrow> ec \<Rightarrow> ec" where
  "liftEC n k [] = []" 
| "liftEC n k (ece#ec) = liftECE n k ece # liftEC n (depthECE ece + k) ec" 

lemma depthECE_liftECE[simp]: "depthECE (liftECE n k ece) = depthECE ece"
  by (cases ece) auto

lemma depthEC_liftEC [simp]: "depthEC (liftEC n k ec) = depthEC ec"
  by (induction n k ec rule: liftEC.induct) (auto simp add: depthEC_def)

fun delVCE :: "nat \<Rightarrow> vce \<Rightarrow> vce" where
  "delVCE n (CTup vs1 vs2) = CTup (map (delV n) vs1) (map (delV n) vs2)"

definition delVC :: "nat \<Rightarrow> vc \<Rightarrow> vc" where
  "delVC n vc = map (delVCE n) vc"

fun delECE :: "nat \<Rightarrow> ece \<Rightarrow> ece" where
  "delECE n (CVal vc) = CVal (delVC n vc)"
| "delECE n CDef = CDef"
| "delECE n (CAppl vc v2) = CAppl (delVC n vc) (delV n v2)"
| "delECE n (CAppr v1 vc) = CAppr (delV n v1) (delVC n vc)"
| "delECE n (CSeql e2) = CSeql (delE n e2)"
| "delECE n (CSeqr e1) = CSeqr (delE n e1)"
| "delECE n (CBarl e2) = CBarl (delE n e2)"
| "delECE n (CBarr e1) = CBarr (delE n e1)"
| "delECE n (CUnil e2) = CUnil (delE n e2)"
| "delECE n (CUnir e1) = CUnir (delE n e1)"
| "delECE n COne = COne"
| "delECE n CAll = CAll"

fun delEC :: "nat \<Rightarrow> ec \<Rightarrow> ec" where
  "delEC n [] = []" 
| "delEC n (ece#ec) = delECE n ece # delEC (depthECE ece + n) ec" 


lemma depthECE_delECE[simp]: "depthECE (delECE n ece) = depthECE ece"
  by (induction ece) auto

lemma depthEC_delEC[simp]: "depthEC (delEC n ec) = depthEC ec"
  by (induction n ec rule: delEC.induct) (auto simp add: depthEC_def)


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

fun occursVCE where
  "occursVCE n (CTup vs1 vs2) \<longleftrightarrow> (\<exists> v \<in> set vs1. occursV n v) \<or>  (\<exists> v \<in> set vs2. occursV n v)"

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
  by (cases vce) auto

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

lemma occursVCE_liftVCE[simp]: "occursVCE n (liftVCE k j vce) \<longleftrightarrow>
    (if n < j then occursVCE n vce else if n < j + k then False else occursVCE (n - k) vce)"
  by (cases vce) (auto simp add: occursV_liftV)

lemma occursVC_liftVC[simp]: "occursVC n (liftVC k j vc) \<longleftrightarrow>
    (if n < j then occursVC n vc else if n < j + k then False else occursVC (n - k) vc)"
  by (auto simp add: occursVC_def liftVC_def)

lemma occursECE_liftECE[simp]: "occursECE n (liftECE k j ece) \<longleftrightarrow>
    (if n < j then occursECE n ece else if n < j + k then False else occursECE (n - k) ece)"
  by (cases ece) (auto simp add: occursECE_def occursV_liftV occursE_liftE)

lemma occursEC_liftEC[simp]: "occursEC n (liftEC k j ec) \<longleftrightarrow>
    (if n < j then occursEC n ec else if n < j + k then False else occursEC (n - k) ec)"
 by (induction k j ec arbitrary: n rule: liftEC.induct) auto

lemma occursVCE_delEC[simp]: "\<not> occursVCE n vce \<Longrightarrow>
  occursVCE k (delVCE n vce) = (if k < n then occursVCE k vce else occursVCE (Suc k) vce)"
  by (cases vce) auto

lemma occursVC_delVC[simp]: "\<not> occursVC n vc \<Longrightarrow>
  occursVC k (delVC n vc) = (if k < n then occursVC k vc else occursVC (Suc k) vc)"
 by (auto simp add: occursVC_def delVC_def)

lemma occursECE_delEC[simp]: "\<not> occursECE n ece \<Longrightarrow>
  occursECE k (delECE n ece) = (if k < n then occursECE k ece else occursECE (Suc k) ece)"
 by (cases ece) (auto simp add: occursECE_def)

lemma occursEC_delEC[simp]: "\<not> occursEC n ec \<Longrightarrow>
  occursEC k (delEC n ec) = (if k < n then occursEC k ec else occursEC (Suc k) ec)"
 by (induction n ec arbitrary: k rule: delEC.induct) auto

lemma occursVCE_substVCE[simp]:
  "occursVCE i (substVCE n v vce) \<longleftrightarrow> 
  (i \<noteq> n \<and> occursVCE i vce) \<or> (occursVCE n vce \<and> occursV i v)"
by (cases vce) auto

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