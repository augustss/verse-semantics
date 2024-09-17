theory NoStutter
imports Rules SubstContext
begin

section \<open>A relation stutters if x \<rightarrow> x\<close>

definition rel_no_stutter :: "red \<Rightarrow> bool"  where
  "rel_no_stutter R \<longleftrightarrow> (\<forall> x. \<not> R x x)"

lemma rel_no_stutter_sup2:
  assumes "rel_no_stutter R" and "rel_no_stutter S"
  shows "rel_no_stutter (R \<squnion> S)"
  using assms by (auto simp add: rel_no_stutter_def)

section \<open>The closure context takes non-stuttering reations to non-stuttering relations.\<close>

lemma rel_no_stutter_cc:
  assumes "rel_no_stutter R"
  shows "rel_no_stutter (cc R)"
  using assms 
by (auto simp add: rel_no_stutter_def elim!: cc.cases)

section \<open>The Verse rules do not stutter\<close>

lemma rel_no_stutter_rule_PAdd: "rel_no_stutter rule_PAdd"
  by (auto simp add: rel_no_stutter_def elim!: rule_PAdd.cases)

lemma rel_no_stutter_rule_PGt: "rel_no_stutter rule_PGt"
  by (auto simp add: rel_no_stutter_def elim!: rule_PGt.cases)

lemma rel_no_stutter_rule_App_Beta: "rel_no_stutter rule_App_Beta"
  by (auto simp add: rel_no_stutter_def elim!: rule_App_Beta.cases simp add: occursV_liftV)

lemma App_eq_bars[simp]:
  "App v1 v2 = bars es \<longleftrightarrow> es = [App v1 v2]"
  by (induction es rule:bars.induct) auto

lemma rel_no_stutter_rule_App_Tup: "rel_no_stutter rule_App_Tup" 
  by (auto simp add: rel_no_stutter_def elim!: rule_App_Tup.cases)

lemma rel_no_stutter_rule_ULit: "rel_no_stutter rule_ULit"
  by (auto simp add: rel_no_stutter_def elim!: rule_ULit.cases)

lemma Uni_eq_seqs[simp]:
  "Uni e1 e2 = seqs es \<longleftrightarrow> es = [Uni e1 e2]"
  by (induction es rule:seqs.induct) auto

lemma rel_no_stutter_rule_UTup: "rel_no_stutter rule_UTup"
  by (auto simp add: rel_no_stutter_def elim!: rule_UTup.cases in_set_zipE)

lemma rel_no_stutter_rule_UX: "rel_no_stutter rule_UX"
  by (auto simp add: rel_no_stutter_def elim!: rule_UX.cases)

lemma rel_no_stutter_rule_UXOccurs: "rel_no_stutter rule_UXOccurs"
  by (auto simp add: rel_no_stutter_def elim!: rule_UXOccurs.cases)

lemma map_eq:
  "xs = map f xs \<longleftrightarrow> (\<forall> x \<in> set xs. x = f x)"
  by (induction xs) auto

lemma appVCE_bind_eq[simp]:
  "appVCE vce e1 = appVCE (bind f vce) e2 \<longleftrightarrow> vce = bind f vce \<and> e1 = e2"
by (cases vce) auto

lemma appVC'_bind_eq[simp]:
  "appVC' vc e1 = appVC' (bind f vc) e2 \<longleftrightarrow> vc = bind f vc \<and> e1 = e2"
by (induction vc) (auto simp add: appVC'_def)

lemma appVC_bind_eq[simp]:
  "appVC vc e1 = appVC (bind f vc) e2 \<longleftrightarrow> vc = bind f vc \<and> e1 = e2"
by (simp add: appVC_def)

lemma appECE_bind_eq[simp]:
  "appECE ece e1 = appECE (bind f ece) e2 \<longleftrightarrow> ece = bind f ece \<and> e1 = e2"
by (cases ece) auto

lemma appEC_bind_eq[simp]:
  "appEC ec e1 = appEC (bind f ec) e2 \<longleftrightarrow> ec = bind f ec \<and> e1 = e2"
  by (induction f ec arbitrary: e1 e2 rule: bind_list.induct)
     (auto simp add: appEC_def)

lemma rel_no_stutter_rule_Subst: "rel_no_stutter rule_Subst"
  by (auto 4 4 simp add: rel_no_stutter_def elim!: rule_Subst.cases simp add: bind_id_iff2)

lemma size_list_congr[cong]:
  "(\<And> x. x \<in> set xs \<Longrightarrow> f x = g x) \<Longrightarrow> size_list f xs = size_list g xs"
  by (induction xs) auto

lemma size_exp_renumberE[simp]: "size_exp (renumber f e) = size_exp e"
  and size_var_renumberV[simp]: "size_val (renumber f v) = size_val v"
  by (induction e and v arbitrary: f and f) (auto)

lemma occursEC_replicate_CDef[simp]: "\<not> occurs k (replicate n CDef)"
  by (induction n arbitrary: k) (auto simp add: occurs_ece_def)

definition "sizeVCE ece = size_val (appVCE ece (Const 0))"

lemma size_val_appVCE[simp]:
  "size_val (appVCE vce v) = sizeVCE vce + size_val v"
by (cases vce) (auto simp add: sizeVCE_def)

definition "sizeVC vc = size_val (appVC vc Fail)"

lemma size_val_appVC[simp]:
  "size_val (appVC vc e) = sizeVC vc + size_exp e"
by (induction vc) (auto simp add: sizeVC_def appVC_def appVC'_def)


definition "sizeECE ece = size_exp (appECE ece Fail)"

lemma size_exp_appECE[simp]:
  "size_exp (appECE ece e) = sizeECE ece + size_exp e"
  by (cases ece) (auto simp add: sizeECE_def)

lemma sizeECE_gt_0[simp]: "sizeECE ece > 0"
  by (cases ece) (auto simp add: sizeECE_def)

definition "sizeEC ec = sum_list (map sizeECE ec)"

lemma sizeEC_eq_0[simp]: "sizeEC ec = 0  \<longleftrightarrow> ec = []"
  by (cases ec) (auto simp add: sizeEC_def)

lemma size_exp_appEC[simp]:
  "size_exp (appEC ec e) = sizeEC ec + size_exp e"
  by (induction ec) (auto simp add: sizeEC_def appEC_def)

lemma sizeVCE_delVCE[simp]: "sizeVCE (renumber f vce) = sizeVCE vce"
  unfolding sizeVCE_def by (induction vce) auto

lemma sizeVC_renumber[simp]: "sizeVC (renumber f vc) = sizeVC vc"
  unfolding sizeVC_def by (induction vc arbitrary: f) auto

lemma sizeECE_renumber[simp]: "sizeECE (renumber f ece) = sizeECE ece"
  by (induction ece) (auto simp add: sizeECE_def)

lemma sizeEC_renumber[simp]: "sizeEC (renumber f ec) = sizeEC ec"
  unfolding sizeEC_def by (induction ec arbitrary: f) auto

lemmas size_differsE = arg_cong[where f = size_exp, elim_format]

lemma rel_no_stutter_rule_SubstRec: "rel_no_stutter rule_SubstRec"
  by (auto simp add: rel_no_stutter_def elim!: rule_SubstRec.cases elim!: size_differsE)

lemma rel_no_stutter_rule_DefEliml: "rel_no_stutter rule_DefEliml"
  by (auto simp add: rel_no_stutter_def elim!: rule_DefEliml.cases elim!: size_differsE)

lemma rel_no_stutter_rule_DefElimr: "rel_no_stutter rule_DefElimr"
  by (auto simp add: rel_no_stutter_def elim!: rule_DefElimr.cases elim!: size_differsE)

lemma rel_no_stutter_rule_Swap: "rel_no_stutter rule_Swap"
  by (auto simp add: rel_no_stutter_def elim!: rule_Swap.cases)

lemma rel_no_stutter_rule_DefFloat: "rel_no_stutter rule_DefFloat"
  apply (auto simp add: rel_no_stutter_def elim!: rule_DefFloat.cases)
  apply (case_tac ec; simp)
  apply (auto simp add: isX_def appEC_def isXE.simps)
  done

lemma rel_no_stutter_rule_Seq: "rel_no_stutter rule_Seq"
  by (auto simp add: rel_no_stutter_def elim!: rule_Seq.cases)

lemma rel_no_stutter_rule_Unify_Seql: "rel_no_stutter rule_Unify_Seql"
  by (auto simp add: rel_no_stutter_def elim!: rule_Unify_Seql.cases)

lemma rel_no_stutter_rule_Unify_Seqr: "rel_no_stutter rule_Unify_Seqr"
  by (auto simp add: rel_no_stutter_def elim!: rule_Unify_Seqr.cases)

lemma rel_no_stutter_rule_Unify_Unifyl: "rel_no_stutter rule_Unify_Unifyl"
  by (auto simp add: rel_no_stutter_def elim!: rule_Unify_Unifyl.cases simp add: occursE_liftE)

lemma rel_no_stutter_rule_Unify_Unifyr: "rel_no_stutter rule_Unify_Unifyr"
  by (auto simp add: rel_no_stutter_def elim!: rule_Unify_Unifyr.cases simp add: occursE_liftE)

lemma rel_no_stutter_rule_DefFail: "rel_no_stutter rule_DefFail"
  by (auto simp add: rel_no_stutter_def elim!: rule_DefFail.cases)

lemma rel_no_stutter_rule_Fail: "rel_no_stutter rule_Fail"
  by (auto simp add: rel_no_stutter_def elim!: rule_Fail.cases)

lemma rel_no_stutter_rule_OneFail: "rel_no_stutter rule_OneFail"
  by (auto simp add: rel_no_stutter_def elim!: rule_OneFail.cases)

lemma rel_no_stutter_rule_OneChoice: "rel_no_stutter rule_OneChoice"
  by (auto simp add: rel_no_stutter_def elim!: rule_OneChoice.cases)

lemma rel_no_stutter_rule_OneValue: "rel_no_stutter rule_OneValue"
  by (auto simp add: rel_no_stutter_def elim!: rule_OneValue.cases)

lemma rel_no_stutter_rule_All: "rel_no_stutter rule_All"
  by (auto simp add: rel_no_stutter_def elim!: rule_All.cases)

lemma rel_no_stutter_rule_FailL: "rel_no_stutter rule_FailL"
  by (auto simp add: rel_no_stutter_def elim!: rule_FailL.cases)

lemma rel_no_stutter_rule_FailR: "rel_no_stutter rule_FailR"
  by (auto simp add: rel_no_stutter_def elim!: rule_FailR.cases)

lemma rel_no_stutter_rule_AssocChoice: "rel_no_stutter rule_AssocChoice"
  by (auto simp add: rel_no_stutter_def elim!: rule_AssocChoice.cases)

lemma rel_no_stutter_rule_Choose: "rel_no_stutter rule_Choose"
  by (auto simp add: rel_no_stutter_def elim!: rule_Choose.cases elim!: size_differsE)

theorem ARs_no_stutter: "rel_no_stutter ARs"
unfolding ARs_def
by (intro rel_no_stutter_sup2
   rel_no_stutter_rule_PAdd
   rel_no_stutter_rule_PGt
   rel_no_stutter_rule_App_Beta
   rel_no_stutter_rule_App_Tup
   rel_no_stutter_rule_ULit
   rel_no_stutter_rule_UTup
   rel_no_stutter_rule_UX
   rel_no_stutter_rule_UXOccurs
   rel_no_stutter_rule_Subst
   rel_no_stutter_rule_SubstRec
   rel_no_stutter_rule_DefEliml
   rel_no_stutter_rule_DefElimr
   rel_no_stutter_rule_Swap
   rel_no_stutter_rule_DefFloat
   rel_no_stutter_rule_Seq
   rel_no_stutter_rule_Unify_Seql
   rel_no_stutter_rule_Unify_Seqr
   rel_no_stutter_rule_Unify_Unifyl
   rel_no_stutter_rule_Unify_Unifyr
   rel_no_stutter_rule_DefFail
   rel_no_stutter_rule_Fail
   rel_no_stutter_rule_OneFail
   rel_no_stutter_rule_OneChoice
   rel_no_stutter_rule_OneValue
   rel_no_stutter_rule_All
   rel_no_stutter_rule_FailL
   rel_no_stutter_rule_FailR
   rel_no_stutter_rule_AssocChoice
   rel_no_stutter_rule_Choose
)

end