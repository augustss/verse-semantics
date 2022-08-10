theory CCJoin
imports 
  CongruenceClosure
  ParallelContexts
begin

section \<open>Local confluence via overlap\<close>

lemma commute_parallel_context:
  assumes "congruent R"
  assumes "parallelEC ec1 ec2"
  assumes "appEC ec1 a = appEC ec2 b"
  assumes "R a a' "
  assumes "R b b'"
  shows "(R OO R\<inverse>\<inverse>) (appEC ec1 a') (appEC ec2 b')" 
  using assms
proof-
  from `parallelEC _ _` `_appEC _ _ = _`
  obtain ec1' ec2'
    where "appEC ec1 a' = appEC ec2' b"
      and "appEC ec2 b' = appEC ec1' a"
      and "appEC ec2' b' = appEC ec1' a'"
    unfolding parallelEC_def by blast
  moreover
  from `R b b'`
  have "R (appEC ec2' b) (appEC ec2' b')" using `congruent R` by auto
  moreover
  from `R a a'`
  have "R (appEC ec1' a) (appEC ec1' a')" using `congruent R` by auto
  ultimately
  show ?thesis by (metis conversepI relcomppI)
qed

lemma cc_local_confluence[case_names red_equiv at_root below_root]:
  assumes "red_equiv (cc R) S"
  assumes at_root: "R\<inverse>\<inverse> OO R \<le> S"
  assumes below_root: "R\<inverse>\<inverse> OO cc' R \<le> S"
  shows "(cc R)\<inverse>\<inverse> OO cc R \<le> S"
proof-
  from `red_equiv _ _`
  have "congruent S"
       "symp S"
       "reflp S"
   and R_left: "cc R OO S \<le> S"
   and R_right: "S OO (cc R)\<inverse>\<inverse> \<le> S"
  unfolding red_equiv_def by auto
{
  fix a1 a2 C1 b C2 c
  assume "appEC C1 a1 = appEC C2 a2"
  assume "R a1 b"
  assume "R a2 c"
  have "S (appEC C1 b) (appEC C2 c)"
    using `appEC _ _ = _`
  proof(induction C1 C2 rule: list_induct2'[case_names Nil_Nil Cons_Nil Nil_Cons Cons_Cons])
    case Nil_Nil
    hence "a1 = a2" by simp
    with `R a1 b` `R a2 c`
    have "(R\<inverse>\<inverse> OO R) b c" by auto
    hence "S b c" using at_root by auto
    then show ?case by simp
  next
    case (Nil_Cons ece ec)
    from `R a2 c` Nil_Cons
    have "cc' R a1 (appEC (ece # ec) c)" by (auto intro: cc'.intros)
    with `R a1 b` Nil Cons
    have "(R\<inverse>\<inverse> OO cc' R) b (appEC (ece # ec) c)" by auto
    hence "S b (appEC (ece # ec) c)" using below_root by auto
    thus ?case using Nil Cons by simp
  next
    case (Cons_Nil ece1 ec1)
    from `R a1 b` Cons_Nil
    have "cc' R a2 (appEC (ece1 # ec1) b)" by (auto intro: cc'.intros)
    with `R a2 c` Nil Cons
    have "(R\<inverse>\<inverse> OO cc' R) c (appEC (ece1 # ec1) b)" by auto
    hence "S c (appEC (ece1 # ec1) b)" using below_root by auto
    hence "S (appEC (ece1 # ec1) b) c" using `symp S` by (auto simp add: symp_def)
    thus ?case using Nil by simp
  next
    case (Cons_Cons ece1 ec1 ece2 ec2)
    then show ?case
    proof (cases "ece1 = ece2")
      case True with Cons_Cons 
      have "appEC ec1 a1 = appEC ec2 a2" by (simp add: appEC_def)
      hence "S (appEC ec1 b) (appEC ec2 c)" using Cons_Cons by auto
      hence "S (appEC [ece1] (appEC ec1 b)) (appEC [ece1] (appEC ec2 c))" using `congruent S` by auto
      then show ?thesis using True by (simp add: appEC_def)
    next
      case False
      hence "parallelECE ece1 ece2" by (rule parallelECE)
      hence "parallelEC [ece1] [ece2]" by (rule parallelEC_singleton)
      from parallelEC_append1[OF this]
      have "parallelEC (ece1 # ec1) (ece2 # ec2)" by simp
      from congruent_cc[of R] this Cons_Cons(2) cc_rootI[of R, OF `R a1 b`] cc_rootI[of R, OF `R a2 c`]
      have "((cc R) OO (cc R)\<inverse>\<inverse> ) (appEC (ece1 # ec1) b) (appEC (ece2 # ec2) c)"
        by (rule commute_parallel_context)
      moreover
      have "(cc R) OO (cc R)\<inverse>\<inverse> \<le> S"
        using R_left R_right `reflp S` by (auto simp add: reflp_def)
      ultimately
      show ?thesis by auto
    qed
  qed

} thus ?thesis using `symp S` by (auto simp add: cc.simps symp_def)
qed

end