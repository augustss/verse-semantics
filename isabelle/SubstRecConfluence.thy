theory SubstRecConfluence
imports
  Syntax
  Contexts
  ParallelContexts
  CongruenceClosure
  CCJoin
  Rules
begin

lemma joinI[case_names Peak]:
  assumes "\<And> a b c. R1 a b \<Longrightarrow> R2 a c \<Longrightarrow> S b c"
  shows "R1\<inverse>\<inverse> OO R2 \<le> S"
  using assms by auto


(* Subst-rec is not yet part of Rs, but here is its own little diagram *)

consts subVC :: "vc \<Rightarrow> vc \<Rightarrow> exp \<Rightarrow> vc"

lemma subVC_eq:
  assumes  "vc1 \<noteq> vc2"
  assumes  "appVC vc1 e1 = appVC vc2 e2"
  shows    "subVC vc1 vc2 e2 = vc1"
  sorry

lemma subVC_swap:
  assumes  "vc1 \<noteq> vc2"
  shows    "appVC (subVC vc1 vc2 e2) e1 = appVC (subVC vc2 vc1 e1) e2"
  sorry

lemma liftVC_subVC:
  "liftVC n k (subVC vc1 vc2 e) = subVC (liftVC n k vc1) (liftVC n k vc2) (liftE n (Suc k) e)"
  sorry

lemma substVC_subVC:
  "substVC n v (subVC vc1 vc2 e) = subVC (substVC n v vc1) (substVC n v vc2) (substE (Suc n) (liftV 1 0 v) e)"
  sorry

lemma liftV_appVC[simp]: "liftV n k (appVC vc e) = appVC (liftVC n k vc) (liftE n (Suc k) e)"
  sorry

lemma substV_appVC[simp]: "substV n v (appVC vc e) = appVC (substVC n v vc) (substE (Suc n) (liftV 1 0 v) e)"
  sorry

lemma liftVC_substVC:
  "liftVC k j (substVC n v vc) =
    (if n < j then substVC n (liftV k j v) (liftVC k j vc)
    else substVC (n+k) (liftV k j v) (liftVC k j vc))"
  sorry

lemma substVC_dead:
  "\<not> occursVC n vc \<Longrightarrow> substVC n v vc = vc"
  sorry


lemma liftVC_liftVC[simp]:
  "l \<le> k2 \<Longrightarrow> k2 \<le> j+l \<Longrightarrow> liftVC k k2 (liftVC j l vc) = liftVC (k+j) l vc"
  sorry

lemma liftVC_liftVC_sort[simp]:
  "k2 < l \<Longrightarrow> liftVC k k2 (liftVC j l vc) = liftVC j (l+k) (liftVC k k2 vc)"
  sorry

lemma substVC_again[simp]:
  "\<not> occursV n v2 \<Longrightarrow> substVC n v1 (substVC n v2 vc) = substVC n v2 vc"
  sorry


lemma substVC_substVC:
  "\<not> occursV n2 v1 \<Longrightarrow>
  substVC n1 v1 (substVC n2 v2 vc) = substVC n2 (substV n1 v1 v2) (substVC n1 v1 vc)"
  sorry

lemma liftE_substE:
  "liftE k j (substE n v e) =
    (if n < j then substE n (liftV k j v) (liftE k j e)
    else substE (n+k) (liftV k j v) (liftE k j e))"
  sorry

lemma substE_dead:
  "\<not> occursE n e \<Longrightarrow> substE n v e = e"
  sorry

lemma liftE_liftE[simp]:
  "l \<le> k2 \<Longrightarrow> k2 \<le> j+l \<Longrightarrow> liftE k k2 (liftE j l e) = liftE (k+j) l e"
  sorry

lemma liftE_liftE_sort[simp]:
  "k2 < l \<Longrightarrow> liftE k k2 (liftE j l e) = liftE j (l+k) (liftE k k2 e)"
  sorry


lemma substE_again[simp]:
  "\<not> occursV n v2 \<Longrightarrow> substE n v1 (substE n v2 e) = substE n v2 e"
  sorry


lemma substE_substE:
  "\<not> occursV n2 v1 \<Longrightarrow>
  substE n1 v1 (substE n2 v2 e) = substE n2 (substV n1 v1 v2) (substE n1 v1 e)"
  sorry

lemma arg_cong3: "\<lbrakk>a = b; c = d; e = g\<rbrakk> \<Longrightarrow> f a c e = f b d g"
  by (iprover intro: refl elim: subst)


lemma Subst_Rec_Subst_Rec: "rule_SubstRec\<inverse>\<inverse> OO rule_SubstRec \<le> (cc rule_SubstRec)\<^sup>*\<^sup>* OO (cc rule_SubstRec)\<^sup>*\<^sup>*\<inverse>\<inverse>"
proof(rule joinI; elim rule_SubstRec.cases; clarify; goal_cases)
  case (1 _ _ _ _ e1 _ vc1 _ n e2 _ vc2 _)

  let "(((cc rule_SubstRec)\<^sup>*\<^sup>* OO (cc rule_SubstRec)\<^sup>*\<^sup>*\<inverse>\<inverse>) ?left ?right)" = "?case"
  let ?letUnderLam = "\<lambda> n vc rhs e. Uni (Val (Var n)) (Val (appVC vc (Def (Seq (Uni (Val (Var 0)) (Val rhs)) e))))"
  let "Uni (Val (Var n)) (Val (appVC vc1 (Def (Seq (Uni (Val (Var 0)) (Val ?rhs1)) ?e1'))))" = "?left"
  let "Uni (Val (Var n)) (Val (appVC vc2 (Def (Seq (Uni (Val (Var 0)) (Val ?rhs2)) ?e2'))))" = "?right"

  have "?letUnderLam n vc1 ?rhs1 ?e1' = ?left".. (* sanity check *)

  show ?case
  proof(cases "vc1 = vc2")
    case True
    with `appVC _ _ = _`
    have "e1 = e2" by auto
    hence "?letUnderLam n vc1 ?rhs1 ?e1' = ?letUnderLam n vc2 ?rhs2 ?e2'" using True by simp
    then show ?thesis by auto
  next
    case False

    have vc1_sub: "\<And> e1. appVC vc1 e1 = appVC (subVC vc2 vc1 e1) e2"
      by (metis "1"(3) False subVC_eq subVC_swap)

    have vc2_sub: "\<And> e2. appVC vc2 e2 = appVC (subVC vc1 vc2 e2) e1"
      by (metis "1"(3) False subVC_eq subVC_swap)

    have helper: "\<And> P x y. x = y \<Longrightarrow> P\<^sup>*\<^sup>* x y" by simp

    have inj: "substVC (Suc (Suc n)) (Var 0) (liftVC 2 0 vc1) \<noteq>
              substVC (Suc (Suc n)) (Var 0) (liftVC 2 0 vc2)" sorry

    show ?thesis
      apply (rule relcomppI)
       apply (rule rtranclp_trans[OF r_into_rtranclp r_into_rtranclp])
        apply (simp only: liftV_appVC substV_appVC)
        apply (subst vc1_sub)
        apply (rule cc_rootI)
        apply (rule rule_SubstRec[OF _ refl refl])
        apply (rule `occursE (n+1) e2`)
      apply (subst subVC_swap[OF False[symmetric]])
      apply (rule congruentE[OF congruent_cc, of _ _ _  "[CUnir e]" for e, simplified appEC_def, simplified])
      apply (rule congruentE[OF congruent_cc, of _ _ _  "[CVal vc]" for vc, simplified appEC_def, simplified])
      apply (rule congruentE[OF congruent_cc, of _ _ _  "[CDef]", simplified appEC_def, simplified])
       apply (rule congruentE[OF congruent_cc, of _ _ _  "[CSeql e]" for e, simplified appEC_def, simplified])
       apply (rule cc_rootI)
       apply (simp only: liftV_appVC[symmetric] substV_appVC[symmetric]) 
       apply (subst vc1_sub)
       apply (simp only: liftV_appVC substV_appVC) 
       apply (rule rule_SubstRec[OF _ refl refl])
      using  `occursE (n+1) e2` apply (simp add: occursE_liftE)
      apply (rule conversepI)
       apply (rule rtranclp_trans[OF r_into_rtranclp rtranclp_trans[OF r_into_rtranclp helper]])
        apply (simp only: liftV_appVC substV_appVC)
        apply (subst vc2_sub)
        apply (rule cc_rootI)
        apply (rule rule_SubstRec[OF _ refl refl])
       apply (rule `occursE (n+1) e1`)
      apply (subst subVC_swap[OF False])
      apply (rule congruentE[OF congruent_cc, of _ _ _  "[CUnir e]" for e, simplified appEC_def, simplified])
      apply (rule congruentE[OF congruent_cc, of _ _ _  "[CVal vc]" for vc, simplified appEC_def, simplified])
      apply (rule congruentE[OF congruent_cc, of _ _ _  "[CDef]", simplified appEC_def, simplified])
       apply (rule congruentE[OF congruent_cc, of _ _ _  "[CSeql e]" for e, simplified appEC_def, simplified])
       apply (rule cc_rootI)
       apply (simp only: liftV_appVC[symmetric] substV_appVC[symmetric]) 
       apply (subst vc2_sub)
       apply (simp only: liftV_appVC substV_appVC) 
       apply (rule rule_SubstRec[OF _ refl refl])
      using  `occursE (n+1) e1` apply (simp add: occursE_liftE)
      (* Now we have to show that the result is the same *)
      apply (simp add: subVC_swap[OF False])
      apply (rule arg_cong2[where f = "\<lambda> e1 e2. appVC (subVC vc2 vc1 e1) e2" for vc1 vc2])
      apply (rule arg_cong2[where f = VLet])
        apply (rule arg_cong2[where f = appVC])
         apply simp
      apply (rule arg_cong2[where f = VLet])
         apply (rule arg_cong2[where f = appVC])
          apply (subst subVC_eq[OF False[symmetric] "1"(3)[symmetric]])
          apply (simp add: liftVC_substVC)
          apply (subst substVC_substVC)
           apply simp
          apply (simp add: substVC_dead)
          apply (simp add: liftE_substE)
          apply (subst substE_substE)
           apply simp
         apply (simp add: substE_dead occursE_liftE)
          apply (simp add: liftE_substE)
          apply (subst substE_substE)
           apply simp
        apply (simp add: substE_dead occursE_liftE)
       apply simp
      apply (rule arg_cong2[where f = VLet])
       apply (simp add: liftVC_subVC substVC_subVC subVC_swap[OF inj])
      apply (rule arg_cong2[where f = "\<lambda> e1 e2. appVC (subVC vc2 vc1 e1) e2" for vc1 vc2])
      apply (rule arg_cong2[where f = VLet])
         apply (rule arg_cong2[where f = appVC])
          apply (subst (2) subVC_eq[OF False "1"(3), symmetric])
          apply (simp add: liftVC_subVC substVC_subVC)
          apply (rule arg_cong3[where f = subVC])
            apply (simp add: liftVC_substVC)
            apply (subst substVC_substVC)
             apply simp
            apply (simp add: substVC_dead)
           apply (simp add: liftVC_substVC)
           apply (subst substVC_substVC)
            apply simp
           apply (simp add: substVC_dead)
          apply (simp add: liftE_substE)
          apply (subst substE_substE)
           apply simp
         apply (simp add: substE_dead occursE_liftE)
          apply (simp add: liftE_substE)
          apply (subst substE_substE)
           apply simp
         apply (simp add: substE_dead occursE_liftE)
          apply (simp add: liftE_substE)
          apply (subst substE_substE)
           apply simp
         apply (simp add: substE_dead occursE_liftE)
       apply simp
      apply simp
      done
  qed
qed

end