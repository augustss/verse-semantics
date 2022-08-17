theory LocalConfluence
imports
  Syntax
  Contexts
  ParallelContexts
  CongruenceClosure
  CCJoin
  Rules
begin

section \<open>Joinability relation\<close>

definition "J = VR\<^sup>*\<^sup>* OO VR\<^sup>*\<^sup>*\<inverse>\<inverse>"

lemma refl_J[simp]: "J x x"
  unfolding J_def by auto

lemma reflp_J[simp]: "reflp J"
  unfolding reflp_def by simp

lemma congruent_J[simp]: "congruent J"
  unfolding J_def VR_def by simp

lemma symp_J[simp]: "symp J"
  unfolding J_def VR_def symp_def by auto

lemma joinI[case_names Peak]:
  assumes "\<And> a b c. R1 a b \<Longrightarrow> R2 a c \<Longrightarrow> S b c"
  shows "R1\<inverse>\<inverse> OO R2 \<le> S"
  using assms by auto

lemma J_VRr[elim]:
  assumes "VR b c"
  assumes "J a c"
  shows "J a b"
  using assms by (auto simp add: J_def VR_def)

lemma J_VRl[elim]:
  assumes "VR c a"
  assumes "J a b"
  shows "J c b"
  using assms by (metis J_VRr sympD symp_J)


lemma J_VRstar[trans]:
  assumes "J a c"
  assumes "VR\<^sup>*\<^sup>* b c"
  shows "J a b"
  using assms by (auto simp add: J_def VR_def)

lemma red_equiv_J: "red_equiv VR J"
  unfolding red_equiv_def
  using converse_relcompp by auto

section \<open>Elementary diagrams at the root\<close>

lemma Seq_Seq: "rule_Seq\<inverse>\<inverse> OO rule_Seq \<le> J"
  by(auto intro!: joinI elim!: rule_Seq.cases)

lemma Seq_Unify_Seql: "rule_Seq\<inverse>\<inverse> OO rule_Unify_Seql \<le> J"
  by(auto intro!: joinI elim!: rule_Seq.cases rule_Unify_Seql.cases)

lemma Seq_Unify_Seqr: "rule_Seq\<inverse>\<inverse> OO rule_Unify_Seqr \<le> J"
  by(auto intro!: joinI elim!: rule_Seq.cases rule_Unify_Seqr.cases)

lemma Unify_Seql_Unify_Seql: "rule_Unify_Seql\<inverse>\<inverse> OO rule_Unify_Seql \<le> J"
  by(auto intro!: joinI elim!: rule_Unify_Seql.cases)

lemma Unify_Seql_Unify_Seqr: "rule_Unify_Seql\<inverse>\<inverse> OO rule_Unify_Seqr \<le> J"
  by(auto intro!: joinI elim!: rule_Unify_Seql.cases rule_Unify_Seqr.cases)

lemma Unify_Seqr_Unify_Seqr: "rule_Unify_Seqr\<inverse>\<inverse> OO rule_Unify_Seqr \<le> J"
  by(auto intro!: joinI elim!: rule_Unify_Seqr.cases)

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


lemma mirror_elementary:
  assumes "R1\<inverse>\<inverse> OO R2 \<le> J"
  shows "R2\<inverse>\<inverse> OO R1 \<le> J"
  by (metis assms converse_relcompp conversep_conversep conversep_mono symp_J symp_conv_conversep_eq)

lemmas root_diagrams
  = Seq_Seq
    Seq_Unify_Seql
    Seq_Unify_Seql[THEN mirror_elementary]
    Seq_Unify_Seqr
    Seq_Unify_Seqr[THEN mirror_elementary]
    Unify_Seql_Unify_Seql
    Unify_Seql_Unify_Seqr
    Unify_Seql_Unify_Seqr[THEN mirror_elementary]
    Unify_Seqr_Unify_Seqr

section \<open>Elementary diagrams not at the root\<close>

lemma Rs_Val[elim!]:
  assumes "Rs (Val v) c"
  obtains False
  using assms unfolding Rs_def
  by (auto elim: Rs_cases)


lemma cc_Val:
  assumes "cc R (Val v) c"
  obtains
    (atVal) "R (Val v) c"
  | (in_Val) vc a b where "cc R a b" and "v = appVC vc a" and "c = Val (appVC vc b)"
  using assms
  apply (elim cc.cases)
  apply (case_tac C)
   apply simp
  apply (case_tac a)
           apply (auto simp add: appEC_def cc.simps)
  apply blast
  done

lemma cc_Seq:
  assumes "cc R (Seq e1 e2) c"
  obtains 
    (here) "R (Seq e1 e2) c"
  | (left) e1' where "cc R e1 e1'" and "c = Seq e1' e2"
  | (right) e2' where "cc R e2 e2'" and "c = Seq e1 e2'"
  using assms
  apply (elim cc.cases)
  apply (case_tac C)
   apply simp
  apply (case_tac a)
           apply (auto simp add: appEC_def cc.simps)
  apply blast
  done

lemma cc'_Seq:
  assumes "cc' R (Seq e1 e2) c"
  obtains (left) e1' where "cc R e1 e1'" and "c = Seq e1' e2"
  | (right) e2' where "cc R e2 e2'" and "c = Seq e1 e2'"
  using assms
  apply (elim cc'.cases)
  apply (case_tac C)
   apply simp
  apply (case_tac a)
           apply (auto simp add: appEC_def cc.simps)
  apply blast
  done


lemma cc'_Uni:
  assumes "cc' R (Uni e1 e2) c"
  obtains (left) e1' where "cc R e1 e1'" and "c = Uni e1' e2"
  | (right) e2' where "cc R e2 e2'" and "c = Uni e1 e2'"
  using assms
  apply (elim cc'.cases)
  apply (case_tac C)
   apply simp
  apply (case_tac a)
           apply (auto simp add: appEC_def cc.simps)
  apply blast
done


lemma Seq_C: "rule_Seq\<inverse>\<inverse> OO cc' Rs \<le> J"
proof (induction rule: joinI)
  case (Peak a b c)
  then show ?case
  proof(induction)
    case (rule_Seq v e)
    from `cc' Rs (Seq (Val v) e) c`
    show ?case
    proof(induct rule: cc'_Seq)
      case (left v')
      thus ?case
      proof(induct rule: cc_Val)
        case atVal
        thus ?case by auto
      next
        case (in_Val vc a b)
        have "VR (Seq (Val (appVC vc b)) e) e" by (intro VR_rootI)
        with `c = _` `v' = _` 
        have "VR c e" by simp
        thus ?thesis by force
      qed
    next
      case (right e')
      have "VR (Seq (Val v) e') e'" by (intro VR_rootI)
      with `c = _`
      have "VR c e'" by simp
      moreover
      from `cc Rs e e'`
      have "VR e e'" unfolding VR_def.
      ultimately
      show ?thesis by force
    qed
  qed
qed

lemma Unify_Seql_C: "rule_Unify_Seql\<inverse>\<inverse> OO cc' Rs \<le> J"
proof (induction rule: joinI)
  case (Peak a b c)
  then show ?case
  proof(induction)
    case (rule_Unify_Seql e1 e2 e3)
    from `cc' Rs (Uni (Seq e1 e2) e3) c`
    show ?case
    proof(induct rule: cc'_Uni)
      case (left e12')
      from `cc Rs (Seq e1 e2) e12'`
      show ?case
      unfolding `c = _`
      proof(induct rule: cc_Seq)
        case here
        (* Overlap with rule Seq *)
        from `Rs (Seq e1 e2) e12'`
        obtain v where "e1 = Val v" and "e12' = e2"
          unfolding Rs_def by (auto elim: Rs_cases)
        have "VR (Seq (Val v) (Uni e2 e3)) (Uni e2 e3)" by (intro VR_rootI)
        thus ?case unfolding `e12' = _` `e1 = _`
          by fastforce
      next
        case (left e1')
        have "VR (Uni (Seq e1' e2) e3) (Seq e1' (Uni e2 e3))" by (intro VR_rootI)
        moreover
        from `cc Rs e1 e1'`
        have "VR e1 e1'" unfolding VR_def.
        hence "VR (Seq e1 (Uni e2 e3)) (Seq e1' (Uni e2 e3))" by (intro VR_C_I)          
        ultimately
        show ?case unfolding `e12' = _` by (fastforce simp add: appEC_def)
      next
        case (right e2')
        have "VR (Uni (Seq e1 e2') e3) (Seq e1 (Uni e2' e3))" by (intro VR_rootI)
        moreover
        from `cc Rs e2 e2'`
        have "VR e2 e2'" unfolding VR_def.
        hence "VR (Seq e1 (Uni e2 e3)) (Seq e1 (Uni e2' e3))" by (intro VR_C_I)          
        ultimately
        show ?case unfolding `e12' = _` by (fastforce simp add: appEC_def)
      qed
    next
      case (right e3')
        have "VR (Uni (Seq e1 e2) e3') (Seq e1 (Uni e2 e3'))" by (intro VR_rootI)
        moreover
        from `cc Rs e3 e3'`
        have "VR e3 e3'" unfolding VR_def.
        hence "VR (Seq e1 (Uni e2 e3)) (Seq e1 (Uni e2 e3'))" by (intro VR_C_I)          
        ultimately
        show ?case unfolding `c = _` by (fastforce simp add: appEC_def)
    qed
  qed
qed

lemma Unify_Seqr_C: "rule_Unify_Seqr\<inverse>\<inverse> OO cc' Rs \<le> J"
proof (induction rule: joinI)
  case (Peak a b c)
  then show ?case
  proof(induction)
    case (rule_Unify_Seqr v e1 e2)
    from `cc' Rs (Uni (Val v) (Seq e1 e2)) c`
    show ?case
    proof(induct rule: cc'_Uni)
      case (left Val')
      from `cc Rs (Val v) Val'`
      show ?case unfolding `c = _`
      proof(induct rule: cc_Val)
        case atVal
        thus ?case by auto
      next
        case (in_Val vc a b)
        from `cc Rs (Val v) _` 
        have "VR (Val (appVC vc a)) (Val (appVC vc b))"
          unfolding `v = _` `Val' = _` VR_def.
        hence "VR (Seq e1 (Uni (Val (appVC vc a)) e2)) (Seq e1 (Uni (Val (appVC vc b)) e2))" by (intro VR_C_I)
        moreover
        have "VR (Uni (Val (appVC vc b)) (Seq e1 e2)) (Seq e1 (Uni (Val (appVC vc b)) e2))" by (intro VR_rootI) 
        ultimately
        show ?case unfolding `v = _` `Val' = _` by force
      qed
    next
      case (right e12')
      from `cc Rs (Seq e1 e2) e12'`
      show ?case
      unfolding `c = _`
      proof(induct rule: cc_Seq)
        case here
        (* Overlap with rule Seq *)
        from `Rs (Seq e1 e2) e12'`
        obtain v2 where "e1 = Val v2" and "e12' = e2"
          unfolding Rs_def by (auto elim: Rs_cases)
        have "VR (Seq (Val v2) (Uni (Val v) e2)) (Uni (Val v) e2)" by (intro VR_rootI)
        thus ?case unfolding `e12' = _` `e1 = _`
          by fastforce
      next
        case (left e1')
        have "VR (Uni (Val v) (Seq e1' e2)) (Seq e1' (Uni (Val v) e2))" by (intro VR_rootI)
        moreover
        from `cc Rs e1 e1'`
        have "VR e1 e1'" unfolding VR_def.
        hence "VR (Seq e1 (Uni (Val v) e2)) (Seq e1' (Uni (Val v) e2))" by (intro VR_C_I)          
        ultimately
        show ?case unfolding `e12' = _` by (fastforce simp add: appEC_def)
      next
        case (right e2')
        have "VR (Uni (Val v) (Seq e1 e2')) (Seq e1 (Uni (Val v) e2'))" by (intro VR_rootI)
        moreover
        from `cc Rs e2 e2'`
        have "VR e2 e2'" unfolding VR_def.
        hence "VR (Seq e1 (Uni (Val v) e2)) (Seq e1 (Uni (Val v) e2'))" by (intro VR_C_I)          
        ultimately
        show ?case unfolding `e12' = _` by (fastforce simp add: appEC_def)
      qed
    qed
  qed
qed

lemmas non_root_diagrams =
  Seq_C
  Unify_Seql_C
  Unify_Seqr_C

theorem local_confluence:
  "VR\<inverse>\<inverse> OO VR \<le> J"
  unfolding VR_def
proof (induct rule: cc_local_confluence)
  case red_equiv show ?case
    using red_equiv_J[unfolded VR_def].
next
  case at_root show ?case
    apply (simp only: Rs_def converse_join relcompp_distrib2 relcompp_distrib)
    apply (intro le_supI root_diagrams)
    done
next
  case below_root show ?case 
    apply (subst Rs_def)
    apply (simp only: converse_join relcompp_distrib2)
    apply (intro le_supI non_root_diagrams)
    done
qed

end
