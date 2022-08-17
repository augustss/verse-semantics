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
