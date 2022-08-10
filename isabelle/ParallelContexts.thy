theory ParallelContexts
  imports Contexts
begin

section \<open>Parallel context\<close>

definition "parallelEC ec1 ec2 \<longleftrightarrow>
  (\<forall> a b a' b'.
  appEC ec1 a = appEC ec2 b \<longrightarrow>
  (\<exists> ec1' ec2'.
  appEC ec1 a' = appEC ec2' b \<and>
  appEC ec2 b' = appEC ec1' a \<and>
  appEC ec2' b' = appEC ec1' a'))"

definition "parallelECE ece1 ece2 \<longleftrightarrow>
  (\<forall> a b a' b'.
  appECE ece1 a = appECE ece2 b \<longrightarrow>
  (\<exists> ece1' ece2'.
  appECE ece1 a' = appECE ece2' b \<and>
  appECE ece2 b' = appECE ece1' a \<and>
  appECE ece2' b' = appECE ece1' a'))"


definition "parallelVC vc1 vc2 \<longleftrightarrow>
  (\<forall> a b a' b'.
  appVC vc1 a = appVC vc2 b \<longrightarrow>
  (\<exists> vc1' vc2'.
  appVC vc1 a' = appVC vc2' b \<and>
  appVC vc2 b' = appVC vc1' a \<and>
  appVC vc2' b' = appVC vc1' a'))"


lemma parallelVC[intro!]:
  assumes neq: "vc1 \<noteq> vc2"
  shows "parallelVC vc1 vc2"
using assms
proof(induct vc1 vc2 rule: list_induct2'[case_names Nil_Nil Cons_Nil Nil_Cons Cons_Cons])
  case Nil_Nil
  then show ?case by auto
next
  case (Cons_Nil vce vc)
  then show ?case by(cases vce)(auto simp add: parallelVC_def appVC_def appVC'_def)
next
  case (Nil_Cons vce vc)
  then show ?case by(cases vce)(auto simp add: parallelVC_def appVC_def appVC'_def)
next
  case (Cons_Cons vce1 vc1 vce2 vc2)
  show ?case
  proof(cases "vce1 = vce2")
    case True with Cons_Cons have "parallelVC vc1 vc2" by auto
    with True show ?thesis
      apply (cases vce1)
      apply (auto simp add: parallelVC_def )
      (* Ugly! *)
      apply (elim allE) apply (erule impE) apply (assumption)
      apply (erule_tac x = a' in allE)
      apply (erule_tac x = b' in allE)
      apply (erule exE)+
      apply (rule_tac x= "CTup x1 x2 # vc1'" in exI)
      apply (rule_tac x= "CTup x1 x2 # vc2'" in exI)
      apply auto
      done
  next
    case False
    then show ?thesis
    proof (cases vce1; cases vce2)
      fix vs1 vs1' vs2 vs2'
      assume "vce1 = CTup vs1 vs1'" "vce2 = CTup vs2 vs2'"
      show ?thesis
      proof(cases "length vs1 + length vs1' = length vs2 + length vs2'")
        case True
        consider "length vs1 < length vs2" | "length vs1 = length vs2" | "length vs1 > length vs2"
          by fastforce     
        thus ?thesis
        proof(cases)
          case 1          
          then show ?thesis
          using `vce1 = _` `vce2 = _` True
          apply (auto simp add: parallelVC_def )
            (* Ugly! *)
          apply (rule_tac x= "CTup vs1 (list_update vs1' (length vs2 - length vs1 - 1) (appVC vc2 b'))
                                   # vc1" in exI)
          apply (rule_tac x= "CTup (list_update vs2 (length vs1) (appVC vc1 a')) vs2' # vc2" in exI)        
          apply (auto simp add: list_update_append1)
            apply (metis list_update_append1 list_update_length)
           apply (metis Suc_diff_Suc less_Suc_eq list_update_append list_update_code(3) list_update_length not_less_eq)          
          apply (smt (verit, ccfv_SIG) Suc_diff_Suc less_Suc_eq list_update_append list_update_code(3) list_update_length not_less_eq) 
          done
        next
          case 2 thus ?thesis
            using  `vce1 \<noteq> vce2`  `vce1 = _` `vce2 = _` by (auto simp add: parallelVC_def)
        next
          case 3
          then show ?thesis
          using `vce1 = _` `vce2 = _` True
          apply (auto simp add: parallelVC_def )
            (* Ugly! *)
          apply (rule_tac x= "CTup (list_update vs1 (length vs2) (appVC vc2 b')) vs1' # vc1" in exI)
          apply (rule_tac x= "CTup vs2 (list_update vs2' (length vs1 - length vs2 - 1) (appVC vc1 a')) # vc2" in exI)        
          apply (auto simp add: list_update_append1)
           apply (metis Suc_diff_Suc less_Suc_eq list_update_append list_update_code(3) list_update_length not_less_eq)          
            apply (metis list_update_append1 list_update_length)
          apply (smt (verit, ccfv_SIG) Suc_diff_Suc less_Suc_eq list_update_append list_update_code(3) list_update_length not_less_eq) 
          done
      qed        
      next
        case False
        hence [simp]: "vs1 @ v1 # vs1' \<noteq> vs2 @ v2 # vs2'" for v1 v2
          by (auto dest!: arg_cong[of _ _ length])
        then show ?thesis
          using `vce1 = _` `vce2 = _`
          by (auto simp add: parallelVC_def appVC_def appVC'_def)
      qed
    qed
  qed  
qed

lemma parallelVC_CVal_ECE[intro!]:
  assumes "parallelVC vc1 vc2"
  shows "parallelECE (CVal vc1) (CVal vc2)"
  using assms
  by (fastforce simp add: parallelVC_def parallelECE_def)

lemma parallelVC_CAppl_ECE[intro!]:
  assumes "parallelVC vc1 vc2 \<or> (v1 \<noteq> v2)"
  shows "parallelECE (CAppl vc1 v1) (CAppl vc2 v2)"
  using assms
  apply (auto simp add: parallelVC_def parallelECE_def)
  by (metis appECE.simps(8))

lemma parallelVC_CAppr_ECE[intro!]:
  assumes "parallelVC vc1 vc2 \<or> (v1 \<noteq> v2)"
  shows "parallelECE (CAppr v1 vc1) (CAppr v2 vc2)"
  using assms
  apply (auto simp add: parallelVC_def parallelECE_def)
  by (metis appECE.simps(9))


lemma parallelECE:
  assumes neq: "ece1 \<noteq> ece2"
  shows "parallelECE ece1 ece2"
using assms
proof(cases ece1)
  case CVal
  thus ?thesis using assms
  proof(cases ece2)
    case (CVal x1)
    then show ?thesis using `ece1 = _` neq by auto
  qed(auto simp add: parallelECE_def)
next case CSeql with neq show ?thesis unfolding parallelECE_def by fastforce
next case CSeqr with neq show ?thesis unfolding parallelECE_def by fastforce
next case CBarl with neq show ?thesis unfolding parallelECE_def by fastforce
next case CBarr with neq show ?thesis unfolding parallelECE_def by fastforce
next case CUnil with neq show ?thesis unfolding parallelECE_def by fastforce
next case CUnir with neq show ?thesis unfolding parallelECE_def by fastforce
next case (CAppl vc1 v1)
  thus ?thesis using assms
  proof(cases ece2)
    case (CAppl vc2 v2)
    then show ?thesis using `ece1 = _` neq by auto
  next
    case (CAppr v2 vc2)
    then show ?thesis using `ece1 = _` neq 
      apply (auto simp add: parallelECE_def)
      apply (rule_tac x= "CAppl vc1 (appVC vc2 b')" in exI)
      apply (rule_tac x= "CAppr (appVC vc1 a') vc2" in exI)
      apply auto
      done
  qed(auto simp add: parallelECE_def)
next
next case (CAppr v1 vc1)
  thus ?thesis using assms
  proof(cases ece2)
    case (CAppr v2 vc2)
    then show ?thesis using `ece1 = _` neq by auto
  next
    case (CAppl vc2 v2)
    then show ?thesis using `ece1 = _` neq 
      apply (auto simp add: parallelECE_def)
      apply (rule_tac x= "CAppr (appVC vc2 b') vc1" in exI)
      apply (rule_tac x= "CAppl vc2 (appVC vc1 a')" in exI)
      apply auto
      done
  qed(auto simp add: parallelECE_def)
next case CDef with neq show ?thesis unfolding parallelECE_def by fastforce
next case COne with neq show ?thesis unfolding parallelECE_def by fastforce
next case CAll with neq show ?thesis unfolding parallelECE_def by fastforce
qed

lemma parallelEC_singleton:
  assumes neq: "parallelECE ece1 ece2"
  shows "parallelEC [ece1] [ece2]"
  using assms
  unfolding parallelECE_def parallelEC_def
  apply (auto simp add: appEC_def)
  apply (elim allE) apply (erule impE) apply (assumption)
  apply (erule_tac x = a' in allE)
  apply (erule_tac x = b' in allE)
  apply (erule exE)+
  apply (rule_tac x= "[ece1']" in exI)
  apply (rule_tac x= "[ece2']" in exI)
  apply auto
  done


lemma parallelEC_append1:
  assumes "parallelEC ec1 ec2"
  shows "parallelEC (ec1 @ ec1') (ec2 @ ec2')"
  unfolding parallelEC_def
proof(intro allI impI; goal_cases)
  case (1 a b a' b')
  hence "appEC ec1 (appEC ec1' a) = appEC ec2 (appEC ec2' b)"
    by (simp add: appEC_append)
  with assms
    obtain ec1'' ec2''
    where "appEC ec1 (appEC ec1' a') = appEC ec2'' (appEC ec2' b)"
      and "appEC ec2 (appEC ec2' b') = appEC ec1'' (appEC ec1' a)"
      and "appEC ec2'' (appEC ec2' b') = appEC ec1'' (appEC ec1' a')"
    unfolding parallelEC_def by blast
  show ?case
  proof(intro exI conjI)
    show "appEC (ec1 @ ec1') a' = appEC (ec2'' @ ec2') b"
      using `appEC ec1 (appEC ec1' a') = _ ` by (simp add: appEC_def)
  next  
    show "appEC (ec2 @ ec2') b' = appEC (ec1'' @ ec1') a"
      using `appEC ec2 (appEC ec2' b') = _ ` by (simp add: appEC_def)
  next
    show "appEC (ec2'' @ ec2') b' =  appEC (ec1'' @ ec1') a'"
      using `appEC ec2'' _ = _` by (simp add: appEC_def)
  qed
qed

lemma parallelCons2:
  assumes "parallelEC ec1 ec2"
  shows "parallelEC (ece # ec1) (ece # ec2)"
  unfolding parallelEC_def
proof(intro allI impI; goal_cases)
  case (1 a b a' b')
  hence "appEC ec1 a = appEC ec2 b" by (simp add: appEC_def)
  with assms
    obtain ec1' ec2'
    where "appEC ec1 a' = appEC ec2' b"
      and "appEC ec2 b' = appEC ec1' a"
      and "appEC ec2' b' = appEC ec1' a'"
    unfolding parallelEC_def by blast
  show ?case
  proof(intro exI conjI)
    show "appEC (ece # ec1) a' = appEC (ece # ec2') b"
      using `appEC ec1 a' = _ ` by (simp add: appEC_def)
  next  
    show "appEC (ece # ec2) b' = appEC (ece # ec1') a"
      using `appEC ec2 b' = _ ` by (simp add: appEC_def)
  next
    show "appEC (ece # ec2') b' = appEC (ece # ec1') a'"
      using `appEC ec2' b' = _` by (simp add: appEC_def)
  qed
qed


end