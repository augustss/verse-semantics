theory OccursCheck
  imports Rules
begin

section \<open>Generalized occurs check\<close>

text \<open>This is lemma “self-tuple” in Ranjit’s notes.\<close>

lemma map2_append:
  assumes "length xs = length ys"
  shows "map2 f (xs @ xs') (ys @ ys') = map2 f xs ys @ map2 f xs' ys'"
using assms
  by (induction xs ys rule:List.list_induct2) auto

lemma seqs_append_Seq[simp]:
  "es2 \<noteq> [] \<Longrightarrow> seqs (es @ [Seq e1 (seqs es2)]) = seqs (es @ e1 # es2)"
  by (induction es) auto

lemma foldr_appECE_map2_CSeqr:
  assumes "length es1 = length es2"
  shows "foldr appECE (map2 (\<lambda>x y. CSeqr (f x y)) es1 es2) e
    = seqs (map2 (\<lambda>x y. f x y) es1 es2 @ [e])"
using assms
by (induction es1 es2 arbitrary: e rule:List.list_induct2) auto

lemma occurs_check: "vc \<noteq> [] \<Longrightarrow> (cc ARs)\<^sup>*\<^sup>* (Uni (Val v) (Val (appVC' vc v))) Fail"
proof(induction v arbitrary: vc rule: measure_induct_rule[where f = size_val])
  case (less v)

  from `vc \<noteq> []` obtain vs1 vs2 vc' where "vc = CTup vs1 vs2 # vc'"
    by (cases vc; simp; case_tac a)

  let ?e1 = "Uni (Val v) (Val (appVC' (CTup vs1 vs2 # vc') v))"
  
  show ?case unfolding `vc = _`
  proof(goal_cases this)
    case this
    show ?case
    proof(cases v)
      case (Var x1)
      have "rule_UXOccurs ?e1 Fail"
        unfolding Var by rule simp
      then show ?thesis by (auto simp add: ARs_def)
    next
      case (Tup vs)

      let ?e2 = "Uni (Val (Tup vs)) (Val (Tup (vs1 @ appVC' vc' (Tup vs) # vs2)))"
  
      have "(cc ARs)\<^sup>*\<^sup>* ?e1 ?e2"
        by (simp add: Tup appVC'_def)
      also have "(cc ARs)\<^sup>*\<^sup>* \<dots> Fail"
      proof(cases "length vs = length vs1 + 1 + length vs2")
        case True
        let ?i = "length vs1"
  
        let ?vc' = "vc' @ [CTup (take ?i vs) (drop (Suc ?i) vs)]"
        from True
        have 2: "appVC' vc' (Tup vs) = appVC' ?vc' (vs ! ?i)"
          apply (subst id_take_nth_drop[of ?i vs]) apply simp
          apply (auto simp add: appVC'_def)
          done
  
        have "rule_UTup ?e2
          (seqs ((map2 (\<lambda> v1 v2. Uni (Val v1) (Val v2)) vs (vs1 @ appVC' vc' (Tup vs) # vs2)) @ [Val (Tup vs)]))"
          by (auto simp add: Tup True intro!: rule_UTup.intros)
        hence "(cc ARs)\<^sup>+\<^sup>+ ?e2 ..."
          by (auto simp add: ARs_def)
        also
        let ?ec = "map2 (\<lambda> v1 v2. CSeqr (Uni (Val v1) (Val v2))) (take ?i vs) vs1 @
          [CSeql (seqs ((map2 (\<lambda> v1 v2. Uni (Val v1) (Val v2)) (drop (Suc ?i) vs) vs2) @ [Val (Tup vs)]))]"
        let ?unif = "Uni (Val (vs ! ?i)) (Val (appVC' ?vc' (vs ! ?i)))"
        from True
        have "(cc ARs)\<^sup>*\<^sup>* ... (appEC ?ec ?unif)" (* really =, but that makes also go slow *)
          apply (subst id_take_nth_drop[of ?i vs]) apply simp
          apply (simp add: map2_append appEC_def foldr_appECE_map2_CSeqr 2)
          done
        also
        have "size_val (vs ! length vs1) < size_val v"
          by (metis One_nat_def True Tup less_add_one linorder_not_less nth_mem size_list_estimation' trans_less_add1 val.size(3))
        hence "(cc ARs)\<^sup>*\<^sup>* ?unif Fail"
          by (rule less(1)) simp
        hence "(cc ARs)\<^sup>*\<^sup>* (appEC ?ec ?unif) (appEC ?ec Fail)"
          by (rule congruentE[OF congruent_star[OF congruent_cc]])
        also
        have "isX ?ec" by (auto simp add: isX_def isXE.simps)
        hence "rule_Fail (appEC ?ec Fail) Fail" by rule simp
        hence "(cc ARs)\<^sup>*\<^sup>* (appEC ?ec Fail) Fail" by (auto simp add: ARs_def)
        finally
        show ?thesis by (rule tranclp_into_rtranclp)
      next
        case False
        hence "rule_UTup ?e2 Fail" by (auto simp add: Tup intro!: rule_UTup.intros)
        thus ?thesis by (auto simp add: ARs_def)
      qed
      finally show ?thesis.
    (* The remaining cases are all a bit boring *)
    next
      case (Const x2)
      have "rule_UX ?e1 Fail"
        unfolding Const
        by (auto simp add: appVC'_def intro!: rule_UX.intros)
      then show ?thesis by (auto simp add: ARs_def)
    next
      case (Lam x4)
      have "rule_UX ?e1 Fail"
        unfolding Lam `vc = _`
        by (auto simp add: appVC'_def intro!: rule_UX.intros simp add: isHNF.simps)
      then show ?thesis by (auto simp add: ARs_def)
    next
      case (Op x5)
      from `vc \<noteq> []`
      have "rule_UX ?e1 Fail"
        unfolding Op `vc = _`
        by (auto simp add: appVC'_def intro!: rule_UX.intros simp add: isHNF.simps)
      then show ?thesis by (auto simp add: ARs_def)
    qed
  qed
qed



end