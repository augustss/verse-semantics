theory Beta
imports SubstContext Rules
begin

section \<open>Beta reduction\<close>

lemma beta_reduct: "(cc ARs)\<^sup>*\<^sup>* (App (Lam e) v) (del 0 (subst 0 (\<up> v) e))"
proof-

  have "rule_App_Beta (exp.App (Lam e) v) (VLet (\<up> v) e)"..
  hence "(cc ARs)\<^sup>*\<^sup>* (exp.App (Lam e) v) (VLet (\<up> v) e)"  unfolding ARs_def by auto

  also

  have "(cc ARs)\<^sup>*\<^sup>* (VLet (\<up> v) e) (VLet (\<up> v) (subst 0 (\<up> v) e))"
  proof(cases "occurs 0 e")
  case True
    have "rule_Subst (appEC [CSeql e] ((Uni (Val (Syntax.Var 0)) (Val (\<up> v)))))
                     (appEC (Subst.subst 0 (\<up> v) [CSeql e]) ((Uni (Val (Syntax.Var 0)) (Val (\<up> v)))))"
      apply rule
        apply (simp add: isX_def isXE.simps)
       apply (simp add: occurs_ece_def) apply (rule True)
      apply simp
      done
    hence "rule_Subst (Seq ((Uni (Val (Var 0)) (Val (\<up> v)))) e)
                     (Seq ((Uni (Val (Syntax.Var 0)) (Val (\<up> v)))) (subst 0 (\<up> v) e))"
      by (simp add: appEC_def)
    hence "ARs (Seq ((Uni (Val (Var 0)) (Val (\<up> v)))) e)
                     (Seq ((Uni (Val (Var 0)) (Val (\<up> v)))) (subst 0 (\<up> v) e))"
      unfolding ARs_def by auto
    hence "cc ARs (Def (Seq ((Uni (Val (Var 0)) (Val (\<up> v)))) e))
                  (VLet (\<up> v) (subst 0 (\<up> v) e))"
      by (rule ccI[where C = "[CDef]", simplified appEC_def, simplified])
    thus ?thesis
      by auto
  next
    case False
    hence "subst 0 (\<up> v) e = e"
      by (simp add: bind_id_iff)
    thus ?thesis by simp
  qed
  also

  have "rule_DefEliml (VLet (\<up> v) (subst 0 (\<up> v) e))
                      ((Seq (Val (del 0 (\<up> v))) (del 0 (subst 0 (\<up> v) e))))"
    apply (rule rule_DefEliml[where n = 0 and ec = "[CSeql (Subst.subst 0 (\<up> v) e)]" , simplified appEC_def, simplified])
        apply (simp add: isX_def isXE.simps)
     apply (simp add: occurs_ece_def)
    apply simp
    done
  hence "(cc ARs)\<^sup>+\<^sup>+ (VLet (\<up> v) (subst 0 (\<up> v) e))
                      ((Seq (Val (del 0 (\<up> v))) (del 0 (subst 0 (\<up> v) e))))" 
    unfolding ARs_def by auto
  also

  have "rule_Seq  ((Seq (Val (del 0 (\<up> v))) (del 0 (subst 0 (\<up> v) e))))
      (del 0 (subst 0 (\<up> v) e))"
    by (rule rule_Seq)
  hence "(cc ARs)\<^sup>*\<^sup>* ((Seq (Val (del 0 (\<up> v))) (del 0 (subst 0 (\<up> v) e))))
      (del 0 (subst 0 (\<up> v) e))"
    unfolding ARs_def by auto

  finally
  show ?thesis  by auto
qed

end
