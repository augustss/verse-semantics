theory Rules
  imports Syntax CongruenceClosure
begin

section \<open>The actual rules\<close>

(* v;e \<rightarrow> e *)
inductive rule_Seq where
  rule_Seq: "rule_Seq (Seq (Val v) e) e"

(* (e1;e2) = e3 \<rightarrow> e1 ; (e2=e3) *)
inductive rule_Unify_Seql where
  rule_Unify_Seql: "rule_Unify_Seql (Uni (Seq e1 e2) e3) (Seq e1 (Uni e2 e3))"

(* v = (e1;e2) \<rightarrow> e1 ; (v=e2) *)
inductive rule_Unify_Seqr where
  rule_Unify_Seqr: "rule_Unify_Seqr (Uni (Val v) (Seq e1 e2)) (Seq e1 (Uni (Val v) e2))"


definition "Rs = (rule_Seq \<squnion> rule_Unify_Seql \<squnion> rule_Unify_Seqr)"
definition "VR = cc Rs"

lemmas releqD1 = iffD1[OF fun_cong[OF fun_cong]]
lemmas releqD2 = iffD2[OF fun_cong[OF fun_cong]]

lemmas Rs_intros =
  Rs_def[THEN releqD2, OF sup2I1, OF sup2I1, OF rule_Seq]
  Rs_def[THEN releqD2, OF sup2I1, OF sup2I2, OF rule_Unify_Seql]
  Rs_def[THEN releqD2, OF sup2I2, OF rule_Unify_Seqr]

lemmas VR_rootI = Rs_intros[THEN VR_def[THEN releqD2, OF cc_rootI]]


(* Can we make this about Rs only? https://stackoverflow.com/q/72950360/946226 *)
lemmas Rs_cases = rule_Seq.cases rule_Unify_Seql.cases rule_Unify_Seqr.cases


lemma transitive_VR[trans]:
  "VR a b \<Longrightarrow> VR b c \<Longrightarrow> VR\<^sup>*\<^sup>* a c"
  by auto

lemma congruent_VR[simp]: "congruent VR"
  unfolding VR_def VR_def by simp

lemmas congruentI =
  congruentE[of _ _ _  "[CSeql e]" for e, simplified appEC_def, simplified]
  congruentE[of _ _ _  "[CSeqr e]" for e, simplified appEC_def, simplified]
  congruentE[of _ _ _  "[CBarl e]" for e, simplified appEC_def, simplified]
  congruentE[of _ _ _  "[CBarr e]" for e, simplified appEC_def, simplified]
  congruentE[of _ _ _  "[CUnil e]" for e, simplified appEC_def, simplified]
  congruentE[of _ _ _  "[CUnir e]" for e, simplified appEC_def, simplified]
  congruentE[of _ _ _  "[CDef]", simplified appEC_def, simplified]
  congruentE[of _ _ _  "[COne]" for e, simplified appEC_def, simplified]
  congruentE[of _ _ _  "[CAll]" for e, simplified appEC_def, simplified]

lemmas VR_C_I = congruentI[OF congruent_VR]
 

end