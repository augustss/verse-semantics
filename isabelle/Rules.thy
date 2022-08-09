theory Rules
  imports Syntax CongruenceClosure Subst
begin

section \<open>The actual rules\<close>

(* add@<k1,k2> \<rightarrow> (k1 + k2) *)
inductive rule_PAdd where
  rule_PAdd: "rule_PAdd (App (Op add_op) (Tup [Const k1, Const k2])) (Val (Const (k1 + k2)))"

(* gt@<k1,k2> \<rightarrow> \<dots> *)
inductive rule_PGt where
  rule_PGt: "k1 > k2 \<Longrightarrow> rule_PGt (App (Op gt_op) (Tup [Const k1, Const k2])) (Val (Const k1))"
| rule_PGt_fail: "k1 \<le> k2 \<Longrightarrow> rule_PGt (App (Op gt_op) (Tup [Const k1, Const k2])) Fail"

(* (\<lambda>x. e)@v \<rightarrow> \<exists> x. x=v;e *)
inductive rule_App_Beta where
  rule_App_Beta: "rule_App_Beta (App (Lam e) v) (Def (Seq (Uni (Val (Var 0)) (Val (\<up>\<^sub>v 1 0 v))) e))"

(* <vs>@v \<rightarrow> \<dots> *)
inductive rule_App_Tup where
  rule_App_Tup: "rule_App_Tup (App (Tup vs) v) (bars (map (\<lambda>(k, v'). Seq (Uni (Val v) (Val (Const i))) (Val v')) (enumerate 0 vs)))"

(* v;e \<rightarrow> e *)
inductive rule_Seq where
  rule_Seq: "rule_Seq (Seq (Val v) e) e"

(* (e1;e2) = e3 \<rightarrow> e1 ; (e2=e3) *)
inductive rule_Unify_Seql where
  rule_Unify_Seql: "rule_Unify_Seql (Uni (Seq e1 e2) e3) (Seq e1 (Uni e2 e3))"

(* v = (e1;e2) \<rightarrow> e1 ; (v=e2) *)
inductive rule_Unify_Seqr where
  rule_Unify_Seqr: "rule_Unify_Seqr (Uni (Val v) (Seq e1 e2)) (Seq e1 (Uni (Val v) e2))"

section \<open>All rules\<close>

definition "ARs =
  rule_PAdd \<squnion>
  rule_PGt \<squnion>
  rule_App_Beta \<squnion>
  rule_App_Tup \<squnion>
  rule_Seq \<squnion>
  rule_Unify_Seql \<squnion>
  rule_Unify_Seqr"

section \<open>The rules as used in the local confluence proof\<close>

text \<open>These are not yet all rules, as the confluence proofs does not scale well.
If we add other theorems we can define other rule sets.\<close>

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