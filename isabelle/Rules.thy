theory Rules
  imports Syntax CongruenceClosure SubstContext
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

(* k1 = k2 \<rightarrow> \<dots> *)
inductive rule_ULit  where
  rule_ULit_eq: "k1 = k2 \<Longrightarrow> rule_ULit (Uni (Val (Const k1)) (Val (Const k2))) (Val (Const k1))"
| rule_ULit_ne: "k1 \<noteq> k2 \<Longrightarrow> rule_ULit (Uni (Val (Const k1)) (Val (Const k2))) Fail"

(* k1 = k2 \<rightarrow> \<dots> *)
inductive rule_UTup where
  rule_UTup: "length vs1 = length vs2 \<Longrightarrow>
    rule_UTup (Uni (Val (Tup vs1)) (Val (Tup vs2)))
              (seqs ((map2 (\<lambda> v1 v2. Uni (Val v1) (Val v2)) vs1 vs2) @ [Val (Tup vs1)]))"
| rule_UTup_ne: "length vs1 \<noteq> length vs2 \<Longrightarrow> rule_UTup (Uni (Val (Tup vs1)) (Val (Tup vs2))) Fail"

(* various failing unification rules *)
inductive rule_UX where
  rule_UX1: "rule_UX (Uni (Val (Const k)) (Val (Tup vs))) Fail"
| rule_UX2: "rule_UX (Uni (Val (Tup vs)) (Val (Const k))) Fail"
| rule_UX3: "isHNF v \<Longrightarrow> rule_UX (Uni (Val (Lam e)) (Val v)) Fail"
| rule_UX4: "isHNF v \<Longrightarrow> rule_UX (Uni (Val v) (Val (Lam e))) Fail"
| rule_UX5: "isHNF v \<Longrightarrow> rule_UX (Uni (Val (Op op)) (Val v)) Fail"
| rule_UX6: "isHNF v \<Longrightarrow> rule_UX (Uni (Val v) (Val (Op op))) Fail"

(* x = V[x] \<rightarrow> fail *)
inductive rule_UXOccurs where
  rule_UXOccurs: "vc \<noteq> [] \<Longrightarrow> rule_UXOccurs (Uni (Val (Var n)) (Val (appVC' vc (Var n)))) Fail"

(* X[x = v] \<rightarrow> X{v/x}[x = v] *)
inductive rule_Subst where
  rule_Subst: "isX ec \<Longrightarrow> occursEC n ec \<Longrightarrow> \<not> occursV n v \<Longrightarrow> rule_Subst
      (appEC ec               (Uni (Val (Var n)) (Val v)))
      (appEC (substEC n v ec) (Uni (Val (Var n)) (Val v)))"

(* x = V[\<lambda>z. e] \<rightarrow> x = V[\<lambda>z. \<exists>x. x = V[\<lambda>z. e]; e] *)
(* This is hairy with de-Brujin-indices *)
inductive rule_SubstRec where
  rule_SubstRec: "occursE (n + 1) e \<Longrightarrow>
      e1 = substE (n+2) (Var 1) (\<up>\<^sub>e 2 0 e) \<Longrightarrow>
      rhs = Val (appVC (liftVC 2 0 vc) e1) \<Longrightarrow>
      e2 = substE (n+1) (Var 0) (\<up>\<^sub>e 1 0 e) \<Longrightarrow>
    rule_SubstRec
      (Uni (Val (Var n)) (Val (appVC vc e)))
      (Uni (Val (Var n)) (Val (appVC vc (Def (Seq (Uni (Val (Var 0)) rhs) e2)))))"

inductive rule_DefEliml where
  rule_DefEliml: "
      isX ec \<Longrightarrow>
      \<not> occursEC n ec \<Longrightarrow>
      \<not> occursV n v \<Longrightarrow>
    rule_DefEliml
      (Def (appEC (replicate n CDef) (appEC ec (Uni (Val (Var n)) (Val v)))))
           (appEC (replicate n CDef) (appEC (delEC n ec) (Val (delV n v))))"

inductive rule_DefElimr where
  rule_DefElimr: "
      isX ec \<Longrightarrow>
      \<not> occursEC n ec \<Longrightarrow>
      \<not> occursV n v \<Longrightarrow>
    rule_DefElimr
      (Def (appEC (replicate n CDef) (appEC ec (Uni (Val v) (Val (Var n))))))
           (appEC (replicate n CDef) (appEC (delEC n ec) (Val (delV n v))))"

(* hnf = x \<rightarrow> x = hnf *)
inductive rule_Swap where
  rule_SwapK: "isHNF v \<Longrightarrow> rule_Swap (Uni (Val v) (Val (Var n))) (Uni (Val (Var n)) (Val v))"

(* X[\<exists>x.e] \<rightarrow> \<exists>x. X[e] *)
inductive rule_DefFloat where
  rule_DefFloat: "isX ec \<Longrightarrow> ec \<noteq> [] \<Longrightarrow> rule_DefFloat (appEC ec (Def e)) (Def (appEC (liftEC 1 0 ec) e))"

(* v;e \<rightarrow> e *)
inductive rule_Seq where
  rule_Seq: "rule_Seq (Seq (Val v) e) e"

(* (e1;e2) = e3 \<rightarrow> e1 ; (e2=e3) *)
inductive rule_Unify_Seql where
  rule_Unify_Seql: "rule_Unify_Seql (Uni (Seq e1 e2) e3) (Seq e1 (Uni e2 e3))"

(* v = (e1;e2) \<rightarrow> e1 ; (v=e2) *)
inductive rule_Unify_Seqr where
  rule_Unify_Seqr: "rule_Unify_Seqr (Uni (Val v) (Seq e1 e2)) (Seq e1 (Uni (Val v) e2))"

(* (e1 = e2) = e3 \<rightarrow> \<exists> x. x = e1; x = e2; x = e3 *)
inductive rule_Unify_Unifyl where
  rule_Unify_Unifyl: "rule_Unify_Unifyl
    (Uni (Uni e1 e2) e3)
    (Def (Seq (Uni (Val (Var 0)) (liftE 1 0 e1))
         (Seq (Uni (Val (Var 0)) (liftE 1 0 e2))
              (Uni (Val (Var 0)) (liftE 1 0 e3)))))"

(* e1 = (e2 = e3) \<rightarrow> \<exists> x. x = e1; x = e2; x = e3 *)
inductive rule_Unify_Unifyr where
  rule_Unify_Unifyr: "rule_Unify_Unifyr
    (Uni e1 (Uni e2 e3))
    (Def (Seq (Uni (Val (Var 0)) (liftE 1 0 e1))
         (Seq (Uni (Val (Var 0)) (liftE 1 0 e2))
              (Uni (Val (Var 0)) (liftE 1 0 e3)))))"

(* \<exists>x. fail \<rightarrow> fail *)
inductive rule_DefFail where
  rule_DefFail: "rule_DefFail (Def Fail) Fail"

(* X[fail] \<rightarrow> fail *)
inductive rule_Fail where
  rule_Fail: "isX ec \<Longrightarrow> rule_Fail (appEC ec Fail) Fail"

(* one{fail} \<rightarrow> fail *)
inductive rule_OneFail where
  rule_OneFail: "rule_OneFail (One Fail) Fail"

(* one{v | e} \<rightarrow> v *)
inductive rule_OneChoice where
  rule_OneChoice: "rule_OneChoice (One (Bar (Val v) e)) (Val v)"

(* one{v} \<rightarrow> v *)
inductive rule_OneValue where
  rule_OneValue: "rule_OneValue (One (Val v)) (Val v)"

(* all rules about all in one, using the bars helper *)
inductive rule_All where
  rule_All: "rule_All (All (bars (map Val vs))) (Val (Tup vs))"

inductive rule_FailL where
  rule_FailL: "isSXE ece \<Longrightarrow> rule_FailL (appECE ece (Bar Fail e)) (appECE ece Fail)"

inductive rule_FailR where
  rule_FailR: "isSXE ece \<Longrightarrow> rule_FailR (appECE ece (Bar e Fail)) (appECE ece Fail)"

inductive rule_AssocChoice where
  rule_AssocChoice: "isSXE ece \<Longrightarrow>
  rule_AssocChoice (appECE ece (Bar (Bar e1 e2) e3)) (appECE ece (Bar e1 (Bar e2 e3)))"

inductive rule_Choose where
  rule_Choose: "isSXE ece \<Longrightarrow> isCX ec \<Longrightarrow> ec \<noteq> [] \<Longrightarrow>
  rule_Choose (appECE ece (appEC ec (Bar e1 e2))) (appECE ece (Bar (appEC ec e1) (appEC ec e2)))"

section \<open>All rules\<close>

definition "ARs =
  rule_PAdd \<squnion>
  rule_PGt \<squnion>
  rule_App_Beta \<squnion>
  rule_App_Tup \<squnion>
  rule_ULit \<squnion>
  rule_UTup \<squnion>
  rule_UX \<squnion>
  rule_UXOccurs \<squnion>
  rule_Subst \<squnion>
  rule_SubstRec \<squnion>
  rule_DefEliml \<squnion>
  rule_DefElimr \<squnion>
  rule_Swap \<squnion>
  rule_DefFloat \<squnion>
  rule_Seq \<squnion>
  rule_Unify_Seql \<squnion>
  rule_Unify_Seqr \<squnion>
  rule_Unify_Unifyl \<squnion>
  rule_Unify_Unifyr \<squnion>
  rule_DefFail \<squnion>
  rule_OneFail \<squnion>
  rule_OneChoice \<squnion>
  rule_OneValue \<squnion>
  rule_All \<squnion>
  rule_FailL \<squnion>
  rule_FailR \<squnion>
  rule_AssocChoice \<squnion>
  rule_Choose
"

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