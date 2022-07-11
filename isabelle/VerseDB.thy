theory VerseDB imports Main begin

datatype "exp" =
  Val val
| Seq exp exp
| Bar exp exp
| App val val
| Def exp
| One exp
| All exp
and val =
  Var nat
| Const int
| Tup "val list"
| Lam exp


fun liftE :: "nat \<Rightarrow> nat \<Rightarrow> exp \<Rightarrow> exp" ("\<up>\<^sub>e")
and liftV :: "nat \<Rightarrow> nat \<Rightarrow> val \<Rightarrow> val" ("\<up>\<^sub>v")
where
  "\<up>\<^sub>e n k (Val v) = Val (\<up>\<^sub>v n k v)"
| "\<up>\<^sub>e n k (Seq e1 e2) = (Seq (\<up>\<^sub>e n k e1) (\<up>\<^sub>e n k e2))"
| "\<up>\<^sub>e n k (Bar e1 e2) = (Bar (\<up>\<^sub>e n k e1) (\<up>\<^sub>e n k e2))"
| "\<up>\<^sub>e n k (App e1 e2) = (App (\<up>\<^sub>v n k e1) (\<up>\<^sub>v n k e2))"
| "\<up>\<^sub>e n k (Def e) = Def (\<up>\<^sub>e n (k+1) e)"
| "\<up>\<^sub>e n k (One e) = (One e)"
| "\<up>\<^sub>e n k (All e) = (All e)"
| "\<up>\<^sub>v n k (Var i) = Var (if i < k then i else i + n)"
| "\<up>\<^sub>v n k (Const c) = (Const c)"
| "\<up>\<^sub>v n k (Tup vs) = Tup (map (\<up>\<^sub>v n k) vs)"
| "\<up>\<^sub>v n k (Lam e) = Lam (\<up>\<^sub>e n (k+1) e)"

section \<open>Contexts\<close>

text \<open>Context element\<close>

datatype vce =
  CTup "val list" "val list"

type_synonym vc = "vce list"

datatype ece =
  CVal vc (* Implicit CLam at the end*)
| CSeql exp
| CSeqr exp
| CBarl exp
| CBarr exp
| CAppl vc val
| CAppr val vc
| CDef
| COne
| CAll

type_synonym ec = "ece list"

fun appVCE :: "vce \<Rightarrow> val \<Rightarrow> val" where
  "appVCE (CTup vs1 vs2) v = Tup (vs1 @ [v] @ vs2)"

lemma appVCE_inj[simp]: "appVCE vce v1 = appVCE vce v2 \<longleftrightarrow> v1 = v2"
  by (cases vce) (auto)

definition appVC' ::  "vc \<Rightarrow> val \<Rightarrow> val" where
  "appVC' vc v = foldr appVCE vc v"

definition appVC ::  "vc \<Rightarrow> exp \<Rightarrow> val" where
  "appVC vc e = appVC' vc (Lam e)"


lemma appVC'_inj[simp]: "appVC' vc v1 = appVC' vc v2 \<longleftrightarrow> v1 = v2"
  by (induction vc) (auto simp add: appVC'_def)

lemma appVC_inj[simp]: "appVC vc v1 = appVC vc v2 \<longleftrightarrow> v1 = v2"
  by (simp add: appVC_def)


fun appECE :: "ece \<Rightarrow> exp \<Rightarrow> exp" where
  "appECE (CVal vc) e = Val (appVC vc e)"
| "appECE (CSeql e2) e1 = Seq e1 e2"
| "appECE (CSeqr e1) e2 = Seq e1 e2"
| "appECE (CBarl e2) e1 = Bar e1 e2"
| "appECE (CBarr e1) e2 = Bar e1 e2"
| "appECE (CAppl vc v) e = App (appVC vc e) v"
| "appECE (CAppr v vc) e = App v (appVC vc e)"
| "appECE CDef e = Def e"
| "appECE COne e = One e"
| "appECE CAll e = All e"

definition appEC ::  "ec \<Rightarrow> exp \<Rightarrow> exp" where
   "appEC ec e = foldr appECE ec e"

lemma appEC_nil[simp]:
  "appEC [] e = e"
  by (simp add: appEC_def)

lemma appEC_append:
  "appEC (ec1 @ ec2) e = appEC ec1 (appEC ec2 e)"
  by (simp add: appEC_def)

lemma appECE_inj[simp]: "appECE ece x = appECE ece y \<longleftrightarrow> x = y"
  by (cases ece) (auto)

lemma appEC_inj[simp]: "appEC ec x = appEC ec y \<longleftrightarrow> x = y"
  by (induction ec) (auto simp add: appEC_def)

type_synonym red = "exp \<Rightarrow> exp \<Rightarrow> bool"

inductive congruent :: "red \<Rightarrow> bool"  where
  congruentI: "(\<And> x y C. R x y \<Longrightarrow> R (appEC C x) (appEC C y)) \<Longrightarrow> congruent R"

lemma congruentE[elim, consumes 2]:
  assumes "congruent R" and "R x y"
  shows "R (appEC C x) (appEC C y)"
  using assms
  by (simp add: congruent.simps)

lemma congruent_OO[simp]:
  assumes "congruent R" and "congruent S"
  shows "congruent (R OO S)"
  using assms
  by(auto intro!: congruentI)

lemma congruent_inv[simp]:
  assumes "congruent R"
  shows "congruent (R\<inverse>\<inverse>)"
  using assms by(auto intro!: congruentI)

lemma congruent_star[simp]:
  assumes "congruent R"
  shows "congruent R\<^sup>*\<^sup>*"
proof
  fix x y C
  assume "R\<^sup>*\<^sup>* x y"
  then show "R\<^sup>*\<^sup>* (appEC C x) (appEC C y)"
  proof (induction rule: converse_rtranclp_induct)
    case base
    then show ?case..
  next
    case (step x z)
    from `R x z`
    have "R (appEC C x) (appEC C z)" using  `congruent R`
      using congruentE by blast
    with `R\<^sup>*\<^sup>* (appEC C z) (appEC C y)`
    show ?case by auto
  qed
qed

inductive cc :: "red \<Rightarrow> red" for R where
  ccI: "R x y \<Longrightarrow> cc R (appEC C x) (appEC C y)"

lemma cc_rootI[intro]: "R x y \<Longrightarrow> cc R x y"
  by (drule ccI[of _ _ _ "[]"]) (simp add: appEC_def)

inductive cc' :: "red \<Rightarrow> red" for R where
  cc'I: "C \<noteq> [] \<Longrightarrow> R x y \<Longrightarrow> cc' R (appEC C x) (appEC C y)"


lemma congruent_cc[simp]:
  "congruent (cc R)"
  by (auto intro!: congruentI elim!:cc.cases 
           simp add: appEC_append[symmetric] intro: cc.intros)

inductive rule_Seq where
  rule_Seq: "rule_Seq (Seq (Val v) e) e"

definition "Rs = rule_Seq"
definition "VR = cc Rs"

lemma transitive_VR[trans]:
  "VR a b \<Longrightarrow> VR b c \<Longrightarrow> VR\<^sup>*\<^sup>* a c"
  by auto

lemma congruent_VR[simp]: "congruent VR"
  unfolding VR_def VR_def by simp

section \<open>Parallel context\<close>

definition "parallelEC ec1 ec2 \<longleftrightarrow>
  (\<forall> a b a' b'.
  appEC ec1 a = appEC ec2 b \<longrightarrow>
  (\<exists> ec1' ec2'.
  appEC ec1 a' = appEC ec2' b \<and>
  appEC ec2 b' = appEC ec1' a \<and>
  appEC ec2' b' = appEC ec1' a'))"

lemma parallelECE:
  assumes neq: "ece1 \<noteq> ece2"
  shows "parallelEC [ece1] [ece2]"
proof-
{ fix a b a' b'
  assume eq: "appECE ece1 a = appECE ece2 b"
  have "\<exists> ec1' ec2'.
      appECE ece1 a' = appEC ec2' b \<and>
      appECE ece2 b' = appEC ec1' a \<and>
       appEC ec2' b' = appEC ec1' a'"
  proof(cases ece1)
    case (CVal vc1)
    with eq
    show ?thesis
    proof (cases ece2)
      case (CVal vc2)
      then show ?thesis sorry
    qed(auto)
  next
    case (CSeql e1)
    with eq neq
    show ?thesis
    proof(cases ece2)
      case (CSeqr e2)
      with CSeql eq have "e1 = b" and "e2 = a" by simp_all
      show ?thesis
      unfolding CSeql CSeqr `e1 = _` `e2 = _`
      proof(intro exI conjI)
        show "appECE (CSeql b) a' = appEC [CSeqr a'] b" by (simp add: appEC_def) 
      next
        show "appECE (CSeqr a) b' = appEC [CSeql b'] a" by (simp add: appEC_def)
      next
        show "appEC [CSeqr a'] b' = appEC [CSeql b'] a'" by (simp add: appEC_def)
      qed
    qed auto
  next
    case (CSeqr x3)
    then show ?thesis sorry
  next
    case (CBarl x4)
    then show ?thesis sorry
  next
    case (CBarr x5)
    then show ?thesis sorry
  next
    case (CAppl x61 x62)
    then show ?thesis sorry
  next
    case (CAppr x71 x72)
    then show ?thesis sorry
  next
    case CDef
    then show ?thesis sorry
  next
    case COne
    then show ?thesis sorry
  next
    case CAll
    then show ?thesis sorry
  qed
} thus ?thesis 
  by (auto simp add: parallelEC_def appEC_def)
qed  

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

lemma cc_local_confluence:
  assumes "congruent S"
  assumes "symp S"
  assumes "reflp S"
  assumes R_left: "cc R OO S \<le> S"
  assumes R_right: "S OO (cc R)\<inverse>\<inverse> \<le> S"
  assumes at_root: "R\<inverse>\<inverse> OO R \<le> S"
  assumes below_root: "R\<inverse>\<inverse> OO cc' R \<le> S"
  shows "(cc R)\<inverse>\<inverse> OO cc R \<le> S"
proof-
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
      hence "parallelEC [ece1] [ece2]" by (rule parallelECE)
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

section \<open>Elementary diagrams at the root\<close>

lemma Seq_Seq: "rule_Seq\<inverse>\<inverse> OO rule_Seq \<le> J"
  by(auto intro!: joinI elim!: rule_Seq.cases)

section \<open>Elementary diagrams not at the root\<close>

lemma Rs_Val[elim!]:
  assumes "Rs (Val v) c"
  obtains False
  using assms unfolding Rs_def
  by (auto elim: rule_Seq.cases)


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
        have "VR (Seq v' e) e" unfolding VR_def Rs_def `v' = _`
          by (intro cc_rootI rule_Seq.intros)
        with `c = _`
        have "VR c e" by simp
        thus ?thesis by force
      qed
    next
      case (right e')
      have "VR (Seq (Val v) e') e'" unfolding VR_def Rs_def
        by (intro cc_rootI rule_Seq.intros)
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


theorem local_confluence:
  "VR\<inverse>\<inverse> OO VR \<le> J"
  unfolding VR_def Rs_def
  apply (rule cc_local_confluence)
     apply simp
       apply simp
      apply simp
  using J_VRl Rs_def VR_def apply auto[1]
  using OO_def Rs_def VR_def apply auto[1]
  apply (rule Seq_Seq)
  apply (rule Seq_C[unfolded Rs_def])
  done


end